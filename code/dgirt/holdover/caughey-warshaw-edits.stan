## Stan code for dynamic group-level IRT model
data {

    int<lower=1> G; ## number of covariate groups
    int<lower=1> Gnat; ## number of national-level demographic groups

    int<lower=1> Q; ## number of items/questions
    int<lower=1> T; ## number of years
    int<lower=1> N; ## number of observed cells
    int<lower=1> S; ## number of geographic units (e.g., states)
    int<lower=1> P; ## number of hierarchical parameters, including geographic
    int<lower=1> H; ## number of predictors for geographic unit effects
    int<lower=1> Hprior; ## number of predictors for geographic unit effects (t=1)

    int<lower=1> D; ## number of difficulty parameters per question
    int<lower=0,upper=1> constant_item; ## indicator for constant item parameters
    int<lower=0,upper=1> separate_years; ## indicator for no over-time smoothing
    int n_vec[N]; ## long vector of trials
    int s_vec[N]; ## long vector of successes
    int NNnat[T, Q, Gnat]; ## trials
    int SSnat[T, Q, Gnat]; ## successes
    int<lower=0> MMM[T, Q, G]; ## missingness array
    matrix<lower=0, upper=1>[G, P] XX; ## indicator matrix for hierarchical vars.
    matrix<lower=0, upper=1>[Gnat, G] WT[T]; ## weight array
    row_vector[H] ZZ[T, S]; ## data for geographic model
    row_vector[Hprior] ZZ_prior[1, S]; ## data for geographic model
    matrix<lower=0, upper=1>[T, Q] nat_only;
}

transformed data {
}

parameters {
    vector[Q] diff_raw[D]; ## raw difficulty
    vector<lower=0>[Q] disc_raw; ## discrimination
    vector[T] xi; ## common intercept
    vector[P] gamma[T]; ## hierarchical parameters
    vector[T] delta_lag; ## weight placed on geo. effects from prev. period
    vector[H] delta_pred[T]; ## weight on geographic predictors
    vector[Hprior] delta_pred_prior; ## weight on geographic predictors (t=1)
    vector[G] theta_bar[T]; ## group mean ability
    vector<lower=0>[T] sd_theta_bar; ## residual sd of group ability means (by period)
    vector<lower=0>[T] sd_theta; ## sd of abilities (by period)
    real<lower=0> sd_geo; ## prior sd of geographic effects
    real<lower=0> sd_demo; ## sd of demographic effecs
    real<lower=0> sd_innov_delta; ## innovation sd of delta_pred and delta_lag
    real<lower=0> sd_innov_logsd; ## innovation sd of sd_theta
    real<lower=0> sd_innov_gamma; ## innovation sd of gamma, xi, and (opt.) diff
}

transformed parameters {
    vector[Q] diff[D]; ## adjusted difficulty
    vector[Q] kappa[D]; ## threshold
    vector<lower=0>[Q] disc; ## normalized discrimination
    vector<lower=0>[Q] sd_item; ## item standard deviation
    vector<lower=0>[Q] var_item; ## item variance
    vector<lower=0>[T] var_theta; ## within-group variance of theta
    ## var. of theta_bar w/in each nat. group **NOT CONSTRAINED TO BE POSITIVE**
    vector[Gnat] var_theta_bar_nat[T];
    vector[G] xb_theta_bar[T]; ## linear predictor for group means
    vector[G] z[T, Q]; ## array of vectors of group deviates
    vector[Gnat] z_nat[T, Q]; ##
    real<lower=0,upper=1> prob[T, Q, G]; ## array of probabilities
    vector[Gnat] prob_nat[T, Q]; ## array of probabilities
    vector[Gnat] theta_nat[T]; ## national-level group abililities

    ## Identify model by rescaling item parameters (Fox 2010, pp. 88-89)
    ## scale (product = 1)
    disc <- disc_raw * pow(exp(sum(log(disc_raw))), (-inv(Q)));
    for (q in 1:Q) {
        sd_item[q] <- inv(disc[q]); ## item standard deviations
    }
    for (d in 1:D) {
        ## location (mean in first year = 0)
        diff[d] <- diff_raw[d] - mean(diff_raw[1]);
        kappa[d] <- diff[d] ./ disc; ## item thresholds
    }
    var_item <- sd_item .* sd_item;
    var_theta <- sd_theta .* sd_theta;
    for (t in 1:T) { ## loop over years
        xb_theta_bar[t] <- xi[t] + XX * gamma[t]; ## Gx1 = GxP * Px1
        ## Weighted average of group means (weights must sum to 1)
        theta_nat[t] <- WT[t] * theta_bar[t]; ## Gnatx1 = GnatxG * Gx1
        for (n in 1:Gnat) {
            matrix[G, G] WTdiag;
            for (g in 1:G) {
                for (h in 1:G) {
                    if (g == h) {
                        WTdiag[g, h] <- WT[t][n][g];
                    }
                    if (g != h) {
                        WTdiag[g, h] <- 0;
                    }
                 }
            }
            ## (y - w'y)' W (y - w'y) = weighted variance
            var_theta_bar_nat[t][n] <- (theta_bar[t] - theta_nat[t, n])' * WTdiag *
                    (theta_bar[t] - theta_nat[t, n]);
        }
        for (q in 1:Q) { ## loop over questions
            real sd_tq;
            real sd_nat_tq[Gnat];
            sd_tq <- sqrt(var_theta[t] + var_item[q]);
            for (n in 1:Gnat) {
                sd_nat_tq[n] <- sqrt(var_theta[t] + var_theta_bar_nat[t, n] + var_item[q]);
            }
            ## Group-level IRT model
            if (constant_item == 0) {
                z[t, q] <- (theta_bar[t] - kappa[t][q]) / sd_tq;
                for (n in 1:Gnat) {
                    z_nat[t, q, n] <- (theta_nat[t, n] - kappa[t][q]) / sd_nat_tq[n];
                    prob_nat[t, q, n] <- Phi_approx(z_nat[t, q, n]);
                }
            }
            if (constant_item == 1) {
                z[t, q] <- (theta_bar[t] - kappa[1][q]) / sd_tq;
                for (n in 1:Gnat) {
                    z_nat[t, q, n] <- (theta_nat[t, n] - kappa[1][q]) / sd_nat_tq[n];
                    prob_nat[t, q, n] <- Phi_approx(z_nat[t, q, n]);
                }
            }
            for (g in 1:G) { ## loop over groups
                prob[t, q, g] <- Phi_approx(z[t, q, g]); ## fast normal CDF
            }
        } ## end question loop
    } ## end year loop
}

model {
    ## TEMPORARY VARIABLES
    real prob_vec[N]; ## long vector of probabilities (empty cells omitted)
    int pos;
    pos <- 0;
    ## PRIORS
    if (constant_item == 1) {
        diff_raw[1] ~ normal(0, 1); ## item difficulty (constant)
    }
    disc_raw ~ lognormal(0, 1); ## item discrimination
    sd_geo ~ cauchy(0, 2.5); ## sd of geographic effects
    sd_demo ~ cauchy(0, 2.5); ## prior sd of demographic parameters
    sd_innov_delta ~ cauchy(0, 2.5); ## innovation sd of delta_pred/delta_lag
    sd_innov_gamma ~ cauchy(0, 2.5); ## innovation sd. of gamma, xi, and diff
    sd_innov_logsd ~ cauchy(0, 2.5); ## innovation sd of theta_sd
    for (t in 1:T) { ## loop over years
        if (separate_years == 1) { ## Estimate model anew each period
            xi[t] ~ normal(0, 10); ## intercept
            for (p in 1:P) { ## Loop over individual predictors (gammas)
                if (p <= S) gamma[t][p] ~ normal(ZZ[t][p]*delta_pred[t], sd_geo);
                if (p > S) gamma[t][p] ~ normal(0, sd_demo);
            }
        }
        if (t == 1) {
            if (constant_item == 0) {
                diff_raw[t] ~ normal(0, 1); ## item difficulty
            }
            ## Priors for first period
            sd_theta_bar[t] ~ cauchy(0, 2.5);
            sd_theta[t] ~ cauchy(0, 2.5);
            delta_lag[t] ~ normal(0.5, 1);
            delta_pred[t] ~ normal(0, 10);
            delta_pred_prior ~ normal(0, 10);
            if (separate_years == 0) {
                xi[t] ~ normal(0, 10); ## intercept
                for (p in 1:P) { ## Loop over individual predictors (gammas)
                    if (p <= S) {
                        gamma[t][p] ~ normal(ZZ_prior[1][p]*delta_pred_prior,
                                             sd_geo);
                    }
                    if (p > S) gamma[t][p] ~ normal(0, sd_demo);
                }
            }
        }
        if (t > 1) {
            ## TRANSITION MODEL
            ## Difficulty parameters (if not constant)
            if (constant_item == 0) {
                diff_raw[t] ~ normal(diff_raw[t - 1], sd_innov_gamma);
            }
            ## predictors in geographic models (random walk)
            delta_lag[t] ~ normal(delta_lag[t - 1], sd_innov_delta);
            delta_pred[t] ~ normal(delta_pred[t - 1], sd_innov_delta);
            sd_theta_bar[t] ~ lognormal(log(sd_theta_bar[t - 1]), sd_innov_logsd);
            sd_theta[t] ~ lognormal(log(sd_theta[t - 1]), sd_innov_logsd);
            if (separate_years == 0) {
                ## Dynamic linear model for hierarchical parameters
                xi[t] ~ normal(xi[t - 1], sd_innov_gamma); ## intercept
                for (p in 1:P) { ## Loop over individual predictors (gammas)
                    if (p <= S) {
                        gamma[t][p] ~ normal(delta_lag[t]*gamma[t - 1][p] +
                                             ZZ[t][p]*delta_pred[t],
                                             sd_innov_gamma);
                    }
                    if (p > S) {
                        gamma[t][p] ~ normal(gamma[t - 1][p], sd_innov_gamma);
                    }
                }
            }
        }
        ## RESPONSE MODEL
        ## Model for group means
        ## (See transformed parameters for definition of xb_theta_bar)
        theta_bar[t] ~ normal(xb_theta_bar[t], sd_theta_bar[t]); ## group means
        for (q in 1:Q) { ## loop over questions
            if (nat_only[t, q] == 1) {
                ## National mean
                SSnat[t, q] ~ binomial(NNnat[t, q], prob_nat[t, q]);
            }
            for (g in 1:G) { ## loop over groups
                if (MMM[t, q, g] == 0) { ## Use only if not missing
                    pos <- pos + 1;
                    prob_vec[pos] <- prob[t, q, g];
                }
            } ## end group loop
        } ## end question loop
    } ## end time loop
    ## Sampling model for group responses
    s_vec ~ binomial(n_vec, prob_vec);
}

generated quantities {
    vector<lower=0>[T] sd_total;
    for (t in 1:T) {
        sd_total[t] <- sqrt(variance(theta_bar[t]) + square(sd_theta[t]));
    }
}
