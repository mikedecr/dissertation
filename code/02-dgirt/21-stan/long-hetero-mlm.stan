// WHY?
// we don't assume that we have groups x parties x items data
// could be missing a party in a group?, items w/in group, etc.
// - but we should expand item x group and fill in zeroes
// data are _ITEM-GROUP LEVEL_; responses, covariates, etc

// data: create a long table of everything
// then: grouped response data & matrix of covariates

// to do:
// - [x] hyper theta (noncentered)
// - [x] hyper sigma (noncentered)
// - [x] identification in GenQ block?
// - [ ] substitute non-centered params
// - [ ] save non-redundant params
// - [ ] implement a to-do code (¿¿??) 
// - [ ] expando model algebra (likelihood as a grid?)
// - [ ] dispersion vs discrimination?
// - [ ] flexi priors (intercept, what else?)
// - [ ] mvnorm hierarchical coefs?
// - [ ] missing data? is this needed?
// - [ ] dynamics (REVERSE) random walk?


data {
   
  // caps
  int<lower = 1> n;    // all groups and items (not strictly product)
  int<lower = 1> n_region;    // n regions
  int<lower = 1> n_state;    // n states
  int<lower = 1> n_district;    // n districts
  int<lower = 1> n_party;    // n parties? (assumed 2?)
  int<lower = 1> n_group;    // n groups, ALL groups not just with data!
  int<lower = 1> n_item;    // n items
  
  // response data (grouped binomial)
  int<lower = 0> y[n];      // successes in grp-item
  int<lower = 0> trials[n];      // trials in grp-item

  // group index trackers
  int<lower = 1, upper = n_region> region[n];   
  int<lower = 1, upper = n_state> state[n];   
  int<lower = 1, upper = n_district> district[n];
  int<lower = 1, upper = n_party> party[n];   
  int<lower = 1, upper = n_group> group[n];   
  int<lower = 1, upper = n_item> item[n];

  // hierarchical covariates
  int k_d;              // num of district covariates
  matrix[n, k_d] X;     // district covariate matrix

  int k_s;              // num of state covariates
  matrix[n, k_s] Z;     // state covariate matrix

  // ¿? n vs. n_* rows? How to deal with this in params


  // ---- prior data ----
  // theta means
  // real prior_party_mean[P];

}

// transformed data {}

parameters {
 
  // --- IRT ---
  vector[n_item] cut_raw; // raw item midpoint
  vector<lower = 0>[n_item] disc_raw;  // raw item discrimination
  // real<lower = 0> sigma_in_g; // only 1. If heteroskedastic: next block

  
  // --- HIERARCHY ---
  // theta hypermean regression
  vector[n_party] const_mean;          // two-length intercept     
  matrix[k_d, n_party] coef_grp_mean;   // two-d group coefs
  matrix[k_s, n_party] coef_st_mean;   // two-d state coefs

  // HET: sigma hypermean regression
  vector[n_party] const_var;          // two-length intercept     
  matrix[k_d, n_party] coef_grp_var;   // two-d group coefs
  matrix[k_s, n_party] coef_st_var;   // two-d state coefs
  
  // errors, theta: 
  vector[n_group] z_grp_mean;    // one-d group errors (all independent)
  matrix[n_state, n_party] z_st_mean;    // two-d state errors
  matrix[n_region, n_party] z_rg_mean;       // RxP two-d region errors
  vector<lower = 0>[n_party] scale_grp_mean;         // two-length group scale
  vector<lower = 0>[n_party] scale_st_mean;         // two-length state scale
  vector<lower = 0>[n_party] scale_rg_mean;  // P-long region scale


  // HET: errors, sigma: 
  vector[n_group] z_grp_var;    // one-d group errors (all independent)
  matrix[n_state, n_party] z_st_var;    // two-d state errors
  matrix[n_region, n_party] z_rg_var;    // two-d state errors
  vector<lower = 0>[n_party] scale_grp_var;         // two-length group scale
  vector<lower = 0>[n_party] scale_st_var;         // two-length state scale
  vector<lower = 0>[n_party] scale_rg_var;         // two-length state scale

  // transpose z-scores for row-indexing below?


}

transformed parameters {

  // item response model
  vector[n] eta;                               // link scale index
  vector[n_group] theta;                       // group mean
  vector<lower = 0>[n_group] sigma_in_g;       // group sd
  vector[n_item] cutpoint;                     // identified cutpoint
  vector<lower = 0>[n_item] discrimination;    // identified discrimination
  vector<lower = 0>[n_item] dispersion;        // inv discrimination

  // vector[n] eta2; // normal CDF
  // vector<lower = 0, upper = 1>[n] pprob; // normal CDF

  // --- hierarchical regressions ---
  // theta regression
  vector[n_group] grp_offset_mean;
  matrix[n_state, n_party] st_offset_mean;
  matrix[n_region, n_party] rg_offset_mean;
  // HET: log sigma regression
  vector[n_group] grp_offset_var;
  matrix[n_state, n_party] st_offset_var;
  matrix[n_region, n_party] rg_offset_var;


  // cutpoints are mean 0 (dynamic: in year 1)
  cutpoint = cut_raw - mean(cut_raw);

  // discrimination are prod = 1
  // dispersion = 1 / discrimination
  discrimination = disc_raw * pow(exp(sum(log(disc_raw))), (-inv(n_item)));
  dispersion = inv(discrimination);

  // loop over groups to get theta
  // future: log(sigma) regressions

  // IRT index (loop group-item)
  // later: expando
  for (i in 1:n) {

    // offsets are f(hypermean + error)
    // Z and X are still N long!
    // hierarchical params assigned ONLY for missing data
    // ¿¿?? we need to assign for EVERY group, not just where we have data?
    //   -> should be handled by {y = 0, n = 0} cases?

    // theta regression
    if (is_nan(rg_offset_mean[region[i], party[i]])) {
      rg_offset_mean[region[i], party[i]] = 
        ( z_rg_mean[region[i], party[i]] * scale_rg_mean[party[i]] );
    }

    if (is_nan(st_offset_mean[state[i], party[i]])) {
      st_offset_mean[state[i], party[i]] = 
        ( Z[i] * coef_st_mean[ , party[i]] ) + 
        ( z_st_mean[state[i], party[i]] * scale_st_mean[party[i]] );
    }

    if (is_nan(grp_offset_mean[group[i]])) {
      grp_offset_mean[group[i]] = 
        ( X[i] * coef_grp_mean[ , party[i]] ) + 
        ( z_grp_mean[group[i]] * scale_grp_mean[party[i]] );
    }
    
    // sigma_g regression
    if (is_nan(rg_offset_var[region[i], party[i]])) {
      rg_offset_var[region[i], party[i]] = 
        ( z_rg_var[region[i], party[i]] * scale_rg_var[party[i]] );
    }

    if (is_nan(st_offset_var[state[i], party[i]])) {
      st_offset_var[state[i], party[i]] = 
        ( Z[i] * coef_st_var[ , party[i]] ) + 
        ( z_st_var[state[i], party[i]] * scale_st_var[party[i]] );
    }

    if (is_nan(grp_offset_var[group[i]])) {
      grp_offset_var[group[i]] = 
        ( X[i] * coef_grp_var[ , party[i]] ) + 
        ( z_grp_var[group[i]] * scale_grp_var[party[i]] );
    }

    // clean up ---- 
    // grp mean (theta)
    if (is_nan(theta[group[i]])) {
      theta[group[i]] = 
        const_mean[party[i]] + 
        grp_offset_mean[group[i]] + 
        st_offset_mean[state[i], party[i]] +
        rg_offset_mean[region[i], party[i]];
    }
    // within-grp sd
    if (is_nan(sigma_in_g[group[i]])) {
      sigma_in_g[group[i]] = 
        exp(
          const_var[party[i]] + 
          grp_offset_var[group[i]] + 
          st_offset_var[state[i], party[i]] +
          rg_offset_mean[region[i], party[i]]
        );
    }

    eta[i] = 
      (theta[group[i]] - cutpoint[item[i]]) ./ 
      sqrt( square(sigma_in_g[group[i]]) + square(dispersion[item[i]]) );
  }

}



model {
 
  // ----- data model -----
  y ~ binomial_logit(trials, eta);  // logit link!!!!
  

  // ----- IRT params -----
  disc_raw ~ lognormal(0, 2); // item params: static for now?
  cut_raw ~ normal(0, 2);

  // ---- district and state regressions ----
  // constants
  const_mean ~ normal([0, 0], [1, 1]); // TK fix
  const_var ~ normal([0, 0], [1, 1]); // TK fix
  
  // group z-scores
  z_grp_mean ~ normal(0, 1);       // group zs, all independent and one-dim
  z_grp_var ~ normal(0, 1);       // group zs, all independent and one-dim
  
  for (p in 1:n_party) {
    
    coef_grp_mean[ , p] ~ normal(0, 1); // multivariate? soon DLM
    coef_st_mean[ , p] ~ normal(0, 1);

    coef_grp_var[ , p] ~ normal(0, 1); // multivariate? soon DLM
    coef_st_var[ , p] ~ normal(0, 1);

    z_st_mean[ , p] ~ normal(0, 1);     // two-d state Z scores
    z_st_var[ , p] ~ normal(0, 1);     // two-d state Z scores

    z_rg_mean[ , p] ~ normal(0, 1);     // two-d region Z scores
    z_rg_var[ , p] ~ normal(0, 1);     // two-d region Z scores

    scale_grp_mean[p] ~ lognormal(0, 1); // two-vectors of error scales
    scale_st_mean[p] ~ lognormal(0, 1);
    scale_rg_mean[p] ~ lognormal(0, 1);

    scale_grp_var[p] ~ lognormal(0, 1); // two-vectors of error scales
    scale_st_var[p] ~ lognormal(0, 1);
    scale_rg_var[p] ~ lognormal(0, 1);

  }

  // to do: DLM


}

generated quantities {

  // standardize theta_g
  vector[n_group] idtheta;
  real theta_iter_mean;
  real theta_iter_sd;

  // identify sigma_g by stdizing log-sigma
  vector[n_group] idsigma;
  real log_sigma_iter_mean;
  real log_sigma_iter_sd;

  // theta mean and sd
  theta_iter_mean = mean(theta);
  theta_iter_sd = sd(theta);

  // log-sigma mean and sd
  log_sigma_iter_mean = mean(log(sigma_in_g));
  log_sigma_iter_sd = sd(log(sigma_in_g));

  // identify
  idtheta = (theta - theta_iter_mean) / theta_iter_sd;
  idsigma = 
    exp(
      ( log(sigma_in_g) - log_sigma_iter_mean ) / 
      log_sigma_iter_sd
    );





}
