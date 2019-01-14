// takes static noncenter, adds modeled heteroskedasticity

data {
   
  // caps
  int<lower = 1> G;              // n groups
  int<lower = 1> J;              // n items
  int< lower = 1> P;             // n parties
  int< lower = 1> S;             // n geographic units
  
  // response data (grouped binomial)
  int<lower = 0> Y[G, J];     // binomial responses
  int<lower = 0> N[G, J];     // sample sizes

  // group-level index trackers
  int<lower = 1, upper = P> party[G];        // party of group g
  int<lower = 1, upper = S> geo[G];        // district of group g

  // district-level covariates
  int k;              // n covariates
  matrix[S, k] X;        // district covariate

  // ---- prior data ----

  // theta means
  real prior_mean_party_1;
  real prior_mean_party_2;

}


// transformed data {}


parameters {
  
  // ogive and within-group sd
  vector[J] cutpoint;
  vector<lower = 0>[J] discrimination;

  // hierarchical parameters for group ideal points
  vector[G] z_theta;                // std normal z score
  real<lower = 0> scale_theta;      // scale factor for z
  vector[P] party_int;              // regression intercepts
  matrix[k, P] party_coefs;         // regression coefs

  // hierarchical parameters for in-group variances
  vector[G] z_sigma;                // std normal z score
  real<lower = 0> scale_sigma;      // scale factor for z
  vector[P] party_int_sigma;        // regression intercepts
  matrix[k, P] party_coefs_sigma;   // regression coefs

  // eventual: 
  // [x] heteroskedasticity
  // [ ] time (indices, t0 priors, innovation variances)
  // [ ] VC matrices is this worth it?
}


transformed parameters {
  
  // ------- declare -------
  // expanders and containers for linear algebra magic
  // "opposite lengths"
  vector<lower = 1, upper = 1>[J] g_vec;
  vector<lower = 1, upper = 1>[G] j_vec;
  matrix[G, J] eta_numerator;
  matrix[G, J] eta_denominator;

  // item response (and reparameterization)
  vector<lower = 0>[J] dispersion;
  matrix[G, J] eta;
  // matrix<lower = 0, upper = 1>[G, J] ppi;

  // hierarchical regression means
  vector[G] theta_hypermean;
  vector[G] theta;                  // realized group ideal point
  vector[G] sigma_g_hypermean;
  vector<lower = 0>[G] sigma_in_g;  // realized group ideal pt variance


  // --- group theta = fixed mean + group residual (noncentered)
  
  // matrix form but looping over groups(?? stupid ??)
  for (g in 1:G) {
    theta_hypermean[g] = party_int[party[g]] + 
                    (X[geo[g], ] * party_coefs[ , party[g]]);
    sigma_g_hypermean[g] = party_int_sigma[party[g]] + 
                    (X[geo[g], ] * party_coefs_sigma[ , party[g]]);
  }
  
  // parameters are affected by randomness in coefs and variances
  theta = theta_hypermean + (scale_theta * z_theta);
  sigma_in_g = exp(sigma_g_hypermean + (scale_sigma * z_sigma));

  // future: loop district within party?
  //         theta would have to be S x P matrix 
  //         (might be easier to keep track of)



  // ------- calculate fraction -------
  // parameterization
  dispersion = inv(discrimination);
  
  // expanders, opposite lengths
  g_vec = rep_vector(1, J);
  j_vec = rep_vector(1, G);
  
  // linear algebra
  eta_numerator = (theta * g_vec') - (j_vec * cutpoint');
  eta_denominator = 
    sqrt((square(sigma_in_g) * g_vec') + (j_vec * square(dispersion)'));

  // index and probability
  eta = eta_numerator ./ eta_denominator;
  // ppi = Phi_approx(eta);





}



model {
  
  // ----- data model -----

  for (j in 1:J) {
    Y[ , j] ~ binomial_logit(N[ , j], eta[ , j]);
  }


  // ----- IRT response level -----

  cutpoint ~ normal(0, 0.1);
  discrimination ~ lognormal(-0.75, 0.35);
  
  // noncentered parameters for theta and sigma_g
  z_theta ~ normal(0, 1);
  z_sigma ~ normal(0, 1);
  scale_theta ~ lognormal(0, 0.5);
  scale_sigma ~ lognormal(0, 0.25);

  
  // ----- group regression -----
  
  for (gr in 1:G) {
    if (party[gr] == 1) {
      party_int[party[gr]] ~ normal(prior_mean_party_1, 1);
      party_int_sigma[party[gr]] ~ normal(-1, 1);
    } else if (party[gr] == 2) {
      party_int[party[gr]] ~ normal(prior_mean_party_2, 1);
      party_int_sigma[party[gr]] ~ normal(1, 1);
    }
  }

  for (p in 1:P) {
    
    // change to mv normal by party ?
    party_coefs[, p] ~ normal(0, 1);
    party_coefs_sigma[, p] ~ normal(0, 1);
  
  }

}




// generated quantities {

  // identify the ideal points here? would that work?

// }
