data {
   
  // caps
  int<lower = 1> G;              // n groups
  int<lower = 1> J;              // n items
  int<lower = 1> P;             // n parties
  int<lower = 1> S;             // n geos
  
  // response data (grouped binomial)
  int<lower = 0> Y[G, J];     // binary response
  int<lower = 0> N[G, J];                // sample sizes

  // group-level index trackers
  int<lower = 1, upper = P> party[G];        // party of group g
  int<lower = 1, upper = S> geo[G];        // geo of group g

  // geo-level covariates
  int k;              // n covariates
  matrix[G, k] X;        // geo covariate

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
  vector<lower = 0>[G] sigma_in_g;

  // hierarchical parameters
  vector[G] z_theta;
  real<lower = 0> scale_theta;
  vector[P] party_int;
  matrix[k, P] party_coefs;

  // eventual: 
  // time dimensions
  // VC matrices
  // innovation variances
  // time-zero priors
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
  // hierarchical model
  vector[G] theta;
  vector[G] theta_hypermean;


  // --- group theta = fixed mean + group residual (noncentered)
  
  // matrix form but looping over groups(?? stupid ??)
  for (g in 1:G) {
    theta_hypermean[g] = party_int[party[g]] + 
                    (X[geo[g], ] * party_coefs[ , party[g]]);
  }

  theta = theta_hypermean + (scale_theta * z_theta);
  // future: loop geo within party?
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
  sigma_in_g ~ lognormal(0, 1); 
  z_theta ~ normal(0, 1);
  scale_theta ~ lognormal(0, 0.5);

  
  // ----- group regression -----
  
  for (gr in 1:G) {
    if (party[gr] == 1) {
      party_int[party[gr]] ~ normal(prior_mean_party_1, 1);
    } else if (party[gr] == 2) {
      party_int[party[gr]] ~ normal(prior_mean_party_2, 1);
    }
  }

  for (p in 1:P) {
    
    // change to mv normal by party 
    party_coefs[, p] ~ normal(0, 1);
  
  }
  
  // party_coefs ~ normal(0, 1);

}




// generated quantities {

  // identify the ideal points here? would that work?

// }
