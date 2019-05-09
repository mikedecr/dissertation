// WHY?
// we don't assume that we have groups x parties x items data
// could be missing a party in a group?, items w/in group, etc.
// - but we should expand item x group and fill in zeroes
// data are _ITEM-GROUP LEVEL_; responses, covariates, etc

// data: create a long table of everything
// then: grouped response data & matrix of covariates

// to do:
// - dispersion vs discrimination?
// - hyper theta (noncentered)
// - hyper sigma (noncentered)
// - identification in GenQ block?
// - flexi priors
// - missing data?
// - dynamics
// - expando model algebra


data {
   
  // caps
  int<lower = 1> n;    // all groups and items (not strictly product)
  int<lower = 1> n_state;    // n states
  int<lower = 1> n_district;    // n districts
  int<lower = 1> n_party;    // n parties? (assumed 2?)
  int<lower = 1> n_group;    // n groups
  int<lower = 1> n_item;    // n items
  
  // response data (grouped binomial)
  int<lower = 0> y[n];      // successes in grp-item
  int<lower = 0> trials[n];      // trials in grp-item

  // group index trackers
  int<lower = 1, upper = n_state> state[n];   
  int<lower = 1, upper = n_district> district[n];
  int<lower = 1, upper = n_party> party[n];   
  int<lower = 1, upper = n_group> group[n];   
  int<lower = 1, upper = n_item> item[n];

  // hierarchical covariates
  int k_d;              // num of district covariates
  matrix[n, k_d] X;     // district covariate

  int k_s;
  matrix[n, k_s] Z;     // num of state covariates


  // ---- prior data ----
  // theta means
  // real prior_party_mean[P];

}

// transformed data {}

parameters {
 
  // ogive model parameters
  vector[n_item] cutpoint;
  vector<lower = 0>[n_item] discrimination;
  vector<lower = 0>[n_group] sigma_in_g;
  // theta in next block once you go hierarchical
  vector[n_group] theta;

  // hierarchical parameters
  // vector[n_group] z_theta;
  // real<lower = 0> scale_theta;
  // vector[P] party_int;
  // matrix[k, P] party_coefs;



}

transformed parameters {

  // item response model
  vector[n] eta;   // probit scale index
  vector[n] pprob; // normal CDF
  vector<lower = 0>[n_item] dispersion;

  // future: theta and sigma hypermean regressions



  dispersion = inv(discrimination);

  for (i in 1:n) {
    eta[i] = 
      (theta[group[i]] - cutpoint[item[i]]) ./ 
      sqrt( square(sigma_in_g[group[i]]) + square(dispersion[item[i]]) );
  }

  pprob = Phi_approx(eta);

}



model {
 
  // ----- data model -----

  y ~ binomial(trials, pprob);

  // ----- IRT  -----

  cutpoint ~ normal(0, 0.1);
  discrimination ~ lognormal(-0.75, 0.35);
  sigma_in_g ~ lognormal(0, 1); 
  theta ~ normal(0, 1);
  // z_theta ~ normal(0, 1);
  // scale_theta ~ lognormal(0, 0.5);

  // ---- district and state regressions ----

}

generated quantities {
 


}
