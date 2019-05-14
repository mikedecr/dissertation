// WHY?
// we don't assume that we have groups x parties x items data
// could be missing a party in a group?, items w/in group, etc.
// - but we should expand item x group and fill in zeroes
// data are _ITEM-GROUP LEVEL_; responses, covariates, etc

// data: create a long table of everything
// then: grouped response data & matrix of covariates

// to do:
// - implement a to-do code (¿¿??) 
// - dispersion vs discrimination?
// - hyper theta (noncentered)
// - hyper sigma (noncentered)
// - identification in GenQ block?
// - flexi priors (intercept, what else)
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
  vector[n_item] cutpoint;
  vector<lower = 0>[n_item] discrimination;
  real<lower = 0> sigma_in_g; // only 1. If heteroskedastic: next block

  
  // --- HIERARCHY ---
  // regression
  vector[n_party] intercept;          // two-length intercept     
  matrix[k_d, n_party] group_coefs;   // two-d group coefs
  matrix[k_s, n_party] state_coefs;   // two-d state coefs
  
  // errors: 
  vector[n_group] e_group;    // two-d group errors
  matrix[n_state, n_party] u_state;    // two-d state errors
  vector<lower = 0>[n_party] scale_group;         // two-length group scale
  vector<lower = 0>[n_party] scale_state;         // two-length state scale

  // transpose z-scores for row-indexing below?


}

transformed parameters {

  // item response model
  vector[n] eta;                         // link scale index
  vector[n_group] theta;                 // group mean
  vector<lower = 0>[n_item] dispersion;

  // vector[n] eta2; // normal CDF
  // vector<lower = 0, upper = 1>[n] pprob; // normal CDF

  // --- THETA HYPERMEAN ---
  // simple hierarchical params
  vector[n_group] group_offset;
  matrix[n_state, n_party] state_offset;


  dispersion = inv(discrimination);

  // loop over groups to get theta
  // future: log(sigma) regressions

  for (g in 1:n_group) {

    // offsets are f(hypermean + error)
    state_offset[state[g], party[g]] = 
      ( Z[state[g]] * state_coefs[ , party[g]] ) + 
      ( u_state[state[g], party[g]] * scale_state[party[g]] );

    group_offset[g] = 
      ( X[g] * group_coefs[ , party[g]] ) + 
      ( e_group[g] * scale_group[party[g]] );

    // clean up
    theta[g] = 
      intercept[party[g]] + 
      group_offset[g] + state_offset[state[g], party[g]];
  }

  // IRT index (loop group-item)
  // later: expando
  for (i in 1:n) {
    eta[i] = 
      (theta[group[i]] - cutpoint[item[i]]) ./ 
      sqrt( square(sigma_in_g) + square(dispersion[item[i]]) );
  }


}



model {
 
  // ----- data model -----

  y ~ binomial_logit(trials, eta);  // logit link!!!!
  


  // ----- IRT params -----

  discrimination ~ lognormal(-0.75, 0.35); // item params: static for now?
  cutpoint ~ normal(0, 0.1);         

  sigma_in_g ~ lognormal(0, 1);    // will become regression




  // ---- district and state regressions ----
  
  // Hypermeans 
  intercept ~ normal([-1, 1], [1, 1]); // TK fix
  // insert coefs  (mvnorm, to be DLM)

  // hierarchical errors
  e_group ~ normal(0, 1);           // group error
  for (p in 1:n_party) {            // state-party error
    u_state[p, ] ~ normal(0, 1);
  }
  // scale params?



  // to do: DLM


}

generated quantities {
 


}
