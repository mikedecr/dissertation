// WHY?
// we don't assume that we have groups x parties x items data
// could be missing a party in a group?, items w/in group, etc.
// - but we should expand item x group and fill in zeroes
// data are _ITEM-GROUP LEVEL_; responses, covariates, etc

// data: create a long table of everything
// then: grouped response data & matrix of covariates

// to do:
// - [x] hyper theta (noncentered)
// - [ ] implement a to-do code (¿¿??) 
// - [ ] identification in GenQ block?
// - [ ] expando model algebra (likelihood as a grid?)
// - [ ] dispersion vs discrimination?
// - [ ] hyper sigma (noncentered)
// - [ ] flexi priors (intercept, what else?)
// - [ ] mvnorm hierarchical coefs?
// - [ ] missing data? is this needed?
// - [ ] dynamics (REVERSE) random walk?


data {
   
  // caps
  int<lower = 1> n;    // all groups and items (not strictly product)
  int<lower = 1> n_state;    // n states
  int<lower = 1> n_district;    // n districts
  int<lower = 1> n_party;    // n parties? (assumed 2?)
  int<lower = 1> n_group;    // n groups, ALL groups not just with data!
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
  vector[n_group] z_group;    // one-d group errors (all independent)
  matrix[n_state, n_party] z_state;    // two-d state errors
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

  // for (g in 1:n_group) {

  //   // offsets are f(hypermean + error)
  //   // Z and X are still N long! 
  //   state_offset[state[g], party[g]] = 
  //     state_hypermean[state[g], party[g]] + state_hypererror[state[g], party[g]];
  //   state_hypermean[state[g], party[g]] = 
  //     ( Z[g, ] * state_coefs[ , party[g]] );
  //   state_hypererror[state[g], party[g]] = 
  //     ( z_state[state[g], party[g]] * scale_state[party[g]] );

  //   group_offset[g] = 
  //     ( X[g, ] * group_coefs[ , party[g]] ) + 
  //     ( z_group[g] * scale_group[party[g]] );

  //   // clean up
  //   theta[g] = 
  //     intercept[party[g]] + 
  //     group_offset[g] + state_offset[state[g], party[g]];
  // }

  // IRT index (loop group-item)
  // later: expando
  for (i in 1:n) {

    // offsets are f(hypermean + error)
    // Z and X are still N long!
    // hierarchical params assigned ONLY for missing data
    // ¿¿?? we need to assign for EVERY group, not just where we have data?
    //   -> should be handled by {y = 0, n = 0} cases?
    if (is_nan(state_offset[state[i], party[i]])) {
      state_offset[state[i], party[i]] = 
        ( Z[i] * state_coefs[ , party[i]] ) + 
        ( z_state[state[i], party[i]] * scale_state[party[i]] );
    }

    if (is_nan(group_offset[group[i]])) {
      group_offset[group[i]] = 
        ( X[i] * group_coefs[ , party[i]] ) + 
        ( z_group[group[i]] * scale_group[party[i]] );
    }

    // clean up
    if (is_nan(theta[group[i]])) {
      theta[group[i]] = 
        intercept[party[i]] + 
        group_offset[group[i]] + state_offset[state[i], party[i]];
    }

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
  intercept ~ normal([-1, 1], [1, 1]); // TK fix
  z_group ~ normal(0, 1);       // group zs, all independent and one-dim
  
  for (p in 1:n_party) {
    
    group_coefs[ , p] ~ normal(0, 1); // multivariate? soon DLM
    state_coefs[ , p] ~ normal(0, 1);

    z_state[ , p] ~ normal(0, 1);     // two-d state Z scores

    scale_group[p] ~ lognormal(0, 1); // two-vectors of error scales
    scale_state[p] ~ lognormal(0, 1);

  }



  // to do: DLM


}

generated quantities {}
