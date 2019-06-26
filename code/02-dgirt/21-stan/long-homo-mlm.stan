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

  // no region covariates?
  // int k_r
  // matrix W

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
  // theta hypermean regression
  vector[n_party] const_mean;          // two-length intercept     
  matrix[k_d, n_party] coef_grp_mean;   // two-d group coefs
  matrix[k_s, n_party] coef_st_mean;   // two-d state coefs

  // HET: sigma hypermean regression
  // vector[n_party] const_var;          // two-length intercept     
  // matrix[k_d, n_party] coef_grp_var;   // two-d group coefs
  // matrix[k_s, n_party] coef_st_var;   // two-d state coefs
  
  // errors, theta: 
  vector[n_group] z_grp_mean;                // G-long grp errors
  matrix[n_state, n_party] z_st_mean;        // SxP state errors
  matrix[n_region, n_party] z_rg_mean;       // RxP two-d region errors
  vector<lower = 0>[n_party] scale_grp_mean; // P-long group scale
  vector<lower = 0>[n_party] scale_st_mean;  // P-long state scale
  vector<lower = 0>[n_party] scale_rg_mean;  // P-long region scale

  // HET: errors, sigma: 
  // vector[n_group] z_grp_var;    // one-d group errors (all independent)
  // matrix[n_state, n_party] z_st_var;    // two-d state errors
  // vector<lower = 0>[n_party] scale_grp_var;       // two-length group scale
  // vector<lower = 0>[n_party] scale_st_var;        // two-length state scale

  // transpose z-scores for row-indexing below?


}

transformed parameters {

  // item response model
  vector[n] eta;                         // link scale index
  vector[n_group] theta;                 // group mean
  vector<lower = 0>[n_item] dispersion;

  // vector[n] eta2; // normal CDF
  // vector<lower = 0, upper = 1>[n] pprob; // normal CDF

  // --- hierarchical regressions ---
  // theta regression
  vector[n_group] grp_offset_mean;
  matrix[n_state, n_party] st_offset_mean;
  matrix[n_region, n_party] rg_offset_mean;

  // HET: log sigma regression
  // vector[n_group] grp_offset_var;
  // matrix[n_state, n_party] st_offset_var;
  // matrix[n_region, n_party] rg_offset_var;


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

    // clean up
    if (is_nan(theta[group[i]])) {
      theta[group[i]] = 
        const_mean[party[i]] + 
        grp_offset_mean[group[i]] + 
        st_offset_mean[state[i], party[i]] +
        rg_offset_mean[region[i], party[i]];
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
  const_mean ~ normal([0, 0], [1, 1]); // TK fix
  z_grp_mean ~ normal(0, 1);       // group zs, all independent and one-dim
  
  for (p in 1:n_party) {
    
    coef_grp_mean[ , p] ~ normal(0, 1); // multivariate? soon DLM
    coef_st_mean[ , p] ~ normal(0, 1);

    z_st_mean[ , p] ~ normal(0, 1);     // SxP state Z scores
    z_rg_mean[ , p] ~ normal(0, 1);     // RxP region Z scores

    // P-vectors of error scales
    scale_grp_mean[p] ~ lognormal(0, 1); // group scale
    scale_st_mean[p] ~ lognormal(0, 1);  // state scales
    scale_rg_mean[p] ~ lognormal(0, 1);  // region scales

  }



  // to do: DLM


}

generated quantities {

  vector[n_group] idtheta;
  real theta_iter_mean;
  real theta_iter_sd;

  theta_iter_mean = mean(theta);
  theta_iter_sd = sd(theta);

  idtheta = (theta - theta_iter_mean) / theta_iter_sd;

}
