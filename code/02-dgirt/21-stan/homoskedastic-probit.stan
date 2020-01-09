// WHY?
// we don't assume that we have groups x parties x items data
// could be missing a party in a group?, items w/in group, etc.
// - but we should expand item x group and fill in zeroes
// data are _ITEM-GROUP LEVEL_; responses, covariates, etc

// data: create a long table of everything
// then: grouped response data & matrix of covariates

// to do:
// - [ ] implement a to-do code (¿¿??) 
// - [ ] expando model algebra (likelihood as a grid?)
// - [ ] flexi priors (intercept, what else?)
// - [ ] mvnorm hierarchical coefs?
// - [ ] mvnorm item params?
    // read the text, exponential priors, lkj on the correlation
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
  int<lower = 1> k_d;              // num of district covariates
  matrix[n, k_d] X;     // district covariate matrix

  int<lower = 0> k_s;              // num of state covariates
  matrix[n, k_s] Z;     // state covariate matrix

  // no region covariates?
  // int k_r; // matrix W;

  // ¿? n vs. n_* rows? How to deal with this in params

  // ---- prior data ----
  // theta means
  // real prior_party_mean[P];

}


parameters {
 
  // --- IRT ---
  real<lower = 0> sigma_in_g; // only 1. If heteroskedastic: next block
  vector[2] item_params[n_item]; // cutpoint and log discrimination

  // hierarchical item/LKJ setup
  vector[2] mu;
  vector<lower = 0>[2] item_scales;
  corr_matrix[2] item_corr;


  
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
  vector<lower = 0, upper = 1>[n] pprob;       // normal CDF
  vector[n] eta;                               // link scale index
  vector[n_group] theta;                       // group mean

  // item parameters
  vector[n_item] cut_raw;       // raw item midpoint
  vector[n_item] log_disc_raw;  // raw item discrimination
  vector[n_item] cutpoint;                  // identified cutpoint
  vector<lower = 0>[n_item] discrimination; // identified discrimination
  vector<lower = 0>[n_item] dispersion;

  // item covariance matrix
  cov_matrix[2] item_sigma; //item_scales %*% item_corr %*% item_scales

  // --- hierarchical regressions ---
  // theta regression
  vector[n_group] grp_offset_mean;
  matrix[n_state, n_party] st_offset_mean;
  matrix[n_region, n_party] rg_offset_mean;

  // extract raw item params
  // first index is ARRAY position, second is vector element?
  cut_raw = to_vector(item_params[ , 1]);
  log_disc_raw = to_vector(item_params[ , 2]);
  // cutpoints are mean 0 (dynamic: in year 1)
  // log-disc are mean 0 (disc is product 1)
  cutpoint = cut_raw - mean(cut_raw);
  discrimination = exp(log_disc_raw - mean(log_disc_raw));
  dispersion = inv(discrimination);

  // create VC matrix for items: diag(v)*M*diag(v)
  item_sigma = quad_form_diag(item_corr, item_scales);

  // loop over groups to get theta
  // IRT index (loop group-item)
  // later: expando algebra style
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

  pprob = Phi_approx(eta);

}



model {
 
  // ----- data model -----
  y ~ binomial(trials, pprob);

  // weighted likelihood
  // target += y .* log(pprob) + (trials - y) .* log(1 - pprob);


  // ----- IRT params -----
  item_corr ~ lkj_corr(2);    // lkj prior
  item_scales ~ normal(0, 1); // half normal scales
  sigma_in_g ~ lognormal(0, 1);    // will become regression
  
  // array of 2-vectors drawn from 2D multinormal normal
  item_params ~ multi_normal(mu, item_sigma);
  
  // if we need to loop items, major index is the array position
  // for (j in 1:n_item) {
  //   item_params[j] ~ multi_normal(mu, item_sigma);
  // }



  // ---- district and state regressions ----
  const_mean ~ normal([0, 0], [1, 1]); // TK fix
  z_grp_mean ~ normal(0, 1);       // group zs, all independent and one-dim
  
  for (p in 1:n_party) {
    
    coef_grp_mean[ , p] ~ normal(0, 0.5); // multivariate? soon DLM
    coef_st_mean[ , p] ~ normal(0, 0.5);

    z_st_mean[ , p] ~ normal(0, 1);     // SxP state Z scores
    z_rg_mean[ , p] ~ normal(0, 1);     // RxP region Z scores

    // P-vectors of error scales
    scale_grp_mean[p] ~ normal(0, 1); // group scale
    scale_st_mean[p] ~ normal(0, 1);  // state scales
    scale_rg_mean[p] ~ normal(0, 1);  // region scales

  }

  // to do: DLM
  
}

generated quantities {

  real item_rho; 
  // vector[n_group] resid_grp_mean;             // G-long grp errors
  // matrix[n_state, n_party] resid_st_mean;     // SxP state errors
  // matrix[n_region, n_party] resid_rg_mean;    // RxP two-d region errors

  item_rho = item_corr[1, 2];   // random off-diagonal
  
}
