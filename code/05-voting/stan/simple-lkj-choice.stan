// still need to identify better


data {
 
  // X and Y data
  int<lower = 1> n;       // number of units
  int<lower = 1> p;       // number of predictors
  matrix[n, p] X;         // all choice data and interactions
  vector<lower = 0>[n] y; // long vector of outcomes win {0, 1}

  // Choice set info
  int<lower = 1> G;                     // number of distinct groups/sets
  int<lower = 1> n_g[G];                // size of each choice set
  int<lower = 1> g_code[n];             // group/set code for each unit (R)
  // int<lower = 1, upper = G> g_index[n]; // group/set INDEX (1:G)

  // user-supplied parameters
  // neuron density? (eventually)
  // variable priors
  real prior_sd;

}



parameters {
 
  vector[p] coefs;
  vector[p] coef_hypermeans;
  vector<lower = 0>[p] coef_scales;
  corr_matrix[p] coef_corr;

}

transformed parameters {
  
  // vcov matrix of coefficients
  // diag(v)*M*diag(v)
  cov_matrix[p] coef_vc; 
  coef_vc = quad_form_diag(coef_corr, coef_scales); 

}

model {

  vector[n] pprob; // softmax utility
  vector[n] util = X * coefs; // linear model
  int pos = 1;     // for segmenting
  
  // from stan manual ("ragged data structures"):l
  // calculate choice probs in each choice set: segment(v, start, length)
  for (g in 1:G) {
    
    pprob[pos:(pos - 1) + n_g[g]] = softmax(segment(util, pos, n_g[g]));
    pos = pos + n_g[g];

  }

  // sums log probability for successes only
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>
  target += y' * log(pprob);

  // priors
  coefs ~ multi_normal(coef_hypermeans, coef_vc);
  coef_hypermeans ~ std_normal();
  coef_corr ~ lkj_corr(2);    // coef off-diagonal corrs
  coef_scales ~ normal(0, prior_sd); // coef scale components

}

