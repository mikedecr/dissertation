// Conditional logit
// layer-1 neural net

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
  int<lower = 1> n_nodes; // neuron density
  real hidden_prior_scale;          // coef scale
  real act_prior_scale;          // coef scale
  // correlation param?

}

transformed data {

  // total hidden params
  int PJ = p * n_nodes;

}



parameters {
 
  matrix[p, n_nodes] hidden_wt;
  vector[n_nodes] act_wt;

  // correlation matrix stuff
  vector[PJ] hidden_hypermeans;
  vector<lower = 0>[PJ] hidden_scales;
  corr_matrix[PJ] hidden_corr;

  vector[n_nodes] act_hypermeans;
  vector<lower = 0>[n_nodes] act_scales;
  corr_matrix[n_nodes] act_corr;

}

transformed parameters {

  cov_matrix[PJ] hidden_VC;
  cov_matrix[n_nodes] act_VC;

  hidden_VC = quad_form_diag(hidden_corr, hidden_scales);
  act_VC = quad_form_diag(act_corr, act_scales); 

}


model {

  vector[n] util;  // latent utility
  vector[n] pprob; // softmax utility
  int pos;     // for segmenting

  util = tanh(X * hidden_wt) * act_wt;
  pos = 1;

  // ----- DATA MODEL -----
      
  // from stan manual ("ragged data structures"):l
  // calculate choice probs in each choice set: segment(v, start, length)
  for (g in 1:G) {
    
    pprob[pos:(pos - 1) + n_g[g]] = softmax(segment(util, pos, n_g[g]));
    pos = pos + n_g[g];

  }

  // sums log probability for successes only
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>
  target += y' * log(pprob);




  // ----- PARAMETER MODEL -----

  // hidden weights
  // multinormal prior: mean, scale, correlation
  to_vector(hidden_wt) ~ multi_normal(hidden_hypermeans, hidden_VC);
  hidden_hypermeans ~ normal(0, 1);
  hidden_scales ~ normal(0, hidden_prior_scale);
  hidden_corr ~ lkj_corr(2);
  
  // activation weights
  act_wt ~ multi_normal(act_hypermeans, act_VC);
  act_hypermeans ~ normal(0, 1);
  act_scales ~ normal(0, act_prior_scale);
  act_corr ~ lkj_corr(2);

}

