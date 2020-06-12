// Conditional logit
// layer-1 neural net

data {
 
  // X + Y
  int<lower = 1> n;       // number of units
  vector<lower = 0>[n] y; // long vector of outcomes win {0, 1}
  
  // key IVs
  // matrix[n, n_samples] theta_matrix;        // treatment
  vector[n] theta;        // treatment
  vector[n] cf_score;      // candidate CFscores
  
  // confounders
  int<lower = 1> P;       // number of confounders
  matrix[n, P] X;         // design matrix of covariates, no leading 1s


  // Choice set info
  int<lower = 1> G;         // number of distinct groups/sets
  int<lower = 1> n_g[G];    // size of each choice set
  // int<lower = 1> g_code[n];   // group/set code for each unit (R)
                                 // not here: group INDEX[G] = ( 1:G )

  // user-supplied parameters
  int<lower = 1> nodes_select;  // neuron density, selection step
  int<lower = 1> nodes_outcome; // neuron density, covariate function
                                
                                // future: neurons for treatment
  
  real hid_prior_select;        // coef scale
  real act_prior_select;        // coef scale
  real hid_prior_outcome;       // coef scale
  real act_prior_outcome;       // coef scale

}

transformed data {
  
  // covariate matrices and dimensions
  int<lower = 1> P_outcome = P + 1; // add 1s to X
  int<lower = 1> P_select = P + 1;  // add resid to X (for now)
  vector[n] theta_x_cf = theta .* cf_score;
  
  // X_outcome happens after residualizing
  // should CF be in the X?
  matrix[n, P_select] X_select;
  
  // add 1s to covariates
  X_select = append_col(rep_vector(1, n), X);

}



parameters {
 
  matrix[P_select, nodes_select] hid_select_raw;
  vector[nodes_select] act_select_raw;
  real<lower = 0> sigma_select;

  matrix[P_outcome, nodes_select] hid_outcome_raw;
  vector[nodes_select] act_outcome_raw;

}

transformed parameters {
  
  // extract theta?
    // int<lower = 1, upper = n_samples> sample_column = rng...
    // vector[n] theta = col(theta_matrix, sample_column);

}


model {

  vector[n] expected_int; // E[treatment | x]
  vector[n] resid;    // treatment - E[treatment | x]

  matrix[n, P_outcome] X_outcome;
  vector[n] util;  // latent utility
  vector[n] pprob; // softmax utility
  int pos = 1;     // for segmenting

  
  // ----- model algebra -----
  
  // predicted and residual treatment
  expected_int = tanh(X_select * hid_select) * act_select_raw;
  resid = theta_x_cf - expected_int; 
  X_outcome = append_col(resid, X); 

  // future goal: "shrink to homogeneity"
  util = tanh(X_outcome * hid_outcome_raw) * act_outcome_raw;
  
  // choice probs in ragged choice sets: segment(v, start, length)
  for (g in 1:G) {
    pprob[pos:(pos - 1) + n_g[g]] = softmax(segment(util, pos, n_g[g]));
    pos = pos + n_g[g];
  }

  // likelihoods
  theta_x_cf ~ normal(expected_int, sigma_select); // student_t?
  target += y' * log(pprob);
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>


  // ----- selection model -----

  to_vector(hid_select_raw) ~ normal(0, hid_prior_select);
  act_select_raw ~ normal(0, act_prior_select);
  sigma_select ~ normal(0, 1);


  // ----- choice model -----

  to_vector(hid_outcome_raw) ~ normal(0, hid_prior_outcome);
  act_outcome_raw ~ normal(0, act_prior_outcome);


}

