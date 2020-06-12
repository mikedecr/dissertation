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
  int<lower = 1> p;       // number of confounders
  matrix[n, p] X;         // design matrix of covariates, no leading 1s


  // Choice set info
  int<lower = 1> G;         // number of distinct groups/sets
  int<lower = 1> n_g[G];    // size of each choice set
  // int<lower = 1> g_code[n];   // group/set code for each unit (R)
                                 // not here: group INDEX[G] = ( 1:G )

  // user-supplied parameters
  int<lower = 1> nodes_select;   // neuron density, selection step
  int<lower = 1> nodes_outcome;  // neuron density, covariate function
  // int<lower = 1> nodes_theta; // neuron density, theta function
  real hidden_scale_select;       // coef scale
  real act_scale_select;          // coef scale
  real hidden_scale_outcome;       // coef scale
  real act_scale_outcome;          // coef scale

}

transformed data {
  
  // covariate matrices and dimensions
  int<lower = 1> p_outcome = p + 1; // add 1s to X
  int<lower = 1> p_select = p + 1;  // add resid to X (for now)
  vector[n] theta_x_cf = theta .* cf_score;
  
  // X_outcome happens after residualizing
  // should CF be in the X?
  matrix[n, p_select] X_select;
  
  // add 1s to covariates
  X_select = append_col(rep_vector(1, n), X);

}



parameters {
 
  matrix[p_select, nodes_select] hidden_select;
  vector[nodes_select] act_select;
  real<lower = 0> sigma_select;

  matrix[p_outcome, nodes_select] hidden_outcome;
  vector[nodes_select] act_outcome;

}

transformed parameters {
  
  // int<lower = 1, upper = n_samples> sample_column = rng...
  // vector[n] theta = col(theta_matrix, sample_column);

}


model {

  vector[n] expected_int; // E[treatment | x]
  vector[n] resid;    // treatment - E[treatment | x]

  matrix[n, p_outcome] X_outcome;
  vector[n] util;  // latent utility
  vector[n] pprob; // softmax utility
  int pos = 1;     // for segmenting

  
  // ----- selection model -----
  expected_int = tanh(X_select * hidden_select) * act_select;

  theta_x_cf ~ normal(expected_int, sigma_select); // student_t?
  to_vector(hidden_select) ~ normal(0, hidden_scale_select);
  act_select ~ normal(0, act_scale_select);
  sigma_select ~ normal(0, 1);


  // ----- outcome model -----
  
  // residualize treatment, add to predictors
  resid = theta_x_cf - expected_int;
  X_outcome = append_col(resid, X);

  // future: "shrink to homogeneity"
  util = tanh(X_outcome * hidden_outcome) * act_outcome;
  
  // from stan manual ("ragged data structures"):
  // calculate choice probs in each choice set: segment(v, start, length)
  for (g in 1:G) {
    
    pprob[pos:(pos - 1) + n_g[g]] = softmax(segment(util, pos, n_g[g]));
    pos = pos + n_g[g];

  }


  // choice model
  // sums log probability for successes only
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>
  target += y' * log(pprob);

  to_vector(hidden_outcome) ~ normal(0, hidden_scale_outcome);
  act_outcome ~ normal(0, act_scale_outcome);

}

