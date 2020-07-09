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
  // int<lower = 1> g_code[n];             // group/set code for each unit (R)
  // int<lower = 1, upper = G> g_index[n]; // group/set INDEX (1:G)

  // user-supplied parameters
  int<lower = 0, upper = 1> hidden_const; // is there a hidden bias
  int<lower = 1> n_nodes; // neuron density
  real hid_prior_scale;          // coef scale
  real act_prior_scale;          // coef scale

}

transformed data {

  // add constant?
  int<lower = p> hid_p = p + hidden_const; // == p if no constant
  matrix[n, hid_p] hid_X;

  // add 1s if we're adding biases
  if (hidden_const == 1) {
    hid_X = append_col(rep_vector(1, n), X); // add constant
  } else if (hidden_const == 0) {
    hid_X = X;
  }

}


parameters {
 
  matrix[hid_p, n_nodes] hid_wt;
  vector[n_nodes] act_wt;

}

transformed parameters {
  
  vector[n] util = tanh(hid_X * hid_wt) * act_wt; // latent utility

}

model {

  vector[n] pprob;     // softmax utility
  int pos = 1;         // for segmenting
  
  // calculate choice probs for each case in each choice set: 
  // from stan manual ("ragged data structures"): segment(v, start, length)
  for (g in 1:G) {
    pprob[pos:(pos - 1) + n_g[g]] = softmax(segment(util, pos, n_g[g]));
    pos = pos + n_g[g];
  }

  // accumulates log prob for successes only
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>
  target += y' * log(pprob);

  // priors
  to_vector(hid_wt) ~ double_exponential(0, hid_prior_scale);
  act_wt ~ normal(0, act_prior_scale);

}

generated quantities {

  vector[G] grp_loglik; // prob for winning candidate in g
  int pos = 1;         // for segmenting

  // loglik from every GROUP (keep winning candidate only)
  for (g in 1:G) {
    grp_loglik[g] = 
      segment(y, pos, n_g[g])' * log(softmax(segment(util, pos, n_g[g])));
    pos = pos + n_g[g];
  }


}

