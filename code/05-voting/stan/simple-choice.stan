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

}

// transformed parameters {}

model {

  vector[n] pprob;            // softmax utility (unidentified)
  vector[n] util = X * coefs; // latent utility
  int pos = 1;                // placeholder for walking across vector

 
 // from stan: walk along long vectors, cutting into short bits
  for (g in 1:G) {

    // e.g. if pos = 1 and n_g = 3, 
    // probs[1:3] gets segment of util, start at 1, length 3
    pprob[pos:(pos - 1) + n_g[g]] = softmax(segment(util, pos, n_g[g]));

    // increment placeholder
    pos = pos + n_g[g];

  }
  
  // sums log probability for successes only
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>
  target += y' * log(pprob);

  // priors
  coefs ~ normal(0, prior_sd);

}

