// still need to identify better


data {
 
  // X and Y data
  int<lower = 1> n;       // total cands
  int<lower = 1> p;       // predictors
  vector<lower = 0, upper = 1>[n] y; // unit win/loss
  matrix[n, p] X;         // all choice data and interactions
  vector[n] CF;
  real theta[n];

  // Choice set info
  int<lower = 1> S;                     // number of distinct groups/sets
  int<lower = 1> n_set[S];                // size of each choice set
  // int<lower = 1> i_in_set[n];          // units in groups
  // int<lower = 1, upper = G> set[n];    // index groups

  // user-supplied parameters
  real prior_sd; // variable priors

}

transformed data {

  real gp_length_prior = fabs(max(theta) - min(theta)) / 3;

}



parameters {
  

  vector[p] wt;

  // --- gaussian process ---
  real coef_CF;            // centers GP on zero
  vector[n] f_theta;       // smooth interaction w/ theta (interaction)
  real<lower=0> gp_length; // length scale
  real<lower=0> gp_sd;     // marginal sd
  real<lower=0> gp_noise;  // residual SD

}

transformed parameters {
  
  // GP covariance matrix
  matrix[n, n] gp_cov =  
    cov_exp_quad(theta, gp_sd, gp_length) + 
    diag_matrix(rep_vector(square(gp_noise), n));
  matrix[n, n] gp_L = cholesky_decompose(gp_cov);

  // linear model
  vector[n] util = 
    (CF * coef_CF) + 
    f_theta +
    (X * wt); 

}

model {

  vector[n] pprob; // softmax utility
  int pos;         // for segmenting
  
  // from stan manual ("ragged data structures"):l
  // calculate choice probs in each choice set: segment(v, start, length)
  pos = 1;
  for (s in 1:S) {
    pprob[pos:(pos - 1) + n_set[s]] = softmax(segment(util, pos, n_set[s]));
    pos = pos + n_set[s];
  }
  
  // sums log probability for successes only
  // <https://khakieconomics.github.io/2019/03/17/The-logit-choice-model.html>
  target += y' * log(pprob);

  // priors
  f_theta ~ multi_normal_cholesky(rep_vector(0, n), gp_L);
  gp_length ~ normal(0, gp_length_prior);
  gp_sd ~ normal(0, 1);
  gp_noise ~ normal(0, 1);

  coef_CF ~ normal(0, prior_sd);
  wt ~ normal(0, prior_sd);

}

generated quantities {

  // vector[S] set_loglik; // prob for winning candidate in g
  // int pos = 1;         // for segmenting

  // // loglik from every GROUP (keep winning candidate only)
  // for (s in 1:S) {
  //   set_loglik[s] = 
  //     segment(y, pos, n_set[s])' * log(softmax(segment(util, pos, n_set[s])));
  //   pos = pos + n_set[s];
  // }

}
