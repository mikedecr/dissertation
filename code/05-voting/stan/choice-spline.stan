// still need to identify better


data {
 
  // X and Y data
  int<lower = 1> n;       // total cands
  int<lower = 1> p;       // predictors
  vector<lower = 0, upper = 1>[n] y; // unit win/loss
  matrix[n, p] X;         // all choice data and interactions
  vector[n] CF;
  // vector[n] theta;
  int<lower = 1> B; // bases
  matrix[n, B] b_theta;

  // Choice set info
  int<lower = 1> S;                     // number of distinct groups/sets
  int<lower = 1> n_set[S];                // size of each choice set
  // int<lower = 1> i_in_set[n];          // units in groups
  // int<lower = 1, upper = G> set[n];    // index groups

  // user-supplied parameters
  real prior_sd; // variable priors

}

transformed data {

  // use for post-estimation prediction

}

parameters {
  
  real coef_CF;
  vector[B] wt_spline_raw;
  real<lower = 0> spline_scale;
  vector[p] wt;

}

transformed parameters {
   
  // scaled 
  vector[B] wt_spline;
  vector[n] coef_int;
  vector[n] util;

  // anchor coef 1 and weight the rest
  wt_spline[1] = wt_spline_raw[1];
  for (b in 2:B) {
    wt_spline[b] = wt_spline[b - 1] + (wt_spline_raw[b] * spline_scale);
  }

  // conditional CF effect
  coef_int = coef_CF + (b_theta * wt_spline);

  // linear model
  util = (CF .* coef_int) + (X * wt);

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
  coef_CF ~ normal(0, prior_sd);
  spline_scale ~ normal(0, 1);
  wt ~ normal(0, prior_sd);

  wt_spline_raw ~ normal(0, 1);

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
