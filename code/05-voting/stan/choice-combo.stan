functions {

  // build a spline recursively
  vector build_b_spline(vector x, real[] ext_knots, int ind, int order);

  vector build_b_spline(vector x, real[] ext_knots, int ind, int order) {
    // INPUTS:
    //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[num_elements(x)] b_spline;
    vector[num_elements(x)] w1 = rep_vector(0, num_elements(x));
    vector[num_elements(x)] w2 = rep_vector(0, num_elements(x));
    
    if (order==1)
      for (i in 1:num_elements(x)) // O1 B-splines are piecewise constant
        b_spline[i] = (ext_knots[ind] <= x[i]) && (x[i] < ext_knots[ind + 1]);
    else {
      if (ext_knots[ind] != ext_knots[ind + order - 1])
        w1 = (to_vector(x) - rep_vector(ext_knots[ind], num_elements(x))) /
             (ext_knots[ind + order - 1] - ext_knots[ind]);
      if (ext_knots[ind + 1] != ext_knots[ind + order])
        w2 = 1 - (to_vector(x) - rep_vector(ext_knots[ind + 1], num_elements(x))) /
                 (ext_knots[ind + order] - ext_knots[ind + 1]);
      // Calculating B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(x, ext_knots, ind, order - 1) +
                 w2 .* build_b_spline(x, ext_knots, ind + 1, order - 1);
    }
    return b_spline;
  }

}

data {
 
  // X and Y data
  int<lower = 1> n;       // total cands
  int<lower = 1> p;       // predictors
  vector<lower = 0, upper = 1>[n] y; // unit win/loss
  matrix[n, p] X;         // all choice data and interactions

  // spline data
  vector[n] CF;
  vector[n] theta;
  // int<lower = 1> B; // bases

  int num_knots;            // num of knots
  int spline_deg;        // the degree of spline (is equal to order - 1)


  // Choice set info
  int<lower = 1> S;                     // number of distinct groups/sets
  int<lower = 1> n_set[S];                // size of each choice set
  // int<lower = 1> i_in_set[n];          // units in groups
  // int<lower = 1, upper = G> set[n];    // index groups

  // user-supplied parameters
  real prior_sd; // variable priors

}

transformed data {
  
  // center ideal point variables and combine
  real CF_offset = mean([ min(CF), max(CF)]);
  real theta_offset = mean([ min(theta), max(theta)]);

  vector[n] CF_center = CF - CF_offset;
  vector[n] theta_center = theta - theta_offset;

  matrix[n, 2] ideals = append_col(CF_center, theta_center);

  // post-estimation data things
  vector[100] CF_post;
  real CF_skipper = (max(CF) - min(CF)) / 100;
  vector[3] theta_post = 
    [mean(theta) - sd(theta), mean(theta), mean(theta) + sd(theta)]' - 
    theta_offset;
  // resume CF_post after all declarations
  matrix[100 * 3, 2] ideals_post;

  // knots and basis functions
  int num_basis = num_knots + spline_deg - 1;
  vector[spline_deg + num_knots] ext_knots_temp;
  vector[(2 * spline_deg) + num_knots] ext_knots; // set of extended knots

  vector[num_knots] knots;  
  real skipsize = (max(CF_center) - min(CF_center)) / (num_knots - 1);
  for (k in 1:num_knots) {
    knots[k] = min(CF_center) + (k - 1 * skipsize);
  }
  
  // extended knots at beginning and end
  ext_knots_temp = 
    append_row(rep_vector(knots[1], spline_deg), knots);
  ext_knots = 
    append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_deg));


  // anything we can do for post-estimation prediction?
  CF_post[1] = min(CF);
  for (i in 2:100) {
    CF_post[i] = CF_post[i - 1] + CF_skipper;
  }
  CF_post = CF_post - CF_offset;
  ideals_post = append_row(
    append_row(
      append_col(CF_post, rep_vector(theta_post[1], 100)), 
      append_col(CF_post, rep_vector(theta_post[2], 100)) 
    ), 
    append_col(CF_post, rep_vector(theta_post[3], 100))
  );


}

parameters {
  
  unit_vector[2] linkers;
  // figure out how many bases
  vector[num_basis] wt_spline_raw;
  real<lower = 0> spline_scale;
  vector[p] wt;

}

transformed parameters {

  // linear model contains spline function value
  vector[n] util;
  vector[n] spline_function;

  // spline coefs and input data
  vector[num_basis] wt_spline = wt_spline_raw * spline_scale;
  vector[n] ideal_distance = ideals * linkers;

  // matrix of basis functions
  matrix[n, num_basis] B;  

  // recursively build basis functions  
  for (b in 1:num_basis) {
    B[:, b] = 
    build_b_spline(ideal_distance, to_array_1d(ext_knots), b, spline_deg + 1);
  }

  // only need if last data exactly == last knot?
  // B[n, num_knots + spline_deg - 1] = 1;

  // linear model
  spline_function = B*wt_spline;
  util = spline_function + (X * wt);

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
  wt ~ normal(0, prior_sd);
  spline_scale ~ cauchy(0, 2);
  wt_spline_raw ~ normal(0, 1);

}

generated quantities {
  
  vector[100 * 3] distances_post = ideals_post * linkers;
  matrix[100 * 3, num_basis] B_post;
  vector[100 * 3] spline_post;

  // recursively build basis functions  
  for (b in 1:num_basis) {
    B_post[:, b] = 
    build_b_spline(distances_post, to_array_1d(ext_knots), b, spline_deg + 1);
  }

  // only need if last data exactly == last knot?
  // B[n, num_knots + spline_deg - 1] = 1;

  spline_post = B_post * wt_spline;

}
