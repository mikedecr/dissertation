data {
 
  int<lower=1> N;      // number of observations
  int<lower = 1> D;    // number of unique districts
  vector[N] y;         // response variable
  int<lower = 1> d[N]; // district index
  
  // predictive data (ideal point is a parameter)
  vector[N] mediator;
  int<lower = 1> K_med;   // n. intermediate confounders
  int<lower = 1> K_trt;   // n. pre-treatment confounders
  matrix[N, K_med] Z_med; // intermediate confounders
  matrix[N, K_trt] X_trt; // pre-treatment confounders

  // fix mediator at some value
  real blip_value;

  // prior hyperparameters for ideal points
  vector[D] ideal_means;
  matrix[D, D] ideal_prec;

  // joint prior
  int<lower = 0, upper = 1> joint_prior;
  real<lower = 1> lkj_value;

}

transformed data {

  // do we eventually want a mundlak device?
  matrix[N, K_med + K_trt] X_med = append_col(X_trt, Z_med);
  // vector[N] y_scale = (y - mean(y)) / sd(y);

}

parameters {

  // ideal point draws
  vector[D] theta_raw; 

  // stage 1 and 2 coefficients
  real const_med;               // constant
  real coef_mediator;           // mediator effect (for blipping)
  real coef_theta_med;          // stage 1 theta coef
  vector[K_med + K_trt] wt_med; // confounder weights

  real const_trt;               // constant
  real coef_theta_trt;          // stage 2 theta coef
  vector[K_trt] wt_trt;         // confounder weights

  // district offsets and hypervariances
  vector[D] ranef_med_raw;
  vector[D] ranef_trt_raw;
  real<lower = 0> hypersigma_med;
  real<lower = 0> hypersigma_trt;
  
  // outcome dispersion under independence
  real<lower = 0> sigma_med;
  real<lower = 0> sigma_trt;

}

transformed parameters {
  
  // rescale theta so space is restricted
  vector[D] theta = (theta_raw - mean(theta_raw)) / sd(theta_raw);

  // noncentering
  vector[D] ranef_med = ranef_med_raw * hypersigma_med;
  vector[D] ranef_trt = ranef_trt_raw * hypersigma_trt; 


}

model {

  // outcomes and transformation
  vector[N] yhat_med;
  vector[N] yhat_trt;
  vector[N] blipdown_function;
  vector[N] blip_y;

  // loop over N to get each theta and ranef where it belongs
  for (i in 1:N) {
   
    yhat_med[i] = 
      const_med + 
      (mediator[i] * coef_mediator) + 
      (theta[d[i]] * coef_theta_med) + 
      (X_med[i, ] * wt_med) + 
      ranef_med[d[i]];  

    yhat_trt[i] = 
      const_trt + 
      (theta[d[i]] * coef_theta_trt) + 
      (X_trt[i, ] * wt_trt) + 
      ranef_trt[d[i]];

  }
  
  blipdown_function = coef_mediator * (mediator - blip_value);
  blip_y = y - blipdown_function;

  y ~ normal(yhat_med, sigma_med);
  blip_y ~ normal(yhat_trt, sigma_trt);
  
  // multivariate prior on RAW theta
  theta_raw ~ multi_normal_prec(ideal_means, ideal_prec);

  // weights
  const_med ~ normal(0, 5); 
  const_trt ~ normal(0, 5); 
  coef_mediator ~ normal(0, 1); 
  coef_theta_trt ~ normal(0, 1); 
  coef_theta_med ~ normal(0, 1); 
  wt_med ~ normal(0, 1); 
  wt_trt ~ normal(0, 1);
  
  // ranefs 
  ranef_med_raw ~ normal(0, 1);
  ranef_trt_raw ~ normal(0, 1);

  // ranef dispersion 
  hypersigma_med ~ cauchy(0, 1);
  hypersigma_trt ~ cauchy(0, 1);

  // outcome dispersion
  sigma_med ~ cauchy(0, 2);
  sigma_trt ~ cauchy(0, 2);

}

generated quantities {
 


}
