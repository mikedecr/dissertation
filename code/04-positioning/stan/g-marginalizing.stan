data {
 
  int<lower=1> N;      // number of observations (candidates)
  vector[N] y;         // response

  int<lower = 1> D;    // number of districts
  int<lower = 1> d[N]; // index district for each unit
  
  // predictors
  vector[N] mediator;     
  int<lower = 1> K_med;   // num. intermediate confounders
  int<lower = 1> K_trt;   // num. pre-treatment confounders
  matrix[N, K_med] Z_med; // intermediate confounder data
  matrix[N, K_trt] X_trt; // pre-treatment confounder data

  // fix mediator at some value for blipdown
  real blip_value;

  // prior hyperparameters for ideal points
  // (moved this to precision since I got the sense it would be faster?)
  vector[D] ideal_means;
  matrix[D, D] ideal_prec;

}

transformed data {

  // do we eventually want a mundlak device?
  
  matrix[N, K_med + K_trt] X_med;  // combined mediator covariates 

  // NxD district identifiers (initialized 0)
  matrix[N, D] L = rep_matrix(0, N, D);

  // N and D identity matrices
  matrix[N, N] NI = diag_matrix(rep_vector(1, N));
  matrix[D, D] DI = diag_matrix(rep_vector(1, D));

  // assign 1 if unit i is in district d
  for (i in 1:N) {
    L[i, d[i]] = 1;
  }
  
  // combined stage 1 predictor matrix
  X_med = append_col(X_trt, Z_med);

}

parameters {

  // ideal point draws
  vector[D] theta_raw; 

  // stage 1 coefs
  real const_med;               // constant
  real coef_mediator;           // mediator effect (for blipping)
  real coef_theta_med;          // stage 1 theta coef
  vector[K_med + K_trt] wt_med; // confounder weights

  // stage 2 coefs
  real const_trt;               // constant
  real coef_theta_trt;          // stage 2 theta coef
  vector[K_trt] wt_trt;         // confounder weights

  // dispersion terms, district and candidate
  real<lower = 0> hypersigma_med;
  real<lower = 0> hypersigma_trt;
  real<lower = 0> sigma_med;
  real<lower = 0> sigma_trt;
  
}

transformed parameters {
  
  vector[D] theta;

  // precision matrices for stages 1 and 2
  matrix[N, N] prec_med;
  matrix[N, N] prec_trt;

  // rescale theta so space is restricted!
  theta = (theta_raw - mean(theta_raw)) / sd(theta_raw);

  // reparametrize multilevel vcov into precision
  // using woodbury identity
  prec_med = 
    (pow(sigma_med, -2) * NI) - 
      pow(sigma_med, -2) * L * 
        inv(
          (pow(hypersigma_med, -2) * DI) + (pow(sigma_med, -2) * L' * L) 
      ) * 
      L' * pow(sigma_med, -2);

  prec_trt = 
    (pow(sigma_trt, -2) * NI) - 
      pow(sigma_trt, -2) * L * 
      inv(
        (pow(hypersigma_trt, -2) * DI) + (pow(sigma_trt, -2) * L' * L) 
      ) * 
      L' * pow(sigma_trt, -2);

}

model {

  // outcomes and transformation
  vector[N] yhat_med;
  vector[N] yhat_trt;
  vector[N] blipdown_function;
  vector[N] blip_y;

  // use parameters to demediate stage 2 outcome
  blipdown_function = coef_mediator * (mediator - blip_value);
  blip_y = y - blipdown_function;

  // create yhat
  // (loop over N to get each theta[d] where it belongs)
  for (i in 1:N) {
   
    yhat_med[i] = 
      const_med + 
      (mediator[i] * coef_mediator) + 
      (theta[d[i]] * coef_theta_med) + 
      (X_med[i, ] * wt_med);  

    yhat_trt[i] = 
      const_trt + 
      (theta[d[i]] * coef_theta_trt) + 
      (X_trt[i, ] * wt_trt);

  }
  
  // outcomes are MVNorm w/ precision matrix
  y ~ multi_normal_prec(yhat_med, prec_med);
  blip_y ~ multi_normal_prec(yhat_trt, prec_trt);
  

  // outcome and district scales
  sigma_med ~ cauchy(0, 1);
  sigma_trt ~ cauchy(0, 1);
  hypersigma_med ~ cauchy(0, 1);
  hypersigma_trt ~ cauchy(0, 1);
  
  // multivariate prior on RAW theta (also precision setup)
  theta_raw ~ multi_normal_prec(ideal_means, ideal_prec);

  // regression weights
  const_med ~ normal(0, 5); 
  const_trt ~ normal(0, 5); 
  coef_mediator ~ normal(0, 1); 
  coef_theta_trt ~ normal(0, 1); 
  coef_theta_med ~ normal(0, 1); 
  wt_med ~ normal(0, 1); 
  wt_trt ~ normal(0, 1);

}

generated quantities {
 
  // maybe some day

}
