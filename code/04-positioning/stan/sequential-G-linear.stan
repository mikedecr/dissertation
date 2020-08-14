data {
 
  int<lower=1> N;      // number of observations
  int<lower = 1> D; // number of unique districts
  vector[N] y;         // response variable
  int<lower = 1> d[N];            // district index
  
  // predictive data (ideal point is parameter)
  vector[N] mediator;
  int<lower = 1> K_med;   // n. intermediate confounders
  int<lower = 1> K_trt;   // n. pre-treatment confounders
  matrix[N, K_med] Z_med; // intermediate confounders
  matrix[N, K_trt] X_trt; // pre-treatment confounders

  // fix mediator at some value
  real blip_value;

  // prior hyperparameters for ideal points
  vector[D] ideal_means;
  matrix[D, D] ideal_cov;

  // joint prior
  int<lower = 0, upper = 1> joint_prior;
  real<lower = 1> lkj_value;

}

transformed data {

  // eventually want to calculate mundlak device (group means)?
  matrix[N, K_med + K_trt] X_med;

  X_med = append_col(X_trt, Z_med);

}

parameters {

  // ideal point draws
  vector[D] theta; 

  // stage 1 and 2 coefficients
  real const_med; 
  real const_trt;
  real coef_mediator;           // mediator effect (for blipping)
  real coef_theta_med;          // stage 1 theta coef
  real coef_theta_trt;          // stage 2 theta coef
  vector[K_med + K_trt] wt_med; // confounder weights
  vector[K_trt] wt_trt;         // confounder weights

  // district offsets and hypervariances
  vector[D] ranef_med;
  vector[D] ranef_trt;
  real<lower = 0> hypersigma_med;
  real<lower = 0> hypersigma_trt;
  
  
  // outcome dispersion under independence
  real<lower = 0> sigma_med;
  real<lower = 0> sigma_trt;
  // outcome dispersion under correlation
  vector<lower = 0>[2] joint_scales;
  corr_matrix[2] joint_corr;
  
}

transformed parameters {
 
  cov_matrix[2] joint_cov;
  joint_cov = quad_form_diag(joint_corr, joint_scales);

}

model {

  // outcome models
  vector[N] yhat_med;
  vector[N] yhat_trt;

  vector[N] blipdown_function;
  vector[N] blip_y;

  // loop over N to get each theta and ranef where it belongs
  for (i in 1:N) {
   
    yhat_med[i] = 
      const_med + 
      (theta[d[i]] * coef_theta_trt) + 
      (mediator[i] * coef_mediator) + 
      (X_med[i, ] * wt_med) + 
      ranef_med[d[i]];  

    blipdown_function[i] = coef_mediator * (mediator[i] - blip_value);
    blip_y[i] = y[i] - blipdown_function[i];

    yhat_trt[i] = 
      const_trt + 
      (theta[d[i]] * coef_theta_trt) + 
      (X_trt[i, ] * wt_trt) + 
      ranef_trt[d[i]];
  }
  
  // how to do joint model?
  if (joint_prior == 0) {

    y ~ normal(yhat_med, sigma_med);
    blip_y ~ normal(yhat_trt, sigma_trt);  
  
  } else if (joint_prior == 1) {

    vector[2] joint_y[N];
    vector[2] joint_yhat[N];

    joint_y[1] = y;
    joint_y[2] = blip_y;
    joint_yhat[1] = yhat_med;
    joint_yhat[2] = yhat_trt;

    for (n in 1:N) {
      joint_y ~ multi_normal(joint_yhat, joint_cov);
    }
    

  }
  
  
  // outcome dispersion
  joint_corr ~ lkj_corr(lkj_value);
  sigma_med ~ normal(0, 5);
  sigma_trt ~ normal(0, 5);
  joint_scales ~ normal(0, 5);
  
  // multivariate ideal point prior
  theta ~ multi_normal(ideal_means, ideal_cov);

  // weights
  const_med ~ normal(0, 10); 
  const_trt ~ normal(0, 10); 
  coef_mediator ~ normal(0, 10); 
  coef_theta_med ~ normal(0, 10); 
  coef_theta_trt ~ normal(0, 10); 
  wt_med ~ normal(0, 10); 
  wt_trt ~ normal(0, 10);
  
  // ranefs 
  ranef_med ~ normal(0, hypersigma_med);
  ranef_trt ~ normal(0, hypersigma_trt);

  // ranef dispersion 
  hypersigma_med ~ normal(0, 2);
  hypersigma_trt ~ normal(0, 2);
  

}

generated quantities {
 


}
