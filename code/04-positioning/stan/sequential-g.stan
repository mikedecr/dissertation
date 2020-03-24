data {
 
  

}

// transformed data {

  // eventually want to calculate group means
  // of level-1 covariates

// }

parameters {
 


}

transformed parameters {
 
  real blip_value;
  vector demediation_function[n];
  vector y_blipdown[n];

  demediation_function = coef_mediator .* (mediator - blip_value);
  y_blipdown = y - demediation_function;

}

model {

  y ~ normal(yhat_mediation, sigma_mediation);
  y_blipdown ~ normal(yhat_direct, sigma_direct);

  

}

generated quantities {
 


}
