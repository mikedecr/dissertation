// ----- Bayesian Neural Net -----

data {
 
  int<lower = 1> n_nodes; 
  int<lower = 1> n;               
  int<lower = 1> p; // number of "inputs"
  matrix[n, p] X;   // predictors
  vector[n] y;      // "output"

}

parameters {

  matrix[p, n_nodes] hidden_wt;
  vector[n_nodes] act_wt;
  real<lower = 0> sigma;
  
}

transformed parameters {

  matrix[n, n_nodes] neurons;
  vector[n] net_output;
  neurons = tanh(X * hidden_wt);  // NxP*PxJ -> NxJ*Jx1
  net_output = (neurons - mean(neurons)) * act_wt;  // NxJ*Jx1 -> Nx1

  // possibility: scale the latent space tanh() output to be mean zero?

}

model {

  y ~ normal(net_output, sigma);
  to_vector(hidden_wt) ~ normal(0, 1);
  to_vector(act_wt) ~ normal(0, 1);
  sigma ~ normal(0, 1);

}
