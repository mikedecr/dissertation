// ----- Bayesian Neural Net for sequential G-----

// what this currently doesn't have:
// - sample theta (last column of Xs?)
// - crossfitting for treatment and outcome (neyman orthog)
// - recovering "true" treatment in GQs (add E() and residual trt)

data {
 
  int<lower = 1> n;
  int<lower = 1> med_neurons;
  int<lower = 1> trt_neurons;

  int<lower = 1> P_med;   // number of "inputs" in mediator model
  int<lower = 1> P_trt;   // number of "inputs" in treatment model
  
  matrix[n, P_med] X_med; // predictors
  matrix[n, P_trt] X_trt; // predictors

  vector[n] Y;            // outcome variable
  real blip_value;        // fixed value of mediator for blipdown

}

transformed data {
  
  matrix[n, P_med] X_blip; // mediator model predictors w/ fixed mediator
  vector[n] blip_vector;   // vector of fixed mediators
  
  // assign mediator column to blip_value
  blip_vector = rep_vector(blip_value, n);

  // re-assign mediator
  // does this mean second column?
  X_blip[2] = blip_vector;

}

parameters {

  matrix[p, med_neurons] med_wt;
  vector[med_neurons] med_act;
  real<lower = 0> med_sigma;

  matrix[p, trt_neurons] trt_wt;
  vector[trt_neurons] trt_act;
  real<lower = 0> trt_sigma;
  
}

transformed parameters {

  vector[n] EY_med;  // E[Y(a, m)]: neural net function for mediator
  vector[n] EY_M0;    // E[Y(a, 0)]: predicted neural net when M = 0

  vector[n] blip_fn; // blipdown function: difference when setting M = 0
  
  vector[n] EY_trt;  // neural net function for treatment

  // NN matrix dimensions: 
  // - neuron: tanh(NxP * PxJ) => NxJ
  // - activation: NxJ * Jx1 => Nx1


  // fit NN to mediator
  // predict NN mean for fixed M 
  EY_med = tanh(X_med * med_wt) * med_act; 
  EY_M0 = tanh(X_blip * med_wt) * med_act; // NxP*PxJ -> NxJ*Jx1 -> Nx1
  

  // blipdown f(): EXPECTED effect of fixing M vs. not
  // subtract difference from true Y
  blip_fn = EY_med - EY_M0; 
  blip_Y = Y - blip_fn;     

  // fit NN to treatment
  EY_trt = tanh(X_trt * trt_wt) * trt_act; // NxP*PxJ -> NxJ*Jx1 -> Nx1

}

model {

  // mediator and treatment (blipped) data models
  Y ~ normal(EY_med, med_sigma);
  blip_Y ~ normal(EY_trt, trt_sigma);

  // priors in each model
  to_vector(med_wt) ~ normal(0, 1);
  to_vector(med_act) ~ normal(0, 1);
  to_vector(trt_wt) ~ normal(0, 1);
  to_vector(trt_act) ~ normal(0, 1);
  med_sigma ~ normal(0, 1);
  trt_sigma ~ normal(0, 1);

}

generated quantities {
  // calculate ACDE at fixed levels
  // read in fixed ACDE values?
}
