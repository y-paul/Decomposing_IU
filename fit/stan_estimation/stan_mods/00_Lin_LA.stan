functions {
  
  real pwf(real p, real gam) {
    
    real wp;
    
    if (p > 0) { wp = p^gam; } else { wp = 0; }
    
    return wp;
  }
}

data {
  
  // n - no. of data points per condition
  int<lower = 0> n;
  
  // N - no. of subjects
  int<lower = 0> N;
  
  // subject ind vector
  int<lower = 1> sub[n];
  
  // drug A objective pars
  matrix[n, 2] xa;
  matrix[n, 2] pa;
  
  // option B objective pars
  matrix[n, 2] xb;
  matrix[n, 2] pb;
  
  // frequencies
  matrix[n, 2] fa;
  matrix[n, 2] fb;
  
  // choices: col1 - de; col2 - experience
  int<lower = 0, upper = 1> choices[n, 2];
  
}

parameters {
  
  // spt individual parameters on z scale
  matrix[2, N] lam_z;
  matrix[2, N] theta_z;
  
  // spt means on probit scale
  row_vector[2] lam_mu_phi;
  row_vector[2] theta_mu_phi;
  
  // spt scales on probit
  vector<lower = 0>[2] lam_sigma;
  vector<lower = 0>[2] theta_sigma;
  
  // spt pars corr matrix
  cholesky_factor_corr[2] L_lam_omega;
  cholesky_factor_corr[2] L_theta_omega;
  
}

transformed parameters {
  
  // spt sv cpmutpation
  matrix[n, 2] v_xa_d;
  matrix[n, 2] w_pa;
  
  matrix[n, 2] v_xb_d;
  matrix[n, 2] w_pb;
  
  matrix[n, 2] v_xa_e;
  matrix[n, 2] w_fa;
  
  matrix[n, 2] v_xb_e;
  matrix[n, 2] w_fb;
  
  matrix[n, 2] sv_d;
  matrix[n, 2] sv_e;
  
  vector[n] de_sv_diff;
  vector[n] ex_sv_diff;
  
  // parameters on wanted scale
  vector[N] lambda_ex;
  vector[N] lambda_de;
  vector[N] theta_ex;
  vector[N] theta_de;
  
  // individual level pars on probit scale
  matrix[N, 2] lam_phi;
  matrix[N, 2] theta_phi;
  
  // transform individual parameters
  // this makes them dependent on the multivariate normal dist
  lam_phi = (diag_pre_multiply(lam_sigma, L_lam_omega) * lam_z)';
  theta_phi = (diag_pre_multiply(theta_sigma, L_theta_omega) * theta_z)';

  lambda_ex = Phi( lam_mu_phi[1] + lam_phi[,1] ) * 5;
  lambda_de = Phi( lam_mu_phi[2] + lam_phi[,2] ) * 5;
  theta_ex = Phi( theta_mu_phi[1] + theta_phi[,1] ) * 5;
  theta_de = Phi( theta_mu_phi[2] + theta_phi[,2] ) * 5;
  
  // for each data point with SOME search
  for(i in 1:n) {
    
    // DESCRIPTION //////////////////////////////////////////////
      
      // A
    v_xa_d[i,1] = xa[i,1]; // benefit
    v_xa_d[i,2] = -lambda_de[ sub[i] ] * xa[i,2]; // side effect
    
    w_pa[i,1] = pwf(pa[i,1], 1) ;
    w_pa[i,2] = pwf(pa[i,2], 1) ;
    
    sv_d[i,1] = v_xa_d[i,1] * w_pa[i,1] + v_xa_d[i,2] * w_pa[i,2];
    
    // B
    v_xb_d[i,1] = xb[i,1]; // benefit
    v_xb_d[i,2] = -lambda_de[ sub[i] ] * xb[i,2]; // side effect
    
    w_pb[i,1] = pwf(pb[i,1], 1) ;
    w_pb[i,2] = pwf(pb[i,2], 1) ;
    
    sv_d[i,2] = v_xb_d[i,1] * w_pb[i,1] + v_xb_d[i,2] * w_pb[i,2];
    
    // difference in favor of A
    de_sv_diff[i] = theta_de[ sub[i] ] * (sv_d[i,1] - sv_d[i,2]);
    
    // EXPPERIENCE //////////////////////////////////////////////
      
    // A
    v_xa_e[i,1] = xa[i,1]; // benefit
    v_xa_e[i,2] = -lambda_ex[ sub[i] ] * xa[i,2]; // side effect
    
    w_fa[i,1] = pwf(fa[i,1], 1) ;
    w_fa[i,2] = pwf(fa[i,2], 1) ;
    
    sv_e[i,1] = v_xa_e[i,1] * w_fa[i,1] + v_xa_e[i,2] * w_fa[i,2];
    
    // B
    v_xb_e[i,1] = xb[i, 1]; // benefit
    v_xb_e[i,2] = -lambda_ex[ sub[i] ] * xb[i, 2]; // side effect
    
    w_fb[i,1] = pwf(fb[i,1], 1) ;
    w_fb[i,2] = pwf(fb[i,2], 1) ;
    
    sv_e[i,2] = v_xb_e[i,1] * w_fb[i,1] + v_xb_e[i,2] * w_fb[i,2];
    
    // difference in favor of A
    ex_sv_diff[i] = theta_ex[ sub[i] ] * (sv_e[i,1] - sv_e[i,2]);
    
  }
  
}

model {
  
  lam_mu_phi ~ std_normal();
  to_vector(lam_z) ~ std_normal();
  lam_sigma ~ normal(.5, .13);
  L_lam_omega ~ lkj_corr_cholesky(4);
  
  theta_mu_phi ~ std_normal();
  to_vector(theta_z) ~ std_normal();
  theta_sigma ~ normal(.5, .13);
  L_theta_omega ~ lkj_corr_cholesky(4);
  
  
  // likelihood
  choices[,1] ~ bernoulli_logit( de_sv_diff ); // desccription
  choices[,2] ~ bernoulli_logit( ex_sv_diff ); // experience
  
}

// logliks for loo
generated quantities {
  
  matrix[2,2] lam_omega;
  matrix[2,2] theta_omega;
  real lam_r;
  real theta_r;
  
  real log_lik_ex[n];
  real log_lik_de[n];
  // real pA_ex[n];
  // real pA_de[n];

  // transform the means
  real mu_lambda_ex = Phi(lam_mu_phi[1]) * 5;
  real mu_lambda_de = Phi(lam_mu_phi[2]) * 5;
  real mu_theta_ex = Phi(theta_mu_phi[1]) * 5;
  real mu_theta_de = Phi(theta_mu_phi[2]) * 5;

  // sigmas for monitoring
  real sig_lambda_ex = lam_sigma[1];
  real sig_lambda_de = lam_sigma[2];
  real sig_theta_ex = theta_sigma[1];
  real sig_theta_de = theta_sigma[2];

  // cor matrix
  lam_omega = L_lam_omega * L_lam_omega';
  lam_r = lam_omega[1,2];
  
  theta_omega = L_theta_omega * L_theta_omega';
  theta_r = theta_omega[1,2];

  // log liks for elpd_loo
  for(i in 1:n) {
    
    log_lik_de[i] = bernoulli_logit_lpmf( choices[i,1] | de_sv_diff[i] );
    log_lik_ex[i] = bernoulli_logit_lpmf( choices[i,2] | ex_sv_diff[i] );
    
    // pA_ex[i] = inv_logit( de_sv_diff[i] ); // p(choose A)
    // pA_de[i] = inv_logit( ex_sv_diff[i] ); // p(choose A)
    
  }
  
  
}