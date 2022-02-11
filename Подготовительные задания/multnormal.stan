// Описываем входные данные
data {
  int<lower=0> n;              // число наблюдений
  vector[2] x[n];              // выборка
}

// Указываем параметры
parameters {
  vector[2] mu;
  vector<lower=0>[2] sigma;
  real<lower=-1,upper=1> rho;
}

transformed parameters {
  cov_matrix[2] Sigma;
  Sigma[1,1] = square(sigma[1]);
  Sigma[1,2] = rho * sigma[1] * sigma[2];
  Sigma[2,1] = rho * sigma[1] * sigma[2];
  Sigma[2,2] = square(sigma[2]);
}

// Описываем модель
// Под target понимается ln(f(mu,sigma|x)) ~= ln(f(mu)) + ln(f(sigma)) + ln(f(x|mu, sigma))
// Где под ~= понимается пропорционально
model {
  target += uniform_lpdf(mu | -100, 100);        // априорное распределение mu, то есть ln(f(mu))
  target += uniform_lpdf(sigma | 0, 50);         // априорное распределение sigma, то есть ln(f(sigma))
  target += uniform_lpdf(rho | -1, 1);
  target += multi_normal_lpdf(x | mu, Sigma);    // распределение данных при, то есть ln(f(x|mu, sigma))
  }                                              // фиксированных параметрах
