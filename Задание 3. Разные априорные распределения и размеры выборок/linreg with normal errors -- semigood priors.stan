// Описываем входные данные
data {
  int<lower=0> n;                      // число наблюдений
  int<lower=0> k;                      // число предикторов
  matrix[n, k] x;                      // выборка
  vector[n] y;                         // значение регрессии
}

// Указываем параметры
parameters {
  vector[k] beta;                      // вектор коэффициентов 
  real<lower=0> sigma;                 // error scale
}

// Описываем модель
model {
  // priors
  beta[1] ~ normal(2, 5);
  beta[2] ~ normal(-5, 5);
  beta[3] ~ normal(10, 5);
  sigma ~ normal(0, 5);
  // likelihood
  y ~ normal(x * beta, sigma); 
}
