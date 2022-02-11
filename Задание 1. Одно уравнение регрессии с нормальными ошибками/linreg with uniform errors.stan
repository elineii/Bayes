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
  beta ~ uniform(-100, 100);
  sigma ~ uniform(0, 50);
  // likelihood
  y ~ normal(x * beta, sigma); 
}
