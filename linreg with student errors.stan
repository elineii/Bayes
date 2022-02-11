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
  beta ~ student_t(3, 0, 50);
  sigma ~ student_t(3, 50, 25);
  // likelihood
  y ~ normal(x * beta, sigma); 
}
