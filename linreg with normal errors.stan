// Описываем входные данные
data {
  int<lower=0> n;                      // число наблюдений
  int<lower=0> k_1;                    // число предикторов для обеих моделей
  int<lower=0> k_2;
  matrix[n, k_1] x_1;                  // выборка для первой модели
  matrix[n, k_2] x_2;                  // выборка для второй модели
  matrix[n, 2] y;                      // значение регрессии
}

// Указываем параметры
parameters {
  vector[k_1] beta_1;                    // вектор коэффициентов 
  vector[k_2] beta_2;
  real<lower=0> sigma_1;               // error scale
  real<lower=0> sigma_2;
  real<lower=-1, upper=1> rho;
}

transformed parameters {
  matrix[n, 2] mu;
  cov_matrix[2] Sigma;
  Sigma[1,1] = square(sigma_1);
  Sigma[1,2] = rho * sigma_1 * sigma_2;
  Sigma[2,1] = rho * sigma_1 * sigma_2;
  Sigma[2,2] = square(sigma_2);
  
  mu[, 1] = (x_1 * beta_1);
  mu[, 2] = (x_2 * beta_2);
}

// Описываем модель
model {
  // priors
  beta_1 ~ normal(0, 50);
  beta_2 ~ normal(0, 50);
  sigma_1 ~ normal(50, 25);
  sigma_2 ~ normal(50, 25);
  rho ~ normal(0, 0.3);
  // likelihood
  for (i in 1:n){
    y[i,] ~ multi_normal(mu[i,], Sigma);
  }
}
