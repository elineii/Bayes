library("rstan")
library('Metrics')
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

metrics <- function(model, lm=FALSE){
  if (LM) {
    lm_beta = coefficients(model)
    lm_MSE_beta = c(rmse(beta[1], coefficients(model_lm)[1]),
                    rmse(beta[2], coefficients(model_lm)[2]),
                    rmse(beta[3], coefficients(model_lm)[3]))
    lm_MAPE_beta = c(mape(beta[1], coefficients(model_lm)[1]),
                     mape(beta[2], coefficients(model_lm)[2]),
                     mape(beta[3], coefficients(model_lm)[3]))
    return (c(lm_beta, lm_MSE_beta, lm_MAPE_beta))
  }
  else {
    posterior <- extract(model)
    beta = colMeans(posterior$beta)
    MSE_beta = c(rmse(beta[1], posterior$beta[, 1]),
                 rmse(beta[2], posterior$beta[, 2]),
                 rmse(beta[3], posterior$beta[, 3]))
    MAPE_beta = c(mape(beta[1], posterior$beta[, 1]),
                  mape(beta[2], posterior$beta[, 2]),
                  mape(beta[3], posterior$beta[, 3]))
    return (c(beta, MSE_beta, MAPE_beta))
  }
}

results = list()

# Bad priors beta ~ (0, 10)
# Semigood priors beta ~ (beta, 5)
# Good priors beta ~ (beta, 1)

# beta = c(2, -5, 10)

for (N in seq(50, 70, 10)) {
  n_iters <- 100                            # Число симуляций
  
  x_1 <- runif(n = N, min = 0, max = 3)     # Генерируем x_1, x_2
  x_2 <- rnorm(n = N, 5, 4)  
  
  X <- cbind(1, x_1, x_2)                   # Аггрегируем все регрессоры => получаем реализацию выборки
  
  beta <- matrix(c(2, -5, 10), nrow = 3)    # Задаем реальные значения alpha, b_0, b_1
  
  epsilon <- rnorm(n = N)                   # Генерируем ошибку из нормального распределения
  
  y_star <- as.vector(X %*% beta + epsilon) # Находим вектор у
  
  rnames = c("b_0","b_1", "b_2", 
             "MSE_b_0", "MSE_b_1", "MSE_b_2", 
             "MAPE_b_0", "MAPE_b_1", "MAPE_b_2")
  
  df_bad <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
  names(df_bad) <- rnames
  df_bad = df_bad[FALSE,]
  
  df_semigood <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
  names(df_semigood) <- rnames
  df_semigood = df_semigood[FALSE,]
  
  df_good <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
  names(df_good) <- rnames
  df_good = df_good[FALSE,]
  
  df_lm <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
  names(df_lm) <- rnames
  df_lm = df_lm[FALSE,]
  
  data <- list(x = X,                       # vector[3] x[n]
               y = y_star,                  # real y[n]
               n = dim(X)[1],               # int<lower=0> n
               k = dim(X)[2])               # int<lower=0> k
  
  for (i in 1:n_iters){
    
    model_bad <- stan(file = "linreg with normal errors -- bad priors.stan",
                         data = data,                # входные данные
                         chains = 1,                 # количество выборок из апостериорного распределения
                         iter = 2000)                # удвоенный объем выборки из апостериорного распределения

    df_bad <- rbind(metrics(model_bad))
    
    model_semigood <- stan(file = "linreg with normal errors -- semigood priors.stan",
                      data = data,                # входные данные
                      chains = 1,                 # количество выборок из апостериорного распределения
                      iter = 2000)                # удвоенный объем выборки из апостериорного распределения
    
    df_semigood <- rbind(metrics(model_semigood))
    
    model_good <- stan(file = "linreg with normal errors -- good priors.stan",
                      data = data,                # входные данные
                      chains = 1,                 # количество выборок из апостериорного распределения
                      iter = 2000)                # удвоенный объем выборки из апостериорного распределения
    
    df_good <- rbind(metrics(model_good))
    
    model_lm <- lm(y_star ~ cbind(x_1, x_2))
    
    df_good <- rbind(metrics(model_lm, lm=TRUE))
    
    #------------------------------------------
  }  
  
  names(df_bad) <- rnames
  names(df_semigood) <- rnames
  names(df_good) <- rnames
  names(df_lm) <- rnames
  
  true = c(beta, 0, 0, 0, 0, 0, 0)
  
  
  table = data.frame(true=true,
                     bad=colMeans(df_normal), 
                     good=colMeans(df_student), 
                     semigood=colMeans(df_log), 
                     lm=colMeans(df_lm))
  
  results <- results.append(table)
}

