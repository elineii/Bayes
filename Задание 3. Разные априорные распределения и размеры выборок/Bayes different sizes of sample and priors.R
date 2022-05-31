library("rstan")
library('Metrics')
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

metrics <- function(model, lm=FALSE){
  if (lm) {
    lm_beta = coefficients(model)
    lm_MSE_beta = c(rmse(beta[1], lm_beta[1]),
                    rmse(beta[2], lm_beta[2]),
                    rmse(beta[3], lm_beta[3]))
    lm_MAPE_beta = c(mape(beta[1], lm_beta[1]),
                     mape(beta[2], lm_beta[2]),
                     mape(beta[3], lm_beta[3]))
    return (c(lm_beta, lm_MSE_beta, lm_MAPE_beta))
  }
  else {
    posterior <- extract(model)
    model_beta = colMeans(posterior$beta)
    MSE_beta = c(rmse(beta[1], model_beta[1]),
                 rmse(beta[2], model_beta[2]),
                 rmse(beta[3], model_beta[3]))
    MAPE_beta = c(mape(beta[1], model_beta[1]),
                  mape(beta[2], model_beta[2]),
                  mape(beta[3], model_beta[3]))
    return (c(model_beta, MSE_beta, MAPE_beta))
  }
}

results = list()

# Bad priors beta ~ (0, 10)
# Semigood priors beta ~ (beta, 5)
# Good priors beta ~ (beta, 1)

# beta = c(2, -5, 10)

# accidentally lm = ml

for (N in seq(50, 300, 10)) {
  n_iters <- 100                             # Число симуляций
  
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
    
    model_bad <- stan(file = "C://Users/kostr/Documents/Bayes/linreg with normal errors -- bad priors.stan",
                      data = data,                # входные данные
                      chains = 1,                 # количество выборок из апостериорного распределения
                      iter = 2000)                # удвоенный объем выборки из апостериорного распределения
    
    df_bad <- rbind(df_bad, metrics(model_bad))
    
    model_semigood <- stan(file = "C://Users/kostr/Documents/Bayes/linreg with normal errors -- semigood priors.stan",
                           data = data,                # входные данные
                           chains = 1,                 # количество выборок из апостериорного распределения
                           iter = 2000)                # удвоенный объем выборки из апостериорного распределения
    
    df_semigood <- rbind(df_semigood, metrics(model_semigood))
    
    model_good <- stan(file = "C://Users/kostr/Documents/Bayes/linreg with normal errors -- good priors.stan",
                       data = data,                # входные данные
                       chains = 1,                 # количество выборок из апостериорного распределения
                       iter = 2000)                # удвоенный объем выборки из апостериорного распределения
    
    df_good <- rbind(df_good, metrics(model_good))
    
    model_lm <- lm(y_star ~ cbind(x_1, x_2))
    
    df_lm <- rbind(df_lm, metrics(model_lm, lm=TRUE))
    
    #------------------------------------------
  }  
  
  names(df_bad) <- rnames
  names(df_semigood) <- rnames
  names(df_good) <- rnames
  names(df_lm) <- rnames
  
  true = c(beta, 0, 0, 0, 0, 0, 0)
  
  
  table = data.frame(true=true,
                     bad=colMeans(df_bad), 
                     semigood=colMeans(df_semigood), 
                     good=colMeans(df_good), 
                     lm=colMeans(df_lm))
  
  results <- c(results, list(table))
}

results

bad_beta0_RMSE <- c()
for (elem in results){
  bad_beta0_RMSE <- c(bad_beta0_RMSE, elem['bad'][4,])
}
bad_beta1_RMSE <- c()
for (elem in results){
  bad_beta1_RMSE <- c(bad_beta1_RMSE, elem['bad'][5,])
}
bad_beta2_RMSE <- c()
for (elem in results){
  bad_beta2_RMSE <- c(bad_beta2_RMSE, elem['bad'][6,])
}
bad_beta0_MAPE <- c()
for (elem in results){
  bad_beta0_MAPE <- c(bad_beta0_MAPE, elem['bad'][7,])
}
bad_beta1_MAPE <- c()
for (elem in results){
  bad_beta1_MAPE <- c(bad_beta1_MAPE, elem['bad'][8,])
}
bad_beta2_MAPE <- c()
for (elem in results){
  bad_beta2_MAPE <- c(bad_beta2_MAPE, elem['bad'][9,])
}

cat(bad_beta0_RMSE)
cat(bad_beta1_RMSE)
cat(bad_beta2_RMSE)
cat(bad_beta0_MAPE)
cat(bad_beta1_MAPE)
cat(bad_beta2_MAPE)

semigood_beta0_RMSE <- c()
for (elem in results){
  semigood_beta0_RMSE <- c(semigood_beta0_RMSE, elem['semigood'][4,])
}
semigood_beta1_RMSE <- c()
for (elem in results){
  semigood_beta1_RMSE <- c(semigood_beta1_RMSE, elem['semigood'][5,])
}
semigood_beta2_RMSE <- c()
for (elem in results){
  semigood_beta2_RMSE <- c(semigood_beta2_RMSE, elem['semigood'][6,])
}
semigood_beta0_MAPE <- c()
for (elem in results){
  semigood_beta0_MAPE <- c(semigood_beta0_MAPE, elem['semigood'][7,])
}
semigood_beta1_MAPE <- c()
for (elem in results){
  semigood_beta1_MAPE <- c(semigood_beta1_MAPE, elem['semigood'][8,])
}
semigood_beta2_MAPE <- c()
for (elem in results){
  semigood_beta2_MAPE <- c(semigood_beta2_MAPE, elem['semigood'][9,])
}

cat(semigood_beta0_RMSE)
cat(semigood_beta1_RMSE)
cat(semigood_beta2_RMSE)
cat(semigood_beta0_MAPE)
cat(semigood_beta1_MAPE)
cat(semigood_beta2_MAPE)

good_beta0_RMSE <- c()
for (elem in results){
  good_beta0_RMSE <- c(good_beta0_RMSE, elem['good'][4,])
}
good_beta1_RMSE <- c()
for (elem in results){
  good_beta1_RMSE <- c(good_beta1_RMSE, elem['good'][5,])
}
good_beta2_RMSE <- c()
for (elem in results){
  good_beta2_RMSE <- c(good_beta2_RMSE, elem['good'][6,])
}
good_beta0_MAPE <- c()
for (elem in results){
  good_beta0_MAPE <- c(good_beta0_MAPE, elem['good'][7,])
}
good_beta1_MAPE <- c()
for (elem in results){
  good_beta1_MAPE <- c(good_beta1_MAPE, elem['good'][8,])
}
good_beta2_MAPE <- c()
for (elem in results){
  good_beta2_MAPE <- c(good_beta2_MAPE, elem['good'][9,])
}

cat(good_beta0_RMSE)
cat(good_beta1_RMSE)
cat(good_beta2_RMSE)
cat(good_beta0_MAPE)
cat(good_beta1_MAPE)
cat(good_beta2_MAPE)

ml_beta0_RMSE <- c()
for (elem in results){
  ml_beta0_RMSE <- c(ml_beta0_RMSE, elem['lm'][4,])
}
ml_beta1_RMSE <- c()
for (elem in results){
  ml_beta1_RMSE <- c(ml_beta1_RMSE, elem['lm'][5,])
}
ml_beta2_RMSE <- c()
for (elem in results){
  ml_beta2_RMSE <- c(ml_beta2_RMSE, elem['lm'][6,])
}
ml_beta0_MAPE <- c()
for (elem in results){
  ml_beta0_MAPE <- c(ml_beta0_MAPE, elem['lm'][7,])
}
ml_beta1_MAPE <- c()
for (elem in results){
  ml_beta1_MAPE <- c(ml_beta1_MAPE, elem['lm'][8,])
}
ml_beta2_MAPE <- c()
for (elem in results){
  ml_beta2_MAPE <- c(ml_beta2_MAPE, elem['lm'][9,])
}

cat(ml_beta0_RMSE)
cat(ml_beta1_RMSE)
cat(ml_beta2_RMSE)
cat(ml_beta0_MAPE)
cat(ml_beta1_MAPE)
cat(ml_beta2_MAPE)
