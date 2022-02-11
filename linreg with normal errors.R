library("rstan")
library('Metrics')
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# --------------------------
# ШАГ 1. Генерируем данные y ~ b_0 + b_1 * x_1 + b_2 * x_2
# --------------------------

N <- 1000                                 # Число наблюдений
n_iters <- 100                            # Число симуляций

x_1 <- runif(n = N, min = 0, max = 1)     # Генерируем x_1, x_2 из равномерного распределения 
x_2 <- runif(n = N, min = 0, max = 1)  

X <- cbind(1, x_1, x_2)                   # Аггрегируем все регрессоры => получаем реализацию выборки

beta <- matrix(c(-5, 2, 10), nrow = 3)    # Задаем реальные значения alpha, b_0, b_1

epsilon <- rnorm(n = N)                   # Генерируем ошибку из нормального распределения

y_star <- as.vector(X %*% beta + epsilon) # Находим вектор у

# --------------------------

# ШАГ 2. Оцениваем параметры модели
# --------------------------
# Формируем данные для модели

rnames = c("b_0","b_1", "b_2", 
           "MSE_b_0", "MSE_b_1", "MSE_b_2", 
           "MAPE_b_0", "MAPE_b_1", "MAPE_b_2")

# Создаем пустые датафреймы, в которых будет содержаться информация об оценках беты
# и метрики для каждого априорного распределения

df_normal <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
names(df_normal) <- rnames
df_normal = df_normal[FALSE,]

df_student <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
names(df_student) <- rnames
df_student = df_student[FALSE,]

df_log <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
names(df_log) <- rnames
df_log = df_log[FALSE,]

df_uniform <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
names(df_uniform) <- rnames
df_uniform = df_uniform[FALSE,]

data <- list(x = X,                       # vector[3] x[n]
             y = y_star,                  # real y[n]
             n = dim(X)[1],               # int<lower=0> n
             k = dim(X)[2])               # int<lower=0> k

for (i in 1:n_iters){

  model_normal <- stan(file = "linreg with normal errors.stan",
                data = data,                # входные данные
                chains = 1,                 # количество выборок из апостериорного распределения
                iter = 2000)                # удвоенный объем выборки из апостериорного распределения
  posterior_normal <- extract(model_normal)
  
  model_student <- stan(file = "linreg with student errors.stan",
                       data = data,                
                       chains = 1,                 
                       iter = 2000)                
  posterior_student <- extract(model_student)
  
  model_log <- stan(file = "linreg with log errors.stan",
                       data = data,                
                       chains = 1,                 
                       iter = 2000)                
  posterior_log <- extract(model_log)
  
  model_uniform <- stan(file = "linreg with uniform errors.stan",
                       data = data,                
                       chains = 1,                 
                       iter = 2000)                
  posterior_uniform <- extract(model_uniform)
  
  # Подсчет метрик
  #------------------------------------------
  normal_beta = colMeans(posterior_normal$beta)
  normal_MSE_beta = c(rmse(beta[1], posterior_normal$beta[, 1]),
               rmse(beta[2], posterior_normal$beta[, 2]),
               rmse(beta[3], posterior_normal$beta[, 3]))
  normal_MAPE_beta = c(mape(beta[1], posterior_normal$beta[, 1]),
                mape(beta[2], posterior_normal$beta[, 2]),
                mape(beta[3], posterior_normal$beta[, 3]))
  df_normal <- rbind(df_normal, c(normal_beta, normal_MSE_beta, normal_MAPE_beta))
  
  student_beta = colMeans(posterior_student$beta)
  student_MSE_beta = c(rmse(beta[1], posterior_student$beta[, 1]),
                      rmse(beta[2], posterior_student$beta[, 2]),
                      rmse(beta[3], posterior_student$beta[, 3]))
  student_MAPE_beta = c(mape(beta[1], posterior_student$beta[, 1]),
                       mape(beta[2], posterior_student$beta[, 2]),
                       mape(beta[3], posterior_student$beta[, 3]))
  df_student <- rbind(df_student, c(student_beta, student_MSE_beta, student_MAPE_beta))
  
  log_beta = colMeans(posterior_log$beta)
  log_MSE_beta = c(rmse(beta[1], posterior_log$beta[, 1]),
                      rmse(beta[2], posterior_log$beta[, 2]),
                      rmse(beta[3], posterior_log$beta[, 3]))
  log_MAPE_beta = c(mape(beta[1], posterior_log$beta[, 1]),
                       mape(beta[2], posterior_log$beta[, 2]),
                       mape(beta[3], posterior_log$beta[, 3]))
  df_log <- rbind(df_log, c(log_beta, log_MSE_beta, log_MAPE_beta))
  
  uniform_beta = colMeans(posterior_uniform$beta)
  uniform_MSE_beta = c(rmse(beta[1], posterior_uniform$beta[, 1]),
                      rmse(beta[2], posterior_uniform$beta[, 2]),
                      rmse(beta[3], posterior_uniform$beta[, 3]))
  uniform_MAPE_beta = c(mape(beta[1], posterior_uniform$beta[, 1]),
                       mape(beta[2], posterior_uniform$beta[, 2]),
                       mape(beta[3], posterior_uniform$beta[, 3]))
  df_uniform <- rbind(df_uniform, c(uniform_beta, uniform_MSE_beta, uniform_MAPE_beta))
  #------------------------------------------
}  

names(df_normal) <- rnames
names(df_log) <- rnames
names(df_student) <- rnames
names(df_uniform) <- rnames

true = c(beta, 0, 0, 0, 0, 0, 0)



table = data.frame(true=true,
                   normal=colMeans(df_normal), 
                   student=colMeans(df_student), 
                   log=colMeans(df_log), 
                   uniform=colMeans(df_uniform))
# --------------------------

# Итоговые таблицы! 
# --------------------------
df_normal
df_log
df_student
df_uniform

table
