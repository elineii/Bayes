library("rstan")
library('Metrics')
library('MASS')
source("auxiliary_functions.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# --------------------------
# ШАГ 1. Генерируем данные 
# y_1* ~ b_10 + b_11 * x_11 + b_12 * x_12 + b_13 * x_13
# y_2* ~ b_20 + b_21 * x_21 + b_22 * x_22

# y_1 = 1, if y* > 0
# y_1 = 0, else
# --------------------------

N <- 1000                                 # Число наблюдений
n_iters <- 2                              # Число симуляций

# Генерируем регрессоры

x_11 <- rnorm(n = N, 2, 4)                # x_11 из N(2, 4)
x_12 <- runif(n = N, min = -2, max = 2)   # x_12 из U(-2, 2)

x_21 <- rnorm(n = N, 0, 2)                # x_21 из N(0, 2)
x_22 <- runif(n = N, min = -7, max = 2)   # x_22 из U(-7, 2)
x_23 <- rt(n = N, df = 10)                # x_23 из t(10)

X_1 <- cbind(1, x_11, x_12)               # Аггрегируем все регрессоры => получаем реализацию выборки
X_2 <- cbind(1, x_21, x_22, x_23)

beta_1 <- matrix(c(-5,2,10), nrow=3)      # Задаем реальные значения b_0, b_1, b_2 для каждого уравнения системы

beta_2 <- matrix(c(10,12,4,18), nrow=4)

mu <- c(1, 3)                             # Генерируем ошибки из двумерного нормального распределения и задаем коэфф. корреляции                
sigma <- matrix(c(1,0.2, 0.2,1), 
                nrow=2, ncol=2)
epsilon <- mvrnorm(n = N, 
                   mu = mu, Sigma = sigma)

epsilon_1 <- epsilon[, 1]
epsilon_2 <- epsilon[, 2]

y_star_1 <- as.vector(X_1 %*% beta_1 + epsilon_1)          # Находим вектор у*
y_star_2 <- as.vector(X_2 %*% beta_2 + epsilon_2)

y_1 <- as.numeric((y_star_1 > 0))                          # Находим вектор у
y_2 <- as.numeric((y_star_2 > 0)) 

y <- cbind(y_1, y_2)

# Посмотрим на сбалансированность выборки
mean(y[, 1])
mean(y[, 2])

# Удалим лишние переменные
rm(x_11, x_12, x_21, x_22, x_23, y_1, y_2, y_star_1, y_star_2, epsilon_1, epsilon_2)

# --------------------------
# ШАГ 2. Оцениваем параметры модели
# --------------------------
# Формируем данные для модели

rnames = c("b_10","b_11", "b_12", "b_20", "b_21", "b_22", "b_23", "rho",
           "MSE_b_10", "MSE_b_11", "MSE_b_12", "MSE_b_20", "MSE_b_21", "MSE_b_22", "MSE_b_23", "MSE_rho",
           "MAPE_b_10", "MAPE_b_11", "MAPE_b_12", "MAPE_b_20", "MAPE_b_21", "MAPE_b_22", "MAPE_b_23", "MAPE_rho")

df_normal <- data.frame(t(rep(0, length(rnames))))
names(df_normal) <- rnames
df_normal = df_normal[FALSE,]

data <- list(x_1 = X_1,
             x_2 = X_2,
             y = y,
             n = dim(X_1)[1],                 # int<lower=0> n
             k_1 = dim(X_1)[2],               # int<lower=0> k
             k_2 = dim(X_2)[2])

model_normal <- stan(file = "Bivariate probit model with normal errors.stan",
                     data = data,                # входные данные
                     chains = 1,                 # количество выборок из апостериорного распределения
                     iter = 2000)                # удвоенный объем выборки из апостериорного распределения
posterior_normal <- extract(model_normal)
  
df_normal <- estimations_metrics(df=df_normal, posterior=posterior_normal, k_1=dim(X_1)[2], k_2=dim(X_2)[2])

names(df_normal) <- rnames

true = c(beta_1, beta_2, sigma[1,2] / (sqrt(sigma[1,1] * sigma[2,2])), rep(0, length(rnames)-length(beta_1)-length(beta_2)-1))

table = data.frame(true=true,
                   normal=colMeans(df_normal))
# --------------------------

# Итоговые таблицы! 
# --------------------------
df_normal

table
