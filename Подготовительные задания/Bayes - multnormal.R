# 3. Повторить пример для оценивания параметров двумерного
#    нормального распределения

library("rstan")
library('MASS')
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Генерируем выборку из 
#нормального распределения
set.seed(123)                                              # для воспроизводимости
n <- 1000                                                  # объем выборки                      
mu <- c(1, 2)                                              # параметры распределения
sigma <- matrix(c(4,2, 2,3), nrow=2, ncol=2)
x <- mvrnorm(n = n, mu = mu, Sigma = sigma)                # реализация выборки

# Формируем данные для модели
data <- list(x = x,                                        # real x[n]
             n = dim(x)[1])                                # int<lower=0> n                                      

# Генерируем выборку из апостериорного
# распределения параметров mu и sigma
model <- stan(file = "multnormal.stan",
              data = data,                                 # входные данные
              chains = 1,                                  # количество выборок из апостериорного распределения
              iter = 2000)                                 # удвоенный объем выборки из
# апостериорного распределения
posterior <- extract(model)
posterior$mu                                               # выборка из апостериорного распределения mu
hist(posterior$mu, breaks = 15)
posterior$sigma                                            # выборка из апостериорного распределения sigma
hist(posterior$sigma, breaks = 15)

# Оценим параметр mu (с sigma по аналогии)
mean(posterior$mu)                                         # как математическое ожидание апостериорного распределения mu
median(posterior$mu)                                       # как медиану апостериорного распределения mu
mean(x)                                                    # при помощи метода максимального правдоподобия

# Описательные статистики по
# апостериорному распределению параметров
summary(model)

mu_1 = mean(posterior$mu[ , 1])
mu_2 = mean(posterior$mu[ , 2])
mu_est = c(mu_1, mu_2)

Sigma_est = matrix(c(mean(posterior$Sigma[ , 1, 1]),mean(posterior$Sigma[ , 1, 2]), 
                 mean(posterior$Sigma[ , 2, 1]),mean(posterior$Sigma[ , 2, 2])),
                 nrow=2, ncol=2)
mu_est
Sigma_est