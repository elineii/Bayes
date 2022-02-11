# Задания
# 1. Повторить этот пример для логистического распределения
#    Подсказка: достаточно заменить функцию normal_lpdf

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Генерируем выборку из 
# логистического распределения
set.seed(123)                                              # для воспроизводимости
n <- 1000                                                  # объем выборки                      
location <- 0                                              # параметры распределения
scale <- 1
x <- rlogis(n, location = location, scale = scale)         # реализация выборки

# Формируем данные для модели
data <- list(x = x,                                        # real x[n]
             n = length(x))                                # int<lower=0> n

# Генерируем выборку из апостериорного
# распределения параметров location и scale
model <- stan(file = "logistic.stan",
              data = data,                                 # входные данные
              chains = 1,                                  # количество выборок из апостериорного распределения
              iter = 2000)                                 # удвоенный объем выборки из
# апостериорного распределения
posterior <- extract(model)
posterior$location                                         # выборка из апостериорного распределения mu
hist(posterior$location, breaks = 15)
posterior$scale                                            # выборка из апостериорного распределения sigma
hist(posterior$scale, breaks = 15)

# Оценим параметр location (с scale по аналогии)
mean(posterior$location)                                   # как математическое ожидание апостериорного распределения mu
median(posterior$location)                                 # как медиану апостериорного распределения mu

# Оценим параметр scale
mean(posterior$scale)                                      # как математическое ожидание апостериорного распределения mu
median(posterior$scale)                                    # как медиану апостериорного распределения mu

# Описательные статистики по
# апостериорному распределению параметров
summary(model)
