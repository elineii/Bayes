options(scipen = 999)

library("mvtnorm")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# --------------------------
# Некоторые теоретические выкладки
# --------------------------
# y_1* ~ b_10 + b_11 * x_11 + b_12 * x_12 + b_13 * x_13
# y_2* ~ b_20 + b_21 * x_21 + b_22 * x_22 + b_23 * x_23
# y_3* ~ b_30 + b_31 * x_31 + b_32 * x_32 + b_33 * x_33

# Индивид выбирает одну из трех альтернатив:
# y = 1, if y_1* > y_2* & y_1* > y_3*
# y = 2, if y_2* > y_1* & y_2* > y_3*
# y = 3, if y_3* > y_1* & y_3* > y_2*

# Предположение: ошибки имеют многомерное нормальное распределение:
#
#       |e_1i|     ||0|  |σ_1^2           ρ_{12}*σ_1*σ_2  ρ_{13}*σ_1*σ_2||
# e_i = |e_2i| ~ N ||0|, |ρ_{12}*σ_1*σ_2  σ_2^2           ρ_{23}*σ_2*σ_3||
#       |e_3i|     ||0|  |ρ_{13}*σ_1*σ_3  ρ_{23}*σ_2*σ_3  σ_3^2         ||
#
# Полагаем y_3* = 0 и σ_1^2 = 1:
#
#       |e_1i|     ||0|  |1           ρ_{12}*σ_2||
# e_i = |    | ~ N || |, |                      ||
#       |e_2i|     ||0|  |ρ_{12}*σ_2  σ_2^2     ||

# --------------------------
# Генерация данных
# --------------------------
set.seed(42) 
n <- 10000                                                 # число наблюдений  
h <- data.frame(income = exp(rnorm(n, 10, 0.7)),           # показатель дохода
                health = pmin(rpois(n, 3), 5),             # показатель здоровья
                age = round(runif(n, 20, 100)))            # показатель возраста
sigma <- matrix(c(1, 0.4, 0.4, 3), 2)
eps <- rmvnorm(n, mean = rep(0, 3), sigma = sigma)         # случайные ошибки     

# Истинные коэффициенты, линейные индексы и латентные переменные
beta_Car <- c(0.1, 0.000025, 0.3, 0.01)
beta_Taxi <- c(0.2, 0.000015, 0.2, 0.015)
beta_Public <- c(3, -0.00002, 0.5, -0.02)
beta_d <- c(-0.3, 0.3)

y_li_Car <- beta_Car[1] +                                  # линейный индекс Машины
  h$income * beta_Car[2] +
  h$health * beta_Car[3] +
  h$age * beta_Car[4]
y_star_Car <- y_li_Car + eps[, 1]                          # латентная переменная Машины

y_li_Taxi <- beta_Taxi[1] +                                # линейный индекс Такси
  h$income * beta_Taxi[2] +
  h$health * beta_Taxi[3] +
  h$age * beta_Taxi[4]
y_star_Taxi <- y_li_Taxi + eps[, 2]                        # латентная переменная Такси

y_li_Public <- beta_Public[1] +                            # линейный индекс Общ. транспорта
  h$income * beta_Public[2] +                           
  h$health * beta_Public[3] +
  h$age * beta_Public[4]
y_star_Public <- y_li_Public + eps[, 3]                    # латентная переменная Общ. транспорта

# Зависимые переменные
h$transport[(y_star_Car >= y_star_Taxi) &                  # те, кто выбрал Машину
              (y_star_Car >= y_star_Public)] <- "Car"
h$transport[(y_star_Taxi >= y_star_Car) & 
              (y_star_Taxi >= y_star_Public)] <- "Taxi"    # те, кто выбрал Такси
h$transport[(y_star_Public >= y_star_Car) & 
              (y_star_Public >= y_star_Taxi)] <- "Public"  # те, кто выбрал Общ. транспорт

summary(as.factor(h$transport))

# --------------------------
# Оценивание модели ММП
# --------------------------
h1 <- dfidx(h,                                  
            shape = "wide",                 
            choice = "transport")   

model_cmprobit <- mlogit(transport ~ 1 | income + health + age, 
                         data = h1, 
                         probit = TRUE)               

# --------------------------
# Оценивание модели с помощью Байеса
# --------------------------





