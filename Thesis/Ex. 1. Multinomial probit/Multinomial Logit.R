# Для удобства отключим 
# экспоненциальную запись чисел
options(scipen = 999)

library("mlogit")                        # мультиномиальный логит
library("stringr")                       # работа со строками
library("tm")                            # обработка данных
library("mvtnorm")
library("EnvStats")

# Симулируем данные
set.seed(123)                                            # для воспроизводимости
n <- 10000                                               # число наблюдений  
h <- data.frame(income = exp(rnorm(n, 10, 0.7)),         # показатель дохода
                health = pmin(rpois(n, 3), 5),           # показатель здоровья
                age = round(runif(n, 20, 100)))      
eps <- cbind(revd(n), revd(n), revd(n))                  # случайные ошибки     
# Разные способы генерации случайных ошибок
# no copula
#eps <- rmvnorm(n, mean = rep(0, 4), sigma = sigma)
# student copula
# eps <- tCopula(param = cov2cor(sigma)[lower.tri(cov2cor(sigma))],
#                dim = 4, dispstr = 'un', df = 5)
# eps <- rCopula(n, eps)
# Gumbel copula
# eps <- claytonCopula(param=1, dim=4)
# eps <- rCopula(n, eps)
# uniform
# eps[, 1] <- (qunif(eps[, 1]) - 0.5) * sqrt(12)
# eps[, 2] <- (qunif(eps[, 2]) - 0.5) * sqrt(12)
# eps[, 3] <- (qunif(eps[, 3]) - 0.5) * sqrt(12)
# eps[, 4] <- (qunif(eps[, 4]) - 0.5) * sqrt(12) * sigma.y
# exponential
# eps[, 1] <- (qexp(eps[, 1]) - 1)
# eps[, 2] <- (qexp(eps[, 2]) - 1)
# eps[, 3] <- (qexp(eps[, 3]) - 1)
# eps[, 4] <- (qexp(eps[, 4]) - 1) * sigma.y
# student
# df <- 5
# eps[, 1] <- qt(eps[, 1], df) * sqrt((df - 2) / df)
# eps[, 2] <- qt(eps[, 2], df) * sqrt((df - 2) / df)
# eps[, 3] <- qt(eps[, 3], df) * sqrt((df - 2) / df)
# eps[, 4] <- qt(eps[, 4], df) * sqrt((df - 2) / df) * sigma.y
# beta bimodal
# a <- 0.1
# b <- 0.1
# beta.mean <- a / (a + b)
# beta.var <- (a * b) / (((a + b) ^ 2) * (a + b + 1))
# eps[, 1] <- (qbeta(eps[, 1], a, b) - beta.mean) / sqrt(beta.var)
# eps[, 2] <- (qbeta(eps[, 2], a, b) - beta.mean) / sqrt(beta.var)
# eps[, 3] <- (qbeta(eps[, 3], a, b) - beta.mean) / sqrt(beta.var)
# eps[, 4] <- ((qbeta(eps[, 4], a, b) - beta.mean) / sqrt(beta.var)) * sigma.y

# Создадим несколько латентных переменных, каждая
# из которых отражает предпочтения в отношении того
# или иного вида транспорта
beta_Car <- c(0.1, 0.000025, 0.3, 0.01)
beta_Taxi <- c(0.2, 0.000015, 0.2, 0.015)
beta_Public <- c(3, -0.00002, 0.5, -0.02)
beta_d <- c(-0.3, 0.3)
y_li_Car <- beta_Car[1] +                                # линейный индекс Машины
  h$income * beta_Car[2] +
  h$health * beta_Car[3] +
  h$age * beta_Car[4]
y_star_Car <- y_li_Car + eps[, 1]                        # латентная переменная Машины
y_li_Taxi <- beta_Taxi[1] +                              # линейный индекс Такси
  h$income * beta_Taxi[2] +
  h$health * beta_Taxi[3] +
  h$age * beta_Taxi[4]
y_star_Taxi <- y_li_Taxi + eps[, 2]                      # латентная переменная Такси
y_li_Public <- beta_Public[1] +                          # линейный индекс
  h$income * beta_Public[2] +                            # общественного транспорта
  h$health * beta_Public[3] +
  h$age * beta_Public[4]
y_star_Public <- y_li_Public + eps[, 3]                  # латентная переменная 
                                                         # общественного транспорта

# Сформируем зависимую переменную
h$transport[(y_star_Car >= y_star_Taxi) &                # те, кто выбрал Машину
            (y_star_Car >= y_star_Public)] <- "Car"
h$transport[(y_star_Taxi >= y_star_Car) & 
            (y_star_Taxi >= y_star_Public)] <- "Taxi"    # те, кто выбрал Такси
h$transport[(y_star_Public >= y_star_Car) & 
            (y_star_Public >= y_star_Taxi)] <- "Public"  # те, кто выбрал
# общественный транспорт
summary(as.factor(h$transport))

# Посмотрим на данные
head(h)

# Подготовим данные
h1 <- dfidx(h,                                   # исходный датафрейм
                                                 # имена эти регрессоров должны 
                                                 # иметь формат имя.альтернатива
            shape = "wide",                 
            choice = "transport")                # переменная, отражающая 
                                                 # выбранную альтернативу

# Оценим параметры модели
model_cmlogit <- mlogit(transport ~  1 | income + health + age,
                        data = h1) 
summary(model_cmlogit)   

# Достанем оценки коэффициентов
coef.Public <- coef(model_cmlogit)[seq(1, 8, by = 2)]
coef.Taxi <- coef(model_cmlogit)[seq(2, 8, by = 2)]

# Сравним истинные значения коэффициентов с оценками
cbind(true = coef.Public, est = beta_Public - beta_Car)
cbind(true = coef.Taxi, est = beta_Taxi - beta_Car)

