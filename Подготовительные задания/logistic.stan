// Описываем входные данные
data {
  int<lower=0> n;         // число наблюдений
  real x[n];              // выборка
}

// Указываем параметры
parameters {
  real location;
  real<lower=0> scale;
}

// Описываем модель
// Под target понимается ln(f(location,scale|x)) ~= ln(f(location)) + ln(f(scale)) + ln(f(x|location, scale))
// Где под ~= понимается пропорционально
model {
  target += uniform_lpdf(location | -100, 100);  // априорное распределение location, то есть ln(f(location))
  target += uniform_lpdf(scale | 0, 50);         // априорное распределение scale, то есть ln(f(scale))
  target += logistic_lpdf(x | location, scale);  // распределение данных при, то есть ln(f(x|location, scale))
  }                                              // фиксированных параметрах
