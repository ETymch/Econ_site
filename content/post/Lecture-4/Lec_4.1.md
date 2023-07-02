---
title: "Лекция 4 - практическая часть"
author: "Е. Тымченко"
date: 2023-07-02
categories: ["R"]
tags: ["Econ"]
---

# Библиотеки


```r
library(tidyverse)
library(hrbrthemes)
library(stargazer)
library(reshape2)
```

# Данные

Сегодня мы построим модель, которая позволит диагностировать сахарный диабет, на основании различных данных о пациенте.


```r
dta <- read_csv('https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/framingham.csv')

test <- sample_n(dta, 800) # тестовая выборка. На ней мы будем проверять модели.
train <- setdiff(dta, test) # на этой выборке мы будем оценивать модели
```

### Почему в данном случае МНК - не лучший вариант для оценки параметров?

Наиболее значимым фактором для постановки такого диагноза является уровень глюкозы в крови. Построим график зависимости диагноза от кровня глюкозы:


```r
dta %>%
  ggplot(aes(x = glucose, y = diabetes)) +
  geom_point() +
  geom_smooth(method = 'lm') + # добавим линию, оценённую с помощью МНК
  geom_vline(xintercept = 99, color = 'red') + # Верхняя граница нормы
  theme_ipsum() +
  labs(x = 'Уровень глюкозы в крови, мг./Дл.',
       y = 'Сахарный диабет')
```

<img src="/post/Lecture-4/Lec_4.1_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Линия плохо описывает взаимосвязь. Возможно, такая форма лучше?


```r
dta %>%
  ggplot(aes(x = glucose, y = diabetes)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  geom_vline(xintercept = 99, color = 'red') +
  theme_ipsum() +
  labs(x = 'Уровень глюкозы в крови, мг./Дл.',
       y = 'Сахарный диабет')
```

<img src="/post/Lecture-4/Lec_4.1_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Оценим модель с помощью МНК.


```r
mod_ols <- lm(diabetes ~ glucose, train)
stargazer(mod_ols, type = 'html', header = F)
```


<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>diabetes</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">glucose</td><td>0.004<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0001)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-0.320<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.008)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,121</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.390</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.390</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.130 (df = 3119)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>1,995.504<sup>***</sup> (df = 1; 3119)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Но как её интерпретировать? Что значит отрицательный `intercept`? Если человек не сдавал анализ на глюкозу, его заболеваемость отрицательная? Также, вспомните [Теорему Гауccа-Маркова](https://ru.wikipedia.org/wiki/%D0%A2%D0%B5%D0%BE%D1%80%D0%B5%D0%BC%D0%B0_%D0%93%D0%B0%D1%83%D1%81%D1%81%D0%B0_%E2%80%94_%D0%9C%D0%B0%D1%80%D0%BA%D0%BE%D0%B2%D0%B0), в которой сказано, что если выполнены несколько условий, одним из которых является `линейность связи между зависимой и независимой переменной`, то оценка МНК - лучшая несмещённая оценка. В нашем случае связь, очевидно, нелинейная, а оценки МНК - не лучшие. А значит нам нужен иной метод - [Метод Максимального Правдоподобия](https://econisfun.netlify.app/2023/06/27/%D0%BB%D0%B5%D0%BA%D1%86%D0%B8%D1%8F-4/).

Для оценки таких моделей у нас есть более удачная функция, чем обыкновенная прямая. Эта функция - сигмоид.

## Логистическая функция

Напишем уравнение для линии.


```r
y <- function(x){
  intercept + b*x
}
```

Уравнение логистической функции.


```r
S <- function(x){
  1 / ( 1 + exp(-y(x)))
}
```

Параметры и интервал


```r
intercept = 1
b = 1
x <- seq(-5, 5, by = 0.1)
```

Теперь нарисуем их:


```r
tibble(x,
       line = y(x),
       sigmoid = S(x)) %>%
  melt(id = 'x') %>%
  ggplot(aes(x = x, y = value, color = variable)) +
  geom_line(lwd = 1, alpha = 0.6) +
  ylim(0, 1) +
  theme_ipsum() +
  labs(title = 'Сигмоид и линия', x = 'X', y = 'Диабет - 1, Здоров - 0')
```

<img src="/post/Lecture-4/Lec_4.1_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Чтобы получить больше интеиции о том, как устроена логистическая функция, Сравним две модели:

* МНК: `\(y_i = intercept + \beta_1 x_i + \epsilon_i\)` 
* Логистическая регрессия: `\(y_i = \frac{1}{1 + e^{-(intercept + \beta_1 x_i)}} + \epsilon_i\)`

Зафиксируем `\(\beta_1\)` и посмотрим, что произойдёт при изменении `intercept`. 

<center>

<video width="800" height="600" controls>
<source src="https://github.com/ETymch/Econometrics_2023/raw/main/Pics/anim_sigmoid_int.mp4" type="video/mp4">
</video>

</center>

Теперь зафиксируем `intercept` и посмотрим, что будет при изменении `\(\beta_1\)`:

<center>

<video width="800" height="600" controls>
<source src="https://github.com/ETymch/Econometrics_2023/raw/main/Pics/anim_sigmoid_b.mp4" type="video/mp4">
</video>

</center>

Оценим две модели:


```r
mod_ml <- glm(diabetes ~ glucose, family = 'binomial', train) # МНК
mod_ols <- lm(diabetes ~ glucose, train) # ММП

stargazer(mod_ols, mod_ml, type = 'html', header = F) # сравним модели
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">diabetes</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td><em>logistic</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">glucose</td><td>0.004<sup>***</sup></td><td>0.079<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0001)</td><td>(0.006)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-0.320<sup>***</sup></td><td>-11.347<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.008)</td><td>(0.688)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,121</td><td>3,121</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.390</td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.390</td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td>-188.226</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td></td><td>380.452</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.130 (df = 3119)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>1,995.504<sup>***</sup> (df = 1; 3119)</td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## Ставим диагнозы при помощи модели


```r
S <- function(x){
  1 / (1 + exp(- (coefficients(mod_ml)[1] + coefficients(mod_ml)[2] * x)))
}

y <- function(x){
  coefficients(mod_ols)[1] + coefficients(mod_ols)[2]*x
}

gl_test <- test$glucose # Столбец с значением анализа для тестовой выборки.
dia_test <- test$diabetes # Столбец с истинным значением диагноза.

# Создаём табличку

result <- tibble(gl_test, 
       dia_test,
       pred_ols = y(gl_test),
       pred_ml = S(gl_test)) %>%
  mutate(pred_ml_01 = ifelse(pred_ml < 0.5, 0, 1)) # создайм ещё одну переменную со значением прогноза на основе предсказанной вероятности.

result %>%
  head(10) # посмотрим на вервые 10 строк в таблице.
```

```
## # A tibble: 10 × 5
##    gl_test dia_test pred_ols  pred_ml pred_ml_01
##      <dbl>    <dbl>    <dbl>    <dbl>      <dbl>
##  1      77        0  0.00691  0.00527          0
##  2      NA        0 NA       NA               NA
##  3     102        0  0.113    0.0371           0
##  4      75        0 -0.00158  0.00450          0
##  5      87        0  0.0494   0.0116           0
##  6      84        0  0.0366   0.00915          0
##  7      84        0  0.0366   0.00915          0
##  8      83        0  0.0324   0.00846          0
##  9      84        0  0.0366   0.00915          0
## 10      83        0  0.0324   0.00846          0
```

В отличие от оценок, сделанных при помощи МНК, оценки ММП смещённые. Это значит, что средняя ошибка в модели не равна 0.


```r
residuals(mod_ols) %>% mean() # Ошибки в модели МНК.
```

```
## [1] 3.746551e-18
```

```r
residuals(mod_ml) %>% mean() # Ошибки в модели ММП.
```

```
## [1] -0.09207685
```

Для большей наглядности, нарисуем распределение ошибок в моделях:


```r
tibble(id = 1:length(residuals(mod_ols)),
                redid_ols = residuals(mod_ols),
                resid_ml = residuals(mod_ml)) %>%
  melt(id = 'id') %>%
  ggplot(aes(x = value, color = variable, fill = variable)) +
  geom_density(alpha = 0.5) +
  theme_minimal()+
  xlim(-1, 0.5)
```

<img src="/post/Lecture-4/Lec_4.1_files/figure-html/unnamed-chunk-13-1.png" width="672" />

Добавим в модель больше параметров и сравним с первой:


```r
mod_ml_1 <- glm(diabetes ~ glucose + age + education + cigsPerDay + totChol, family = 'binomial', train) # Диагноз теперь зависит от уровня глюкозы, возраста, уровня образования, интенсивности курения и уровня холестерина.

stargazer(mod_ml, mod_ml_1, type = 'html', header = F)
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">diabetes</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">glucose</td><td>0.079<sup>***</sup></td><td>0.077<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.007)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">age</td><td></td><td>0.057<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">education</td><td></td><td>-0.233</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.175)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">cigsPerDay</td><td></td><td>-0.010</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.016)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">totChol</td><td></td><td>0.003</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-11.347<sup>***</sup></td><td>-14.425<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.688)</td><td>(1.557)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,121</td><td>3,013</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-188.226</td><td>-173.875</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>380.452</td><td>359.750</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Сравним, какая модель стваит диагнозы: с однйо объясняющей переменной или с несколькими:


```r
S <- function(x){
  1 / (1 + exp(-x))
}

predict_ml <- predict(mod_ml, test %>% select(-diabetes)) %>%
  S()
predict_ml_1 <- predict(mod_ml_1, test %>% select(-diabetes)) %>%
  S()

predictions <- tibble(true_diagnoses = test$diabetes,
       prob_ml = predict_ml,
       prob_ml_1 = predict_ml_1) %>%
  mutate(dia_ml = ifelse(prob_ml < 0.5, 0, 1),
         dia_ml_1 = ifelse(prob_ml_1 < 0.5, 0, 1)
         ) %>%
  mutate(error_ml = ifelse(dia_ml == true_diagnoses, 0, 1),
         error_ml_1 = ifelse(dia_ml == true_diagnoses, 0, 1)
         )

predictions %>%
  tail(10) # последние 10 наблюдений в табличке
```

```
## # A tibble: 10 × 7
##    true_diagnoses  prob_ml prob_ml_1 dia_ml dia_ml_1 error_ml error_ml_1
##             <dbl>    <dbl>     <dbl>  <dbl>    <dbl>    <dbl>      <dbl>
##  1              0  0.00174   0.00231      0        0        0          0
##  2              0 NA        NA           NA       NA       NA         NA
##  3              1  0.720     0.815        1        1        0          0
##  4              0  0.00668   0.0195       0        0        0          0
##  5              1  1.00      1.00         1        1        0          0
##  6              0  0.0200    0.0572       0        0        0          0
##  7              0  0.00915   0.00807      0        0        0          0
##  8              0  0.0252    0.0472       0        0        0          0
##  9              0  0.00571   0.00244      0        0        0          0
## 10              0  0.00571   0.00635      0        0        0          0
```


```r
predictions$error_ml %>% na.omit() %>% sum
```

```
## [1] 9
```

```r
predictions$error_ml_1 %>% na.omit() %>% sum
```

```
## [1] 9
```

Удивительно, но обе модели ошиблись в диагнозах `менее 20 раз из 800` - менее 5%! Модель с одной объясняющей переменной показала отличную способность прогнозировать диагнозы, потому что мы выбрали наиболее значимую объясняющую переменную - уровень инсулина.
