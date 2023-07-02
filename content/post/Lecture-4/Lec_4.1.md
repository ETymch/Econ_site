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


```r
dta <- read_csv('https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/framingham.csv')

test <- sample_n(dta, 800) # тестовая выборка. На ней мы будем проверять модели.
train <- setdiff(dta, test) # на этой выборке мы будем оценивать модели
```

Первый график:


```r
dta %>%
  ggplot(aes(x = glucose, y = diabetes)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_vline(xintercept = 99, color = 'red') + # Верхняя граница нормы
  theme_ipsum() +
  labs(x = 'Уровень глюкозы в крови, мг./Дл.',
       y = 'Сахарный диабет')
```

<img src="/post/Lecture-4/Lec_4.1_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Линия плохо описывает взаимосвязь. Возможно, такая форма лучше описывает зависимость?


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
<tr><td style="text-align:left">Constant</td><td>-0.312<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.008)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,129</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.371</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.371</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.131 (df = 3127)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>1,844.903<sup>***</sup> (df = 1; 3127)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Но как её интерпретировать? Что значит отрицательный `intercept`. Если человек не сдавал анализ на глюкозу, его заболеваемость отрицательная?

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
* Логистическая регрессия: `\(y_i = \frac{1}{1 + e^{-(ntercept + \beta_1 x_i)}} + \epsilon_i\)`

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
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">glucose</td><td>0.004<sup>***</sup></td><td>0.081<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0001)</td><td>(0.007)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-0.312<sup>***</sup></td><td>-11.495<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.008)</td><td>(0.700)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,129</td><td>3,129</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.371</td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.371</td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td>-193.250</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td></td><td>390.500</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.131 (df = 3127)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>1,844.903<sup>***</sup> (df = 1; 3127)</td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Прогнозы:


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
##    gl_test dia_test pred_ols pred_ml pred_ml_01
##      <dbl>    <dbl>    <dbl>   <dbl>      <dbl>
##  1      80        0   0.0199 0.00676          0
##  2      65        0  -0.0423 0.00201          0
##  3     127        1   0.215  0.237            0
##  4      63        0  -0.0506 0.00170          0
##  5      61        0  -0.0589 0.00145          0
##  6      81        0   0.0241 0.00733          0
##  7      93        0   0.0739 0.0192           0
##  8      78        0   0.0116 0.00575          0
##  9      64        0  -0.0465 0.00185          0
## 10      85        0   0.0407 0.0101           0
```

В отличие от оценок, сделанных при помощи МНК, оценки ММП смещённые. Это значит, что средняя ошибка в модели не равна 0.


```r
residuals(mod_ols) %>% mean() # Ошибки в модели МНК.
```

```
## [1] -5.322319e-18
```

```r
residuals(mod_ml) %>% mean() # Ошибки в модели ММП.
```

```
## [1] -0.09257632
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

```
## Warning: Removed 120 rows containing non-finite values (`stat_density()`).
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
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">glucose</td><td>0.081<sup>***</sup></td><td>0.078<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.007)</td><td>(0.007)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">age</td><td></td><td>0.058<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.020)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">education</td><td></td><td>-0.154</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.168)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">cigsPerDay</td><td></td><td>-0.003</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.015)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">totChol</td><td></td><td>0.002</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-11.495<sup>***</sup></td><td>-14.457<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.700)</td><td>(1.496)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,129</td><td>3,023</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-193.250</td><td>-180.529</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>390.500</td><td>373.057</td></tr>
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
##  1              0 NA        NA           NA       NA       NA         NA
##  2              0  0.00575   0.00188      0        0        0          0
##  3              0  0.0286    0.0313       0        0        0          0
##  4              0  0.00236   0.00261      0        0        0          0
##  5              0  0.00623   0.0145       0        0        0          0
##  6              0  0.00384   0.00328      0        0        0          0
##  7              0 NA        NA           NA       NA       NA         NA
##  8              0  0.00794   0.00346      0        0        0          0
##  9              0 NA        NA           NA       NA       NA         NA
## 10              0  0.00530   0.00315      0        0        0          0
```


```r
predictions$error_ml %>% na.omit() %>% sum
```

```
## [1] 8
```

```r
predictions$error_ml_1 %>% na.omit() %>% sum
```

```
## [1] 8
```

Удивительно, но обе модели ошиблись в диагнозах `менее 20 раз из 800` - менее 5%! Модель с одной объясняющей переменной показала отличную способность прогнозировать диагнозы, потому что мы выбрали наиболее значимую объясняющую переменную - уровень инсулина.
