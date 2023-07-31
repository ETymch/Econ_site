---
title: "Лекция 5"
author: "Е. Тымченко"
date: 2023-07-11
categories: ["R"]
tags: ["Econ"]
---

## Повторение: Метод максимального правдоподобия

Идея метода наименьших квадратов:

$$
\min_\beta (y - \mathbb{E}[y|X])^2 = \min_\beta(y - X\beta)^2
$$

Идея ММП:

`$$\max_\beta \mathbb{P}(данные \sim какое-то распределение|\beta)$$`
Примеры:

* Логистическая регрессия
* Мультиномимльная логистическая регрессия
* Пуассоновская регрессия

### Пример: аренда велосипедов

<center>

![Rent](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/bikes.gif?raw=true)

</center>

У нас есть данные об аренде ведосипедов в городе Вашингтон на протяжении нескольких лет. Мы хотим посстроить модель, способную прогнозировать аренду на основе данных о погоде.

Нам потребуются библиотеки:


```r
library(tidyverse) # Работа с данными
library(ggExtra) # Красивые типы  графиков
library(showtext) # Рендер шрифтов в ggplot2
library(sysfonts) # загрузка шрифтов в R
library(stargazer) # Удобные таблицы статистики моделей.
library(reshape2) # Для преобразования данных
library(patchwork) # С помощью этой библиотеки мы модем делать различные выкладки графиков. Например, График_1 | График_2.
showtext_auto()

# Шрифты для графиков
font_add_google('Lobster')
font_add_google('Cormorant SC')

# Любимый цвет для графиков
col1 <- '#ad466c'
```

Загрузка данных:


```r
bikes_train <- read.csv('https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/bikes_train.csv')
bikes_test <- read.csv('https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/bikes_test.csv')
```


```r
b <- bikes_train %>%
  ggplot(aes(x = atemp, y = count)) +
  geom_jitter(color = col1, size = 3.0, alpha = 0.2) +
  theme_minimal(base_family = 'Lobster', base_size = 16) +
  labs(x = 'Температура, С', y = 'Число арендованных велосипедов')
ggMarginal(b, type="density", size = 5, fill = col1, alpha = 0.6, color = NA)
```

<img src="/post/Lecture-5/Lecture_5_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Оценим 4 модели, 3 из которых - [пуассоновская регрессия](https://en.wikipedia.org/wiki/Poisson_regression) о одна - МНК.


```r
mod_0 <- glm(count ~ humidity + season, family = poisson, bikes_train)
mod_1 <-glm(count ~ humidity + season + atemp + windspeed, family = poisson, bikes_train)
mod_2 <-glm(count ~ humidity + season + atemp + windspeed + workingday, family = poisson, bikes_train)
mod_ols <-glm(count ~ humidity + season + atemp + windspeed + workingday, family = gaussian, bikes_train)

stargazer(mod_0, mod_1, mod_2, mod_ols, header = F, type = 'html')
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">count</td></tr>
<tr><td style="text-align:left"></td><td colspan="3"><em>Poisson</em></td><td><em>normal</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">humidity</td><td>-0.018<sup>***</sup></td><td>-0.015<sup>***</sup></td><td>-0.015<sup>***</sup></td><td>-2.992<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.00004)</td><td>(0.00004)</td><td>(0.00004)</td><td>(0.092)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">season</td><td>0.203<sup>***</sup></td><td>0.150<sup>***</sup></td><td>0.150<sup>***</sup></td><td>22.292<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td><td>(0.001)</td><td>(1.573)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">atemp</td><td></td><td>0.037<sup>***</sup></td><td>0.037<sup>***</sup></td><td>7.308<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.0001)</td><td>(0.0001)</td><td>(0.203)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">windspeed</td><td></td><td>0.006<sup>***</sup></td><td>0.006<sup>***</sup></td><td>1.052<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.0001)</td><td>(0.0001)</td><td>(0.214)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">workingday</td><td></td><td></td><td>0.004<sup>**</sup></td><td>-0.166</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.002)</td><td>(3.547)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>5.817<sup>***</sup></td><td>4.760<sup>***</sup></td><td>4.757<sup>***</sup></td><td>135.229<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.003)</td><td>(0.004)</td><td>(0.005)</td><td>(9.284)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>9,000</td><td>9,000</td><td>9,000</td><td>9,000</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-655,441.500</td><td>-573,657.500</td><td>-573,654.200</td><td>-58,246.660</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>1,310,889.000</td><td>1,147,325.000</td><td>1,147,320.000</td><td>116,505.300</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>
Прогнозы для тестовой выборки:


```r
pr_0 <- predict(mod_0, bikes_test %>% select(-count)) %>% exp()
pr_1 <- predict(mod_1, bikes_test %>% select(-count)) %>% exp()
pr_2 <- predict(mod_2, bikes_test %>% select(-count)) %>% exp()
pr_ols <- predict(mod_ols, bikes_test %>% select(-count))

# Сводная таблица со всеми данными

result <- 
  tibble(pr_0, # прогнозы моделей
       pr_1,
       pr_2,
       pr_ols,
       true_value = bikes_test$count, # истинные значения
       id = seq(1, length(pr_0), by = 1)) %>% # номер наблюдения
  mutate(error_0 = (pr_0 - true_value)^2, # квадраты ошибок
         erroe_1 = (pr_1 - true_value)^2,
         error_2 = (pr_2 - true_value)^2,
         error_ols = (pr_ols - true_value)^2
  )
```

Статистика ошибок в четырёх моделях.


```r
result %>%
  select(error_0,
         erroe_1,
         error_2,
         error_ols) %>%
  sapply(mean) %>% # Применить функцию "среднее" к каждому столбцу
  sapply(sqrt) # Применить функцию "Квадратный корень" к каждому столбцу
```

```
##   error_0   erroe_1   error_2 error_ols 
##  163.2686  152.1845  152.1992  152.5746
```

На основании данных о средних ошибках мы не понимаем, какая модель лучше описывает данные. Давайте посмотрим, насколько равномерно распределены ошибки в зависимости от истинных значений аренды велосипедов.


```r
p1 <- result %>%
  select(error_2, true_value) %>%
  filter(error_2 <= 100000) %>%
  ggplot(aes(x = error_2, y = true_value)) +
  geom_point(color = col1, alpha = 0.15, size = 2) +
  theme_minimal(base_family = 'Cormorant SC', base_size = 16) +
  labs(x = 'Ошибки в модели пуассоновской регрессии',
       y = 'Истинное значение')

p2 <- 
  result %>%
  select(error_ols, true_value) %>%
  filter(error_ols <= 100000) %>%
  ggplot(aes(x = error_ols, y = true_value)) +
  geom_point(color = col1, alpha = 0.15, size = 2) +
  theme_minimal(base_family = 'Cormorant SC', base_size = 16) +
  labs(x = 'Ошибки в модели, оценённой МНК',
       y = 'Истинное значение')

p1 | p2
```

<img src="/post/Lecture-5/Lecture_5_files/figure-html/unnamed-chunk-7-1.png" width="672" />
Мы видим, что в модели пуассоновской регрессии ошибки распределены более равномерно!

# Инструментальные переменные

## [Angrist (1990)](https://www.jstor.org/stable/2006669)

* Добровольцы,
* Отобранные повредством лотереи.

### Лотереи - идеальный естественнный эксперимент

<center>

![draft](https://media1.giphy.com/media/nbX5NnozM2C97haytR/giphy.gif?cid=ecf05e47y0hg47apw6q26jn51o296t1lggov0q7sqo8dg4i0&ep=v1_gifs_search&rid=giphy.gif&ct=g)

</center>

* 366 шкриков. На каждом - день рождения (1 янв = 1, и так далее).
* Ведущий по очереди вытаскивает шарики.
* Порядок вытащенных номеров формирует призывную очередь.
* Дамми: высокий выбор на драфте и низкий выбор на драфте.

`$$y_i = \alpha + \beta M_i + \ldots + \varepsilon_i$$`
`\(M_i = 0\)`, если учавствовал.
`\(M_i = 1\)`, если не участвовал.

Проблема!

> `\(M_i\)` связан с ненаблюдаемыми переменными, влияющими на доходы.
Но возможно, что эти люди пошли в армию, а значит оценки Теорема Гаусса-Маркова не работает и оценки МНК - не лучшие из возможных.

Почему `\(M_i\)` связан с `\(\varepsilon_i\)`?

**Самоотбор**: вероятно, в армию отбираются люди, менее заинтересованные в больших деньгах и офисной работе.

Из-за этой связи оценки МНК будут больше (по модулю), чем истинные оценки влияния военной службы на доходы.

> Как это влияет на оценки параметров?

$$\hat{\beta} = \frac{dy}{dx} = \frac{\partial y}{\partial x} +  \frac{\partial \varepsilon}{\partial x} $$
Вспомните, [ТГМ](https://ru.wikipedia.org/wiki/%D0%A2%D0%B5%D0%BE%D1%80%D0%B5%D0%BC%D0%B0_%D0%93%D0%B0%D1%83%D1%81%D1%81%D0%B0_%E2%80%94_%D0%9C%D0%B0%D1%80%D0%BA%D0%BE%D0%B2%D0%B0) предполагает полное отсутствие связи между `\(x\)` и ошибками в модели. Если данное предположение нарушается, то оценки `\(\hat{\beta}\)` становятся смещёнными:

**Рисунок**

$$
`\begin{align}
\hat{\beta} \\
&= \frac{dy}{dx} \\
&= \frac{\partial y}{\partial x} + \frac{\partial \varepsilon}{\partial x} \\
&= \beta + \phi 
\end{align}`
$$

Где `\(\beta\)` - истинное влияние `\(x\)` на `\(y\)`,
`\(\hat{\beta}\)` - оценка МНК.
`\(\phi\)` - влияние упущенной информации `\(\varepsilon\)` на `\(x\)`
`\(\gamma\)` - влияние `\(x\)` на `\(\varepsilon\)`

Оценка с помощью инструментальных переменных:


Нужно найти такую переменную `\(z\)`, которая влияет на `\(x\)`, но не влияет на `\(\varepsilon\)`. Так:

`$$\frac{\partial y}{\partial z} = \frac{\partial y}{\partial x} \frac{\partial x}{\partial z} + 0$$`

`$$\beta_{IV} = \frac{\partial y / \partial z}{\partial x / \partial z}$$`

Рисунок

## Структурная оценка против сокращённой:

* Необходимость микрообоснований.
* Пример: [статья Keane в JOE](https://editorialexpress.com/jrust/econ615/readings/keane_article_je.pdf). Альтернативные объяснения - изменение услий, вкладываемых в образование и поиск работы при получении высокого номера на драфте.
* 2SLS, пример: налоги.
