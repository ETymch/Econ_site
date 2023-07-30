---
title: "Лекция 6"
author: "Е. Тымченко"
date: 2023-07-25
categories: ["R"]
tags: ["Econ"]
---

# Панельные данные

> Приводит ли рост цен на жильё к снижению динамики строительства? ответ на этот вопрос неочевиден.
* С одной стороны, покупатели жилья видят увеличение цен, перестают покупать жильё -> у строителей меньше активов, строительство снижается.
* С другой, очень часто может быть, что увеличение строительства - следствие увеличения спроса на жильё, а спрос в краткосрочном периоде сдерживается ценами.
Как проверить, какой вариант выполняется?

Библиотеки:


```r
library(tidyverse)
library(stargazer)
library(rio)
library(plm)
library(ggrepel)
library(sysfonts)
library(showtext)
showtext.auto()

font_add_google('Merriweather')
font_add_google('Lobster')
```


```r
data_housing <- read.csv('https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/data.housing.csv') %>%
  tibble()

# Посмотрим на несколько наблюдений в начале таблицы:

data_housing %>%
  head()
```

```
## # A tibble: 6 × 25
##       X  ...1 Region      Date    NA. Population   HPI  S_Unf    HR   CPI GRP_pc
##   <int> <int> <chr>       <chr> <int>      <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl>
## 1     1     1 Алтайский … 2002…     1   2624071.  129. 5.84e5 2143.  113. 27697.
## 2     2     2 Алтайский … 2002…     2   2622863.  127. 5.39e5 2143.  113. 27814.
## 3     3     3 Алтайский … 2002…     3   2620446.  123. 4.48e5 2142.  113. 28050.
## 4     4     4 Алтайский … 2002…     4   2616820.  118. 3.13e5 2141.  113. 28404.
## 5     5     5 Алтайский … 2003…     5   2611986.  111. 1.32e5 2139.  113. 28875.
## 6     6     6 Алтайский … 2003…     6   2606296.  106. 6.82e3 2139.  113. 29672.
## # ℹ 14 more variables: HR_pc <dbl>, BP <dbl>, PI_Roads_Materials <dbl>,
## #   Price_m2_fm <dbl>, Price_m2_sm <dbl>, Materials_pi <dbl>,
## #   N_of_Credits <int>, Avg_IR <dbl>, Avg_Sum_of_credit <dbl>,
## #   Avg_Duration_of_Credit <dbl>, Mortgage_Debt <dbl>, Usage <dbl>,
## #   Usage_ratio <chr>, S_unf_redone <dbl>
```

Посмотрим, как выгляит строительство и стоимость жилья в статике:


```r
data_housing %>%
  filter(Date == '2019-10-01', # Оставляем один период в выборке
         HPI <= 125) %>% # Убираем Москву, где индекс цен на жильё заметно выше, чем в других регионах.
  ggplot(aes(x = HPI, y = S_unf_redone, size = GRP_pc)) + # помимо осей х и у, размер кружочка - ВРП на душу населения.
  geom_point(alpha = 0.4, color = 'orange', fill = 'orange', show.legend = F) +
  geom_text_repel(aes(label = Region), size = 3, max.overlaps = 7 ,show.legend = F, family = 'Lobster',
                  nudge_x = 0.8, nudge_y = 100) + # Авоматические подписи
  theme_classic(base_family = 'Merriweather', base_size = 12) +
  scale_size_continuous(range = c(3, 15)) + # Минимальный и максимальный размеры кружочков
  labs(y = 'Площадь в процессе застройки, млн. м2', x = 'Индекс цен на недвижимость, г/г.', # Заголовки
       title = 'Стоимость недвижимости и динамика строительства',
       subtitle = '4 кв. 2019 г.',
       caption = '*Размер кружка - относительный ВРП на душу населения')
```

```
## Warning: ggrepel: 42 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps
```

<img src="/post/Lecture-6/Lec_6_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Мы хотим посмотреть, как изменялось это соотношение во времени. Сделать *анимацию* имея на руках готовый график - очень просто!


```r
p <- data_housing %>%
  filter(HPI <= 125) %>% # Убираем Москву, где индекс цен на жильё заметно выше, чем в других регионах.
  ggplot(aes(x = HPI, y = S_unf_redone, size = GRP_pc)) + # помимо осей х и у, размер кружочка - ВРП на душу населения.
  geom_point(alpha = 0.4, color = 'orange', fill = 'orange', show.legend = F) +
  geom_text_repel(aes(label = Region), size = 4, max.overlaps = 7 ,show.legend = F, family = 'Lobster',
                  nudge_x = 1, nudge_y = 120) + # Добавляем подписи
  theme_classic(base_family = 'Merriweather', base_size = 12) + # более аккуратная тема для графика
  scale_size_continuous(range = c(3, 15)) + # Размеры кружочков
  transition_time(as.Date(Date)) + # Изменение положения и размера кружочков во времени
  labs(y = 'Площадь в процессе застройки, млн. м2', x = 'Индекс цен на недвижимость, г/г.', # Подписи
       title = 'Стоимость недвижимости и динамика строительства',
       subtitle = '{frame_time}', # Изменяющийся подзаголовок
       caption = '*Размер кружка - относительный ВРП на душу населения')

animate(p, height = 6, width = 8, units = "in", res = 200, duration = 70,
        fps = 30, end_pause = 50, render = av_renderer("anim_construction.mp4"))
```

<center>

<video width="800" height="600" controls>
<source src="https://github.com/ETymch/Econometrics_2023/raw/main/Pics/anim_construction.mp4" type="video/mp4">
</video>

</center>

Мы хотим проверить, как рост цен на жильё влияет на динамику строительства. Проблема в том, что в выборке разные регионы, которые между собой заметно отличаются и это может повлиять на результат. Учесть неоднородность регионов в выборке поможет **модель с фиксированными эффектами**.

## Теория

Допустим, наша модель имеет вид:

`\(y = \alpha + \beta_1 x + \beta_2z + \varepsilon\)`

Где `\(x\)` - наблюдаемые переменные.
`\(z\)` - ненаблюдаемые переменные, но те, которые мы хотели бы учесть, и мы знаем, что они важные.

<center>

![yay](https://github.com/ETymch/Econometrics_2023/raw/main/Pics/plot_scheme_11.svg)

</center>

Как мы помним из [лекции об инструментальных переменных](https://econisfun.netlify.app/2023/07/11/%D0%BB%D0%B5%D0%BA%D1%86%D0%B8%D1%8F-5/), в таких случаях оценки МНК - **смещённые**:(

> Смещённость - свойства оценки, которое появляется, если `\(\theta \neq \mathbb{E}\theta\)`, т.е. ожидаемая ошибка (на выборке) в статистической модели не равна нулю.

* Не меняющаяся во времени гетерогенность `\(z\)`. С ней бороться очень просто. Если `\(z\)` во времени не меняется, то `\(z_t = z_{t-1}\)` и так далее.

**Пример!**

Тогда мы можем оценить модель в дифференциалах (*first difference*) в следующей форме и получить корректные оценки:

$$
`\begin{align}
\Delta y_t = \\
&= y_t - y_{t-1} \\
&= [\alpha + \beta_1 x_t + \beta_2z_t + \varepsilon_t] - [\alpha + \beta_1 x_{t-1} + \beta_2z_{t-1} + \varepsilon_{t-1}]\\
&=\beta_1[x_t - x_{t-1}] + \epsilon_t\\
&=\beta_1\Delta x_t + \epsilon_t
\end{align}`
$$
Другая стратегия - оценить МНК модель с дамми переменными для каждой индивида/группы/страны. Так делали в старину и лучше обходить стороной эту идею.

Намного лучше - **модель с фиксированными эффектами**. Что мы делаем:
* Для независимой и зависимой переменной вычитаем для каждого индивида/страны/группы среднее значение переменной для индивида/страны/группы.

`$$y_{it} - \bar{y}_{it} = [x_{it} - \bar{x}_n]\beta + [\varepsilon_{it} -\bar{\varepsilon}_i]$$`
Иногда ещё делят на стандартное отклонение, чтобы получить нормальную случайную величину с дисперсией 1, но это не всегда необходимо.

Давайте рассмотрим более общий случай:

$$
`\begin{align}
y_{it} 
&= X_{it}\beta + \varepsilon_{it}\\
&= X_{it}\beta + \eta_i + \nu_{it}
\end{align}`
$$
Где:
* `\(\eta_i\)` - индивидуальный эффект,
* `\(\eta_i\)` - остаточный эффект.

Далее можно просто показать табличку с панельными данными.

* *Within variation* - то, как индивид/страна меняется во времени.
* *Between variation* - то, как один индивид/страна отличается от остальных в выборке.

В **fixed effects** мы контролируем *between variation*

Теперь проверим наши гипотезы и сравним модели, где индивидуальные характеристики регионов не учтены (**МНК**) и **модель с фиксированными эффектами**:


```r
ols <- lm(S_unf_redone ~ Population + HPI + CPI + GRP_pc, data = data_housing) # МНК
FE <- plm(S_unf_redone ~ Population + HPI + CPI + GRP_pc, data = data_housing,
          index = c("Region"), model ="within") # Модель с фиксированными эффектами

stargazer(ols, FE, type = 'html', header = FALSE) # Сравнение моделей
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">S_unf_redone</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td><em>panel</em></td></tr>
<tr><td style="text-align:left"></td><td><em></em></td><td><em>linear</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Population</td><td>0.0003<sup>***</sup></td><td>0.001<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.00000)</td><td>(0.0001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">HPI</td><td>1.399<sup>***</sup></td><td>0.608<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.369)</td><td>(0.294)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">CPI</td><td>0.466</td><td>-4.931<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.424)</td><td>(1.260)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">GRP_pc</td><td>0.002<sup>***</sup></td><td>0.001<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0001)</td><td>(0.0002)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-450.052<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(149.046)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>5,472</td><td>5,472</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.587</td><td>0.102</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.587</td><td>0.089</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>406.077 (df = 5467)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>1,945.608<sup>***</sup> (df = 4; 5467)</td><td>152.772<sup>***</sup> (df = 4; 5396)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Как мы видим, модель с фиксированными эффектами намного успешнее уловила влияние инфляции (*CPI*) на строительство. В остальном, обе модели подтверждают гипотезу о том, что рост спроса на жильё в регионе увеличивает цены на недвижимость. Затем застройщики пытаются компенсировать неравновесное состояние на рынке жилья, увеличивая интенсивность строительства.

## Пример 2
### Экономическое развитие и продолжительность жизни

Данные:


```r
gm <- read.csv('https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/gapminder.csv') %>%
  as.tibble() %>%
  mutate(log_GDPperCap = log(gdpPercap)) %>%
  group_by(country) %>%
  mutate(lifeExp_within = lifeExp - mean(lifeExp),
         log_GDPperCap_within = log_GDPperCap - mean(log_GDPperCap)) %>%
  ungroup()
```

```
## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
## ℹ Please use `as_tibble()` instead.
## ℹ The signature and semantics have changed, see `?as_tibble`.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```r
gm %>% head()
```

```
## # A tibble: 6 × 10
##       X country     continent  year lifeExp      pop gdpPercap log_GDPperCap
##   <int> <chr>       <chr>     <int>   <dbl>    <int>     <dbl>         <dbl>
## 1     1 Afghanistan Asia       1952    28.8  8425333      779.          6.66
## 2     2 Afghanistan Asia       1957    30.3  9240934      821.          6.71
## 3     3 Afghanistan Asia       1962    32.0 10267083      853.          6.75
## 4     4 Afghanistan Asia       1967    34.0 11537966      836.          6.73
## 5     5 Afghanistan Asia       1972    36.1 13079460      740.          6.61
## 6     6 Afghanistan Asia       1977    38.4 14880372      786.          6.67
## # ℹ 2 more variables: lifeExp_within <dbl>, log_GDPperCap_within <dbl>
```

У нас есть данные о ВВП на душу населения и ожидаемой продолжительности жизни более чем в **100 странах** на временном промежутке **с 1952 по 2007** гг. с интервалом 5 лет. Мы хотим измерить, как экономическое развитие влияет на ожидаемую продолжительность жизни.

* Снова имеем *индивидуальную гетерогенность*. Страны разнятся в культуре, географии, предрасположенности к болезням, представлениями о гигиене и пр.
* Данных об этих межстрановых различия у нас нет:(
* Тем не менее, Мы можем *предположить*, что все эти эффекты (межстрановые различия, названные выше) - фиксированные эффекты.

Давайте изобразим данные! Изобразить на одном графике данные по всем странам - сложно. На лекции мы проголосовали за 4 страны:


```r
gm %>%
  filter(country %in% c('France', 'Korea, Dem. Rep.', 'Korea, Rep.', 'Albania')) %>%
  ggplot(aes(x = lifeExp_within, y = log_GDPperCap_within, color = country)) +
  geom_point(size = 5, alpha = 0.5) +
  theme_minimal() +
  facet_wrap(~country)
```

<img src="/post/Lecture-6/Lec_6_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Оценим модель:


```r
mod_0 <- lm(lifeExp ~ log_GDPperCap, gm)
mod_1 <- plm(lifeExp ~ log_GDPperCap, gm, index = c('country'))

stargazer(mod_0, mod_1, type = 'html', header = FALSE) 
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">lifeExp</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td><em>panel</em></td></tr>
<tr><td style="text-align:left"></td><td><em></em></td><td><em>linear</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">log_GDPperCap</td><td>8.405<sup>***</sup></td><td>9.769<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.149)</td><td>(0.297)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-9.101<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.228)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,704</td><td>1,704</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.652</td><td>0.410</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.652</td><td>0.356</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>7.620 (df = 1702)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>3,192.273<sup>***</sup> (df = 1; 1702)</td><td>1,085.295<sup>***</sup> (df = 1; 1561)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Заметьте, мы получили **очень** большой эффект экономического развития на продолжительность жизни! Наш результат говорит, что **удвоение ВВП на душу населения** приводит к увеличению ожидаемой продолжительности жизни **на 9 лет**. Дело в переменных, которые мы пропустили! Выборка охватывает временной промежуток более 50 лет и за это время значительно изменилось состояние медицины, общемировое представление о гигиене и другие факторы, подверженные влиянию технического прогресса. Как это можно учесть в модели, чтобы получить более точные оценки параметров?

Для решения этой проблемы мы можем использовать **двусторонние фиксированные эффекты**. Мы предполагаем, что существует также гетерогенность во времени. Тогда мы также вводим дамми для каждого периода.

$$
y_{it} = \alpha_i + \alpha_t + X_{it}\beta + \varepsilon_{it}
$$
Оценим модель с двусторонними фиксированными эффектами:


```r
mod_2 <- plm(lifeExp ~ log_GDPperCap, gm, index = c('country', 'year'), effect = 'twoways')

stargazer(mod_0, mod_1, mod_2, type = 'html', header = FALSE) 
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">lifeExp</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td colspan="2"><em>panel</em></td></tr>
<tr><td style="text-align:left"></td><td><em></em></td><td colspan="2"><em>linear</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">log_GDPperCap</td><td>8.405<sup>***</sup></td><td>9.769<sup>***</sup></td><td>1.450<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.149)</td><td>(0.297)</td><td>(0.268)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-9.101<sup>***</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.228)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,704</td><td>1,704</td><td>1,704</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.652</td><td>0.410</td><td>0.019</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.652</td><td>0.356</td><td>-0.078</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>7.620 (df = 1702)</td><td></td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>3,192.273<sup>***</sup> (df = 1; 1702)</td><td>1,085.295<sup>***</sup> (df = 1; 1561)</td><td>29.366<sup>***</sup> (df = 1; 1550)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

По сравнению с обыкновенной моделью с фиксированными эффектами, коэффициент, отвечающий за влияние экономического роста на ожидаемую продолжительность жизни в модели с двусторонними эффектами уменьшился приблизительно в *6 раз*, тем не менее, он остался *значимым*! После того, как мы учли межвременную неоднородность, нам удалось более точно выделить влияние факторов, связанных с экономическим ростом на ожидаемую продолжительность жизни и получить значительно более достоверный результат!


## Пример 3.
### Банковские кризисы в отсутствие паники

<center>

![bank runs](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/bank_runs.gif?raw=true)

</center>

Данные пример основан на статье [Baron et al.](https://wxiong.mycpanel.princeton.edu/papers/BankingCrisis.pdf) в QJE за 2020 год. В ней авторы исследуют случаи, когда активы банков теряли в цене, за этим следовало сокращение кредитования и экономический спад. Дело в том, что в макроэкономике банковские кризисы часто связывают с паникой [заложили эту традицию Diamond, D., Dybvig, P.](https://www.jstor.org/stable/1837095), когда вкладчики стремятся забрать из банков свои накопления. У банков не хватает резервов, чтобы осуществить выплаты по всем вкладам и случается кризис и экономический спад. на выборке по 46 странам с 1870 по 2016 гг. авторы доказывают, что основной первопричиной банковских кризисов является не паника вкладчиков, а сокращение активов банков.

Они ставят вопросы:

* Влияют ли существенные снижения ликвидности капиталов банков на макроэкономическую активность?
* Является ли паника на банковском рынке необходимым условием для возникновения негативных среднесрочных последствий для макроэкономики?

$$
\Delta GDP_{t+h} = \alpha + \beta BankEquities_t + \beta Panic_t + \gamma X_{t,...,t+h} + \varepsilon_{t+h}
$$
Давайте проверим и мы!


```r
data <- rio::import('https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/BVX_annual_regdata.dta')

data %>%
  head()
```

```
##     country ccode ISO3 year decade postwar rgdp_gr credit_to_gdp   Rtot_real
## 1 Argentina    32  ARG 1870   1870       0      NA            NA  0.06641372
## 2 Argentina    32  ARG 1871   1870       0      NA            NA  0.11768574
## 3 Argentina    32  ARG 1872   1870       0      NA            NA  0.20709625
## 4 Argentina    32  ARG 1873   1870       0      NA            NA  0.03341857
## 5 Argentina    32  ARG 1874   1870       0      NA            NA  0.09157762
## 6 Argentina    32  ARG 1875   1870       0      NA            NA -0.07226025
##   Rtot_real_w Rtot_nonfin_real Rtot_nonfin_real_w C_B30 C_N30 jointcrisis JC
## 1  0.06641372               NA                 NA     0    NA          NA  0
## 2  0.11768574               NA                 NA     0    NA          NA  0
## 3  0.20709625               NA                 NA     0    NA          NA  0
## 4  0.03341857               NA                 NA     0    NA          NA  0
## 5  0.09157762               NA                 NA     0    NA          NA  0
## 6 -0.07226025               NA                 NA     0    NA          NA  0
##   revisedcrisis RC PANIC PANIC_ind PANIC_year PANIC_finer PANIC_f PANIC_finer_f
## 1            NA  0     0         0          0           0       0             0
## 2            NA  0     0         0          0           0       0             0
## 3            NA  0     0         0          0           0       0             0
## 4            NA  0     0         0          0           0       0             0
## 5            NA  0     0         0          0           0       0             0
## 6            NA  0     0         0          0           0       0             0
##   bankeqdecline bankfailure_narrative bankfailure_narrative_f
## 1            NA                     0                       0
## 2            NA                     0                       0
## 3            NA                     0                       0
## 4            NA                     0                       0
## 5            NA                     0                       0
## 6            NA                     0                       0
##   ReinhartRogoffCrisis LaevenValenciaCrisis  P P2  T     inf_cpi g0y g1y g2y
## 1                   NA                   NA NA NA NA  0.04693500  NA  NA  NA
## 2                   NA                   NA NA NA NA  0.07198795  NA  NA  NA
## 3                   NA                   NA NA NA NA  0.17589210  NA  NA  NA
## 4                   NA                   NA NA NA NA  0.00000000  NA  NA  NA
## 5                   NA                   NA NA NA NA -0.02293907  NA  NA  NA
## 6                   NA                   NA NA NA NA -0.03497187  NA  NA  NA
##   g3y g4y g5y g6y g7y g8y g9y g10y g11y g12y g13y g14y g15y g0Rtot_real_w
## 1  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA    0.06641372
## 2  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA    0.11768574
## 3  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA    0.20709625
## 4  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA    0.03341857
## 5  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA    0.09157762
## 6  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA   -0.07226025
##   g1Rtot_real_w g2Rtot_real_w g3Rtot_real_w g4Rtot_real_w g5Rtot_real_w
## 1    0.19191541    0.43875661   0.486837804    0.62299889     0.5057206
## 2    0.34915426    0.39424106   0.521922350    0.41194785     0.2293077
## 3    0.24743567    0.36167285   0.263278037    0.09986882     0.2441169
## 4    0.12805657    0.04654292  -0.088830896    0.03066917     0.1851690
## 5    0.01269994   -0.11829617  -0.002660481    0.14684315     0.5098159
## 6   -0.19226648   -0.08633202   0.050629053    0.38315034     0.5823517
##   g6Rtot_real_w g7Rtot_real_w g8Rtot_real_w g9Rtot_real_w g10Rtot_real_w
## 1     0.3109506     0.4828821     0.7051697      1.244851       1.568155
## 2     0.3905317     0.5989758     1.1050475      1.408217       1.738262
## 3     0.4306131     0.8833983     1.1546454      1.449939       1.887932
## 4     0.5602718     0.7849823     1.0296135      1.392462       1.882212
## 5     0.7272598     0.9639801     1.3150951      1.789008       1.939934
## 6     0.7992125     1.1208707     1.5550245      1.693289       2.491918
##   g11Rtot_real_w g12Rtot_real_w g13Rtot_real_w g14Rtot_real_w g15Rtot_real_w
## 1       1.920120       2.442171       3.146802       3.371205       4.667379
## 2       2.227801       2.888549       3.098977       4.314428       6.111607
## 3       2.479107       2.667378       3.754849       5.362795       6.569223
## 4       2.038182       2.939081       4.271159       5.270605       7.521569
## 5       2.811700       4.100701       5.067827       7.246000       8.555224
## 6       3.672779       4.558768       6.554203       7.753590       6.994351
##   g0credit_to_gdp g1credit_to_gdp g2credit_to_gdp g3credit_to_gdp
## 1              NA              NA              NA              NA
## 2              NA              NA              NA              NA
## 3              NA              NA              NA              NA
## 4              NA              NA              NA              NA
## 5              NA              NA              NA              NA
## 6              NA              NA              NA              NA
##   g4credit_to_gdp g5credit_to_gdp g6credit_to_gdp g7credit_to_gdp
## 1              NA              NA              NA              NA
## 2              NA              NA              NA              NA
## 3              NA              NA              NA              NA
## 4              NA              NA              NA              NA
## 5              NA              NA              NA              NA
## 6              NA              NA              NA              NA
##   g8credit_to_gdp g9credit_to_gdp g10credit_to_gdp g11credit_to_gdp
## 1              NA              NA               NA               NA
## 2              NA              NA               NA               NA
## 3              NA              NA               NA               NA
## 4              NA              NA               NA               NA
## 5              NA              NA               NA               NA
## 6              NA              NA               NA               NA
##   g12credit_to_gdp g13credit_to_gdp g14credit_to_gdp g15credit_to_gdp
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##          Fd1y Fd1credit_to_gdp Fd1Rtot_real_w       Fd2y Fd2credit_to_gdp
## 1          NA               NA     0.11768574         NA               NA
## 2          NA               NA     0.20709625         NA               NA
## 3          NA               NA     0.03341857         NA               NA
## 4          NA               NA     0.09157762         NA               NA
## 5          NA               NA    -0.07226025         NA               NA
## 6 0.001833181               NA    -0.12935333 0.07877161               NA
##   Fd2Rtot_real_w        Fd3y Fd3credit_to_gdp Fd3Rtot_real_w       Fd4y
## 1     0.34915426          NA               NA     0.39424106         NA
## 2     0.24743567          NA               NA     0.36167285         NA
## 3     0.12805657          NA               NA     0.04654292         NA
## 4     0.01269994          NA               NA    -0.11829617         NA
## 5    -0.19226648          NA               NA    -0.08633202         NA
## 6    -0.01516779 0.004881032               NA     0.13246097 0.02177944
##   Fd4credit_to_gdp Fd4Rtot_real_w       Fd5y Fd5credit_to_gdp Fd5Rtot_real_w
## 1               NA    0.521922350         NA               NA     0.41194785
## 2               NA    0.263278037         NA               NA     0.09986882
## 3               NA   -0.088830896         NA               NA     0.03066917
## 4               NA   -0.002660481         NA               NA     0.14684315
## 5               NA    0.050629053         NA               NA     0.38315034
## 6               NA    0.490881830 -0.0191423               NA     0.70559877
##          Fd6y Fd6credit_to_gdp Fd6Rtot_real_w      Fd7y Fd7credit_to_gdp
## 1          NA               NA      0.2293077        NA               NA
## 2          NA               NA      0.2441169        NA               NA
## 3          NA               NA      0.1851690        NA               NA
## 4          NA               NA      0.5098159        NA               NA
## 5          NA               NA      0.5823517        NA               NA
## 6 -0.02539623               NA      0.9393505 0.1761866               NA
##   Fd7Rtot_real_w      Fd8y Fd8credit_to_gdp Fd8Rtot_real_w     Fd9y
## 1      0.3905317        NA               NA      0.5989758       NA
## 2      0.4306131        NA               NA      0.8833983       NA
## 3      0.5602718        NA               NA      0.7849823       NA
## 4      0.7272598        NA               NA      0.9639801       NA
## 5      0.7992125        NA               NA      1.1208707       NA
## 6      1.2860622 0.2578847               NA      1.7540317 0.290888
##   Fd9credit_to_gdp Fd9Rtot_real_w    Fd10y Fd10credit_to_gdp Fd10Rtot_real_w
## 1               NA       1.105047       NA                NA        1.408217
## 2               NA       1.154645       NA                NA        1.449939
## 3               NA       1.029613       NA                NA        1.392462
## 4               NA       1.315095       NA                NA        1.789008
## 5               NA       1.555025       NA                NA        1.693289
## 6               NA       1.903065 0.404037                NA        2.763898
##      Fd11y Fd11credit_to_gdp Fd11Rtot_real_w     Fd12y Fd12credit_to_gdp
## 1       NA                NA        1.738262        NA                NA
## 2       NA                NA        1.887932        NA                NA
## 3       NA                NA        1.882212        NA                NA
## 4       NA                NA        1.939934        NA                NA
## 5       NA                NA        2.491918        NA                NA
## 6 0.369558                NA        4.036735 0.3879839                NA
##   Fd12Rtot_real_w     Fd13y Fd13credit_to_gdp Fd13Rtot_real_w     Fd14y
## 1        2.227801        NA                NA        2.888549        NA
## 2        2.479107        NA                NA        2.667378        NA
## 3        2.038182        NA                NA        2.939081        NA
## 4        2.811700        NA                NA        4.100701        NA
## 5        3.672779        NA                NA        4.558768        NA
## 6        4.991733 0.4817841                NA        7.142589 0.4978984
##   Fd14credit_to_gdp Fd14Rtot_real_w     Fd15y Fd15credit_to_gdp Fd15Rtot_real_w
## 1                NA        3.098977        NA                NA        4.314428
## 2                NA        3.754849        NA                NA        5.362795
## 3                NA        4.271159        NA                NA        5.270605
## 4                NA        5.067827        NA                NA        7.246000
## 5                NA        6.554203        NA                NA        7.753590
## 6                NA        8.435394 0.3904704                NA        7.617020
```

Давайте посмотрим, существует ли какая-либо зависимость между паникой на банковском рынке и сокращением банковских активов. Выберем 4 случайных страны:


```r
data %>%
  filter(country %in% c('Italy', 'Spain', 'Austria', 'Argentina')) %>%
  ggplot(aes(x = Rtot_real_w, y = PANIC_ind)) +
  geom_point(color = '#ad466c', size = 2, alpha = 0.6) +
  facet_wrap(~country) +
  theme_bw(base_family = 'Lobster') +
  labs(x = 'Индекс реальной доходности собственного капитала банка',
       y = 'Паника')
```

```
## Warning: Removed 87 rows containing missing values (`geom_point()`).
```

<img src="/post/Lecture-6/Lec_6_files/figure-html/unnamed-chunk-11-1.png" width="672" />


В данном случае мы опускаем существование межвременной гетерогенности и оцениваем две модели: обыкновенную модель, не учитывающую межстрановые различия и модель с индивидуальными фиксированными эффектами.

Переменные:

* `g3y` - среднее значение роста выпуска в последующие 3 года.
* `Rtot_real_w` - индекс реальной доходности собственного капитала банка.
* `PANIC_ind` - индикатор паники на банковском рынке.


```r
mod_0 <- plm(g3y ~ Rtot_real_w + PANIC_ind, data, model = 'within', index = c('country'))
mod_0_ols <- lm(g3y ~ Rtot_real_w + PANIC_ind, data)

stargazer(mod_0_ols, mod_0, type = 'html', header = FALSE)
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">g3y</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td><em>panel</em></td></tr>
<tr><td style="text-align:left"></td><td><em></em></td><td><em>linear</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Rtot_real_w</td><td>0.047<sup>***</sup></td><td>0.044<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.006)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">PANIC_ind</td><td>-0.068<sup>***</sup></td><td>-0.068<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.129<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.002)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,786</td><td>3,786</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.034</td><td>0.036</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.033</td><td>0.024</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.112 (df = 3783)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>65.805<sup>***</sup> (df = 2; 3783)</td><td>70.577<sup>***</sup> (df = 2; 3738)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

В данном случае, даже короткая регрессия помогла нам получить глубокий теоретический результат:
* Даже в отсутствии паники, снижение стоимости активов банков приводит к существенному снижению кредитования, что приводит к снижению экономической активности.
* Таким образом, невозможно объяснять финансовые кризисы лишь паникой, возникающей на финансовых рынках. Снижение выпуска имеет и иные причины, зачастую более важные, чем поведенческие.
