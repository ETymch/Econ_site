---
title: "Лекция 3"
author: "Е. Тымченко"
date: 2023-06-14
categories: ["R"]
tags: ["Econ"]
---

Сегодня мы поупражняемся в тестировании гипотез при помощи ранее изученной теории.

Нам понадобятся библиотеки:


```r
library(wooldridge) # Библиотека с датасетами из учебника Introductory Econometrics by J. Wooldridge. 
library(tidyverse) # Для работы с табличками в R, рисования и написания кода
library(stargazer) # Красивые таблицы со статистикой регрессий

library(hrbrthemes) # Темы для графиков ggplot2, попробуйте `+theme_ipsum()`
library(ftplottools) # Популярная в аналитике тема в ggplot, похожая на ту, которую используют для графиков Financial Times.
library(showtext) # Для рендера шрифтов в ggplot2.
library(sysfonts) # Для загрузки шрифтов из google fonts. Найдите шрифты, которые вам по душе https://fonts.google.com/.
font_add_google('Merriweather') # так мы можем загрузить шрифт из google fonts
showtext.auto() # Для использования библиотеки showtext для рендера шрифта по-умолчанию.
```

Для анализа мы будем использовать данные из статьи [Hamermesh, D.S. and J.E. Biddle (1994)](https://www.jstor.org/stable/2117767), в которой авторы эмпирически тестировали трудовую дискриминацию в пользу более привлекательных людей. А также то, что этот эффект существует и значим как для мужчин, так и для женщин. Наш набор данных переработан автором учебника и включает лишь часть данных, использованных в статье, что не мешает нам протестировать гипотезы из статьи. Подробную информацию о данных можно получить, набрав `?wooldridge::beauty`.


```r
data <- beauty # Загружаем наш набор данных
```

Для начала, давайте проверим, есть ли вообще какая-либо разница между доходами более привлекательных и менее привлекательных людей. В данных есть переменная, отвечающая за внешнюю привлекательность - `looks`, которая принимает значения от 1 до 5. Если `looks` принимает значение 1 или 2, мы говорим, что внешняя привлекательность человека ниже среднего (below average или сокращённо belavg). Давайте сравним средние зарплаты для групп с внешней привлекательностью ниже среднего (`belavg == 1`) и прочими участниками исследования (`belavg == 0`)



```r
data_l <- data %>%
  filter(belavg == 1) # Часть данных, в которой переменная belavg = 1 (привлекательность ниже средней)
data_h <- setdiff(data, data_l) # Остальные участники (belavg = 0)

s <- mean(data_h$lwage) - mean(data_l$lwage) # разница между заработными платами более привлекательных и менее привлекательных людей
s
```

```
## [1] 0.1930406
```

Предположим, что на самом деле эта разница в 0.19 получилась в нашей выборке случайно и привлекательность на заработные платы **не влияет**.  Тогда мы можем случайным образом разбить наши исходные данные на 2 кусочка, посчитать разницу между средними заработными платами в этих кусочках и получить 0.19 много раз. Это бы значило, что в нашей выборке разница 0.19 между средними зп более привлекательных и менее привлекательных людей не значима и заработные платы скорее всего не связаны с внешним обаянием. Проведём такой тест


```r
stat <- c() # создаём пустой вектор
for (i in 1:1000){ # Запускаем петлю с 1000 итераций.
data_0 <- sample_n(data, 155) # Случайным образом выбираем 155 наблюдений выборки
data_1 <- setdiff(data, data_0) # Дополнение data_0 до выборки.

stat[i] <- mean(data_1$lwage) - mean(data_0$lwage) # Разница в средних заработных платах между двумя выборками
}
```

Мы получили вектор stat, состоящий из 1000 наблюдений. Каждое наблюдение - разница между средними заработными платами в случайном разбиении нашей выборки на 2. Теперь мы можем построить график в ggplot2.

# Построим график в ggplot


```r
tibble(result = stat) %>% # Создаём табличку с нашим вектором stat.
  ggplot(aes(x = result)) + # aes - ось. В данном случае мы отобразим наш вектор stat (result), по горизонтальной оси x.
  geom_density(alpha = 0.2, fill = 'orange', color = NA) + # мы хотим нарисовать функцию плотности
  theme_ipsum() + # Красивая тема из библиотеки hrbrthemes.
  labs(title = 'Красота имеет значение',
       x = 'Плотность',
       y = 'Логарифм заработных плат') + # Добавим подписи осей и название
  geom_vline(xintercept = mean(stat)) + # Вертикальная линия, среднее разницы зп в нашем численном эксперименте 
  geom_vline(xintercept = s, color = 'red', linetype = 'dotted', size = 1.2) # Разница заработных плат между более привлекательными и менее привлекательными людьми в выборке.
```

<img src="/post/Lecture-3/Lec_3_fin_files/figure-html/unnamed-chunk-5-1.png" width="672" />

Данный результат можно получить, оценив модель с одной объясняющей переменной методом наименьших квадратов.

`$$lwage_i = \alpha + \beta belavg_i + \epsilon_i$$`

```r
mod_0 <- lm(lwage ~ belavg, data) # линейная модель с одно объясняющей переменной.

mod_0 %>% summary # Описательная статистика к нашей модели. Обратите внимание, значение коэффициента при belavg в точности равно 0.19!
```

```
## 
## Call:
## lm(formula = lwage ~ belavg, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.66274 -0.36079  0.01307  0.38895  2.67057 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.68255    0.01779  94.581  < 2e-16 ***
## belavg      -0.19304    0.05072  -3.806 0.000148 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5913 on 1258 degrees of freedom
## Multiple R-squared:  0.01138,	Adjusted R-squared:  0.0106 
## F-statistic: 14.49 on 1 and 1258 DF,  p-value: 0.000148
```

Описательная статистика предлагает нам много информации о коэффициентах регрессии и прилагающейся к ним статистике. Например, `Std. Error` для полученных нами коэффициентов. Давайте разберёмся, что это значит.
Дело в том, что мы можем думать о коэффициентах регрессии ($\beta$), как о случайных величинах, которые тоже имеют какое-то распределение. Это распределение мы можем получить при помощи **Бутстрапa**.
Бутстрап - очень простой, и вместе с тем, очень полезный и значимый для современной статистики алгоритм. Допустим, мы хотим узнать распределение коэффициента `\(\beta\)` при переменной belavg. Чтобы это сделать, нужно всего-навсего:

* Выбрать случайным образом (с возвращением, т.е. какое-то наблюдение можно выбрать несколько раз) какое-то кол-во наблюдений из нашей выборки (назовём это подвыборкой).
* Оценить на этой подвыборке нашу регрессионную модель `\(lwage_i = \alpha + \beta belavg_i + \epsilon\)`.
* Записать значение коэффициента `\(\beta\)`.
* Слелать это несколько тысяч раз.
* Тогда записанные нами значения `\(\beta\)` и будут распределением нашего коэффициента.
* По центральной предельной теореме, при увеличении количества наблюдений в подвыборке (из пункта 1), распределение `\(\beta\)` всё больше напоминает нормально распределение с параметрами `\((\hat{\beta}, Std. Error^2)\)`.

Само собой, мы можем вывести формулу для стандартного отклонения коэффициентов аналитически. На мой взгляд, это излишне и не даёт ученикам нужной интуиции при первом знакомстве с предметом.


```r
stat_1 <- c() # пустой вектор, куда мы будем записывать значения коэффициентов.
for (i in 1:3000){ # проведём эксперимент 3000 раз!
samp <- sample_n(data, 1500, replace = T) # Выберем случайным образом 1500 наблюдений из нашей выборки с 1260 наблюдений (да, мы можем себе это позволить, потому что выбираем мы с возвращением (одно наблюдение может быть выбрано несколько раз)). При увеличении числа наблюдений с 1500 до большего числа распределение нашего итогового результата будет стремиться к нормальному с параметрами (b, std. error).
mod <- lm(lwage ~ belavg, samp) # Оцениваем мнк модель для нашей подвыборки
stat_1[i] <- mod$coefficients[2] # Записываем получившийся коэффициент в вектор
} # Повторяем 3000 раз.

sd(stat_1) # стандартное отклонение случайной величины
```

```
## [1] 0.04526369
```

```r
mean(stat_1) # среднее
```

```
## [1] -0.194014
```

## Проверим гипотезу 1:

> При прочих равных менее привлекательные люди зарабатывают меньше, чем более красивые.

Для этого нам нужно построить и проанализировать регрессионную модель

`$$lwage_i = \alpha + \beta_1 looks_i + \beta_2 exper_i + \beta_3 educ_i + \beta_4 bigcity_i + beta_5 black_i + \epsilon_i$$`
В которой:

* `lwage` - логарифм заработных плат.
* `looks` - значение внешней привлекательности от 1 до 5.
* `exper` - опыт работы в годах (предполагается, что более опытные сотрудники получают бОльшие заработные платы).
* `educ` - кол-во лет, проведённых за получением образования (предполагается, что более образованные люди получают большую заработную плату).
* `bigcity` - крупный ли город, где человек работает (предполагается, что в крупных городах заработные платы выше)
* `black` = 1, если участник опроса чернокожий. Иначе 0. Мы включили этот параметр, чтобы учесть социальные факторы, влияющие на заработные платы.


```r
mod_1 <- lm(lwage ~ looks + exper + educ + bigcity + black, data) # полная модель
mod_2 <- lm(lwage ~ looks, data) # укороченная модель

stargazer(mod_0, mod_1, type = 'html', header = FALSE) # функция stargazer из одноимённой библиотеки позволяет нам строить такие красивые таблички и использовать их в отчётах, публикациях и пр.
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">lwage</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">belavg</td><td>-0.193<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.051)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">looks</td><td></td><td>0.061<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">exper</td><td></td><td>0.018<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">educ</td><td></td><td>0.066<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.006)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">bigcity</td><td></td><td>0.209<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.036)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">black</td><td></td><td>-0.208<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.057)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>1.683<sup>***</sup></td><td>0.275<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.018)</td><td>(0.104)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,260</td><td>1,260</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.011</td><td>0.232</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.011</td><td>0.229</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.591 (df = 1258)</td><td>0.522 (df = 1254)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>14.485<sup>***</sup> (df = 1; 1258)</td><td>75.661<sup>***</sup> (df = 5; 1254)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


## Значимые наблюдения:

В эконометрических исследованиях норма, когда выборка не репрезентативная. Иначе говоря, не представляет собой идеальный срез генеральной совокупности. Бывает, что в выборку попадают наблюдения, заметно отличающиеся от всех прочих, вероятность появления которых в действительности значительно меньше той, с которой оно попало в нашу выборку (1/число наблюдений в выборке).
Такие наблюдениявносят большой вклад в то, какими получаются коэффициенты регрессии. Бывает, что они искажают наши результаты. Объясним на примере:


```r
stat_2 <- c() # создадим пустой вектор
for (i in 1:nrow(data)){ # для каждого наблюдения в ввыборке
mod <- lm(lwage ~ looks + exper + educ + bigcity + black, data[-i,]) # оценим регрессии для подвыборки (выборка за исключением этого наблюдения)
stat_2[i] <- sum(mod$residuals^2) # посчитаем сумму квадратов остатков
}

stat_2 <- stat_2 / mean(stat_2) # нормируем для удобства наш вектор

plot(stat_2)
```

<img src="/post/Lecture-3/Lec_3_fin_files/figure-html/unnamed-chunk-9-1.png" width="672" />
Мы видим, что если бы мы исключили 603 наблюдение, сумма квадратов остатков уменьшилась бы заметно. Это означает, что 603 наблюдение заметно отличается от прочих в нашей выборке. Давайте посмотрим на него:


```r
data[603, ]
```

```
##      wage    lwage belavg abvavg exper looks union goodhlth black female
## 603 77.72 4.353113      0      1     9     4     1        1     1      1
##     married south bigcity smllcity service expersq educ
## 603       1     0       1        0       1      81   13
```

Действительно, мы имеем привлекательную замужнюю чернокожую женщину из большого города с заработной платой, значительно превосходящей всех прочих людей в выборке. Вероятно, она принадлежит к общественной группе, заметно отстоящей от индивидов в выборке.


```r
mod_3 <- lm(lwage ~ looks + exper + educ + bigcity + black, data[-603,]) # Оценим ещё одну модель, но уже без 603 наблюдения
stargazer(mod_1, mod_2, mod_3, type = 'html', header = FALSE) # Удивительно, но даже одно наблюдение, если оно заметно отличается от прочих, может значительно влиять на получаемые нами коэффициенты.
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">lwage</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">looks</td><td>0.061<sup>***</sup></td><td>0.051<sup>**</sup></td><td>0.058<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td><td>(0.024)</td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">exper</td><td>0.018<sup>***</sup></td><td></td><td>0.018<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td></td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">educ</td><td>0.066<sup>***</sup></td><td></td><td>0.066<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td></td><td>(0.006)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">bigcity</td><td>0.209<sup>***</sup></td><td></td><td>0.201<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.036)</td><td></td><td>(0.036)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">black</td><td>-0.208<sup>***</sup></td><td></td><td>-0.238<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.057)</td><td></td><td>(0.057)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.275<sup>***</sup></td><td>1.496<sup>***</sup></td><td>0.286<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.104)</td><td>(0.080)</td><td>(0.102)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,260</td><td>1,260</td><td>1,259</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.232</td><td>0.003</td><td>0.237</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.229</td><td>0.003</td><td>0.234</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.522 (df = 1254)</td><td>0.594 (df = 1258)</td><td>0.516 (df = 1253)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>75.661<sup>***</sup> (df = 5; 1254)</td><td>4.349<sup>**</sup> (df = 1; 1258)</td><td>77.987<sup>***</sup> (df = 5; 1253)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## Проверим гипотезу 2:

> Для женщин эффект привлекательности на заработные платы больше, чем для мужчин.

Давайте посмотрим, есть ли какая-либо разница в эффекте привлекательности на заработные платы, если мы оценим 2 модели: первую - для мужчин в нашей выборке, вторую - для женщин.


```r
data_m <- data %>%
  filter(female == 0) # мужчины в выборке
data_f <- setdiff(data, data_m) # женщины в выборке

mod_m <- lm(lwage ~ looks + exper + educ + black + bigcity, data_m) # модель для мужчин
mod_f <- lm(lwage ~ looks + exper + educ + black + bigcity, data_f) # модель для женщин

mod_m$coefficients[2] - mod_f$coefficients[2]
```

```
##         looks 
## -0.0003200253
```

Чтобы проверить гипотезу, вновь воспользуемся бутстрапом. На сей раз статистика, которая нам нужна - разница между заработными коэффициентами `\(\beta_1\)` в двух моделях.


```r
boot_lm <- function(data, n){
  stat <- c()
  for (i in 1:n){
  
  data_m <- data %>%
  sample_n(500)
  
  data_f <- setdiff(data, data_m)

mod_m <- lm(lwage ~ looks + exper + educ + black + bigcity, data_m)
mod_f <- lm(lwage ~ looks + exper + educ + black + bigcity, data_f)

stat[i] <- mod_m$coefficients[2] - mod_f$coefficients[2]
  }
  stat
}
```
Нарисуем результат:


```r
st_boot <- boot_lm(data, 1000)
tibble(st_boot = st_boot) %>%
  ggplot(aes(x = st_boot)) + 
  geom_density(alpha = 0.2, fill = '#4D081F', color = NA) +
  ft_theme(base_family = 'Merriweather') +
  labs(title = 'Мужская красота, как и женская, имеет значение', x = 'Разница между коэффициентами', y = 'Плотность')
```

<img src="/post/Lecture-3/Lec_3_fin_files/figure-html/unnamed-chunk-14-1.png" width="672" />

Мы можем заключить: внешняя привлекательность **влияет** на заработные платы мужчин **точно так же**, как на заработные платы женщин. Вместе с тем, мы видим, что ожидаемые заработные платы женщин при прочих равных ниже заработных плат мужчин.


```r
mod_4 <- lm(lwage ~ looks + exper + educ + bigcity + black + female, data[-603,]) # добавим в модель переменную female.
stargazer(mod_1, mod_2, mod_3, mod_4, type = 'html', header = FALSE)
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">lwage</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">looks</td><td>0.061<sup>***</sup></td><td>0.051<sup>**</sup></td><td>0.058<sup>***</sup></td><td>0.051<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td><td>(0.024)</td><td>(0.022)</td><td>(0.020)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">exper</td><td>0.018<sup>***</sup></td><td></td><td>0.018<sup>***</sup></td><td>0.014<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td></td><td>(0.001)</td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">educ</td><td>0.066<sup>***</sup></td><td></td><td>0.066<sup>***</sup></td><td>0.065<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td></td><td>(0.006)</td><td>(0.005)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">bigcity</td><td>0.209<sup>***</sup></td><td></td><td>0.201<sup>***</sup></td><td>0.187<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.036)</td><td></td><td>(0.036)</td><td>(0.033)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">black</td><td>-0.208<sup>***</sup></td><td></td><td>-0.238<sup>***</sup></td><td>-0.142<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.057)</td><td></td><td>(0.057)</td><td>(0.052)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">female</td><td></td><td></td><td></td><td>-0.460<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.029)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.275<sup>***</sup></td><td>1.496<sup>***</sup></td><td>0.286<sup>***</sup></td><td>0.564<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.104)</td><td>(0.080)</td><td>(0.102)</td><td>(0.095)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,260</td><td>1,260</td><td>1,259</td><td>1,259</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.232</td><td>0.003</td><td>0.237</td><td>0.365</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.229</td><td>0.003</td><td>0.234</td><td>0.362</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.522 (df = 1254)</td><td>0.594 (df = 1258)</td><td>0.516 (df = 1253)</td><td>0.471 (df = 1252)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>75.661<sup>***</sup> (df = 5; 1254)</td><td>4.349<sup>**</sup> (df = 1; 1258)</td><td>77.987<sup>***</sup> (df = 5; 1253)</td><td>119.816<sup>***</sup> (df = 6; 1252)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>
