---
title: '4. Графики было-стало'
author: "Е. Тымченко"
date: 2023-12-28
categories: ["R"]
tags: ["ggplot2"]
---

Графики было-стало - обобщённый вид графика, в котором есть данные в какой-то начальной точке и данные в конечной точке. Такой тип графика часто используется для межстрановых сопоставлений.

Прежде всего, загрузим библиотеки, шрифты, сделаем необходимые предустановки.


```r
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(httr)
library(ggbump) # В этой библиотеке есть геометрия geom_sigmoid, которую мы будем использовать во втором примере

showtext_auto()
options(scipen=999) # убираем научный (дефолтный) формат цифр
```

Установим шрифты. У вас они наверняка уже скачаны, если нет, данный код сам скачает и установит шрифт.


```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Regular.otf', write_disk('HSESans-Regular.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Bold.otf', write_disk('HSESans-Bold.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Italic.otf', write_disk('HSESans-Italic.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-SemiBold.otf', write_disk('HSESans-SemiBlod.otf', overwrite = T))

font_add(family = 'HSE Sans',
         regular = "HSESans-Regular.otf",
         bold = 'HSESans-Bold.otf',
         italic = 'HSESans-Italic.otf',
         bolditalic = 'HSESans-SemiBlod.otf'
) 
```

## Первый тип графика - Точки-линии.

Для этого типа графиков мы используем датасет по экспорту услуг в Россию (по стране экспортёру) в 2021 и 2022 гг.


```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/services_plotting_data.xlsx', write_disk('services_plotting_data.xlsx', overwrite = T))
```

```r
data <- readxl::read_excel('services_plotting_data.xlsx')
```

Посмотрим на табличку:


```r
data %>% head
```

```
## # A tibble: 6 × 7
##   Страна      Экспорт в Россию 202…¹ Экспорт в Россию 202…² `Экспорт всего 2021`
##   <chr>                        <dbl>                  <dbl>                <dbl>
## 1 Австрия                     0.495                  0.308                  69.9
## 2 Бельгия                     0.352                  0.257                 138. 
## 3 Болгария                    0.0191                 0.0179                 10.9
## 4 Великобрит…                 2.68                   1.49                  455. 
## 5 Германия                    2.73                   1.84                  388. 
## 6 Дания                       0.0312                 0.0201                 95.5
## # ℹ abbreviated names: ¹​`Экспорт в Россию 2021`, ²​`Экспорт в Россию 2022`
## # ℹ 3 more variables: `Экспорт всего 2022` <dbl>,
## #   `Доля экспорта в Россию 2021` <dbl>, `Доля экспорта в Россию 2022` <dbl>
```

Аккуратный график, который фокусирует внимание на том, насколько сильным было снижение импорта Россией услуг из недружественных стран. Данные из платёжного баланса + оценки за 2022 г.


```r
data %>%
  arrange(`Экспорт в Россию 2021`) %>%
  mutate(Страна = factor(Страна, unique(Страна))) %>%
  ggplot() +
  geom_segment(aes(x=Страна, xend=Страна, y=`Экспорт в Россию 2021`, yend=`Экспорт в Россию 2022`), color=rgb(0.7,0.2,0.1,0.5), size = 1.4, alpha = 0.4) +
  geom_point( aes(x = Страна, y=`Экспорт в Россию 2021`), color='#054949', size=3, alpha = 0.5) +
  geom_point( aes(x = Страна, y=`Экспорт в Россию 2022`), color='#E57F6E', size=3, alpha = 0.5) +
  coord_flip() +
  theme_minimal(base_family = 'HSE Sans') +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(face = 'italic', size = 7),
    axis.title.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Экспорт услуг в Россию, млрд долл.") +
  labs(caption = "Зелёные точки - 2021 г., красные точки - 2022 г.")
```

<center>

![plot](https://raw.githubusercontent.com/ETymch/Econometrics_2023/9dcf6ffff4cfd928fa6113475d9e7c36bf25df71/Pics/serv_summary_plot_by_country.svg)

</center>

## Второй тип графика - распределение по местам

В названии всё сказано. Такой тип графика больше фокусирует внимание не на изменении какого-то показателя, а на изменении ранга (места), которое занимает какая-то страна, спортсмен и пр. в списке (хотя изменение самого показателя тоже отражено на графике). В общем, выбор типа графика больше зависит от того, какую идею вы хотите донести. В данном случае я хочу показать, что в 2022 г. список основных партнёров по транзиту для России изменился.

Загрузим и преобразуем данные:


```r
data <- read.csv('https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/country_data_transit.csv') %>%
  mutate(rank_2021 = rank(total_2021, ties.method = "random"), # В исходной табличке, само собой нет данных о месте занимаемом той или иной страной в списке партнёров. Создадим соответствующий столбец
         rank_2022 = rank(total_2022, ties.method = "random"),
         start_2021 = -40, # Последние 4 столбца мы создаём для геометрии сегмент. У этой геометрии должны быть начальные и конечные точки.
         start_2022 = 40,
         end_2021 = start_2021 - total_2021 / 18000, # Пока просто запустите, позже станет понятнее, зачем оно надо
         end_2022 = start_2022 + total_2022 / 18000
  )
```

Посмотрим на табличку:


```r
data %>% head()
```

```
##   X              country total_2021 total_2022       country_ru rank_2021
## 1 1              Estonia   61342.85   54855.41          Эстония         1
## 2 2            Singapore   74495.00   67059.22         Сингапур         2
## 3 3              Finland   84349.03   74250.44        Финляндия         3
## 4 4            Lithuania   87626.70   78205.74            Литва         4
## 5 5   Korea, Republic of  121837.44   85724.99 Республика Корея         5
## 6 6 United Arab Emirates  133031.57   94845.46              ОАЭ         6
##   rank_2022 start_2021 start_2022  end_2021 end_2022
## 1         1        -40         40 -43.40794 43.04752
## 2         2        -40         40 -44.13861 43.72551
## 3         3        -40         40 -44.68606 44.12502
## 4         4        -40         40 -44.86815 44.34476
## 5         5        -40         40 -46.76875 44.76250
## 6         6        -40         40 -47.39064 45.26919
```

`ВАЖНО!` Дальше будет большая сложная последовательность наложения слоёв. Чтобы не запутаться, запускайте их последовательно, по одной. Например, сначала я рисую соединительные линии на разных концах графика.


```r
data %>%
  ggplot() + 
  geom_sigmoid(aes(x = -25, xend = 25, # горизонтальные координаты конца и начала можно задавать вручную. Я выбрал удобный вариант -25,25
        y = rank_2021, yend = rank_2022, # Вертикальное положение концов сигмоида - ранг, который мы создавали дополнительно в табличке
        group = country_ru, 
        color = rank_2022,
        color = after_scale(colorspace::lighten(color, .4))), 
    alpha = .45, smooth = 8, size = 1.2)
```

<img src="/gg/Tutorial_4/Tut_4_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Добавляйте слои по одному, смотрите, что получается, играйте с параметрами. Итоговый график:


```r
data %>%
  ggplot() + 
  geom_sigmoid( # Сигмоид
    aes(x = -25, xend = 25, # горизонтальные координаты конца и начала можно задавать вручную. Я выбрал удобный вариант -25,25
        y = rank_2021, yend = rank_2022, # Вертикальное положение концов сигмоида - ранг, который мы создавали дополнительно в табличке
        group = country_ru, 
        color = rank_2022,
        color = after_scale(colorspace::lighten(color, .4))), 
    alpha = .45, smooth = 8, size = 1.2
  ) +
  geom_point( # Точки-палочки слева. Так красивее
    aes(x = -25, y = rank_2021,
        color = rank_2021,
        color = after_scale(colorspace::desaturate(colorspace::lighten(color, .2), .2))), 
    size = 4,
    shape = "|"
  ) + 
  geom_point( #Точки-палочки справа
    aes(x = 25, y = rank_2022,
        color = rank_2022,
        color = after_scale(colorspace::desaturate(colorspace::lighten(color, .2), .2))),
    size = 4,
    shape = "|"
  ) +
  geom_segment( # Линии
    ## make bars a bit shorter because of rounded lineends
    aes(x = start_2021 - 2.5, xend = end_2021 + 1.1, 
        y = rank_2021, yend = rank_2021, 
        color = rank_2021,
        color = after_scale(colorspace::lighten(color, .2))), 
    size = 4, 
    lineend = "round"
  ) + 
  geom_segment( # Линии
    aes(x = start_2022 + 3, xend = end_2022 - 1.1, 
        y = rank_2022, yend = rank_2022, 
        color = rank_2022,
        color = after_scale(colorspace::lighten(color, .2))), 
    size = 4, 
    lineend = "round"
  ) +
  ## label costs
  geom_richtext( # подпись результатов слева
    aes(x = -25, y = rank_2021, 
        label = (total_2021 / 1000) %>% round(0)), # цифры я округлил
    hjust = 1, 
    color = "grey50",
    size = 3.5,
    family = "HSE Sans",
    fill = NA, 
    label.color = NA
  ) +
  geom_richtext( # Подпись результатов справа
    aes(x = 41, y = rank_2022, 
        label = (total_2022 / 1000) %>% round(0)), 
    hjust = 1, 
    color = "grey50",
    size = 3.5,
    family = "HSE Sans",
    fill = NA, 
    label.color = NA
  ) +
  geom_richtext( # подпись страны слева
    aes(x = end_2021, y = rank_2021,
        label = country_ru),
    hjust = 1,
    color = "grey35",
    size = 3.5,
    family = "HSE Sans",
    nudge_x = -2,
    fill = NA, 
    label.color = NA
  ) +
  geom_text( # Подпись страны справа
    aes(x = end_2022 + 2, y = rank_2022, 
        label = country_ru), 
    hjust = 0, 
    color = "grey35",
    size = 3.5,
    family = "HSE Sans",
    nudge_x = 2.5
  ) +
  xlim(-100,90) +
  coord_cartesian(clip = "off") + # Всегда используйте
  scico::scale_color_scico(palette = "lapaz", direction = -1) + # одна из возможных палитр, советую библиотеку scico
  labs(title = 'Россия: экспорт услуг по транспортировке грузов',
       subtitle = '2021 - 2022',
       caption = 'млн долл.') + # подписи осей
  theme_void() + # тема без осей и пр.
  theme(legend.position = "none", # легенда тут не нужна
          plot.margin = margin(25, 35, 15, 35), # рамка вокруг графика
          plot.background = element_rect(fill = "white", color = 'white'), # белый фон
          plot.title = element_markdown(color = "grey35", size = 14, hjust = 0.5, # Заголовок - markdown. Можете изменить цвета внутри заголовка, если хотите
                                        family = "HSE Sans", face = "bold", # тип текста заголовка - жирный
                                        lineheight = 1.2),
          plot.subtitle = element_markdown(color = "grey50", size = 12, hjust = 0.525, # подзаголовок
                                           lineheight = 1.2, family = 'HSE Sans'),
          plot.caption = element_text(color = "grey35", size = 10, family = 'HSE Sans', face = 'italic')) # тип текста подписи внизу - наклонный
```

<center>

![plot](https://raw.githubusercontent.com/ETymch/Econometrics_2023/65fc1f0fd01bde52cfc8d264321c830a042d4114/Pics/transit_by_country.svg)

</center>

В общем, разновидностей графиков было-стало полным-полно, я показал лишь пару частных случаев. Стройте графики исходя из основной мысли, которую вы хотите донести до читателя, придумывайте свои графики на основе созданного другими. У **R** и `ggplot2` замечательное сообщество, которое постоянно придумывает что-то новое.
