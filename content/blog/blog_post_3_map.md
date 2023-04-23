---
title: "Карты России в R"
author: "Е. Тымченко"
date: 2023-04-23
categories: ["R"]
tags: ["gamt", 'ggplot']
---

В последние годы мы видим всё меньше карт России в официальных аналитических отчётах. Связано это в том числе с тем, что институты, которые делают качественные карты и имеют удобный API не меняли карту России с 2014 г. В 2022 году в соства России вступили ещё 4 региона и на доступных картах России их нет. В этой статье я предлагаю простое решение этой проблемы.

> В этой статье я не пытаюсь никого оскорбить. Эта статья про работу с данными и красивые картинки в форме карт.

<center>

![Картинка!](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/map.gif?raw=true)

</center>

Для работты нам понадобятся библиотеки:


```r
library(GADMTools) # Работа с картами GADM
library(terra) # Библиотека для обработки карт
library(tidyverse) 
library(geodata) # Выгрузка карт GADM
library(tidyterra) # Для рисования объектов SpatVector в ggplot
library(readxl) # загрузка excel
library(rcartocolor) # Цвета для карт
library(sysfonts) # биллиотека для работы с шрифтами
library(showtext) # для компиляции шрифтов в ggplot
showtext_auto() # использовать showtext для рендера текста
font_add_google('Rampart One') # Красивый шрифт для графика
```
Библиотеки, связанные с картами, обновляются чаще прочих. Чтобы всё работало, используйте `update.packages()`.

Выгрузим карту России с GADM. Аргумент `level = 1` означает, что мы хотим карту на уровне регионов.


```r
rus <- gadm(country = 'RUS', level = 1, path=tempdir()) %>%
  project('+proj=lcc +lat_1=0 +lat_2=1 +lon_0=15 +datum=WGS84')
```

Мы уже можем построить карту России, но опубликовать её не получится.


```r
plot(rus)
```

<img src="/blog/blog_post_3_map_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Загрузим карту Украины.


```r
ukr <- gadm(country = 'UKR', level = 1, path = tempdir()) %>%
  project('+proj=lcc +lat_1=0 +lat_2=1 +lon_0=15 +datum=WGS84')
russia_with_crimea <- rbind(rus, ukr[which(ukr$NAME_1 == 'Crimea')])
plot(russia_with_crimea)
```

<img src="/blog/blog_post_3_map_files/figure-html/unnamed-chunk-4-1.png" width="672" />
Теперь у нас есть карта России после 2014 года. точно так же мы поступим с 4-мя регионами, вошедшими в состав России в 2022 году.


```r
russia_with_crimea <- rbind(russia_with_crimea, ukr[c(7, 10, 16, 27)])
```

Мы сделали карту России в *примерно* текущих границах. По крайней мере, такую карту мы сможем опубликовать.

На следующем этапе встаёт вопрос о том, насколько такую карту удобно использовать вместе с Российской статистикой. Дело в том, что для построения карт на основе данных Российской статистики нужно объединить объект `SpatVector` и `data.frame` с данными, например, Росстата. Мы можем сделать это при сопоставлении названий регионов в двух объектах. Проблема в том, что названы они по-разному. Я проделал небольшую работу и *переназвал* регионы в новой карте так, чтобы они соответствовали официальным названиям, которые мы наблюдаем в статистике. Новую карту вы можете [скачать из моего `GitHub` хранилища]('https://github.com/ETymch/Econometrics_2023/blob/main/Datasets/New_Map.zip?raw=true').


```r
map <- vect('rwc.shp')
```

## Графики в ggplot

Для примера совмещения карт c данными Росстата мы возьмём статистику ЕМИСС об [Индексе Потребительских цен.](https://www.fedstat.ru/indicator/31074)


```r
data <- rio::import('https://github.com/ETymch/Econometrics_2023/blob/main/Datasets/data_reg_cpi.xls?raw=true') %>% 
  dplyr::glimpse() %>%
  na.omit()
```

```
## New names:
## • `` -> `...2`
## • `` -> `...3`
## • `` -> `...4`
## • `` -> `...5`
```

```
## Rows: 99
## Columns: 5
## $ `Индексы потребительских цен на товары и услуги (процент)` <chr> NA, NA, NA,…
## $ ...2                                                       <chr> NA, NA, NA,…
## $ ...3                                                       <chr> "2023", "К …
## $ ...4                                                       <chr> "2023", "К …
## $ ...5                                                       <chr> "2023", "К …
```

```r
colnames(data)[c(1,5)] <- c('NL_NAME_1', 'ИПЦ')
data <- data %>% select(NL_NAME_1, ИПЦ) %>%
  mutate(ИПЦ = as.numeric(ИПЦ) - 100)
```

Поскольку мы проделали работу и теперь данные Росстата полностью совместимы с нашей картой, это действие занимает одну строчку кода:


```r
map <- map %>% merge(data)
```

Наконец, можно нарисовать карту:


```r
ggplot() +
  geom_spatvector(data = map, aes(fill = ИПЦ, color = ИПЦ)) + 
  theme_classic() +
  rcartocolor::scale_color_carto_c(palette = 'TealRose') +
  rcartocolor::scale_fill_carto_c(palette = 'TealRose') +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(family = 'Rampart One', size = 20, hjust = 0.5, color = '#d0587e'),
        plot.subtitle = element_text(family = 'Rampart One', size = 15, hjust = 0.5, color = '#d0587e'),
        legend.text = element_text(family = 'Rampart One', size = 13, color = '#d0587e')
  ) +
  labs(title = 'Рост цен в регионах России',
       subtitle = 'март 2022 - март 2023')
```

<img src="/blog/blog_post_3_map_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Ура! Теперь рисовать статистические карты в **R** - проще простого. Надеюсь, вам помог этот текст.
