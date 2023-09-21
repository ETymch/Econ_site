---
title: '2. Бублики и композиция'
author: "Е. Тымченко"
date: 2023-09-21
categories: ["R"]
tags: ["ggplot2"]
---

## Идея

Сегодня я хочу показать:

* Как работать с необычными геометриями,
* Текстовыми аннотациями,
* Как удобно и просто комбинировать графики, чтобы графики вместе рассказывали историю.

Для этого мы будем использовать данные платёжного баланса об экспорте Россией услуг по транспортировке грузов различными путями. Мы хотим рассказать, что произошло с экспортом этих услуг в **2022** году. А именно:

* Рассказать о том, как исторически развивался экспорт Россией этих услуг.
* Показать, насколько он снизился. Сделать на этом смысловой акцент на графике.
* Показать какие виды услуг по транспортировке грузов снизились больше прочих.

Все эти цели будут определять то, какие графики мы будем строить и как именно они будут скомбинированы в итоговом графике. Таким образом, чтобы показать изменение в 2022 г. нужно отдельно выделить 2021 (мирный) год и 2022 год, сделать акцент на разнице между ними. Также я считаю, что необходим ретроспективный график с большим горизонтом, чтобы читатель графика понимал, как примерно всё развивалось до 2021 г.

Мы наметили план той истории, которую хотим сообщить. Теперь приступим к технической части!

## Подготовка рабочего пространства

Библиотеки


```r
library(tidyverse) # Обработка данных
library(readxl) # Работа с excel файлами в R
library(showtext) # Рендер текста в ggplot2
library(sysfonts) # загрузка шрифтов в R
library(httr) # Скачать файлы из сети
library(patchwork) # комбинирование графиков

showtext_auto() # рендер текста в ggplot при помощи showtext 
```

Давайте на этот раз загрузим все стили шрифта HSE Sans. Сначала я скачиваю шрифт с github при помощи библиотеки `hhtr`, затем устанавливаю через `sysfonts`.


```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Regular.otf', write_disk('HSESans-Regular.otf', overwrite = T))
```

```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Bold.otf', write_disk('HSESans-Bold.otf', overwrite = T))
```

```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Italic.otf', write_disk('HSESans-Italic.otf', overwrite = T))
```

```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-SemiBold.otf', write_disk('HSESans-SemiBlod.otf', overwrite = T))
```

```r
font_add(family = 'HSE Sans',
         regular = "HSESans-Regular.otf",
         bold = 'HSESans-Bold.otf',
         italic = 'HSESans-Italic.otf',
         bolditalic = 'HSESans-SemiBlod.otf'
        ) # Установка HSE Sans, вместо bolditalic я загружаю полужирный.
```

Загрузим сырые данные Банка России из платёжного баланса за 2021 и 2022 гг. Выберем то, что нам нужно.

## Выгрузка данных


```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/difference.xls', write_disk('difference.xls', overwrite = T))
```

```
## Response [https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/difference.xls]
##   Date: 2023-09-21 12:03
##   Status: 200
##   Content-Type: application/octet-stream
##   Size: 58.9 kB
## <ON DISK>  C:\Users\evgen\econometrics_2023\Econ_site\content\gg\Tutorial_2\difference.xls
```

```r
data <- read_excel('difference.xls', skip = 4)[c(29,41,68,80,86),2:3] %>%
  mutate(cat = c('Морской', 'Воздушный', 'ЖД', 'Авто', "Трубопроводный"),
         Diff = `2022` - `2021`)
data
```

```
## # A tibble: 5 × 4
##   `2021` `2022` cat                Diff
##    <dbl>  <dbl> <chr>             <dbl>
## 1  1595.  1436. Морской         -159.  
## 2  3099.   919. Воздушный      -2180.  
## 3  1232.   951. ЖД              -282.  
## 4  1154.  1162. Авто               7.58
## 5  1923.  1810. Трубопроводный  -114.
```

Посмотрите на табличку data. Часто мы визуализируем это столбиками. Я покажу, как это можно сделать в следующих постах. Сегодня давайте воспользуемся продвинутыми возможностями визуализации в **R**, сделаем графики-бублики!

Для третьего графика нам понадобятся данные о совокупном экспорте услуг по перевозке грузов в млрд долл.


```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/totals.xlsx', write_disk('totals.xlsx', overwrite = T))
```

```
## Response [https://raw.githubusercontent.com/ETymch/Econometrics_2023/main/Datasets/totals.xlsx]
##   Date: 2023-09-21 12:03
##   Status: 200
##   Content-Type: application/octet-stream
##   Size: 11.2 kB
## <ON DISK>  C:\Users\evgen\econometrics_2023\Econ_site\content\gg\Tutorial_2\totals.xlsx
```

```r
totals <- read_excel('totals.xlsx') %>%
  mutate(date = seq(as.Date('2003-01-01'), length.out = length(Год), by = 'year'),
         Сумма = Сумма /10^6
  )
```


## Подготовка данных

Подготовим данные для графиков бубликов.


```r
data_percents_21 <- 
  data %>%
  mutate(percent = (`2021` / 1000) %>% round(1)) # в млрд долл

data_percents_22 <- 
  data %>%
  mutate(percent = (`2022` /1000) %>% round(1)) # в млрд долл

# Функция для подготовки данных

make_donut_coords <- function(df, cat, percent){
  df %>%
    arrange(desc(cat)) %>%
    mutate(ypos = cumsum(percent) - 0.5*percent ) %>% # координаты для подписей
    mutate(ymax = cumsum(percent)) %>%
    mutate(ymin = c(0, head(ymax, n=-1))
    ) %>%
    mutate(label_pos = (ymax + ymin) / 2) %>%
    mutate(percent_label = percent) # формат подписей
}

# Применяем нашу функцию

data_1 <- make_donut_coords(data_percents_21, cat, `2021`)
data_2 <- make_donut_coords(data_percents_22, cat, `2022`)
```

Подготовим данные для линейного графика. Я хочу дополнительно выделить цветом изменение объёма услуг в 2021-2022 гг. Для этого я разбиваю один ряд на два, чтобы позже выделить из разными цветами в ggplot.


```r
totals_plt <- 
  totals %>%
  mutate(series_1 = c(Сумма[1:nrow(totals)-1], NA), # первая часть, будет жёлтенькой
         series_2 = c(rep(NA, nrow(totals) - 2), totals$Сумма[(nrow(totals)-1):nrow(totals)]) # Последние наблюдения - красные. Они же вынесены в бублики.
         ) %>%
  select(date, series_1, series_2) %>% # выбираю нужное
  pivot_longer(series_1:series_2) # делаю длинную табличку, чтобы кормить её ggplot.
```

## Рисование

Внешние параметры графиков лучше выносить вовне. Так будет проще что-то менять в будущем.


```r
size_header = 8 # размер заголовка
size_text = 3 # размер названий и подписей
size_center = 8 # размер текста в центре
label_position = 2.1

colors <- c('#CEB09F','#9B1631','#71797E','#0A99C3', '#040273') # цвета по порядку: Дорожная пыль для автомобильного транспорта, тёмно-красный - авиа, стальной - ЖД, морская волна - морской транспорт, цвет морских глубин - трубопроводный транспорт.
colors_text <- c('#040273', '#0A99C3', '#71797E','#9B1631', '#CEB09F')
```

Графики-бублики:


```r
plot_1 <- 
  data_1 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = cat)) + # Для geom_rect нужны эти aes. Попробуйте поменять xmin и xmax - Посмотрите, что получится!
  geom_rect(alpha = 0.8) + # Основная геометрия - столбики. Но эти столбики можно сделать более интересными!
  geom_text(x=label_position, aes(y=label_pos, label=percent_label), size=size_text, family = 'HSE Sans', color = colors_text) + # Подписи
  coord_polar(theta = 'y') + # Сделаем график-пирог!
  xlim(c(-1, 4)) + # Вырежем центр пирога - получится график бублик. Зачем вырезать центр? В центре мы хотим что-то написать!
  annotate('text', x = -1, y = 1, label = '2021', family = 'HSE Sans', fontface = 'bold', size = size_center, alpha = 0.4) + # Подпись в центре
  #annotate('text', x = 0.2, y = 3 * 3 / 2 , label = '(млрд $)', family = 'HSE Sans', fontface = 'bold', size = size_center / 3, alpha = 0.7) + # Изначально я хотел сделать подпись с единицами измерения в центре, но перепробовав несколько вариантов решил, что без этого можно обойтись.
  scale_fill_manual(values = c(colors)) +
  theme_void(base_family = 'HSE Sans') +
  #theme(legend.position = 'bottom') +
  xlim(c(-1, 4))

plot_2 <- 
  data_2 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = cat)) +
  geom_rect(alpha = 0.8) +
  geom_text(x = label_position, aes(y = label_pos, label = percent_label), size = size_text, family = 'HSE Sans', color = colors_text) +
  coord_polar(theta = 'y') +
  annotate('text', x = -1, y = 1, label = '2022', family = 'HSE Sans', fontface = 'bold', size = size_center, alpha = 0.4) +
  #annotate('text', x = 0.2, y = 3.14 , label = '(млрд $)', family = 'HSE Sans', fontface = 'bold', size = size_center / 3, alpha = 0.7) +
  scale_fill_manual(values = c(colors)) +
  theme_void(base_family = 'HSE Sans') +
  #theme(legend.position = 'bottom') +
  xlim(c(-1, 4))
```

Собираем один график из нескольких при помощи библиотеки `patchwork`.


```r
plot_1 + plot_2 + # комбинация графиков 1 и 2
  plot_annotation(caption = 'млрд долл.') + # подпись с единицами измерений
  plot_layout(guides = 'collect') & # сделать единую легенду
  theme(legend.position = 'bottom', # легенду - вниз
        legend.title = element_blank(), # название легенды нам не нужно
        legend.text = element_text(size = 8)) # Размер текста легенды
```

<center>

![1+1](https://raw.githubusercontent.com/ETymch/Econometrics_2023/a610fe2d0751b143a302658b18ac4d3f08f44d3b/Pics/export_21_22.svg)
</center>

Добавим сверху историю.


```r
colors_plt_3 <- c("#FFD54E", "#A80002")

plot_3 <- 
  totals_plt %>%
  ggplot(aes(x = date, y = value, color = name)) + # горизонтальная ось - даты, вертикальная значение (экспорт транспортных услуг), цвет - ряд.
  geom_line(show.legend = F, alpha = 0.9) + # линия. Иногда полезно сразу написать, чтобы легенда не отображалась.
  geom_point(alpha = 0.4, show.legend = F) + # если комбинировать линии с точками - будет красивее.
  scale_color_manual(values = colors_plt_3) + # красивые цвета, чтобы выделить нужное по смыслу.
  theme_minimal(base_family = 'HSE Sans') + # Тема и шрифт
  theme(axis.title.y = element_text(size = 8), # Уменьшил размер шрифта названия оси
        plot.title = element_text(hjust = 0.5, face = 'bold'), # Название графика - по середине, жирный шрифт
        panel.grid.major.x = element_blank(), # убираем ненужную разметку
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank()) + # Убираем название оси х.
  labs(y = 'Млрд долл.', # Текст на графике, ось x
       title = 'Российский экспорт услуг по \n транспортировке грузов') # \n - для переноса на следующую строку. Бывает очень полезно.
```


```r
plot_3 / (plot_1 + plot_2) +
  plot_annotation(caption = 'млрд долл.') + # комбинируем графики
  plot_layout(guides = 'collect', heights = c(2,5)) & # выкладка, общая легенда, соотношение сторон.
  theme(legend.position = 'bottom', # легенда - внизу
        legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        legend.key = element_rect(size = 1), # рамка у цветных квадратиков. Так красивее, не спорьте.
        legend.key.size = unit(0.4, 'cm'), # размер цветных квадратиков
        plot.caption = element_text(family = 'HSE Sans', face = 'italic') # Подпись внизу - курсивом
  )
```

<center>

![final](https://raw.githubusercontent.com/ETymch/Econometrics_2023/a610fe2d0751b143a302658b18ac4d3f08f44d3b/Pics/export_trans.svg)

</center>

Сохраним график в хорошем разрешении.


```r
ggsave('export_trans.svg')
```

