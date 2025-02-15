---
title: '3. Точки и ящики с усами'
author: "Е. Тымченко"
date: 2023-10-19
categories: ["R"]
tags: ["ggplot2"]
---

## Мотивация

GGplot - очень гибкий инструмент визуализации. Он позволяет сочитать рнесколько типов геометрий на отном графике, чего в простых редакторах добиться трудно. Сегодня мы посмотрим, как совместить две геометрии - ящик с усами и точки, а также уделим особое внимение цвету.

## Данные

Для примера мы используем датасет с прогнозами экономического роста в 2023 г. и тем, как эти прогнозы пересматривались с течением времени. Прогнозы экономического роста собраны из пяти изданий [World Economic Outlook](https://www.imf.org/en/Publications/WEO) за 2021-2023 гг.

## Код

Загрузка библиотек и шрифтов:


```r
library(tidyverse) # обработка данных
library(showtext) # Рендер текста в ggplot2
library(sysfonts) # загрузка шрифтов в R
library(httr) # Скачать файлы из сети
library(ggtext) # Работа с текстом на графике

showtext_auto() # рендер текста в ggplot при помощи showtext 

GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Regular.otf', write_disk('HSESans-Regular.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Bold.otf', write_disk('HSESans-Bold.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Italic.otf', write_disk('HSESans-Italic.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-SemiBold.otf', write_disk('HSESans-SemiBlod.otf', overwrite = T))
font_add(family = 'HSE Sans',
         regular = "HSESans-Regular.otf",
         bold = 'HSESans-Bold.otf',
         italic = 'HSESans-Italic.otf',
         bolditalic = 'HSESans-SemiBlod.otf'
) # Установка HSE Sans, вместо bolditalic я загружаю полужирный.

colors_plt <- c("#FFD54E", "#A80002") # Цвета
```

Загрузим данные из нашей базы:


```r
dat <- readxl::read_excel('fcast.xlsx') %>%
  pivot_longer(Oct21:Oct23) %>% # Придаём данным подходящий вид, чтобы можно было скормить их ggplot
  filter(value < 10) %>% # Уберём страны, которые переживали всплески роста или глубокие рецессии.
  filter(value > -5) %>%
  filter(UNFR >= -1) %>% # Уберём пропуски в данных
  mutate(UNFR = as.factor(UNFR)) %>% # Дружественная или недружественная страна - факторная переменная
  mutate(UNFR = ifelse(UNFR == 0, "Дружественные", 'Недружественные')) %>%  # Переименование
  mutate(name = ifelse(name == 'Oct21', 'Окт 21', name)) %>% # Замена английских месяцев на русские
  mutate(name = ifelse(name == 'Apr22', 'Апр 22', name)) %>%
  mutate(name = ifelse(name == 'Oct22', 'Окт 22', name)) %>%
  mutate(name = ifelse(name == 'Apr23', 'Апр 23', name)) %>%
  mutate(name = ifelse(name == 'Oct23', 'Окт 23', name)) %>%
  mutate(name = factor(name, levels = c('Окт 21', 'Апр 22', 'Окт 22', 'Апр 23', 'Окт 23'))) # Правильная последовательность
```


```r
dat %>%
  ggplot(aes(x = name, y = value, fill = UNFR, color = UNFR)) + # по горизонтальной оси - дата, по вертикальной - значение, цвет и заполнение - в зависимости от типа страны
  geom_jitter(alpha = 0.25, size = 1.2) + # Точки - хороший способ (помимо функции плотности) иллюстрировать распределение
  theme_minimal(base_family = 'HSE Sans') + # глобальный шрифт
  geom_boxplot(alpha = 0.05) + # прозрачные ящики с усами
  scale_color_manual(values = colors_plt) + # Цвет
  theme(axis.title.y = element_blank(), # подписи оси y - не нужно
        panel.grid.major.x = element_blank(), # убираем ненужную разметку
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(), # подписи оси x - не нужно
        legend.title = element_blank(), # название легенды - тоже не нужно. Как и сама легенда в нашем случае
        plot.title = element_markdown(family = "HSE Sans", color = "#333333", 
                                      size = 9, face = 'bold', hjust = 0.5
                                      ), # параметры заголовка: шрифт, цвет, размер, тип шрифта - жирный, положение - посередине.
        plot.caption = element_text(face = 'italic'), # сноска с наклогом
        legend.position = 'none') + # нет легенды
  labs(y = '',
       title = 'Пересмотры прогнозов экономического роста на 2023 г. для <b style="color:#FFD54E">дружественных</b> и <b style="color:#A80002">недружественных</b> стран', # Вместо легенды используем цвет в заголовке графика. чтобы это получилось, заголовок должен ыбть объектом element_markdown!
       caption = 'Прогнозы IMF') # подпись
```

<center>

![plot](https://github.com/ETymch/Econometrics_2023/raw/main/Pics/plot_fc.svg)

</center>

Получается хороший, но не до конца аккуратный граффик. Можно заметить, что красные и жёлтые точки на графике перемешаны. В идеале мы, конечно же, хотим, чтобы эти точки были собраны в группы вокруг соответствующих ящиков с усами. Сделать это легко. Нужно всего-навсего заменить `geom_jitter` на `geom_point`. Давайте попробуем!


```r
dat %>%
  ggplot(aes(x = name, y = value, fill = UNFR, color = UNFR)) + 
  geom_point(position=position_jitterdodge(), alpha = 0.25) + # теперь у нас просто точки, с указанной выкладкой
  theme_minimal(base_family = 'HSE Sans') +
  geom_boxplot(alpha = 0.01, fill = 'transparent') # да, в R 'transparent' - это тоже цвет.
  scale_color_manual(values = colors_plt) +
  theme(axis.title.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_markdown(family = "HSE Sans", color = "#333333", 
                                      size = 9, face = 'bold', hjust = 0.5
        ),
        plot.caption = element_text(face = 'italic'),
        legend.position = 'none') +
  labs(y = '',
       title = 'Пересмотры прогнозов экономического роста на 2023 г. для <b style="color:#FFD54E">дружественных</b> и <b style="color:#A80002">недружественных</b> стран',
       caption = 'Прогнозы IMF')
```

<center>

![plot](https://raw.githubusercontent.com/ETymch/Econometrics_2023/6726cfbe06d965f4cdabf06f0df3eb2236266885/Pics/plot_boxes_dots_new.svg)

</center>

На мой взгляд, так получше.

Как мы видим, `ggplot2` позволяет сочетать на графике элементы! А это открывает большой простор для творчества. Придумывайте свои комбинации геометрий, работайте с цветом - создавайте уникальные визуализации.
