---
title: '5. Продвинутые диаграммы рассеяния'
author: "Е. Тымченко"
date: 2024-01-09
categories: ["R"]
tags: ["ggplot2"]
---

В этой заметке мы выучим новые геометрии, научимся их комбинировать, попробуем разнообразить скучнейший тип графика - `scatterplot`. Нам понадобятся библиотеки:


```r
library(tidyverse)
library(httr)
library(readxl)
library(ggExtra) # Красивые типы  графиков
library(showtext) # Рендер шрифтов в ggplot2
library(sysfonts) # загрузка шрифтов в R

showtext_auto()
```

Шрифт, на случай, если его нет под рукой.


```r
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Regular.otf', write_disk('HSESans-Regular.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Bold.otf', write_disk('HSESans-Bold.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-Italic.otf', write_disk('HSESans-Italic.otf', overwrite = T))
GET('https://github.com/ETymch/Econometrics_2023/raw/main/Plotting/HSESans-SemiBold.otf', write_disk('HSESans-SemiBlod.otf', overwrite = T))
```


```r
font_add(family = 'HSE Sans',
         regular = "HSESans-Regular.otf",
         bold = 'HSESans-Bold.otf',
         italic = 'HSESans-Italic.otf',
         bolditalic = 'HSESans-SemiBlod.otf'
)
```

Данные:


```r
GET("https://github.com/ETymch/Econometrics_2023/raw/main/Datasets/data_expenditures.xlsx",
    write_disk('data_expenditures.xlsx'), overwrite = T)
```

```r
data <- read_excel('data_expenditures.xlsx') %>%
  mutate(`Мусульманская страна` = ifelse(`Мусульманская страна` == 0, 'Нет', 'Да')
         )
```

Посмотрим на данные.


```r
data %>% head %>% kableExtra::kable()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Страна </th>
   <th style="text-align:right;"> Траты на еду, % </th>
   <th style="text-align:right;"> Траты на алкоголь, % </th>
   <th style="text-align:left;"> Мусульманская страна </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Algeria </td>
   <td style="text-align:right;"> 37.255870 </td>
   <td style="text-align:right;"> 1.024986 </td>
   <td style="text-align:left;"> Да </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Angola </td>
   <td style="text-align:right;"> 49.737928 </td>
   <td style="text-align:right;"> 1.502935 </td>
   <td style="text-align:left;"> Нет </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Argentina </td>
   <td style="text-align:right;"> 23.198097 </td>
   <td style="text-align:right;"> 1.907243 </td>
   <td style="text-align:left;"> Нет </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Australia </td>
   <td style="text-align:right;"> 9.974969 </td>
   <td style="text-align:right;"> 4.239678 </td>
   <td style="text-align:left;"> Нет </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Austria </td>
   <td style="text-align:right;"> 11.312785 </td>
   <td style="text-align:right;"> 3.608416 </td>
   <td style="text-align:left;"> Нет </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Azerbaijan </td>
   <td style="text-align:right;"> 43.555817 </td>
   <td style="text-align:right;"> 2.024452 </td>
   <td style="text-align:left;"> Да </td>
  </tr>
</tbody>
</table>

Идея графика - проста. Мы хотим посмотреть на разницу в распределении расходов для разных групп стран.

Предустановки для всех графиков:


```r
color_1 <- c('#ff6361', '#ffa600')
alpha_default <- 0.4
density_size= 7
```

Делайте графики с кастомными цветами! Палитры на любой вкус можно сгенерировать или выбрать, например, на [Data color picker](https://www.learnui.design/tools/data-color-picker.html) или [Pigment](https://pigment.shapefactory.co/)

Самый простой график. Такой вы уже видели 10 миллиардов раз. Возможно, он вам даже наскучил.


```r
a <- data %>%
  ggplot(aes(x = `Траты на алкоголь, %`, y = `Траты на еду, %`)) +
  geom_jitter(color = color_1[2], size = 3.0, alpha = alpha_default) +
  theme_minimal(base_family = 'HSE Sans', base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
        )
a
```

<img src="/gg/Tutorial_5/Tut_5_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Добавим функции плотности. Они помогают лучше ориентироваться в том, как распределены данные. `ggMarginal` - полезная функция из библиотеки `ggExtra`, которая автоматически строит функции распределения.


```r
ggMarginal(a, type="density", size = density_size, fill = color_1[2], alpha = alpha_default, color = NA) # Поэкспериментируйте. Вместо type = 'density' выберите type = 'histogram'. Выберите цвет границы color = 'black'.
```

<img src="/gg/Tutorial_5/Tut_5_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Теперь разделим выборку на две группы. В одной будут страны с долей мусульманского населения менее 20%, в другой - более 20%.


```r
b <- data %>%
  ggplot(aes(x = `Траты на алкоголь, %`, y = `Траты на еду, %`, color = `Мусульманская страна`, fill = `Мусульманская страна`)) +
  geom_jitter(size = 3.0, alpha = alpha_default) +
  scale_color_manual(values = color_1) +
  theme_minimal(base_family = 'HSE Sans', base_size = 16) +
  theme(legend.position = 'bottom',
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )

ggMarginal(b, type="density", size = density_size, alpha = alpha_default, groupFill = T, color = NA)
```

<img src="/gg/Tutorial_5/Tut_5_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Мы хотим дополнительно подчеркнуть разницу распределения расходов на алкоголь и еду для разных групп стран. Добавим линии, показывающие средние значения расходов.


```r
c <- 
  b + 
  geom_hline(yintercept = data %>% # Создадим горизонтальные и вертикальные линии, показывающие средние значения трат на еду и на алкоголь
               filter(`Мусульманская страна` == 'Да') %>% # Всё это можно сделать не создавая дополнительных таблиц, а просто добавив слой
               pull(`Траты на еду, %`) %>% mean(), # с другим источником данных, которые вы обработали уже внутри графика
             linetype = 'dashed', # тип линии
             color = color_1[1], # правильный цвет
             alpha = alpha_default + 0.2,
             size = 0.8
             ) +
  geom_hline(yintercept = data %>%
               filter(`Мусульманская страна` == 'Нет') %>%
               pull(`Траты на еду, %`) %>% mean(),
             linetype = 'dashed',
             color = color_1[2],
             alpha = alpha_default + 0.2,
             size = 0.8
  ) +
  geom_vline(xintercept = data %>%
               filter(`Мусульманская страна` == 'Да') %>%
               pull(`Траты на алкоголь, %`) %>% mean(),
             linetype = 'dashed',
             color = color_1[1],
             alpha = alpha_default + 0.2,
             size = 0.8
  ) +
  geom_vline(xintercept = data %>%
               filter(`Мусульманская страна` == 'Нет') %>%
               pull(`Траты на алкоголь, %`) %>% mean(),
             linetype = 'dashed',
             color = color_1[2],
             alpha = alpha_default + 0.2,
             size = 0.8
  )

ggMarginal(c, type="density", size = density_size, alpha = alpha_default, groupFill = T, color = NA)
```

<img src="/gg/Tutorial_5/Tut_5_files/figure-html/unnamed-chunk-10-1.png" width="672" />

На мой взгляд, график кажется перегруженным. Линии занимают слишком много места. Да и кода для такой пустяковой задачи написано многовато. Попробуем другой способ - нарисуем крестики.


```r
size_cross <- 5

# Для этого нам понадобится создать ещё одну табличку

summary_data <- data %>% 
  group_by(`Мусульманская страна`) %>%
  summarize(`Медиана, алк` = median(`Траты на алкоголь, %`),
            `Медиана, еда` = median(`Траты на еду, %`)
  )
  
# Обновлённый график

d <- 
  b +
  geom_errorbar(data = summary_data,# geom_errorbar - уже встроенная в ggplot2 функция. Она очень гибкая и помогает строить крестики разных форм, с разными содержательными смыслами.
    aes(
      x = `Медиана, алк`,# Крестик состоит из двух пересекающихся линий. Первая - вертикальная. Её центр - x
      ymin = `Медиана, еда` - size_cross, # Длина линии
      ymax = `Медиана, еда` + size_cross,
      color = `Мусульманская страна`),
    inherit.aes = F,
    width = .6,# попробуйте изменить этот параметр
    size = .8,# попробуйте изменить этот параметр
    show.legend = F,
    alpha = alpha_default + 0.3
  ) +
  geom_errorbar(data = summary_data,# Горизонтальная часть.
    aes(y = `Медиана, еда`,
      xmin = `Медиана, алк` - size_cross * 0.3,
      xmax = `Медиана, алк` + size_cross * 0.3,
      color = `Мусульманская страна`),
    inherit.aes = F,
    width = 0.8,
    size = 0.8,
    show.legend = F,# попробуйте изменить этот параметр
    alpha = alpha_default + 0.3 # попробуйте изменить этот параметр
  )

# Как лучше сохранить такой график?

ggsave(file = "density_multilevel.png", dpi = 500,
       ggMarginal(d, type="density", size = density_size, alpha = alpha_default, groupFill = T, color = NA),
)
```

<center>

![plot](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/density_multilevel.png?raw=true)

</center>

Улучшать графики можно бесконечно, но на этом, думаю, можно остановиться. Такой график выглядит уже достаточно профессионально и хорошо сообщает читателю графика нашу основную идею. Экспериментируйте с различными комбинациями *слоёв*, **размерами**, `цветом`, осваивайте лучшую библиотеку для визуализации данных, ggplot2!
