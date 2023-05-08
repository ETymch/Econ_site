---
title: "Карты для проекта Zabastcom"
author: "Е. Тымченко"
date: 2023-05-08
categories: ["R"]
tags: ["Статистика", 'ggplot2', 'карты']
---

Не так давно наткнулся на любопытную базу данных, которую собирает проект [Забастком](https://www.zabastcom.org/). Поскольку я немного увлекаюсь визуализацией в `ggplot2`, решил что-нибудь нарисовать. Тем более, что у преекта есть удобный API и не нужно долго возиться с выгрузкой. Вот что получилось.

<center>

![Забастком, 2023](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/zabastcom_chart_1.png?raw=true)

</center>

## Код

Библиотеки:


```r
library(httr)
library(jsonlite)
library(tidyverse)
library(terra)
library(tidyterra)
library(hrbrthemes)
library(showtext)
library(ggtext)
library(sysfonts)
showtext.auto()
font_add_google('Yeseva One')
```

Выгрузка данных через API:


```r
retrieve_from_zabastkom <- function(lists){
call <- list()
for (i in 1:lists){
  call[[i]] <- GET(paste0('https://zabastcom.org/api/v2/all/events?page=', i,'&perPage=50'))
  call[[i]] <- 
  call[[i]][['content']] %>%
    rawToChar() %>%
    fromJSON() %>%
    data.frame()
}
 bind_rows(call)
}

data <- retrieve_from_zabastkom(125)
data$data.date <- as.POSIXct(data$data.date, origin="1970-01-01") %>%
  as.Date()
```

Теперь мы загружаем актуальную курту России. [Как сделать её самому я писал здесь](https://econisfun.netlify.app/blog/blog_post_3_map/)


```r
data_1 <- data %>%
  filter(data.date >= '2023-01-01') # Далее будем работать только с данными за текущий год.

# Отобразим координаты на те, которые мы использовали при создании карты.
df <- data.frame(lon = data_1$data.longitude,
                 lat = data_1$data.latitude) %>%
  vect(crs = 'EPSG:4326') %>%
  terra::project('+proj=lcc +lat_1=0 +lat_2=1 +lon_0=15 +datum=WGS84')
```

Первый график, где точками описаны все события трудовых конфликтов за 2023 г.


```r
ggplot() +
  geom_spatvector(data = map, color = 'grey40', fill = '#FFEEE1') +
  geom_spatvector(data = df, color = '#F96262', shape = 21, fill = 'white', size = 2) +
  geom_spatvector(data = df, shape = 21, fill = '#F96262', alpha = 0.2, size = 2) +
  theme_void(base_family = 'Yeseva One') +
  theme(plot.background = element_rect(colour = 'grey40', fill = '#FFEEE1')) +
  theme(plot.background = element_rect(fill = '#FFEEE1', color = 'grey40', linewidth = 1.5),
        legend.position = 'bottom',
        plot.margin = margin(20, 50, 20, 50),
        plot.title = element_markdown(hjust = 0.5, size = 28),
        plot.caption = element_markdown(hjus = 0.5, size = 7,
                                        margin = margin(t = 15, b = 10))
  ) +
  labs(title = 'География <b style="color:#672044">трудовых конфликтов</b> в России',
       caption = 'Данные: <b style="color:#672044">zabastcom.org</b>')
```


<center>

![Первый рисунок](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/zabastcom_chart_base_1.png?raw=true)

</center>

Меня этот результат не удовлетворил, потому что удобнее было бы смотреть на ситуацию в региональном разрезе. К сожалению, в исходных данных не было информации о регионах, в которых события происходили. Поэтому я написал небольшой код, который сопоставляет локации с регионами. Проще было сформировать другой запрос через API, но лёгких путей я не искал.


```r
region_j <- c()
region <- c()
for (j in 1:length(df)){
for (i in 1:length(map)){
a <- df[j] %>% ext %>%
  as.vector()

a_x <- a[1]
a_y <- a[3]

b <- map[i] %>% ext() %>%
  as.vector()

region[i] <- ifelse(a_x >= b[1] & a_x <= b[2] & a_y >= b[3] & a_y <= b[4], T, F)
}
region_j[j] <- ifelse(sum(region) == 1, map[which(region == T)]$NL_NAME_1, F)
region_j[j] <- ifelse(sum(region) > 1, sample(map[which(region == T)]$NL_NAME_1, 1), region_j[j])
region <- c()
}

df$region <- region_j

#Объединяем всё это с картой!

df_1 <- df %>%
  group_by(region) %>%
  summarize(cases = sum(region ==  region)) %>%
  rename(NL_NAME_1 = region)

map_1 <- merge(map, df_1, all.x = T)
```

Код для финального рисунка рисунок:


```r
ggplot() +
  geom_spatvector(data = map_1, aes(color = cases, fill = cases)) +
  rcartocolor::scale_color_carto_c(palette = 'Burg', name = 'Случаев в 2023 г.', na.value = '#FFBBBB') +
  rcartocolor::scale_fill_carto_c(palette = 'Burg', guide = FALSE, na.value = 0) +
  theme_void(base_family = 'Yeseva One') +
  guides(color = guide_colorbar(title.position = 'bottom', title.hjust = 0.5, barwidth = 9, barheight = 0.5)) +
  theme(plot.background = element_rect(fill = '#FFEEE1', color = 'grey40', linewidth = 1.5),
        legend.position = 'bottom',
        plot.margin = margin(20, 50, 20, 50),
        plot.title = element_markdown(hjust = 0.5, size = 28),
        plot.caption = element_markdown(hjus = 0.5, size = 7,
                                        margin = margin(t = 15, b = 10))
        ) +
  labs(title = 'География <b style="color:#672044">трудовых конфликтов</b> в России',
       caption = 'Данные: <b style="color:#672044">zabastcom.org</b>')
```

<center>

![Фин](https://github.com/ETymch/Econometrics_2023/blob/main/Pics/zabastcom_chart_1.png?raw=true)

</center>

Надеюсь, этот пост кому-нибудь поможет и в проектах по анализу данных станет `больше красивых визуализаций с картами`!
