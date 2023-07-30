---
title: "Лекция 5"
author: "Е. Тымченко"
date: 2023-07-11
categories: ["R"]
tags: ["Econ"]
---

## Повторение: ММП

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

**Пример**

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