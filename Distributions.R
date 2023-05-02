library(tidyverse)
library(hrbrthemes)
library(sysfonts)
library(showtext)
showtext.auto()

font_add_google('Josefin Sans')

# Prob Distributions:

# Uniform

runif(0, 10, n = 10000) %>%
  density() %>% 
  plot(xlim = c(0, 10), main = 'Uniform Distribution')
 
dunif(min = 0, max = 1, 0.1) # density
punif(min = 0, max = 1, 0.3) # Cumulative Distribution Function Prob(x <= X_i)
qunif(min = 0, max = 1, 0.6) # quantile function


dunif(min = 0, max = 10) 


# Let's generate some random numbers!

trials <- sample(1:10, replace = T, 1000)

table(trials) # count

# Normal Distribution

# Generate normally distributed data
normally_distributed <- rnorm(10000,     
                              mean = 0,    
                              sd = 1)

library(ggplot2)

# Plot the density curve with the cutoff areas


data <- rnorm(mean = 0, sd = 1, 1000) %>%
  density() %>%
  with(data.frame(x, y))


# Get prob of observing a value less than -1
prob_under_minus1 <- pnorm(q = -1,        
                           mean = 0,
                           sd = 1)

# Get prob of observing a value over 1
prob_over_1 <-  1-pnorm(q = 1,            
                        mean = 0,
                        sd = 1)

ggplot(data, aes(x = x, y = y)) +   # Create the plot
  geom_line(alpha = 0.2) +
  geom_ribbon(data = subset(data,x < -1),
              aes(ymax=y, ymin=0),
              fill="orange", 
              alpha=0.3) +
  geom_ribbon(data = subset(data, x > 1),
              aes(ymax = y, ymin = 0),
              fill="orange", 
              alpha=0.3) +
  geom_ribbon(data = subset(data, x > -1 & x < 1),
              aes(ymax = y, ymin = 0),
              fill = "lightgreen", 
              alpha = 0.3) +
  annotate('text', x = -1.6,y = 0.03, label = round(prob_under_minus1, 2) ,size = 4) + 
  annotate('text', x = 1.6,y = 0.03, label = round(prob_under_minus1, 2),size = 4) +
  annotate('text', x = 0,y = 0.1, label = round(1-(prob_under_minus1*2),2),size = 5) +
  xlim(-4, 4) +
  theme_minimal(base_family = 'Josefin Sans')
#
#########################
# Fitting Distributions #
#########################

data <- read.csv('201709-CAH_PulseOfTheNation.csv')

income <- data$Income %>%
  na.omit() %>%
  as.numeric()

income %>%
  density() %>%
  plot(main = 'Density of Income Distribution')

# Красота самой идеи о распределении. Краткая математическая формула позволяет понять, как устроены большие-большие данные.
# 

# Кейс - Data imputation.

# Зачем фиттить распределения?
# Чтобы сэмплить из них и проводить экперименты.

income %>% sample(replace = T, 200000) %>%
  density(bw = 15000) %>%
  lines()


rlnorm(n = 15000, meanlog = 11.25, sdlog = .9) %>%
  density %>%
  plot(col = 'red')

# Хмм, наше распределение похоже на логнормальное!

# Важно!
# Зачем мне вообще фиттить распределение к данным?
# * Наши данные - это просто выборка из генеральной совокупности. Они не дают представление о случайной величине, как таковой. Пример с весом.
# * Использование реальных данные в стохастической модели приводит к оверфиттингу. Чё?
# * 

library(fitdistrplus)
library(scales)

income %>% 
  data.frame %>%
  ggplot() +
    geom_histogram(mapping = aes(x = income), bins = 15,
                   col = 'skyblue',
                   fill = 'green', alpha = 0.3) +
    scale_x_continuous(breaks = breaks_width(50000)) +
    theme_minimal()


# О чём спросить себя?

# * Это распределение симметрично?
# * Если оно ассиметрично, то в какую сторону смещён центр?
# * Один пик или несколько? и т.д.

str(income)
fitdistrplus::plotdist(income, histo = T, demp = T)

# Cullen and Frey graph
# Этот график помогает оценить, насколько данные ассиметричны и то, насколько острый у них пик.

descdist(income, discrete = F, boot = 500) # Распределение доходов похоже скорее на Гамма-распределение, чем на Логнормальное.

# Чтобы понять, какое распределение лучше описывает даныне, нужно:
# Оценить параметры этих распределений и посмотреть, какое теоретическое распределение лучше описывает эмпирику.
# Для этого у нас есть инструменты:
# Q-Q plot. - сравнение квантилей теоретического и эмпирического распределений.
# P-P plot - сравнение PMF.
# Goodness of fit measures.
?fitdist
fit_weibull <- fitdist(income, 'weibull') # MLE! Кстати, тут можно дать вводные про разные методы оценки с математикой
fit_weibull %>% summary()
str(fit_weibull)
fit_weibull$estimate[1:2]

fit_gamma <- fitdist(income, 'gamma', method = 'mme') # MLE! Кстати, тут можно дать вводные про разные методы оценки с математикой
fit_gamma %>% summary()

fit_lnorm <- fitdist(income, 'lnorm')
fit_lnorm %>% summary

# Какое распределение лучше описывает данные?

dists <- list(fit_weibull, fit_gamma, fit_lnorm)
plot.legend <- c('Weibull', 'Gamma', 'LogNormal')

denscomp(dists, legendtext = plot.legend)
cdfcomp(dists, legendtext = plot.legend)
qqcomp(dists, legendtext = plot.legend)
ppcomp(dists, legendtext = plot.legend)

gofstat(dists, fitnames = plot.legend)

# Advanced stuff

bootdist(fit_weibull) %>%
  summary(niter = 500)

# Functional Programming

df <- tibble::tibble(a = rnorm(1,2, n = 100),
               b = rnorm(2,4, n = 100),
               c = rnorm(10, 20, n =  100),
               d = rnorm(-30,1, n = 100))

nor_l <- function(x){
  (x - mean(x)) / sd(x)
}

df$d %>% nor_l %>% density %>% plot

for (i in colnames(df)){
  df[[i]] <- df[[i]] %>% nor_l
}

f <- function(x){
  x + 12
}

g <- function(x){
  x - 100
}

1 %>% f() %>% g()

h <- compose(f,g)

h(1)

# Use map function instead of loops!

map_df(df, nor_l)

values <- 1:5
map(values, function(x) x * 2) # returns a list
map(values, \(x) x *2) %>% unlist()
map(values, ~ .x * 2)

map_dbl(values, ~ .x * 2) # return a double vector
