library(rmarkdown)
library(knitr)
library(blogdown)
library(distill)

blogdown::new_site(force = T)
blogdown::serve_site()
blogdown::stop_server()


x = seq(-5, 5, by = 0.01)

f <- function(x){
  (3 - 3*x)^2
}
library(tidyverse)
plot(x, f(x))

f(1/3)
argmin(f(x))
arg
optimize(f(x),x)
?optimize
optimize(f, x)
optimize(f(x))
blogdown::config_netlify()

# Monty Hall

library(tidyverse)
library(hrbrthemes)
library(sysfonts)
library(showtext)
library(rcartocolor)
showtext_auto()

font_add_google('Monoton')


MontyHall <- function(SwitchPolicy){
  prize <- sample(1:3, 1)
  choice <- sample(1:3, 1)
  if(prize == choice){
    revealed <- setdiff(1:3, choice) %>%
      sample(1)
  }else{
    revealed <- setdiff(1:3, c(prize, choice))
  }
  
  if(SwitchPolicy){
  choice <- setdiff(1:3, c(revealed, choice))[1]
  }
  return(choice == prize)
}


MontyHall(T)

WinProb <- function(Switch, N){
wins <- c()
for (i in 1:N){
   wins[i]<- MontyHall(Switch)
}
wins %>% 
  mean() %>%
  return()
}

WinProb(F, 100)
WinProb(T, 100)

#Давайте покажем, как вероятность выигрыша сходится к своему истинному значению при увеличении числа наблюдений!

set.seed(123)
K = 1000
WinProb_K <- c()
for (j in 1:K){
  WinProb_K[j] <- WinProb(T, j)
}

data.frame(P = WinProb_K,
           K = seq(1, K, by = 1)) %>%
  mutate(mycolor = ifelse(P > 0.66667, "type1", "type2")) %>%
  ggplot(aes(x = K, y = P)) +
  geom_segment(aes(x = K, xend = K, y = 0.66667, yend = P, color = mycolor), size = 1.0, alpha = 0.4, show.legend = F) +
  geom_hline(yintercept = 0.66667, size = 0.2, alpha = 0.3)  +
  scale_color_carto_d(palette = 'TealRose') +
  theme_minimal(base_family = 'Monoton') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(colour = 'white')) +
  labs(title = 'Monty Hall, and Statistics Theory')
ggsave('monty.png', dpi = 400)
