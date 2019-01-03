library(tidyverse)


ggplot(diamonds)+
  geom_histogram(mapping=aes(x=y), binwidth=0.5)+
  coord_cartesian(ylim=c(0,50))

(unusual<- diamonds%>%
    filter(y<3|y>20) %>%
    arrange(y))
par(mfrow=c(3,1))

g=ggplot(diamonds)
g+geom_histogram(aes(x=x))
g+geom_histogram(aes(x=y))
g+geom_histogram(aes(x=x))
g+geom_histogram(aes(x=price, binwidth=0.5))

filter(diamonds, carat%in%c(.99,1))%>%
  ggplot()+geom_histogram(aes(x=carat, color=carat>0.99))


diamonds%>% ggplot(aes(y))+geom_histogram()+xlim(c(0,50))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


fl <- 
  flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  )
fl %>%
  ggplot(aes(sched_dep_time, ..density.., colour = cancelled)) +
  geom_freqpoly(binwidth = 1/2)
fl %>%
  ggplot(aes(sched_dep_time, colour = cancelled)) +
  geom_density()
fl %>%
  ggplot(aes(cancelled, sched_dep_time)) +
  geom_boxplot()
display(lm(price ~ ., diamonds), detail = T)
diamonds %>%
  ggplot(aes(cut, carat)) +
  geom_boxplot()
diamonds %>%
  ggplot(aes(carat, colour = cut)) +
  geom_bar(position = "dodge")
diamonds %>%
  ggplot(aes(carat, colour = cut)) +
  geom_density(position = "dodge")
diamonds %>%
  group_by(cut) %>%
  summarise(cor(carat, price))

library(ggstance)
diamonds %>%
  ggplot(aes(cut, carat)) +
  geom_boxplot() +
  coord_flip()
diamonds %>%
  ggplot(aes(carat, cut)) +
  geom_boxploth()
library(lvplot)
p <- ggplot(diamonds, aes(cut, price, colour = ..LV..))
p + geom_lv()
p <- ggplot(diamonds, aes(cut, carat, fill = ..LV..))
p + geom_lv()


diamonds %>%
  ggplot(aes(cut, price)) +
  geom_violin()
diamonds %>%
  ggplot(aes(price)) +
  geom_histogram() +
  facet_wrap(~ cut, scale = "free_y", nrow = 1)
diamonds %>%
  ggplot(aes(price)) +
  geom_freqpoly(aes(colour = cut))

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))


diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(color, cut, fill = perc)) +
  geom_tile()

library(viridis)
library(forcats)
flights %>%
  ggplot(aes(x = month, y = dest, fill = dep_delay)) +
  geom_tile()

library(viridis)
library(forcats)
flights %>%
  ggplot(aes(x = month, y = dest, fill = dep_delay)) +
  geom_tile()
flights %>%
  mutate(tot_delay = dep_delay + arr_delay) %>%
  filter(tot_delay > 0) %>%
  group_by(dest, month) %>%
  summarize(dep_del_dev = mean(tot_delay, na.rm = T)) %>%
  filter(n() == 12) %>%
  ungroup() %>%
  ggplot(aes(x = factor(month), y = fct_reorder(dest, dep_del_dev), fill = dep_del_dev)) +
  geom_tile() +
  scale_fill_viridis()

diamonds %>%
  count(color, cut) %>%
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

ggplot(data = diamonds, 
       mapping = aes(x = price,
                     colour = cut_width(carat, 0.3))) +
  geom_freqpoly()
diamonds %>%
  filter(between(carat, 0, 2.5)) %>%
  mutate(carat = cut_width(carat, 1)) %>%
  ggplot(aes(price)) +
  geom_histogram() +
  facet_wrap(~ carat)
ggplot(diamonds, aes(carat, y = ..density.., colour = cut_width(price, 2000))) +
  geom_freqpoly()
ggplot(data = diamonds, aes(x=cut_width(price, 2000), y=carat)) +
  geom_boxplot()
diamonds %>%
  filter(between(carat, 0, 2.5)) %>%
  mutate(carat = cut_width(carat, 1)) %>%
  ggplot(aes(price)) +
  geom_histogram() +
  facet_wrap(~ carat)
diamonds %>%
  filter(between(carat, 0, 2.5)) %>%
  mutate(carat = cut_width(carat, 1)) %>%
  ggplot(aes(cut, price)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ carat)

ggplot(diamonds, aes(x, y)) +
  geom_point(alpha=1/100, position="jitter") +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
ggplot(diamonds, aes(x, y)) +
  geom_hex()
x
ggplot(data=mpg)+
  geom_boxplot(mapping=aes(x=reorder(class, hwy, FUN = median), y=hwy))+
  coord_flip()
flight1<-flights%>%mutate(hour=dep_time%%100, min)
ggplot(flights)+ geom_boxplot()


ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")
ggplot(diamonds, aes(x = cut_width(price, 2000, boundary = 0), y = carat)) +
  geom_boxplot(varwidth = TRUE) +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex()+
  facet_wrap(~ cut, ncol = 3) +
  scale_fill_viridis()
ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_lv(position="dodge")
ggplot(diamonds, aes(colour = cut_number(carat, 3), y = price, x = cut)) +
  geom_lv()
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))



library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid), alpha=1/50)

ggplot(data = diamonds2) + 
  geom_violin(mapping = aes(x = cut, y = resid))+
  geom_line(aes(cut~residual))

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()
