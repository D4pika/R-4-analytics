library(nycflights13)
library(tidyverse)

flights

dec25<-filter(flights,day==25,month==12)
#if you wnat to simultaneously then wrap the assignment in parenthesis
(dec25<-filter(flights,day==25,month==12))
#== is comparision operator = is assisgnment in R

near(1/49*49, 1)
nov_dec<-filter(flights, month %in% c(11,12))
nov_dec
(filter(flights, !arr_delay>120|dep_delay<120))
#| and  & are in bitwise && and || are used in conditional expression
is.na(flights)
new_flights2<- filter (flights, dest%in%c("IAH","HOU"))
new_flights1<- filter (flights, arr_delay>120)
new_flights3<- filter (flights, carrier%in%c("AA","DL","UA"))
new_flights4<- filter (flights, month%in%c(7,8,9))
new_flights5<- filter (flights, arr_delay>=120&dep_delay<=0)
new_flights6<- filter (flights, arr_delay>=60&dep_delay<=30)
new_flights7<- filter (flights, dep_time>=00&dep_time<=600)
new_flights8<- filter (flights, between(dep_time,0,6))
new_flights4==new_flights8
count(filter(flights, is.na(dep_time)))

arrange( flights,year, month, day)
arrange (flights ,desc(dep_delay)).tail
flights %>% mutate(travel_time = ifelse((arr_time - dep_time < 0), 
                                        2400+(arr_time - dep_time),
                                        arr_time - dep_time)) %>% 
  arrange(travel_time) %>% select(arr_time, dep_time, travel_time)

flights%>% mutate(travel_time=ifelse((arr_time-dep_time<0),2400+(arr_time-dep_time),arr_time-dep_time))%>%arrange(travel_time)%>%select(arr_time,dep_time,travel_time)

select(flights, -(year:day))


# standard ways
select(flights, dep_time,  dep_delay, arr_time, arr_delay)
select(flights, c(dep_time,  dep_delay, arr_time, arr_delay))
flights %>% select(dep_time,  dep_delay, arr_time, arr_delay)
flights %>% select_("dep_time",  "dep_delay", "arr_time", "arr_delay")
flights %>% select_(.dots=c("dep_time",  "dep_delay", "arr_time", "arr_delay"))
# fancier ways
flights %>% select(dep_time:arr_delay, -c(contains("sched")))
flights %>% select(ends_with("time"),  ends_with("delay")) %>% select(-c(starts_with("sched"), starts_with("air")))
flights %>% select(contains("dep"), contains("arr"), -contains("sched"), -carrier)
flights %>% select(matches("^dep|arr_delay|time$"))
flights %>% select(matches("^dep|^arr"))
flights %>% select(matches("^dep|^arr.*time$|delay$"))
flights %>% select(matches("^dep|^arr_time$|delay$"))


mutate(flights, gain=dep_delay-arr_delay, hours=air_time/60,gain_per_hour=gain/hours)
transmute(flights, gain=dep_delay-arr_delay, hours=air_time/60,gain_per_hour=gain/hours)

lead(flights)
lag(flights)

dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100))


filter(flights, min_rank(desc(dep_delay))<=10)

flights %>% top_n(n = 10, wt = dep_delay)

summarise (flights , delay=mean(dep_delay,na.rm=TRUE))  
by_day<-group_by(flights, year , month, day)
summarise(by_day,delay=mean(dep_delay, na.rm=TRUE))

grouped<-flights%>%
  group_by(dest)%>%
  summarise(
    count=n(),
    dist = mean(distance, na.rm =TRUE),
   delay = mean(arr_delay, na.rm=TRUE)) %>% 
  filter(count > 20, dest!="HNL")


delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")


    by_dest <- group_by(flights, dest)
    delay <- summarise(
                       count = n(),
                       dist = mean(distance, na.rm = TRUE),
                       delay = mean(arr_delay, na.rm = TRUE)
    )
    delay <- filter(delay, count > 20, dest != "HNL")    
    
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

ggplot (data =delay, mapping =aes (x=dist,y=delay ))+ 
  geom_point(aes(size=count),alpha=1/3)+
  geom_smooth (se=FALSE)

not_cancelled<-flights%>%
  filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled%>%group_by(year, day,month)%>% 
  summarise(mean=mean(dep_delay))
delays<-not_cancelled%>%group_by(tailnum)%>% 
  summarise(delay=mean(dep_delay))
ggplot(data=delays, mapping=aes(x=delay))+geom_freqpoly(binwidth=10)


delays<-not_cancelled%>%group_by(tailnum)%>% 
  summarise(delay=mean(dep_delay, na.rm=TRUE), n=n())
ggplot(data=delays, mapping=aes(x=delay,y=n))+geom_point(alpha=1/10)


batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )


batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
batters %>% 
  arrange(desc(ba))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>% group_by(dest )%>%
  summarise(distance_sd=sd (distance))%>%
  arrange (desc(distance_sd))


not_cancelled%>% group_by(year, month, day )%>%
  summarise(first=min (dep_time),
            last=max(dep_time))%>%
  arrange(first,desc(last))

not_cancelled%>% group_by(year, month, day)%>%
  summarise(
    first_dep=first(dep_time),
    last_dep=last(dep_time))

  )
not_cancelled%>% count(tailnum, wt=distance )

not_cancelled%>%group_by(year,month , day )%>%
  summarise(n_early=sum(dep_time<500))
#proportion delaye more than an hour

not_cancelled%<% 
  group_by(year, month , day )%>%
  summarise(hour_perc=mean (srr_delay>60))


daily<-group_by( flights, year , month,day)
(per_day<-summarise(daily, flights=n()))
daily%>%ungroup()%>%
  summarise(flights=n())


flight_delay_summary <- group_by(flights, flight) %>% summarise(num_flights = n(),
                                                                percentage_on_time = sum(arr_time == sched_arr_time)/num_flights,
                                                                percentage_early = sum(arr_time < sched_arr_time)/num_flights, 
                                                                percentage_15_mins_early = sum(sched_arr_time - arr_time == 15)/num_flights,
                                                                percentage_late = sum(arr_time > sched_arr_time)/num_flights,
                                                                percentage_15_mins_late = sum(arr_time - sched_arr_time == 15)/num_flights,
                                                                percentage_2_hours_late = sum(arr_time - sched_arr_time == 120)/num_flights)
flight_delay_summary



not_cancelled <- filter(flights, !is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  group_by(dest) %>%
  tally()
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))


flights %>%
  group_by(departed = !is.na(dep_delay), arrived = !is.na(arr_delay)) %>%
  summarise(n=n())



flights %>%
  mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
  group_by(dep_date) %>%
  summarise(cancelled = sum(is.na(dep_delay)), 
            n = n(),
            mean_dep_delay = mean(dep_delay,na.rm=TRUE),
            mean_arr_delay = mean(arr_delay,na.rm=TRUE)) %>%
  ggplot(aes(x= cancelled/n)) + 
  geom_point(aes(y=mean_dep_delay), colour='blue', alpha=0.15) + 
  geom_point(aes(y=mean_arr_delay), colour='red', alpha=0.15) + 
  ylab('mean delay (minutes)')


flights %>%
  filter(arr_delay > 0) %>%
  group_by(carrier) %>%
  summarise(average_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
  arrange(desc(average_arr_delay))



flights %>%
  summarise(n_distinct(carrier),
            n_distinct(origin),
            n_distinct(dest))

flights %>%
  mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
  group_by(tailnum) %>%
  arrange(dep_date) %>%
  filter(!cumany(arr_delay>60)) %>%
  tally(sort = TRUE)


flights %>%
  group_by(tailnum) %>%
  summarise(prop_on_time = sum(arr_delay <= 30 & !is.na(arr_delay))/n(),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE),
            flights = n()) %>%
  arrange(prop_on_time, desc(mean_arr_delay))
flights %>%
  group_by(tailnum) %>%
  filter(all(is.na(arr_delay))) %>%
  tally(sort=TRUE)


flights %>%
  ggplot(aes(x=factor(hour), fill=arr_delay>5 | is.na(arr_delay))) + geom_bar()

flights %>%
  mutate(new_sched_dep_time = lubridate::make_datetime(year, month, day, hour, minute)) %>%
  group_by(origin) %>%
  arrange(new_sched_dep_time) %>%
  mutate(prev_flight_dep_delay = lag(dep_delay)) %>%
  ggplot(aes(x=prev_flight_dep_delay, y= dep_delay)) + geom_point()


flights %>%
  group_by(dest) %>%
  filter(n_distinct(carrier)>=2) %>%
  group_by(carrier) %>%
  summarise(possible_transfers = n_distinct(dest)) %>%
  arrange(desc(possible_transfers))
