#https://www.bluemetrica.com/r-przetwarzanie-danych-z-pakietem-dplyr/

installed.packages("tidyverse")
library(tidyverse)

#wczytywanie danych
wines<-read_csv("winemag-data-130k-v2.csv")
wines%>%
  view()
class(wines)
str(wines)
glimpse(wines)
filter(wines,points >=94,price <25)%>%
  view()
sample_frac(wines,0.01)
top_n(wines,3,points)
top_n(wines,5,price)
top_n(wines,5,-price)
arrange(wines,points)
arrange(wines,desc(points))
arrange(wines,points)
select(wines,country,province:region_2)
select(wines,-X1)%>%
  view()
usd_to_pl <-3.7
wines%>%mutate(price_pln = price*usd_to_pl)%>%
  view()
summarise(wines,
         mean_price = mean(price),
         std_proce = sd(price))

summarise(wines,
          mean_price = mean(price,na.rm = T),
          std_proce = sd(price,na.rm = T))


wines%>%summarise(mean_price=mean(price,na.rm=T),
                 std_price = sd(price,na.rm=T))%>%
  view()
                 
quantile(wines$price,na.rm = T,probs = c(0,0.1,0.25,0.50,0.75,0.90,1)) 

price_pln=price*usd_to_pl
wines %>%
  mutate(price_pln=price*usd_to_pl)%>%
  mutate(price_score_ratio = price_pln/points)%>%
  select(title, price_pln,points, price_score_ratio)%>%
  arrange(price_score_ratio)
                 
wines %>%
  mutate(price_pln=price*usd_to_pl)%>%
  mutate(price_score_ratio = price_pln/points)%>%
  select(title, price_pln,points, price_score_ratio)%>%
  filter(points>=90)%>%
  arrange(price_score_ratio) 

wines %>%
  mutate(price_pln=price*usd_to_pl)%>%
  group_by(country)%>%
  summarise(median_price_pl= median(price_pln,na.rm = T))
             

wines%>%
  mutate(price_pln=price*usd_to_pl)%>%
  group_by(country)%>%
  summarise(
    mediana_price_pln = median(price_pln,na.rm = T),
    avg_score = mean(points,na.rm=T),
    n_of_wines = n())%>%
  arrange(mediana_price_pln)%>%
  filter(n_of_wines >=20)
    
  
mutate_at(wines,vars(country,taster_name,variety), as.factor)

wines%>%
  view()

rename_all(wines,stringr::str_to_title)%>%
  view()




    
                 