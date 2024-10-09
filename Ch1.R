library(tidyverse)

# Practice 1

data1 <- readRDS("data/macro_data.rds")

data1_wide <- 
  data1 %>% 
  select(year, RGDP_YG, CPI_YG,NGDP_YG) %>% 
  filter(between(year,2011,2022))

data1_long <- 
  data1_wide %>% 
  pivot_longer(-year)

ggplot(data1_long,aes(year,value))+
  geom_line(aes(col=name))

ggplot(data1_wide,aes(NGDP_YG,CPI_YG))+
  geom_point()+
  geom_smooth(method="lm")



# Practice 2

library(gapminder)

gapminder %>% 
  filter(year==1997) %>% 
  ggplot(aes(continent,lifeExp))+
  geom_boxplot()

gapminder %>% 
  filter(year==1997) %>% 
  ggplot(aes(gdpPercap,lifeExp))+
  geom_point(aes(color=continent))+
  facet_grid(~continent)


data2 <- 
gapminder %>% 
  filter(country %in% c("Vietnam","Thailand","Indonesia")) 

ggplot(data2,aes(year,pop))+
  geom_line(aes(col=country))

ggplot(data2,aes(lifeExp,gdpPercap,col=country))+
  geom_point()+
  geom_smooth(method="gam",se=FALSE)

