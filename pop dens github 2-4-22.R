library(plyr)
library(ggplot2)
library(tidyverse)

#data from https://www.census.gov/dataviz/visualizations/054/508.php
#https://www.census.gov/dataviz/visualizations/054/
A = read.csv('pop dens vs distance from center.csv') %>%
  rename(City = Population.density.in.rings.moving.out.from.central.business.district..in.1.mile.increments) %>%
  select(-X2010.population)

#NC = data.frame(City = as.character(A$City))
NC = as.character(A$City)

B = A %>% select(-City)
B = B %>% apply(2, function(x)  as.numeric( gsub(',', '', as.character(x)) )) %>% data.frame()

B$City = NC
B$City = as.character(B$City)

#B[1,1:10]
#A[1,1:10]
#class(A$X0)
#class(A$X87)

C = B %>% pivot_longer(!City, names_to = 'radius', values_to = 'dens') %>% mutate(radius = gsub('X','', radius) %>% as.numeric()) %>% data.frame() %>%
  mutate(densK = dens/1000)
C$ldens = log(C$dens)

#levels(factor(C$City))
#levels(factor(C$City))[grep('Washington', levels(factor(C$City)))]
cities_select = c('New York-Northern New Jersey-Long Island, NY-NJ-PA Metro Area',
                  #'Houston-Sugar Land-Baytown, TX Metro Area',
                  'Los Angeles-Long Beach-Santa Ana, CA Metro Area',
                  'Columbus, OH Metro Area',
                  #'Atlanta-Sandy Springs-Marietta, GA Metro Area',
                  #'Phoenix-Mesa-Glendale, AZ Metro Area',
                  #'Chicago-Joliet-Naperville, IL-IN-WI Metro Area',
                  'Boston-Cambridge-Quincy, MA-NH Metro Area')
                  #'Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area')
rm(D)
D = C[which(C$City %in% cities_select),] 
D = D %>% mutate(City = mapvalues(City, from = c('New York-Northern New Jersey-Long Island, NY-NJ-PA Metro Area',
                                                 #'Houston-Sugar Land-Baytown, TX Metro Area',
                                                 'Los Angeles-Long Beach-Santa Ana, CA Metro Area',
                                                 'Columbus, OH Metro Area',
                                                 #'Atlanta-Sandy Springs-Marietta, GA Metro Area',
                                                 #'Phoenix-Mesa-Glendale, AZ Metro Area',
                                                 #'Chicago-Joliet-Naperville, IL-IN-WI Metro Area',
                                                 'Boston-Cambridge-Quincy, MA-NH Metro Area'),
                                                 #'Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area'),
                                  to = c('NYC', 'LA', 'Columbus', 'Boston')))


ggplot(data = D) + 
  geom_line(aes(x = radius, y =  densK, color = City), size = 1) + 
  ylab('pop. density [thousand per sq. mi]') + xlab('radius from center [mi]') +
  xlim(c(0, 30)) +
  theme_minimal() + theme(text = element_text(size=25),
                          legend.position = c(0.8, 0.6)) 



