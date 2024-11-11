library(tidyr)
library(janitor)
library(readxl)
library(dplyr)
library(clock)


spiro_data <-  read_xlsx('/home/iz-user/Documents/univer/kursach/kursovaya.xlsx') %>% 
  janitor::clean_names()

spyro_p11_3 <- spiro_data %>% filter(pacient == 'П11-3') %>%  select(c(1, 3:19))
