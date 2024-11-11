library(tidyr)
library(janitor)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

#прочтение оригинальных данных
spiro_data <-  read_xlsx('/home/iz-user/Documents/univer/kursach/kursovaya.xlsx') %>% 
  janitor::clean_names()

#выделение моей даты
spyro_p11_3 <- spiro_data %>% filter(pacient == 'П11-3') %>%  select(c(1, 3:19))
spyro_p11_3 <- spyro_p11_3 %>%   mutate(vrema_obsled = as.numeric(hms(vrema_obsled) - hms(vrema_obsled[1])))

#отображение
ggplot(data = spyro_p11_3, aes(vrema_obsled, lf_r50_om)) +
  geom_line()

#переводит фрей в лонг формат, для отображения на одной картинке (бессмысленно, они слишком разных размерностей)
df_long <- spyro_p11_3 %>% 
  pivot_longer(cols = -c(pacient, vrema_obsled), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = vrema_obsled, y = value, color = variable)) +
  geom_line() +
  labs(title = "График времени для разных столбцов",
       x = "Время",
       y = "Значение") +
  theme_minimal()

#делает все графики по отдельности
for (i in names(spyro_p11_3)[-(1:2)]){
  p <- ggplot(data = spyro_p11_3, aes_string(x = "vrema_obsled", y = i)) +
    geom_line()
  print(p)
}
