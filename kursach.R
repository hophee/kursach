library(tidyr)
library(dplyr)
library(janitor) #обработка загаловков таблицы в едином формате
library(openxlsx) #загрузка датасета с гита
library(lubridate) #работа с датами
library(ggplot2) #визуализация
library(zoo) #реализация фильтра скользящего окна
#лучше использовать library(readxl), если загружается локально, тогда read.xlsx заменяется на read_xlsx

#прочтение оригинальных данных
spiro_data <-  read.xlsx('https://raw.githubusercontent.com/hophee/kursach/main/kursovaya.xlsx', sheet=1) %>% 
  janitor::clean_names()

#выделение нужной даты
spyro_p11_3 <- spiro_data %>% filter(pacient == 'П11-3') %>%  select(c(1, 3:19))
spyro_p11_3 <- spyro_p11_3 %>% mutate(vrema_obsled = as.numeric(hms(vrema_obsled) - hms(vrema_obsled[1])))
spyro_p11_3 <- spyro_p11_3 %>% mutate(across(rn_r50_om:lf_phi5_grad, ~ rollmean(., k = 3, fill = c((.[1]+.[2])/2, (.[length(.)]+.[length(.)-1])/2)))) 

#отображение одного столбца
ggplot(data = spyro_p11_3, aes(vrema_obsled, lf_r50_om)) +
  geom_line()

#переводит фрейм в лонг формат, для отображения на одной картинке (бессмысленно, они слишком разных размерностей)
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

#хуйня какая-то
vremya <- spyro_p11_3$vrema_obsled
fourie <- fftfilt(vremya, spyro_p11_3$rn_r50_om)
plot(vremya, fourie)


test_df <- data_frame(spyro_p11_3$vrema_obsled, spyro_p11_3$rn_r50_om)
names(test_df) <- c('t', 'rn_r50')
plot(test_df$t, test_df$rn_r50)

