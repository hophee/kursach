library(tidyr)
library(dplyr)
library(janitor) #обработка загаловков таблицы в едином формате
library(openxlsx) #загрузка датасета с гита
library(lubridate) #работа с датами
library(ggplot2) #визуализация
library(zoo) #реализация фильтра скользящего окна
#лучше использовать library(readxl), если загружается локально, тогда read.xlsx заменяется на read_xlsx
#функция для фильтра сигнала, по умолчанию выставлена чатоста среза 10, частота дискретизации 50, чтобы упростить вызов под задачу
fourie_filter <- function(signa) {
  vector_name <- deparse(substitute(signa))
  if (grepl("50", vector_name)) {
    samp_rate = 50
    cutoff_freq = 10
  } else {
    samp_rate = 5
    cutoff_freq = 1
  }
  nyquist <- samp_rate / 2
  cutoff <- floor(cutoff_freq / nyquist * length(signa) / 2)
  filtered_fft <- fft(signa)
  filtered_fft[(cutoff+1):(length(signa)-cutoff)] <- 0
  filtered_data <- Re(fft(filtered_fft, inverse = TRUE)) / length(signa)
  return (filtered_data)
}

#прочтение оригинальных данных
spiro_data <-  read.xlsx('https://raw.githubusercontent.com/hophee/kursach/main/kursovaya.xlsx', sheet=1) %>% 
  janitor::clean_names()

#для одного случая выделение нужной даты
spyro_p11_3 <- spiro_data %>% filter(pacient == 'П11-3') %>%  select(c(1, 3:19))
spyro_p11_3 <- spyro_p11_3 %>% mutate(vrema_obsled = as.numeric(hms(vrema_obsled) - hms(vrema_obsled[1])))
spyro_p11_3 <- spyro_p11_3 %>% mutate(across(rn_r50_om:lf_phi5_grad, ~ fourie_filter(.))) 
spyro_p11_3 <- spyro_p11_3 %>% mutate(across(rn_r50_om:lf_phi5_grad, ~ rollmean(., k = 5, fill = na.fill(., "extend")))) 


#функция получения спирограмы
get_pacient_spyro <- function(df, pacient_code, roll_mean_num) {
  spyro <- df %>% filter(pacient == pacient_code) %>% select(c(1, 3:19))
  spyro <- spyro %>% mutate(vrema_obsled = as.numeric(hms(vrema_obsled) - hms(vrema_obsled[1])))
  spyro <- spyro %>% mutate(across(rn_r50_om:lf_phi5_grad, ~ fourie_filter(.))) 
  spyro <- spyro %>% mutate(across(rn_r50_om:lf_phi5_grad, ~ rollmean(., k = roll_mean_num, fill = na.fill(., "extend"))))
  for (i in names(spyro)[-(1:2)]){
    p <- ggplot(data = spyro, aes_string(x = "vrema_obsled", y = i)) +
      geom_line()
    print(p)
  }
}
#вызов для одного пацента
get_pacient_spyro(spiro_data, 'П13', 5)

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

#запись всех графиков в один лист
plots <- lapply(3:18, function(i){
  ggplot(spyro_p11_3, aes_string(x = 'vrema_obsled', y = names(spyro_p11_3)[i])) +
    geom_line()
})

#делает все графики по отдельности
for (i in names(spyro_p11_3)[-(1:2)]){
  p <- ggplot(data = spyro_p11_3, aes_string(x = "vrema_obsled", y = i)) +
    geom_line()
  print(p)
}
