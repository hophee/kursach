library(tidyr)
library(dplyr)
library(janitor) #обработка загаловков таблицы в едином формате
library(openxlsx) #загрузка датасета с гита
library(lubridate) #работа с датами
library(ggplot2) #визуализация
library(zoo) #реализация фильтра скользящего окна
library(gsignal)#библиотека для обработки сигналов
library(purrr)#создание стобцов по маске
#лучше использовать library(readxl), если загружается локально, тогда read.xlsx заменяется на read_xlsx
setwd(readLines("~/config.txt"))

#функция для фильтра сигнала
fourie_filter <- function(signa, norm_freq=0.4, n=4, type='low') {
  bf <- butter(n, norm_freq,type='low')
  filtered_data <- filtfilt(bf, signa)
  return (filtered_data)
}
#переназначение функции, для избежания конфликтов
filter <- dplyr::filter

#прочтение оригинальных данных
spiro_data <-  read.xlsx('https://raw.githubusercontent.com/hophee/kursach/main/kursovaya.xlsx', sheet=1) %>% 
  janitor::clean_names()
#список пациентов по заданию
my_patients <- c('П11-3', 'П13',	'П13-3')

#функция получения спирограмы
get_pacient_spyro <- function(df, pacient_code, roll_mean_num, long=F, safe=F) {
  get_div <- function(ser, time){
    ser_dif <- c(diff(ser)[1], diff(ser))
    time_dif <- c(diff(time)[1], diff(time))
    diver <- ser_dif / time_dif
    return (diver)
  }
  spyro <- df %>% filter(pacient == pacient_code) %>% select(c(1, 3:19)) #отбор нужного пациента, удаление столбца дат
  spyro <- spyro %>% mutate(
    r50_om = (rn_r50_om * lf_r50_om) / (rn_r50_om + lf_r50_om),
    xc50_om = (rn_xc50_om * lf_xc50_om) / (rn_xc50_om + lf_xc50_om),
    z50_om = (rn_z50_om * lf_z50_om) / (rn_z50_om + lf_z50_om),
    phi50_grad = (rn_phi50_grad * lf_phi50_grad) / (rn_phi50_grad + lf_phi50_grad),
    r5_om = (rn_r5_om * lf_r5_om) / (rn_r5_om + lf_r5_om),
    xc5_om = (rn_xc5_om * lf_xc5_om) / (rn_xc5_om + lf_xc5_om),
    z5_om  = (rn_z5_om * lf_z5_om) / (rn_z5_om + lf_z5_om),
    phi5_grad = (rn_phi5_grad * lf_phi5_grad) / (rn_phi5_grad + lf_phi5_grad)) %>% 
      select(vrema_obsled, r50_om, xc50_om, z50_om, phi50_grad, r5_om, xc5_om, z5_om , phi5_grad) #пересчёт по двум отведениям
  spyro <- spyro %>% mutate(vrema_obsled = as.numeric(hms(vrema_obsled) - hms(vrema_obsled[1]))) #перевод времени в отсчёт времени
  spyro <- spyro %>% mutate(across(-vrema_obsled, ~ get_div(., vrema_obsled), .names = "div_{.col}")) #добавление производных
  spyro <- spyro %>% mutate(across(r50_om:div_phi5_grad, ~ fourie_filter(.)))  #применение фильтра Баттерворта
  spyro <- spyro %>% mutate(across(r50_om:div_phi5_grad, ~ rollmean(., k = roll_mean_num, fill = NA))) # скользящее среднее
  #spyro <- spyro %>% mutate(across(r50_om:phi5_grad, ~ scale(.))) # нормирование значений
  
  if (long==F) {
    for (i in names(spyro)[2:9]){
      p <- ggplot(data = spyro, aes_string(x = "vrema_obsled", y = i)) +
        geom_line(lwd=0.9) +
        labs(title = paste0(pacient_code,'_', i, ', rollmean=', as.character(roll_mean_num)),
             x = "Время",
             y = "Объём лёгких")
      print(p)
      if (safe==T) {
        ggsave(paste0('plots/',pacient_code,'_', i,'.png'), plot = p, width = 10, height = 6)
      }
    }
    for (i in 2:9){
      p <- ggplot(data = spyro, aes_string(x = names(spyro)[i], y = names(spyro)[i+8])) +
        geom_path(lwd=0.9) +
        labs(title = paste0(pacient_code,'_', names(spyro)[i+8], ', rollmean=', as.character(roll_mean_num)),
              x = "Объём",
              y = "Объёмная скорость")
       print(p)
      if (safe==T) {
         ggsave(paste0('plots/',pacient_code,'_', names(spyro)[i+8],'.png'), plot = p, width = 10, height = 6)
       }
      } # создание отдельных графиков
  }else {
    df_long <- spyro %>% 
      pivot_longer(cols = -c(vrema_obsled), names_to = "variable", values_to = "value")
    
    p <- ggplot(df_long, aes(x = vrema_obsled, y = value, color = variable)) +
      geom_line(lwd=1.2) +
      labs(title = "Все отведения",
           x = "Время",
           y = "Значение") +
      theme_minimal()
    print(p)
    if (safe==T) {
      ggsave(paste0('plots/',pacient_code,'_allPlots.png'), plot = p, width = 10, height = 6)
    }
  } # все графики на одном плоте
  return (spyro)
}

#вызов для одного пацента
test <- get_pacient_spyro(spiro_data, my_patients[3], 7)

#вызов для всех
dfs_of_my_patients <- lapply(my_patients, function(pats) {
  get_pacient_spyro(spiro_data, pats, 7, safe=T)
})