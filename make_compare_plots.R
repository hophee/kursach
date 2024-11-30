#дополнительный скрипт для генерации сравнительных графиков, работает в том же глобальном окружении, в котором и запущен основной скрипт
only_data_get_pacient_spyro <- function(df, pacient_code, roll_mean_num, long=F, safe=F) {
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
  spyro <- spyro %>% mutate(across(-vrema_obsled, ~ get_div(., vrema_obsled), .names = "div_{.col}"))
  before_filtering <- spyro
  spyro <- spyro %>% mutate(across(r50_om:div_phi5_grad, ~ fourie_filter(.)))  #применение фильтра Баттерворта
  spyro <- spyro %>% mutate(across(r50_om:div_phi5_grad, ~ rollmean(., k = roll_mean_num, fill = NA))) # скользящее среднее
  #spyro <- spyro %>% mutate(across(r50_om:phi5_grad, ~ scale(.))) # нормирование значений
  res <- list(spyro, before_filtering)
  return (res)
}

#поменять здесь индекс у списка пациентов, чтобы получить другой 
df_after <- only_data_get_pacient_spyro(spiro_data, my_patients[1], 5)[[1]]
df_before <- only_data_get_pacient_spyro(spiro_data, my_patients[1], 5)[[2]]

df <- data.frame(df_before$vrema_obsled, df_before$r50_om,df_after$r50_om)
names(df) <- c('vrema_obsled', 'before_r50_om', 'after_r50_om')
p <- ggplot(data=df, aes(x=vrema_obsled)) +
  geom_line(aes(y=before_r50_om, color = 'Before'), lwd=0.8) +
  geom_line(aes(y=after_r50_om, color = 'After'), lwd=0.9) +
  scale_color_manual(values = c("Before" = '#ef233c', "After" = '#3a86ff'),
                     name = "Измерения",
                     labels = c("До фильтрации", "После фильтрации")) +
  labs(x='Время',
       y='Объём лёгких',
       title='Сравнение данных до и после фильтрации', 
       subtitle = 'Спирограмма, пациент П11-3') #здесь поменять лэйбл
p
ggsave('p11_3_spiro_compare.png', plot = p, width = 10, height = 6, dpi=600)
#здесь тоже

df <- data.frame(df_before$vrema_obsled, df_before$r50_om, df_before$div_r50_om, df_after$r50_om, df_after$div_r50_om)
names(df) <- c('vrema_obsled', 'before_r50_om', 'before_div', 'after_r50_om', 'after_div')
p <- ggplot(data=df) +
  geom_path(aes(y=before_div, x=before_r50_om, color = 'Before')) +
  geom_path(aes(y=after_div, x=after_r50_om, color = 'After')) +
  scale_color_manual(values = c("Before" = '#ef233c', "After" = '#3a86ff'),
                     name = "Измерения",
                     labels = c("До фильтрации", "После фильтрации")) +
  labs(x='Объём лёгких',
       y='Объёмная скорость',
       title='Сравнение данных до и после фильтрации', 
       subtitle = 'Пневмотахограмма, пациент П11-3') #и здесь
p
ggsave('p11_3_pnevmo_compare.png', plot = p, width = 10, height = 6, dpi=600)
#тут тоже