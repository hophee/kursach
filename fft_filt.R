# Пример ряда данных
data <- c(...)  # Замените на ваш ряд измерений
sampling_rate <- 50  # Частота дискретизации, Гц

# Выполняем преобразование Фурье
fft_data <- fft(test_df$rn_r50)

# Определяем частоту среза (например, фильтруем компоненты выше 10 Гц)
cutoff_freq <- 10  # Гц
nyquist <- sampling_rate / 2  # Частота Найквиста (25 Гц)
cutoff <- floor(cutoff_freq / nyquist * length(fft_data) / 2)

# Удаляем высокочастотные компоненты
filtered_fft <- fft_data
filtered_fft[(cutoff+1):(length(fft_data)-cutoff)] <- 0

# Обратное преобразование Фурье
filtered_data <- Re(fft(filtered_fft, inverse = TRUE)) / length(data)

# Результат 'filtered_data' содержит ваш фильтрованный ряд
 test_df$filtered <- filtered_data
ggplot(data = test_df, aes(t, filtered)) +
  geom_line()
ggplot(data = spyro_p11_3, aes(vrema_obsled, rn_r50_om)) +
  geom_line()
test_df <- test_df %>% mutate(filtered = rollmean(filtered, k=3, fill = NA))
