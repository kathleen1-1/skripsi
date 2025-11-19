#female--------------------------------------------
library(readxl)
library(StMoMo)
nMx_raw<-read_excel("D:/skripsi/nmx_female.xlsx",sheet="Sheet2")
ages <- colnames(nMx_raw)[-1]                             # baris = kelompok umur (misal "0-4")
ages_numeric <- as.numeric(sub("-.*", "", ages))
library(dplyr)

# Tentukan tahun awal untuk filter (harus sesuai dengan data Anda)
START_YEAR_FILTER <- 1950 

# --- Filter baris data (tahun) menjadi interval 5 tahunan ---
nMx_raw <- nMx_raw %>%
  filter(
    # Logika: (Tahun - Tahun Awal) harus habis dibagi 5 (remainder = 0)
    (Year - START_YEAR_FILTER) %% 5 == 0 
  )
# 1. Pengecekan Khusus untuk 100+ (Sudah benar)
# Asumsi: Baris ke-21 adalah "100+"
if(length(ages_numeric) >= 21) {
  ages_numeric[21] <- 100
}

# 2. Hapus sisa NA yang tidak valid
# Ini adalah langkah pembersihan agar min()/max() atau loop berikutnya tidak error.
ages_numeric_clean <- ages_numeric[!is.na(ages_numeric)] # ambil umur bawah (misal 0 dari "0-4")
years <- nMx_raw$Year                  # kolom = tahun
years_numeric <- as.numeric(years)


nMx_matrix <- as.matrix(nMx_raw[, -1]) 

# Lakukan Transpose (memutar matriks) agar USIA menjadi Baris dan TAHUN menjadi Kolom
nMx_matrix <- t(nMx_matrix) 

# Tentukan nama baris dan kolom yang sudah dihitung
# Panjang vektor sekarang sesuai: rownames (~21 usia), colnames (~74 tahun)
rownames(nMx_matrix) <- ages_numeric
colnames(nMx_matrix) <- years_numeric

# Pastikan semua isinya di dalam matriks adalah numerik
nMx_matrix <- apply(nMx_matrix, c(1, 2), as.numeric) 

# 4. Buat exposure dummy (jumlah penduduk hidup) = 1
E <- matrix(1, nrow = length(ages_numeric), ncol = length(years_numeric),
            dimnames = list(ages_numeric, years_numeric))

# 5. Hitung jumlah kematian (Dxt = Mx * E)
Dxt <- nMx_matrix * E

# 6. Buat objek StMoMoData (Koreksi nama variabel menjadi data_female)
data_female <- list(
  Dxt = Dxt,
  Ext = E,
  ages = ages_numeric,
  years = years_numeric,
  type = "central"
)
class(data_female) <- "StMoMoData"

# Fit model Lee Carter
LC_model <- lc(link = "log", const = "sum")

# Koreksi: Gunakan data_female yang benar
LC_fit <- fit(LC_model, data = data_female) 
save(LC_fit, file = "D:/skripsi/fit_LC_female.RData")

# View summary
summary(LC_fit)

a_x = as.vector(LC_fit$ax)
b_x = as.vector(LC_fit$bx)
k_t = as.vector(LC_fit$kt)
residual_deviance <- LC_fit$dev 
n_parameters <- LC_fit$npar # Jumlah parameter model (ax, bx, kt, dll.)
n_data_points <- length(LC_fit$Dxt) # Total observasi (X * T)

# 2. Hitung Degrees of Freedom Sisa (DF_res)
# DF_res = Total Data Points - Total Parameters
residual_df_manual <- n_data_points - n_parameters 

# 3. Hitung Varians Error (Phi = Sigma Kuadrat)
phi_estimate_sigma2 <- residual_deviance / residual_df_manual

# 4. Hitung Deviasi Standar (Sigma)
sigma_error_hat <- sqrt(phi_estimate_sigma2)

cat("=========================================\n")
cat("ESTIMASI PARAMETER DISTRIBUSI ERROR:\n")
cat(sprintf("DF Sisa (DF_res): %d\n", residual_df_manual))
cat(sprintf("Varians (Sigma^2 / Phi): %.4f\n", phi_estimate_sigma2))
cat(sprintf("Deviasi Standar (Sigma): %.4f\n", sigma_error_hat))
cat("=========================================\n")
library(ggplot2)
library(dplyr)
library(tidyr)

# --- ASUMSI: Variabel ini harus didefinisikan ---
# residuals_lc: Objek sisaan dari residuals(LC_fit)
# sigma_error_hat: Deviasi standar error yang sudah dihitung (misal 0.0189)
# ----------------------------------------------------

# 1. Ambil vektor sisaan dan bersihkan NA/Inf
residuals_vector <- as.vector(LC_fit$residuals)
residuals_vector_clean <- residuals_vector[is.finite(residuals_vector)]

# 2. Buat dataframe untuk plotting
residuals_df <- data.frame(Residuals = residuals_vector_clean)

library(ggplot2)
library(dplyr)
library(tidyr)

# --- ASUMSI: Variabel ini sudah didefinisikan ---
# residuals_lc: Objek sisaan dari residuals(LC_fit)
# sigma_error_hat: Deviasi standar error yang sudah dihitung (misal 0.012)
# ----------------------------------------------------

# 1. Ambil vektor sisaan dan bersihkan NA/Inf
residuals_vector <- as.vector(LC_fit$fittingModel$residuals)
residuals_vector_clean <- residuals_vector[is.finite(residuals_vector)]

# 2. Buat dataframe untuk plotting
residuals_df <- data.frame(Residuals = residuals_vector_clean)

qq_plot <- ggplot(residuals_df, aes(sample = Residuals)) +
  # Tambahkan titik kuantil sampel Anda
  stat_qq() +
  # Tambahkan garis referensi (wajib untuk menilai Normalitas)
  stat_qq_line(color = "red", linewidth = 1.2, linetype = "dashed") +
  
  labs(
    title = "Normal Quantile-Quantile (Q-Q) Plot Residual Lee-Carter",
    subtitle = "Menguji asumsi Normalitas error model",
    x = "Kuantil Teoritis (Distribusi Normal)",
    y = "Kuantil Sampel (Residuals)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(qq_plot)
a_x
b_x
k_t 
ages <- LC_fit$ages
years <- LC_fit$years

# 1. Plot a_x
plot(ages, a_x, type = "b", pch = 16, col = "darkgreen",
     xlab = "Umur", ylab = expression(a[x]),
     main = "a(x): Rata-rata log mortalitas pada usia")

# 2. Plot b_x
plot(ages, b_x, type = "b", pch = 16, col = "blue",
     xlab = "Umur", ylab = expression(b[x]),
     main = "b(x): Sensitivitas log mortalitas pada waktu")

# 3. Plot k_t
# Asumsi: Vektor 'years' dan 'k_t' berisi data dari 1950 hingga 2023.

ages <- seq(1950,2020, by = 5)

# Plot
plot(ages, k_t, type = "b", pch = 19, col = "blue",
     xlab = "Tahun", ylab = "k_t", main = "Plot k_t terhadap Umur",
     xaxt = "n")
axis(1, at = ages) 
library(writexl)

# Create Year sequence for k_t
Year <- years

df1 <- data.frame(a_x, b_x)
df2 <- data.frame(Year, k_t)
df1
df2
write_xlsx(df1, path = "lee_carter_axbx (female).xlsx")
write_xlsx(df2, path = "lee_carter_kt (female).xlsx")
# Plot the fitted parameters
plot(LC_fit)

# Extract residuals (deviance residuals)
residuals_lc <- residuals(LC_fit)

#1. plot histogram
hist(residuals_lc$residuals, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)

# 2.  Create a QQ plot of the residuals
qqnorm(residuals_lc$residuals, 
       main = "QQ Plot of Residuals", 
       col = "blue", 
       pch = 19)  # Create the QQ plot

# Add the reference line for normality
qqline(residuals_lc$residuals, col = "red", lwd = 2) 

# Kolmogorov-Smirnov test untuk membandingkan residual dengan distribusi normal
ks_test_result <- ks.test(residuals_lc$residuals, "pnorm", mean = mean(residuals_lc$residuals), sd = sd(residuals_lc$residuals))

# Menampilkan hasil KS test
print(ks_test_result)
#p-value = 0.001819 < 0.05 -> not normally distributed

# Plot residuals with different styles
plot(residuals_lc, type = "colourmap")
library(forecast)
library(tseries)
library(TSA)

kt <- ts(k_t)
plot(kt)

acf(kt) #possible lag 1
pacf(kt)

adf.test(kt)
#H0 : not stationary, ha: stationary
#p-value > 0.05 -> not stationary
## R Markdown

dkt <- diff(kt)
adf.test(dkt)
# pvalue < 0.05 -> stationary

acf(dkt)
pacf(dkt)
eacf(dkt)
# Model terpilih secara otomatis
best_model_auto <- auto.arima(kt, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)

cat("\n== Hasil auto.arima (Model Terbaik) ==\n")
print(summary(best_model_auto))
library(tidyverse)
library(forecast) # Pastikan dimuat

# --- 1. Tentukan Tahun Proyeksi ---
# Hitung tahun awal proyeksi (misalnya 2025)
tahun_awal_proj <- 2025# Gunakan tahun_terakhir + interval waktu
h_forecast <- 10 

# Jalankan kembali fungsi forecast()
# Asumsi 'best_model_auto' sudah didefinisikan
kt_forecast_result <- forecast(best_model_auto, h = h_forecast) 

# --- 2. Kemudian jalankan baris yang error ---
# Asumsi tahun_awal_proj sudah didefinisikan
tahun_proyeksi_vektor <- seq(from = tahun_awal_proj, by = 5, length.out = length(kt_forecast_result$mean))
# --- 2. Bentuk Data Frame Hasil ---
kt_proj_df <- tibble(
  Tahun = tahun_proyeksi_vektor,
  # Estimasi Titik (Ramalan Rata-rata)
  kt_Mean = as.vector(kt_forecast_result$mean),
  # Interval Prediksi 95% (Batas Ketidakpastian)
  Lower_95 = as.vector(kt_forecast_result$lower[, "95%"]),
  Upper_95 = as.vector(kt_forecast_result$upper[, "95%"])
)

# --- 3. Tampilkan Hasil ---
cat("== HASIL PROYEKSI TREN MORTALITAS (kappa_t) ==\n")
print(kt_proj_df)
 library(ggplot2)
library(tidyverse)

# Membuat grafik garis dengan pita ketidakpastian
kt_ts_corrected<-k_t
# 1. Konversi Data Historis ke Tidy Format
kt_hist_df <- tibble(
  Tahun = as.numeric(time(kt_ts_corrected)),
  kt_Mean = as.vector(kt_ts_corrected),
  # Set Lower/Upper bounds sama dengan mean untuk garis historis yang solid
  Lower_95 = as.vector(kt_ts_corrected),
  Upper_95 = as.vector(kt_ts_corrected)
)

# 2. Gabungkan Data Historis dan Proyeksi
kt_all_df <- bind_rows(kt_hist_df, kt_proj_df)
# Data ini sekarang berisi seluruh tren dari 1971 hingga 2070.
kt_all_df$Tahun<-c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070)

library(ggplot2)

# Plot κt dengan interval prediksi
kt_forecast_plot <- ggplot(kt_all_df, aes(x = Tahun, y = kt_Mean)) +
  
  # Pita interval prediksi 95%
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "gray", alpha = 0.4) +
  
  # Garis tren κt
  geom_line(color = "blue", linewidth = 1) +
  
  # Titik-titik κt
  geom_point(color = "blue", size = 2) +
  
  # Judul dan label sumbu
  labs(
    title = "Tren Historis dan Proyeksi Mortalitas (κt) Jangka Panjang",
    subtitle = "Area abu-abu menunjukkan Interval Prediksi 95%",
    x = "Tahun",
    y = expression(kappa[t])
  ) +
  
  # Skala sumbu-x agar tidak terlalu padat
  scale_x_continuous(breaks = seq(1, 25, by = 5)) +
  
  # Tema visual
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted")
  )+ scale_x_continuous(breaks = seq(1950, 2070, by = 10))

print(kt_forecast_plot)
# Simpan plot ke file
ggsave("kt_forecast_projection.png", kt_forecast_plot, width = 10, height = 6)

cat("\nGrafik proyeksi tren mortalitas telah dibuat dan disimpan sebagai 'kt_forecast_projection.png'.\n")

