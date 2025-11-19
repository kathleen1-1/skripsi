#fertility---------------------------------------------
fert<-read.csv("D:/skripsi/fertility.csv",sep=";")
library(dplyr)
library(tidyr)
library(zoo)
library(dplyr)
library(tidyr)
library(zoo)
library(StMoMo)

# --- Step 4: Interpolasi CFM untuk tahun-tahun di antara observasi (per 5 tahun) ---

# --- Step 1: Mapping age ke titik tengah ---
age_midpoints <- c(
  "15-19" = 17, "20-24" = 22, "25-29" = 27,
  "30-34" = 32, "35-39" = 37, "40-44" = 42, "45-49" = 47
)

# --- Step 2: Clean data ---
fert_clean <- fert %>%
  mutate(
    population = gsub(",", ".", population),
    population = as.numeric(population) / 1000,  # dibagi 1000 langsung
    age = age_midpoints[age]
  ) %>%
  filter(!is.na(age), !is.na(population)) %>%
  rename(kelahiran = population)

# --- Step 3: Buat daftar tahun asli ---
original_years <- sort(unique(fert_clean$year))
target_years <- seq(min(original_years), max(original_years), by = 5)  # LIMA TAHUNAN
# --- Step 4: Interpolasi ---
cfm_interp <- data.frame()

for (a in unique(fert_clean$age)) {
  df_age <- fert_clean %>% filter(age == a)
  
  for (i in 1:(length(original_years) - 1)) {
    y0 <- original_years[i]
    y1 <- original_years[i + 1]
    
    q0 <- df_age$kelahiran[df_age$year == y0]
    q1 <- df_age$kelahiran[df_age$year == y1]
    
    if (length(q0) > 0 && length(q1) > 0) {
      q0 <- q0[1]
      q1 <- q1[1]
      if (is.na(q0) || is.na(q1) || q0 <= 0 || q1 <= 0) next
    } else {
      next
    }
    
    # --- Midpoint (kasus khusus untuk 1971–1980) ---
    if (y0 == 1971 & y1 == 1980) {
      mid_year <- 1975  # kasus khusus
    } else {
      mid_year <- floor((y0 + y1) / 2)  # normalnya floor midpoint
    }
    
    # hitung interpolasi di titik mid_year
    t <- (mid_year - y0) / (y1 - y0)
    log_interp <- (1 - t) * log(1 - q0) + t * log(1 - q1)
    q_interp <- 1 - exp(log_interp)
    
    cfm_interp <- bind_rows(
      cfm_interp,
      data.frame(age = a, year = mid_year, kelahiran = q_interp)
    )
  }
}

# Gabungkan dengan data asli + interpolasi
fert_nMx_full <- fert_clean %>%
  select(age, year, kelahiran) %>%
  bind_rows(cfm_interp) %>%
  arrange(age, year)

# === Bikin matrix kelahiran (nMx_matrix) ===
nMx_wide <- fert_nMx_full %>%
  group_by(age, year) %>%
  summarise(kelahiran = mean(kelahiran, na.rm = TRUE), .groups = "drop") %>%  # ambil rata-rata kalau duplikat
  pivot_wider(names_from = year, values_from = kelahiran) %>%
  arrange(age)

nMx_matrix <- as.matrix(nMx_wide[,-1])
rownames(nMx_matrix) <- nMx_wide$age

# Exposure dummy: 1 (bisa diganti 100000 kalau mau hitung "death count")
Ext <- matrix(1, nrow = nrow(nMx_matrix), ncol = ncol(nMx_matrix),
              dimnames = list(rownames(nMx_matrix), colnames(nMx_matrix)))
exposure_matrix <- Ext

# "Deaths" = rate × exposure
Dxt <- nMx_matrix * Ext
death_matrix <- Dxt

# --- Step 5: Hitung lx dan dx → death_matrix ---
# --- Step 6: Finalisasi Dxt dan Ext ---
Dxt <- matrix(as.numeric(death_matrix),
              nrow = nrow(death_matrix),
              ncol = ncol(death_matrix),
              dimnames = dimnames(death_matrix))

Ext <- matrix(as.numeric(exposure_matrix),
              nrow = nrow(exposure_matrix),
              ncol = ncol(exposure_matrix),
              dimnames = dimnames(exposure_matrix))
# Create Lee-Carter model object
LC <- lc()
ages  <- as.numeric(rownames(death_matrix))  # atau rownames(nMx_matrix)
years <- as.numeric(colnames(death_matrix))  # atau colnames(nMx_matrix)


# Fit the Lee-Carter model
fit_LC <- tryCatch({
  fit(
    LC,
    Dxt = death_matrix,
    Ext = exposure_matrix,
    ages = ages,
    years = years,
    ages.fit = ages,
    years.fit = years,
    wxt = matrix(1, nrow = length(ages), ncol = length(years)),  # All weights set to 1
    gc.ctrl = list(maxit = 100)  # Maximum iterations set to 100
  )
}, error = function(e) {
  cat("Lee-Carter fitting failed:", e$message, "\n")
  return(NULL)
})
fit_LC_fert<-fit_LC
save(fit_LC_fert, file = "D:/skripsi/fit_LC.RData")

# Check if the model fitting was successful and extract parameters
if (!is.null(fit_LC_fert)) {
  # Extract the model parameters
  ax <- fit_LC$ax  # Age-specific intercepts
  bx <- fit_LC$bx  # Sensitivity to period effects (kt)
  kt <- fit_LC$kt  # Time trend for the period
  
  # Print the extracted parameters
  cat("a (age-specific intercepts) for LC model:\n")
  print(ax)
  
  cat("b (sensitivity to period effects) for LC model:\n")
  print(bx)
  
  cat("kt (time trend for period) for LC model:\n")
  print(kt)
} else {
  cat("The Lee-Carter model fitting was unsuccessful.\n")
}
# Load necessary library for plotting
library(ggplot2)
residual_deviance <- fit_LC_fert$dev 
n_parameters <- fit_LC_fert$npar # Jumlah parameter model (ax, bx, kt, dll.)
n_data_points <- length(fit_LC_fert$Dxt) # Total observasi (X * T)

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
# 1. Ambil vektor sisaan dan bersihkan NA/Inf
residuals_vector <- as.vector(fit_LC$fittingModel$residuals)
residuals_vector_clean <- residuals_vector[is.finite(residuals_vector)]

# 2. Buat dataframe untuk plotting
residuals_df <- data.frame(Residuals = residuals_vector_clean)

qq_plot <- ggplot(residuals_df, aes(sample = Residuals)) +
  # Tambahkan titik kuantil sampel Anda
  stat_qq() +
  # Tambahkan garis referensi (wajib untuk menilai Normalitas)
  stat_qq_line(color = "red", linewidth = 1.2, linetype = "dashed") +
  
  labs(
    title = "Normal Quantile-Quantile (Q-Q) Plot Residuals Lee-Carter",
    subtitle = "Menguji asumsi Normalitas error model",
    x = "Kuantil Teoritis (Distribusi Normal)",
    y = "Kuantil Sampel (Residuals)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(qq_plot)
# Age-specific intercepts (a) from LC model
a <- fit_LC$ax  # Age-specific intercepts

# Sensitivity to period effects (b) from LC model
b <- fit_LC$bx  # Sensitivity to period effects

# Time trend for period (kt) from LC model
kt <- fit_LC$kt  # Time trend for period

# Create data frames for each parameter
a_df <- data.frame(Age = as.numeric(names(a)), Intercepts = a)
kt_df <- data.frame(
  Year = years,     # use the time vector used to fit LC
  Trend = as.numeric(kt)
)

# Plot a (age-specific intercepts)
ggplot(a_df, aes(x = Age, y = Intercepts)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "a(x): Rata-rata log fertilita pada usia", 
       x = "Umur", 
       y = expression(a[x])) +
  theme_minimal()+
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )

# Extract 'b' (sensitivity to period effects) from the model fit
b <- fit_LC$bx

# Check if 'b' contains values and print the first few entries
cat("First few values of 'b':\n")
print(head(b))  # Should display the first few values of 'b'

# Check the length of 'b' and 'ages' to ensure they match
cat("Length of b:", length(b), "\n")
cat("Length of ages:", length(ages), "\n")

# Create a data frame only if 'b' is valid and has the same length as 'ages'
if (length(b) > 0 && length(b) == length(ages)) {
  # Ensure that 'ages' and 'b' are properly paired
  b_df <- data.frame(Age = ages, Sensitivity = b)
  
  # Check the structure of the data frame to ensure everything is correct
  cat("Structure of b_df:\n")
  str(b_df)
  
  # Plot the data for sensitivity to period effects (b)
  ggplot(b_df, aes(x = Age, y = X1)) +
    geom_line(color = "red") +
    geom_point(color = "red") +
    labs(title = "b(x): Sensitivitas log mortalitas pada waktu", 
         x = "Umur", 
         y = expression(b[x])) +
    theme_minimal()+
    theme(
      # KOREKSI: Tambahkan kotak luar (border)
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
    )
} else {
  cat("Error: 'b' is empty or its length does not match the number of ages.")
}
# Load necessary libraries
library(ggplot2)
library(zoo)

# Plot the period effect (kt) from the LC model
ggplot(kt_df, aes(x = Year, y = Trend)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "k(t): Berubahnya waktu pada indeks mortalita ",
    x = "Tahun",
    y = expression(kappa[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )+
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )
library(forecast)
kt<-kt_df$Trend
# 1. Cek plot & ACF
ts.plot(kt)
acf(kt)
pacf(kt)

# 2. Fit ARIMA secara otomatis
fit_arima <- auto.arima(kt)
summary(fit_arima)
library(tseries)  # untuk adf.test

# Misal k_t hasil dari Lee-Carter
kt <- fit_LC$kt[1,]  # ambil komponen pertama (kalau ada lebih dari satu)

# ADF untuk k_t asli
adf.test(kt)
d1 <- diff(kt)         # differencing 1×
d2 <- diff(kt, lag = 1, differences = 2)  # differencing 2×

adf.test(d1)  # cek apakah sudah stasioner
adf.test(d2)  # untuk ARIMA(0,2,0), pastikan d2 stasioner
best_model_auto <- auto.arima(kt, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)

cat("\n== Hasil auto.arima (Model Terbaik) ==\n")
print(summary(best_model_auto))
library(tidyverse)
library(forecast) # Pastikan dimuat

# --- 1. Tentukan Tahun Proyeksi ---
# Hitung tahun awal proyeksi (misalnya 2025)
tahun_awal_proj <- max(time(kt_ts_corrected)) + 2 # Gunakan tahun_terakhir + interval waktu
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


kt_forecast_plot <- ggplot(kt_all_df, aes(x = Tahun, y = kt_Mean)) +
  
  # A. Tambahkan Pita Interval Prediksi (Muncul mulus dari data historis)
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "gray", alpha = 0.4) +
  
  # B. Garis Estimasi Rata-rata untuk seluruh periode
  geom_line(color = "black", linewidth = 1) +
  
  # C. Tambahkan Garis Vertikal (Opsional: menandakan awal proyeksi)
  geom_vline(xintercept = max(kt_hist_df$Tahun), linetype = "dashed", color = "red") +
  
  # 3. Label dan Judul
  labs(
    title = "Tren Historis dan Proyeksi Mortalitas (κt) Jangka Panjang",
    subtitle = "Area abu-abu menunjukkan Interval Prediksi 95%",
    x = "Tahun",
    y = expression(kappa[t])
  ) +
  
  # 4. Penataan Tema dan Skala
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(kt_all_df$Tahun), max(kt_all_df$Tahun), by = 10)) +
  theme(plot.title = element_text(face = "bold"))+
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )

print(kt_forecast_plot)
