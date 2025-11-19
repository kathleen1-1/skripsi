# ================== PIPELINE LEE–CARTER BAYESIAN (OpenBUGS) FEMALE ==================
# Data input: sudah age-grouped (0-4, 5-9, 10-14, dst)
# File format: kolom 'Year', lalu kolom age group
# ============================================================================

# --- 0. Libraries
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(R2OpenBUGS)
  library(coda)
  library(tibble)
})

# --- 1. Konfigurasi
RAW_EXCEL_PATH <- "D:/skripsi/nmx_female.xlsx"         # <-- ganti ke file kamu
OPENBUGS_PATH  <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
OUT_DIR        <- getwd()
MODEL_FILE     <- file.path(OUT_DIR, "lc1_clean.txt")

nMx_raw <- read_excel(RAW_EXCEL_PATH,sheet="Sheet2")

# Pastikan ada kolom Year
stopifnot("Year" %in% names(nMx_raw))
age_cols <- setdiff(names(nMx_raw), "Year")

# --- 2. Pilih tahun tiap 5 tahun saja ---
years_5yr <- seq(min(nMx_raw$Year), max(nMx_raw$Year), by = 5)
nMx_5yr <- nMx_raw %>% filter(Year %in% years_5yr)

# --- 3. Bentuk matriks log(nMx) [age x 5yr interval] ---
log_mx_mat <- nMx_5yr %>%
  pivot_longer(-Year, names_to = "age_group", values_to = "nMx") %>%
  pivot_wider(names_from = Year, values_from = nMx) %>%
  column_to_rownames("age_group") %>%
  as.matrix()

# cek semua nilai valid
if (any(log_mx_mat <= 0, na.rm = TRUE)) stop("Ada nMx <= 0, tidak bisa log-transform.")

Y  <- log(log_mx_mat)
I  <- nrow(Y)
Tt <- ncol(Y)

dataList <- list(I = I, Tt = Tt, y = Y)

# === STEP 1: Simpan model ke file (pakai useBytes supaya encoding aman) ===
model_path <- "D:/skripsi/lee_carter_model.txt"

# Model dengan identifiability constraints:
# - sum_x bx = 1  (realized via bx = bx_raw / sum(bx_raw))
# - sum_t kt = 0  (realized via kt = kt_raw - mean(kt_raw))
writeLines("
model {
  # --- Likelihood ---
  for (x in 1:I) {
    for (t in 1:Tt) {
      y[x, t] ~ dnorm(mu[x, t], tau_obs)
      mu[x, t] <- ax[x] + bx[x] * kt[t]
    }
    # prior untuk ax
    ax[x] ~ dnorm(0, 0.01)
    # prior untuk bx_raw (positif), nanti dinormalisasi
    bx_raw[x] ~ dnorm(0.1, 0.01) T(0, )
  }

  # --- Random walk untuk kt_raw ---
  kt_raw[1] ~ dnorm(0, 0.01)
  for (t in 2:Tt) {
    kt_raw[t] ~ dnorm(kt_raw[t-1], tau_kt)
  }

  # --- Identifiability constraints (deterministic transforms) ---
  sum_bx_raw <- sum(bx_raw[])
  for (x in 1:I) {
    bx[x] <- bx_raw[x] / sum_bx_raw          # sum_x bx = 1
  }
  mean_kt_raw <- sum(kt_raw[]) / Tt
  for (t in 1:Tt) {
    kt[t] <- kt_raw[t] - mean_kt_raw         # sum_t kt = 0
  }

  # --- Priors untuk varians ---
  tau_obs ~ dgamma(2, 2)
  tau_kt  ~ dgamma(2, 2)
}
", con = model_path, useBytes = TRUE)

# === STEP 2: Inisialisasi ===
# Inisialisasi hanya untuk node stokastik: ax, bx_raw, kt_raw, tau_obs, tau_kt
set.seed(123)
inits <- function() {
  list(
    ax      = rnorm(I, 0, 0.1),
    bx_raw  = abs(rnorm(I, 0.1, 0.05)),
    kt_raw  = c(0, cumsum(rnorm(Tt - 1, 0, 0.05))),  # RW init yang halus
    tau_obs = rgamma(1, 2, 2),
    tau_kt  = rgamma(1, 2, 2)
  )
}

# Monitor parameter yang sudah ter-constraint (bx, kt) + ax
params <- c("ax", "bx", "kt", "tau_obs", "tau_kt")

# === STEP 3: Jalankan OpenBUGS ===
# (kamu tadi minta 'banyakin lagi'; contoh ini pakai 100k iterasi. Silakan turunkan ke 50k kalau mau cepat.)
set.seed(123)
result_pilot <- bugs(
  data = dataList,
  inits = replicate(4, inits(), simplify = FALSE),
  parameters.to.save = params,
  model.file = model_path,
  n.chains = 4,     # kurangi chain dulu
  n.iter = 10000,   # pilot 10k
  n.burnin = 2000,
  n.thin = 1,       # jangan thinning dulu
  debug = FALSE,    # GUI bisa bikin sedikit lambat
  OpenBUGS.pgm = OPENBUGS_PATH
)
library(coda)
ml <- as.mcmc.list(result_pilot)
print(gelman.diag(ml, autoburnin = FALSE))
library(coda)

# 1. Definisikan Parameter yang Ingin Diplot (Perwakilan + Tau)
# Anda bisa menyesuaikan indeks di sini (misalnya ax[1] dan ax[20])
parameters_to_plot <- c(
  "ax[1]",
  "bx[1]",
  "kt[1]",
  "tau_kt", "tau_obs"
)

# 2. Buat Subset (hanya parameter yang dipilih)
ml_subset <- ml[, parameters_to_plot]

# 3. Tampilkan Trace Plot untuk Subset Parameter
plot(ml_subset)
# === STEP 4: Simpan hasil supaya tidak perlu rerun ===
saveRDS(result_pilot, file.path(OUT_DIR, "lee_carter_result_constrained.rds"))
# Kalau perlu format .RData:
save(result_pilot, file = file.path(OUT_DIR, "lee_carter_result_constrained.RData"))
# load hasil
result<- load("lee_carter_result_constrained.RData")
# cek isinya
print(result)
ax_hat <- colMeans(result_pilot$sims.list$ax)

# 2) Ambil bx_hat (Rata-rata Sensitivitas Usia)
bx_hat <- colMeans(result_pilot$sims.list$bx)

# 3) Ambil kt_hat (Rata-rata Tren Periode)
kt_hat <- colMeans(result_pilot$sims.list$kt)
X_len <- length(ax_hat)
T_len <- length(kt_hat) 

# 1. Definisikan Vektor Usia dan Tahun yang konsisten
ages <- seq(0, by = 5, length.out = X_len) # Usia awal (misalnya 0, 5, ..., 95)
START_YEAR_TARGET <- 1950
END_YEAR_TARGET <- 2020 # Digunakan sebagai tahun terakhir yang valid di subset
years_hist_vector <- seq(START_YEAR_TARGET, by = 5, length.out = T_len) # Buat vektor tahun sesuai panjang kt_hat

# --- 1. Plot a_x dan b_x ---
plot(ages, ax_hat, type = "b", pch = 16, col = "darkgreen",
     xlab = "Umur", ylab = expression(alpha[x]), 
     main = "Rata-rata log mortalitas pada usia (αx)")

plot(ages, bx_hat, type = "b", pch = 16, col = "blue",
     xlab = "Umur", ylab = expression(beta[x]),
     main = "Sensitivitas log mortalitas pada waktu (βx)")


# --- 2. PERSIAPAN TIME SERIES KAPPA_T ---

# Cari indeks awal dan akhir yang valid di vektor tahun
index_start <- which(years_hist_vector == START_YEAR_TARGET)
index_end <- which(years_hist_vector == END_YEAR_TARGET)

if (length(index_start) == 0 || length(index_end) == 0) {
  stop("Tahun 1950 atau 2020 tidak ditemukan dalam vektor years_hist_vector.")
}

# Potong vektor k_t dan vektor tahun pada indeks yang benar
k_t_subset <- kt_hat[index_start:index_end]
years_subset <- years_hist_vector[index_start:index_end]

# 3. Buat time series object yang sudah dikoreksi (kt_ts_corrected)
kt_ts_corrected <- ts(k_t_subset, start = START_YEAR_TARGET, frequency = 0.2) 
# 4. Plot ulang grafik historis
plot(kt_ts_corrected, 
     type = "b", 
     pch = 16, 
     col = "red",
     xlab = "Tahun", 
     ylab = expression(kappa[t]), 
     main = "k(t): Tren Mortalitas Historis")

# --- 5. ANALISIS ARIMA ---
library(forecast)
# KOREKSI: auto.arima harus dijalankan pada objek time series yang sudah dikoreksi
best_model_auto <- auto.arima(kt_ts_corrected, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)

cat("\n== Hasil auto.arima (Model Terbaik) ==\n")
print(summary(best_model_auto)) 


library(tidyverse)
library(forecast) # Pastikan dimuat

# --- 1. Tentukan Tahun Proyeksi ---
# Hitung tahun awal proyeksi (misalnya 2025)
tahun_awal_proj <-2025# Gunakan tahun_terakhir + interval waktu
h_forecast <- 10 

# Jalankan kembali fungsi forecast()
# Asumsi 'best_model_auto' sudah didefinisikan
kt_forecast_result <- forecast(best_model_auto, h = h_forecast) 

# --- 2. Kemudian jalankan baris yang error ---
# Asumsi tahun_awal_proj sudah didefinisikan
tahun_proyeksi_vektor <- seq(from = 2025, by = 5, length.out = length(kt_forecast_result$mean))
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
  theme(
    plot.title = element_text(face = "bold"),
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )

print(kt_forecast_plot)


# Ambil semua kt dan tahun yang telah digabungkan (Historis + Proyeksi)
# Catatan: kt_all_df sudah dibuat di bagian plotting.
kt_selected <- kt_all_df$kt_Mean
years_selected <- kt_all_df$Tahun

# --- 6) Hitung log(mx), mx, qx, px untuk seluruh periode terpilih ---
# Pastikan dimensi ax/bx cocok: ax,bx panjang = jumlah usia (I)
if (length(ax_hat) != length(bx_hat)) stop("Panjang ax_hat dan bx_hat beda.")
I <- length(ax_hat)
T_sel <- length(kt_selected) # T_sel sekarang terdefinisi dengan benar

# Buat matriks log_mx_hat: rows = ages, cols = waktu terpilih
# ax_hat dan bx_hat ASUMSI sudah didefinisikan dari output MCMC sebelumnya
log_mx_hat <- outer(ax_hat, rep(1, T_sel)) + outer(bx_hat, kt_selected)
mx_hat <- exp(log_mx_hat)
# 1. Ambil vektor sampel presisi (tau_obs)
tau_obs_samples <- result_pilot$sims.list$tau_obs

# 2. Hitung rata-rata Presisi
tau_obs_mean <- mean(tau_obs_samples)

# 3. Hitung Varians Error (Sigma Kuadrat)
sigma2_hat <- 1 / tau_obs_mean

# 4. Hitung Deviasi Standar Error (Sigma)
sigma_hat <- sqrt(sigma2_hat)

cat(sprintf("Varians Error (Sigma^2) adalah: %.6f\n", sigma2_hat))
cat(sprintf("Deviasi Standar (Sigma) adalah: %.4f\n", sigma_hat))
library(ggplot2)
I_data <- nrow(Y) 
Tt_data <- ncol(Y)

# --- RE-HITUNG mu_hat MENGGUNAKAN DIMENSI Y ---
# Matriks Alpha (Alpha_x diperluas ke semua tahun)
alpha_matrix <- outer(ax_hat, rep(1, Tt_data)) 

# Matriks Beta * Kappa (Beta_x * Kappa_t)
beta_kappa_matrix <- outer(bx_hat, kt_hat)

# KOREKSI: Gunakan dimensi yang sama
# Ini adalah matriks fitted mean log-mortalitas:
log_mx_hat <- alpha_matrix + beta_kappa_matrix

# --- PENGHITUNGAN RESIDUALS ---
# Residuals_matrix = Observed - Fitted
residuals_matrix <- Y - log_mx_hat
residuals_vector_clean <- as.vector(residuals_matrix)
residuals_vector_clean <- residuals_vector_clean[is.finite(residuals_vector_clean)]

# 2. Buat dataframe dan plot Q-Q Plot
residuals_df <- data.frame(Residuals = residuals_vector_clean)

qq_plot <- ggplot(residuals_df, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Normal Quantile-Quantile (Q-Q) Plot Residuals Bayesian LC",
    subtitle = "Mendukung asumsi error N(0, σ²)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 14)


print(qq_plot)
# probabilitas kematian & bertahan per interval n = 5 tahun
n <- 5
qx_hat <- 1 - exp(-n * mx_hat)
qx_hat[qx_hat > 1] <- 1
px_hat <- 1 - qx_hat

# --- 7) Label kolom dengan tahun, dan baris dengan usia (jika ada nama) ---
colnames(mx_hat) <- years_selected
colnames(px_hat) <- years_selected

# Jika kamu punya label age group dari input nMx_raw:
age_cols <- rownames(log_mx_mat) # Menggunakan rownames dari matriks input
if (length(age_cols) == nrow(mx_hat)) {
  rownames(mx_hat) <- age_cols
  rownames(px_hat) <- age_cols
} else {
  # fallback: beri nama baris "age1, age2, ..."
  rownames(mx_hat) <- paste0("age", seq_len(nrow(mx_hat)))
  rownames(px_hat) <- paste0("age", seq_len(nrow(px_hat)))
}

# === STEP 1: Load dan Persiapkan Data Fertilitas ===
fert <- read.csv("D:/skripsi/fertility.csv", sep = ";")

# Mapping midpoint umur
age_midpoints <- c(
  "15-19" = 17, "20-24" = 22, "25-29" = 27,
  "30-34" = 32, "35-39" = 37, "40-44" = 42, "45-49" = 47
)

library(dplyr)
library(tidyr)

# --- STEP 1: Bersihkan & bagi 1000 ---
fert_clean <- fert %>%
  mutate(
    population = as.numeric(gsub(",", ".", population)) / 1000,  # bagi seribu
    age = age_midpoints[age]
  ) %>%
  filter(!is.na(age), !is.na(population))

# --- STEP 2: Interpolasi setiap 5 tahun ---
# Tahun target (kelipatan 5)
tahun_asli <- sort(unique(fert_clean$year))
tahun_target <- seq(min(tahun_asli), max(tahun_asli), by = 5)

fert_interp <- data.frame()

for (a in unique(fert_clean$age)) {
  df_age <- fert_clean %>% filter(age == a) %>% arrange(year)
  
  # fungsi approx untuk interpolasi nilai populasi pada tahun_target
  interp_vals <- approx(x = df_age$year,
                        y = df_age$population,
                        xout = tahun_target,
                        rule = 2)$y
  
  fert_interp <- bind_rows(
    fert_interp,
    data.frame(age = a,
               year = tahun_target,
               population = interp_vals)
  )
}

# --- STEP 3: Bentuk matriks fertilitas per 5 tahun ---
fert_matrix <- fert_interp %>%
  pivot_wider(names_from = year, values_from = population) %>%
  arrange(age) %>%
  column_to_rownames("age") %>%
  as.matrix()

log_fert_matrix <- log(fert_matrix)

y_fert <- log_fert_matrix
X_fert <- nrow(y_fert)
T_fert <- ncol(y_fert)
data_fert <- list(log_fert = y_fert, X = X_fert, T = T_fert)

# === STEP 4: Simpan Model OpenBUGS ===
model_path_fert <- "D:/skripsi/lee_carter_fertility_model.txt"
writeLines("
model {
  for (x in 1:X) {
    for (t in 1:T) {
      log_fert[x, t] ~ dnorm(mu[x, t], tau_obs)
      mu[x, t] <- ax[x] + bx[x] * kt[t]
    }
    ax[x] ~ dnorm(0, 0.01)
    bx[x] ~ dnorm(0.1, 0.01)T(0,)
  }
  for (t in 2:T) {
    kt[t] ~ dnorm(kt[t-1], tau_kt)
  }
  kt[1] ~ dnorm(0, 0.01)
  tau_obs ~ dgamma(0.1, 0.1)
  tau_kt ~ dgamma(0.1, 0.1)
}", con = model_path_fert)

# === STEP 5: Fitting Bayesian ===
inits_fert <- function() {
  list(
    ax = rnorm(X_fert, 0, 0.1),
    bx = abs(rnorm(X_fert, 0.05, 0.05)),
    kt = rnorm(T_fert, 0, 0.1),
    tau_obs = runif(1, 0.5, 2),
    tau_kt = runif(1, 0.5, 2)
  )
}
params_fert <- c("ax", "bx", "kt","tau_obs","tau_kt")

result_fert <- bugs(
  data = data_fert,
  inits = replicate(4, inits_fert(), simplify = FALSE),
  parameters.to.save = params_fert,
  model.file = model_path_fert,
  n.chains = 4, n.iter = 120000, n.burnin = 20000, n.thin = 50,
  debug = TRUE,
  OpenBUGS.pgm = "C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
)
print(result_fert)
save(result_fert, file = "bugs_result_fert.RData")
load("bugs_result_fert.RData")
sims <- result_fert$sims.array   # array: iterations x chains x parameters

# 2. Ubah tiap chain jadi mcmc object
chains <- lapply(1:dim(sims)[2], function(ch) {
  mcmc(sims[, ch, ])  # ambil chain ke-ch
})

# 3. Gabungkan semua chain ke mcmc.list
mcmc_list <- mcmc.list(chains)

# 4. Gelman-Rubin diagnostic
rhats <- gelman.diag(mcmc_list, autoburnin = FALSE, multivariate = FALSE)
print(rhats)

# 5. Traceplot contoh parameter
traceplot(mcmc_list[, c("ax[1]", "bx[1]", "kt[1]","tau_obs","tau_kt")])
library(forecast)


ax_fert <- colMeans(result_fert$sims.list$ax) # Alpha_x (Fertilitas)
bx_fert <- colMeans(result_fert$sims.list$bx) # Beta_x (Fertilitas)

# Vektor kt fertilitas historis (dari matriks)
kt_mat <- matrix(result_fert$sims.list$kt, ncol = ncol(log_fert_matrix), byrow = TRUE)
kt_fert_hat <- colMeans(result_fert$sims.list$kt) 
X_len_fert <- length(ax_fert)
Age_Groups_Fert <- seq(15, by = 5, length.out = X_len_fert) 

# Tahun Historis (Asumsi interval 5 tahunan, disesuaikan dengan panjang kt_hat)
T_len_fert <- length(kt_fert_hat) 
Years_Fert <- seq(1970, by = 5, length.out = T_len_fert) 

# Data Frame Alpha dan Beta
fert_params_df <- data.frame(
  Age = Age_Groups_Fert,
  Alpha = ax_fert,
  Beta = bx_fert
)

# Data Frame Kappa
kt_fert_df <- data.frame(
  Year = Years_Fert,
  Kappa_t = kt_fert_hat
)

# --- 2. Plot Alpha_x (Intersep Usia Fertilitas) ---
alpha_fert_plot <- ggplot(fert_params_df, aes(x = Age, y = Alpha)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Intersep Spesifik Usia Fertilitas" ,
       x = "Usia (Tahun)",
       y = expression(alpha[x])) +
  scale_x_continuous(breaks = Age_Groups_Fert) +
  theme_minimal(base_size = 13) +
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )

print(alpha_fert_plot)

# --- 3. Plot Beta_x (Sensitivitas Usia Fertilitas) ---
beta_fert_plot <- ggplot(fert_params_df, aes(x = Age, y = Beta)) +
  geom_line(color = "red") +
  geom_point(color = "red", size = 3) +
  labs(title = "Sensitivitas Usia Fertilitas terhadap Tren",
       x = "Usia (Tahun)",
       y = expression(beta[x])) +
  scale_x_continuous(breaks = Age_Groups_Fert) +
  theme_minimal(base_size = 13) +
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )
print(beta_fert_plot)

# --- 4. Plot Kappa_t (Tren Periode Fertilitas) ---
kt_fert_plot <- ggplot(kt_fert_df, aes(x = Year, y = Kappa_t)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Tren Periode Fertilitas Historis",
       x = "Tahun",
       y = expression(kappa[t])) +
  scale_x_continuous(breaks = seq(min(Years_Fert), max(Years_Fert), by = 10)) +
  theme_minimal(base_size = 13) +
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )
print(kt_fert_plot)
# --- KONVERSI TIME SERIES UNTUK FORECASTING ---
years_hist_fert <- as.numeric(colnames(log_fert_matrix))
kt_ts_fert <- ts(kt_fert_hat, start = min(years_hist_fert), frequency = 5) # Frequency 5 untuk 5-tahunan
best_model_auto <- auto.arima(kt_ts_fert, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)

cat("\n== Hasil auto.arima (Model Terbaik) ==\n")
print(summary(best_model_auto))
h_forecast<-10
# 2. Lakukan Peramalan (Forecast)
kt_forecast_result <- forecast(best_model_auto, h = h_forecast) 


# === 2. BLOK FORECAST MORTALITAS (Diperlukan untuk kt_all_df) ===
kt_forecast_result <- forecast(best_model_auto, h = h_forecast) 
tahun_terakhir_hist <- end(kt_ts_fert)[1]

# 2. Hitung tahun awal proyeksi (TAHUN TERAKHIR + 5 tahun)
tahun_awal_proj <- 2020 + 5 

# Sekarang, kode Anda akan berjalan:
tahun_proyeksi_vektor <- seq(from = tahun_awal_proj, 
                             by = 5, 
                             length.out = length(kt_forecast_result$mean))
tahun_proyeksi_vektor <- seq(from = tahun_awal_proj, by = 5, length.out = length(kt_forecast_result$mean))

# Buat Data Frame Proyeksi (kt_proj_df)
kt_proj_df <- tibble(
  Tahun = tahun_proyeksi_vektor,
  kt_Mean = as.vector(kt_forecast_result$mean),
  Lower_95 = as.vector(kt_forecast_result$lower[, "95%"]),
  Upper_95 = as.vector(kt_forecast_result$upper[, "95%"])
)

# Konversi Data Historis (Mortality) ke Tidy Format dan Gabungkan
START_YEAR_HIST <- 1971
T_len_hist <- length(kt_fert_hat)

# --- 1. KOREKSI KRUSIAL: Definisikan Time Series dengan Frequency 0.2 ---
# kt_ts_corrected adalah objek ts yang benar
kt_ts_corrected <- ts(kt_fert_hat, start = START_YEAR_HIST, frequency = 0.2) 

# 2. Buat Data Frame Historis (menggunakan objek ts yang sudah benar)
kt_hist_df <- tibble(
  # time() pada objek ts yang benar akan menghasilkan 1971.0, 1976.0, 1981.0, dst.
  Tahun = as.numeric(time(kt_ts_corrected)), 
  kt_Mean = as.vector(kt_fert_hat),
  Lower_95 = as.vector(kt_fert_hat),
  Upper_95 = as.vector(kt_fert_hat)
)
kt_all_df <- bind_rows(kt_hist_df, kt_proj_df)
tahun_pisah <- 2020
kt_forecast_plot <- ggplot(kt_all_df, aes(x = Tahun, y = kt_Mean)) +
  
  # A. Tambahkan Pita Interval Prediksi 95% (Area abu-abu)
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "gray", alpha = 0.4) +
  
  # B. Garis Estimasi Rata-rata (Garis Hitam)
  geom_line(color = "black", linewidth = 1) +
  
  # C. Garis Vertikal (Menandakan awal proyeksi)
  geom_vline(xintercept = tahun_pisah, linetype = "dashed", color = "red") +
  
  # D. Label dan Judul
  labs(
    title = "Tren Historis dan Proyeksi Mortalitas (κt) Jangka Panjang",
    subtitle = "Area abu-abu menunjukkan Interval Prediksi 95%",
    x = "Tahun",
    y = expression(kappa[t])
  ) +
  
  # 4. Penataan Tema dan Skala
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(kt_all_df$Tahun), max(kt_all_df$Tahun), by = 10)) +
  theme(plot.title = element_text(face = "bold")) +
  theme(
    # KOREKSI: Tambahkan kotak luar (border)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) 
  )

print(kt_forecast_plot)
# === 3. PERHITUNGAN MATRIKS FERTILITAS FINAL ===

# 1. Forecast kt_fert
kt_fert_forecast <- as.numeric(forecast(best_model_auto, h = h_forecast)$mean)

# 2. Gabungkan Historis dan Forecast untuk Fertilitas
kt_fert_all <- c(as.numeric(kt_fert_hat), kt_fert_forecast)
years_fert_all <- c(years_hist_fert, seq(max(years_hist_fert) + 5, by = 5, length.out = h_forecast))
target_start <- 2020
target_end <- 2070
# 3. Ambil Subset Tahun yang Sesuai dengan Proyeksi (2020 - 2070)
sel_fert <- (years_fert_all >= target_start) & (years_fert_all <= target_end)
kt_fert_selected <- kt_fert_all[sel_fert]
years_fert_selected <- years_fert_all[sel_fert] 

# 4. Prediksi Fertilitas (ASFRs)
log_fert_hat <- outer(ax_fert, rep(1, length(kt_fert_selected))) +
  outer(bx_fert, kt_fert_selected)
fert_hat <- exp(log_fert_hat)
fert_hat<-fert_hat*5
TFR_TARGET<-2.18
# 5. Normalisasi ke Target TFR (2.18) dan Label Final
fert_hat <- apply(fert_hat, 2, function(col) {
  if(sum(col, na.rm = TRUE) > 0) col / sum(col, na.rm = TRUE)  else col
})

# Finalisasi Matriks
rownames(fert_hat) <- c(15,20,25,30,35,40,45)
colnames(fert_hat) <- c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070)

cat("\n✅ Matriks Fertilitas Final (fert_hat) telah dibuat dan siap untuk Leslie Matrix.\n")
print(head(fert_hat))
# 1. Ambil vektor sampel presisi (tau_obs)
tau_obs_samples <- result_fert$sims.list$tau_obs

# 2. Hitung rata-rata Presisi
tau_obs_mean <- mean(tau_obs_samples)

# 3. Hitung Varians Error (Sigma Kuadrat)
sigma2_hat <- 1 / tau_obs_mean

# 4. Hitung Deviasi Standar Error (Sigma)
sigma_hat <- sqrt(sigma2_hat)

cat(sprintf("Varians Error (Sigma^2) adalah: %.6f\n", sigma2_hat))
cat(sprintf("Deviasi Standar (Sigma) adalah: %.4f\n", sigma_hat))
library(ggplot2)
I_data <- nrow(y_fert) 
Tt_data <- ncol(y_fert)

# --- RE-HITUNG mu_hat MENGGUNAKAN DIMENSI Y ---
# Matriks Alpha (Alpha_x diperluas ke semua tahun)
alpha_matrix <- outer(ax_fert, rep(1, Tt_data)) 

# Matriks Beta * Kappa (Beta_x * Kappa_t)
beta_kappa_matrix <- outer(bx_fert, kt_fert_hat)

# KOREKSI: Gunakan dimensi yang sama
# Ini adalah matriks fitted mean log-mortalitas:
log_mx_hat <- alpha_matrix + beta_kappa_matrix

# --- PENGHITUNGAN RESIDUALS ---
# Residuals_matrix = Observed - Fitted
residuals_matrix <- y_fert - log_mx_hat
residuals_vector_clean <- as.vector(residuals_matrix)
residuals_vector_clean <- residuals_vector_clean[is.finite(residuals_vector_clean)]

# 2. Buat dataframe dan plot Q-Q Plot
residuals_df <- data.frame(Residuals = residuals_vector_clean)

qq_plot <- ggplot(residuals_df, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1.2, linetype = "dashed") +
  
  labs(
    title = "Normal Quantile-Quantile (Q-Q) Plot Residuals Bayesian LC",
    subtitle = "Mendukung asumsi error N(0, σ²)"
  ) +
  theme_minimal(base_size = 14)

print(qq_plot)
# === OUTPUT SIAP PAKAI DI LESLIE MATRIX ===
# Gunakan:
# log_asfr <- ax_fert + bx_fert * kt_fert_proj[i]
# asfr <- exp(log_asfr)


# === STEP 3: Proyeksi Populasi Wanita ===
tahun_tersedia <- intersect(colnames(fert_hat), colnames(px_hat))
tahun <- as.numeric(tahun_tersedia)
n_years <- length(tahun)
## === DATA AWAL ===
# --- DATA AWAL ---
pop2020 <- data.frame(
  Kelompok_Umur = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75+"),
  Perempuan = c(10778.8, 10799, 10746.1, 10816.9, 11050.1, 10945.2,
                10795.5, 10354.3, 9928.5, 8996.9, 7874, 6574.5,
                5117.8, 3772.6, 2374.9, 2617)
)


# --- PERBAIKI SETUP TAHUN ---
# Gunakan hanya tahun yang tersedia di kedua parameter
tahun_tersedia <- intersect(colnames(px_hat), colnames(fert_hat))
tahun_tersedia <- as.numeric(tahun_tersedia)
tahun_proyeksi <- seq(2025, 2070, by = 5)
pop_vector <- pop2020$Perempuan

# --- Mendefinisikan variabel yang dibutuhkan ---
# Variabel yang menyebabkan error
n_age <- length(pop_vector)
# --- PROYEKSI ---
n_proj <- length(tahun_proyeksi)
pop_matrix <- matrix(0, nrow = n_age, ncol = n_proj + 1)
pop_matrix[, 1] <- pop_vector

cat("Memulai proyeksi...\n")
umur_vector <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                 "60-64", "65-69", "70-74", "75+")
idx_subur <- which(umur_vector %in% rownames(fert_hat))
start_col_index <- which(colnames(px_hat) == "2020")

# 2. Ambil SEMUA baris (usia) dari indeks tersebut hingga kolom terakhir
px_2025 <- px_hat[, start_col_index:ncol(px_hat)]
# Asumsi: TFR_TARGET (misalnya 2.18) sudah didefinisikan di luar loop.

TFR_TARGET <- 2.18    # TFR target BPS
SRB_FEMALE <- 0.488  # Rasio perempuan saat lahir
valid_fert_ages <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
umur_vector <- rownames(pop_matrix) # Asumsi nama baris sudah ada

# Hitung Indeks Usia Subur
idx_subur <- which(umur_vector %in% valid_fert_ages)

# Inisialisasi pop_matrix
pop_matrix <- matrix(NA, nrow = n_age, ncol = n_proj + 1)
pop_matrix[, 1] <- pop_vector
colnames(pop_matrix) <- c(2020, tahun_proyeksi)
rownames(pop_matrix) <- umur_vector


PROJECTION_START_YEAR <- 2025
TARGET_END_YEAR <- 2070

# 1. Cari indeks kolom untuk tahun 2025
start_col_index <- which(colnames(px_hat) == as.character(PROJECTION_START_YEAR))

# 2. Cari indeks kolom untuk tahun 2070
end_col_index <- which(colnames(px_hat) == as.character(TARGET_END_YEAR))

# Cek: Jika salah satu indeks tidak ditemukan, gunakan yang ada
if (length(start_col_index) == 0 || length(end_col_index) == 0) {
  stop("Gagal menemukan tahun target dalam px_hat. Pastikan tahun tersebut ada.")
}

# 3. Buat Matriks Subset Final (px_proj)
px_hat <- px_hat[, start_col_index:end_col_index]
TARGET_ROWS <- 16 

# Subsetting matriks px_hat_f
px_hat <- px_hat[1:TARGET_ROWS, ]

TFR_TARGET <- 2.18
DURATION <- 5
SRB_FEMALE <- 0.488
SCALING_FACTOR_NUMERIC <- TFR_TARGET * DURATION * SRB_FEMALE # 5.3192

idx_subur <- 4:10 

# -------------------------------------------------------------
SRB_FEMALE_PROPORTION <- 1/2.06 # Proporsi bayi perempuan (1 / 2.04)
# Asumsi: kt_mort_proj, kt_fert_proj, ax_mort, ax_fert, dll. sudah didefinisikan.
# -------------------------------------------------------------------------

SCALING_FACTOR_NUMERIC <- TFR_TARGET * DURATION * SRB_FEMALE 
# ----------------------------------------------------------------------
tahun5_all<-c(2025,2030,2035,2040,2045,2050,2055,2060,2065,2070)

# --- ASUMSI: TFR_TARGET, DURATION (5), SRB_FEMALE sudah didefinisikan ---
for(t in 1:n_proj){
  L_f<- matrix(0, nrow = n_age, ncol = n_age)
  
  # 1. Ambil rates
  f_total_year <- fert_hat[, as.character(tahun5_all[t])]
  current_px <- px_hat[, as.character(tahun5_all[t])]
  
  # 2. NET MATERNITY FUNCTION (f_male)
  f_female <- f_total_year * TFR_TARGET * SRB_FEMALE
  
  # Mapping ke vektor f1
  f1_vector_f <- rep(0, n_age)
  f1_vector_f[idx_subur] <- f_female
  
  # 3. Bangun Leslie Matrix (L_M)
  L_f[1, idx_subur] <- f1_vector_f[idx_subur] # Kelahiran
  
  # 4. Survival Sub-diagonal (Survival Pria hingga 70-74)
  for(j in 2:n_age){
    L_f[j, j-1] <- current_px[j-1]
  }
  
  
  # --- Proyeksi populasi ---
  next_pop <- L_f %*% matrix(pop_matrix[, t], ncol = 1)
  pop_matrix[, t+1] <- as.vector(next_pop)
}

cat("✅ Proyeksi populasi wanita selesai (menggunakan Px mentah).\n")

# --- HASIL ---
colnames(pop_matrix) <- c(2020, tahun_proyeksi)
rownames(pop_matrix) <- umur_vector

print("Proyeksi berhasil!")
print(pop_matrix)

library(tidyverse)

# ubah matrix jadi data frame panjang
pop_df <- as.data.frame(pop_matrix) %>%
  rownames_to_column(var = "AgeGroup") %>%      # AgeGroup dari rownames
  pivot_longer(-AgeGroup, names_to = "Year", values_to = "Population")

# cek hasil
head(pop_df)
target_year=2070
library(dplyr)

pop_df <- pop_df %>%
  mutate(
    # ambil angka pertama sebelum tanda "-" atau "+"
    AgeStart = as.numeric(sub("^(\\d+).*", "\\1", AgeGroup))
  )
pop_df <- pop_df %>%
  arrange(AgeStart) %>%
  mutate(
    AgeGroup = factor(AgeGroup, levels = unique(AgeGroup))
  )


library(ggplot2)
ggplot(filter(pop_df, Year == target_year),
       aes(x = AgeGroup, y = Population)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  labs(title = paste("Piramida Penduduk Perempuan Tahun", target_year),
       x = "Kelompok Umur",
       y = "Populasi") +
  theme_minimal(base_size = 14)
# ================== PIPELINE LEE–CARTER BAYESIAN (OpenBUGS) MALE ==================
# Data input: sudah age-grouped (0-4, 5-9, 10-14, dst)
# File format: kolom 'Year', lalu kolom age group
# ============================================================================

# --- 0. Libraries
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(R2OpenBUGS)
  library(coda)
  library(tibble)
})

RAW_EXCEL_PATH_M <- "D:/skripsi/nmx_m.xlsx"

# --- 3. Baca Excel tanpa header ---
nMx_raw_male <- read_excel(RAW_EXCEL_PATH_M,sheet="Sheet2")
OPENBUGS_PATH  <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
OUT_DIR        <- getwd()
MODEL_FILE     <- file.path(OUT_DIR, "lc1_clean.txt")


# Pastikan ada kolom Year
stopifnot("Year" %in% names(nMx_raw_male))
age_cols <- setdiff(names(nMx_raw_male), "Year")

# --- 2. Pilih tahun tiap 5 tahun saja ---
years_5yr <- seq(min(nMx_raw_male$Year), max(nMx_raw_male$Year), by = 5)
nMx_5yr <- nMx_raw_male %>% filter(Year %in% years_5yr)

# --- 3. Bentuk matriks log(nMx) [age x 5yr interval] ---
log_mx_mat <- nMx_5yr %>%
  pivot_longer(-Year, names_to = "age_group", values_to = "nMx") %>%
  pivot_wider(names_from = Year, values_from = nMx) %>%
  column_to_rownames("age_group") %>%
  as.matrix()

# cek semua nilai valid
if (any(log_mx_mat <= 0, na.rm = TRUE)) stop("Ada nMx <= 0, tidak bisa log-transform.")

Y  <- log(log_mx_mat)
I  <- nrow(Y)
Tt <- ncol(Y)

dataList <- list(I = I, Tt = Tt, y = Y)

# === STEP 1: Simpan model ke file (pakai useBytes supaya encoding aman) ===
model_path <- "D:/skripsi/lee_carter_model.txt"

# Model dengan identifiability constraints:
# - sum_x bx = 1  (realized via bx = bx_raw / sum(bx_raw))
# - sum_t kt = 0  (realized via kt = kt_raw - mean(kt_raw))
writeLines("
model {
  # --- Likelihood ---
  for (x in 1:I) {
    for (t in 1:Tt) {
      y[x, t] ~ dnorm(mu[x, t], tau_obs)
      mu[x, t] <- ax[x] + bx[x] * kt[t]
    }
    # prior untuk ax
    ax[x] ~ dnorm(0, 0.01)
    # prior untuk bx_raw (positif), nanti dinormalisasi
    bx_raw[x] ~ dnorm(0.1, 0.01) T(0, )
  }

  # --- Random walk untuk kt_raw ---
  kt_raw[1] ~ dnorm(0, 0.01)
  for (t in 2:Tt) {
    kt_raw[t] ~ dnorm(kt_raw[t-1], tau_kt)
  }

  # --- Identifiability constraints (deterministic transforms) ---
  sum_bx_raw <- sum(bx_raw[])
  for (x in 1:I) {
    bx[x] <- bx_raw[x] / sum_bx_raw          # sum_x bx = 1
  }
  mean_kt_raw <- sum(kt_raw[]) / Tt
  for (t in 1:Tt) {
    kt[t] <- kt_raw[t] - mean_kt_raw         # sum_t kt = 0
  }

  # --- Priors untuk varians ---
  tau_obs ~ dgamma(2, 2)
  tau_kt  ~ dgamma(2, 2)
}
", con = model_path, useBytes = TRUE)

# === STEP 2: Inisialisasi ===
# Inisialisasi hanya untuk node stokastik: ax, bx_raw, kt_raw, tau_obs, tau_kt
set.seed(123)
inits <- function() {
  list(
    ax      = rnorm(I, 0, 0.1),
    bx_raw  = abs(rnorm(I, 0.1, 0.05)),
    kt_raw  = c(0, cumsum(rnorm(Tt - 1, 0, 0.05))),  # RW init yang halus
    tau_obs = rgamma(1, 2, 2),
    tau_kt  = rgamma(1, 2, 2)
  )
}

# Monitor parameter yang sudah ter-constraint (bx, kt) + ax
params <- c("ax", "bx", "kt", "tau_obs", "tau_kt")

# === STEP 3: Jalankan OpenBUGS ===
# (kamu tadi minta 'banyakin lagi'; contoh ini pakai 100k iterasi. Silakan turunkan ke 50k kalau mau cepat.)
set.seed(123)
result_pilot_m <- bugs(
  data = dataList,
  inits = replicate(4, inits(), simplify = FALSE),
  parameters.to.save = params,
  model.file = model_path,
  n.chains = 4,     # kurangi chain dulu
  n.iter = 10000,   # pilot 10k
  n.burnin = 2000,
  n.thin = 1,       # jangan thinning dulu
  debug = FALSE,    # GUI bisa bikin sedikit lambat
  OpenBUGS.pgm = OPENBUGS_PATH
)
library(coda)
ml <- as.mcmc.list(result_pilot_m)
print(gelman.diag(ml, autoburnin = FALSE))
parameters_to_plot <- c(
  "ax[1]",
  "bx[1]",
  "kt[1]",
  "tau_kt", "tau_obs"
)

# 2. Buat Subset (hanya parameter yang dipilih)
ml_subset <- ml[, parameters_to_plot]

# 3. Tampilkan Trace Plot untuk Subset Parameter
plot(ml_subset)
# === STEP 4: Simpan hasil supaya tidak perlu rerun ===
saveRDS(result_pilot_m, file.path(OUT_DIR, "lee_carter_result_constrained_m.rds"))
# Kalau perlu format .RData:
save(result_pilot_m, file = file.path(OUT_DIR, "lee_carter_result_constrained_m.RData"))
# load hasil
result_male<- load("lee_carter_result_constrained_m.RData")
# cek isinya
print(result_male)

# --- 1) Ambil ax, bx, kt dari hasil MCMC ---
ax_hat <- colMeans(result_pilot_m$sims.list$ax)
bx_hat <- colMeans(result_pilot_m$sims.list$bx)
kt_hat <- colMeans(result_pilot_m$sims.list$kt)

X_len <- length(ax_hat)
T_len <- length(kt_hat) 

# 1. Definisikan Vektor Usia dan Tahun yang konsisten
ages <- seq(0, by = 5, length.out = X_len) # Usia awal (misalnya 0, 5, ..., 95)
START_YEAR_TARGET <- 1950
END_YEAR_TARGET <- 2020 # Digunakan sebagai tahun terakhir yang valid di subset
years_hist_vector <- seq(START_YEAR_TARGET, by = 5, length.out = T_len) # Buat vektor tahun sesuai panjang kt_hat

# --- 1. Plot a_x dan b_x ---
plot(ages, ax_hat, type = "b", pch = 16, col = "darkgreen",
     xlab = "Umur", ylab = expression(alpha[x]), 
     main = "Rata-rata log mortalitas pada usia (αx)")

plot(ages, bx_hat, type = "b", pch = 16, col = "blue",
     xlab = "Umur", ylab = expression(beta[x]),
     main = "Sensitivitas log mortalitas pada waktu (βx)")


# --- 2. PERSIAPAN TIME SERIES KAPPA_T ---

# Cari indeks awal dan akhir yang valid di vektor tahun
index_start <- which(years_hist_vector == START_YEAR_TARGET)
index_end <- which(years_hist_vector == END_YEAR_TARGET)

if (length(index_start) == 0 || length(index_end) == 0) {
  stop("Tahun 1950 atau 2020 tidak ditemukan dalam vektor years_hist_vector.")
}

# Potong vektor k_t dan vektor tahun pada indeks yang benar
k_t_subset <- kt_hat[index_start:index_end]
years_subset <- years_hist_vector[index_start:index_end]

# 3. Buat time series object yang sudah dikoreksi (kt_ts_corrected)
kt_ts_corrected <- ts(k_t_subset, start = START_YEAR_TARGET, frequency = 0.2) 
# 4. Plot ulang grafik historis
plot(kt_ts_corrected, 
     type = "b", 
     pch = 16, 
     col = "red",
     xlab = "Tahun", 
     ylab = expression(kappa[t]), 
     main = "k(t): Tren Mortalitas Historis")
library(forecast)
# --- 5. ANALISIS ARIMA ---
# KOREKSI: auto.arima harus dijalankan pada objek time series yang sudah dikoreksi
best_model_auto <- auto.arima(kt_ts_corrected, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)

cat("\n== Hasil auto.arima (Model Terbaik) ==\n")
print(summary(best_model_auto)) 


library(tidyverse)
library(forecast) # Pastikan dimuat

# --- 1. Tentukan Tahun Proyeksi ---
# Hitung tahun awal proyeksi (misalnya 2025)
tahun_awal_proj <- 2025 # Gunakan tahun_terakhir + interval waktu
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

# Ambil semua kt dan tahun yang telah digabungkan (Historis + Proyeksi)
# Catatan: kt_all_df sudah dibuat di bagian plotting.
kt_selected <- kt_all_df$kt_Mean
years_selected <- kt_all_df$Tahun

# --- 6) Hitung log(mx), mx, qx, px untuk seluruh periode terpilih ---
# Pastikan dimensi ax/bx cocok: ax,bx panjang = jumlah usia (I)
if (length(ax_hat) != length(bx_hat)) stop("Panjang ax_hat dan bx_hat beda.")
I <- length(ax_hat)
T_sel <- length(kt_selected) # T_sel sekarang terdefinisi dengan benar

# Buat matriks log_mx_hat: rows = ages, cols = waktu terpilih
# ax_hat dan bx_hat ASUMSI sudah didefinisikan dari output MCMC sebelumnya

log_mx_hat <- outer(ax_hat, rep(1, T_sel)) + outer(bx_hat, kt_selected)

mx_hat <- exp(log_mx_hat)
tau_obs_samples <- result_pilot_m$sims.list$tau_obs

# 2. Hitung rata-rata Presisi
tau_obs_mean <- mean(tau_obs_samples)

# 3. Hitung Varians Error (Sigma Kuadrat)
sigma2_hat <- 1 / tau_obs_mean

# 4. Hitung Deviasi Standar Error (Sigma)
sigma_hat <- sqrt(sigma2_hat)

cat(sprintf("Varians Error (Sigma^2) adalah: %.6f\n", sigma2_hat))
cat(sprintf("Deviasi Standar (Sigma) adalah: %.4f\n", sigma_hat))
library(ggplot2)
I_data <- nrow(Y) 
Tt_data <- ncol(Y)

# --- RE-HITUNG mu_hat MENGGUNAKAN DIMENSI Y ---
# Matriks Alpha (Alpha_x diperluas ke semua tahun)
alpha_matrix <- outer(ax_hat, rep(1, Tt_data)) 

# Matriks Beta * Kappa (Beta_x * Kappa_t)
beta_kappa_matrix <- outer(bx_hat, kt_hat)

# KOREKSI: Gunakan dimensi yang sama
# Ini adalah matriks fitted mean log-mortalitas:
log_mx_hat <- alpha_matrix + beta_kappa_matrix

# --- PENGHITUNGAN RESIDUALS ---
# Residuals_matrix = Observed - Fitted
residuals_matrix <- Y - log_mx_hat
residuals_vector_clean <- as.vector(residuals_matrix)
residuals_vector_clean <- residuals_vector_clean[is.finite(residuals_vector_clean)]

# 2. Buat dataframe dan plot Q-Q Plot
residuals_df <- data.frame(Residuals = residuals_vector_clean)

qq_plot <- ggplot(residuals_df, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1.2, linetype = "dashed") +
  
  labs(
    title = "Normal Quantile-Quantile (Q-Q) Plot Residuals Bayesian LC",
    subtitle = "Mendukung asumsi error N(0, σ²)",
    x = "Kuantil Teoritis",
    y = "Kuantil Sampel Residual"
  ) +
  theme_minimal(base_size = 14)


print(qq_plot)
# probabilitas kematian/bertahan (interval 5 tahun)
n <- 5
qx_hat <- 1 - exp(-n * mx_hat)
qx_hat[qx_hat > 1] <- 1
px_hat_m <- 1 - qx_hat

# --- 7) Label baris dan kolom ---
colnames(mx_hat) <- years_selected
colnames(px_hat_m) <- years_selected

# kalau punya AgeGroup (misalnya di nMx_raw_male$AgeGroup), gunakan itu
if ("AgeGroup" %in% names(nMx_raw_male)) {
  age_groups <-  c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79","80-84","85-89","90-95")
  if (length(age_groups) == nrow(mx_hat)) {
    rownames(mx_hat) <- age_groups
    rownames(px_hat_m) <- age_groups
  } else {
    rownames(mx_hat) <- paste0("age", seq_len(nrow(mx_hat)))
    rownames(px_hat_m) <- paste0("age", seq_len(nrow(px_hat)))
  }
} else {
  rownames(mx_hat) <- paste0("age", seq_len(nrow(mx_hat)))
  rownames(px_hat_m) <- paste0("age", seq_len(nrow(px_hat)))
}

# --- 8) Cek hasil ---
dim(mx_hat)  # baris=usia, kolom=tahun
dim(px_hat)
head(mx_hat)
head(px_hat)


# === 2. Populasi awal laki-laki 2020 ===
pop2020_m <- data.frame(
  Kelompok_Umur = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75+"),
  Laki_Laki = c(11293.7, 11295.3, 11449.8, 11495.7, 11632.2, 11410.8,
                11109.1, 10556.7, 10014.6, 9025.6, 7872.4, 6546.3,
                5091.7, 3681.5, 2179.1, 2007.5)
)
pop_vector_m <- pop2020_m$Laki_Laki   # pastikan nama kolom sesuai
n_age_m <- length(pop_vector_m)       # harusnya 16


# --- 1. Parameter dasar ---
valid_fert_ages <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
idx_subur <- which(pop2020_m$Kelompok_Umur %in% valid_fert_ages)


# tahun proyeksi 5 tahunan (misalnya)
tahun5_all <- seq(2025, 2070, by = 5)  # sesuaikan dengan tahunmu
n_proj <- length(tahun5_all)


# matriks populasi kosong (baris = usia, kolom = tahun)
pop_matrix_m <- matrix(0, nrow = n_age_m, ncol = n_proj + 1)
pop_matrix_m[, 1] <- pop_vector_m  # aman


SRB_MALE<-1.06/(1+1.06)
for(t in 1:n_proj){
  L_m <- matrix(0, nrow = n_age_m, ncol = n_age_m)
  
  # 1. Ambil rates
  f_total_year <- fert_hat[, as.character(tahun5_all[t])]
  current_px_m <- px_hat_m[, as.character(tahun5_all[t])]
  
  # 2. NET MATERNITY FUNCTION (f_male)
  f_male <- f_total_year * TFR_TARGET * SRB_MALE
  
  # Mapping ke vektor f1
  f1_vector_m <- rep(0, n_age_m)
  f1_vector_m[idx_subur] <- f_male
  
  # 3. Bangun Leslie Matrix (L_M)
  L_m[1, idx_subur] <- f1_vector_m[idx_subur] # Kelahiran
  
  # 4. Survival Sub-diagonal (Survival Pria hingga 70-74)
  for(j in 2:n_age_m){
    L_m[j, j-1] <- current_px_m[j-1]
  }
  

  # --- Proyeksi populasi ---
  next_pop <- L_m %*% matrix(pop_matrix_m[, t], ncol = 1)
  pop_matrix_m[, t+1] <- as.vector(next_pop)
}
# --- 3. Label hasil ---
colnames(pop_matrix_m) <- c(2020, tahun5_all)
rownames(pop_matrix_m) <- pop2020_m$Kelompok_Umur

# --- 4. Output data.frame ---
# Perbaikan: Konversi matriks dengan benar, gunakan nama kolom yang konsisten
pop_df_male <- as.data.frame(pop_matrix_m)
names(pop_df_male) <- c("2020", as.character(tahun5_all))
pop_df_male$Umur <- rownames(pop_matrix_m)
total_pop_m <- colSums(pop_matrix_m)
# --- 5. Plot ---
plot(c(2020, tahun5_all), total_pop_m, type="l", col="blue", lwd=2,
     main="Proyeksi 5-Tahunan Populasi Laki-laki (2020–2070)",
     xlab="Tahun", ylab="Jumlah Penduduk Laki-laki")

#----------------------------------------------------------
library(dplyr)
library(tidyr)
AGE_ORDER <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
               "60-64", "65-69", "70-74", "75+")

pop_df <- as.data.frame(pop_matrix) %>%
  rownames_to_column(var = "AgeGroup") %>%      # AgeGroup dari rownames
  pivot_longer(-AgeGroup, names_to = "Year", values_to = "Population")%>%
  mutate(
    Year = as.numeric(Year),
    Gender = "Female"
  )
library(dplyr)

# 1. Definisikan pemetaan (mapping) kelompok usia
# Asumsi: AgeGroup 1 = 0-4, AgeGroup 16 = 75+
age_labels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", 
  "25-29", "30-34", "35-39", "40-44", "45-49", 
  "50-54", "55-59", "60-64", "65-69", "70-74", 
  "75+" # Baris ke-16
)

# 2. Tambahkan kolom rentang usia baru ke pop_df
pop_df <- pop_df %>%
  # Konversi AgeGroup dari character ke integer agar bisa dicocokkan dengan indeks vektor
  mutate(AgeGroup_num = as.integer(AgeGroup)) %>% 
  # Tambahkan kolom AgeRange baru
  mutate(AgeGroup = age_labels[AgeGroup_num]) %>%
  # Hapus kolom AgeGroup numerik sementara jika tidak diperlukan lagi
  select(-AgeGroup_num)


pop_df_male <- as.data.frame(pop_matrix_m) %>%
  rownames_to_column(var = "AgeGroup") %>%
  pivot_longer(
    cols = -AgeGroup,
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Gender = "Male"
  )

# Gabungkan kedua data frame menjadi satu
pop_long <- bind_rows(pop_df, pop_df_male)

# --- KOREKSI: Terapkan Urutan Faktor ke Data Frame ---
pop_long <- pop_long %>%
  mutate(
    # Konversi AgeGroup menjadi faktor dan atur levelnya
    AgeGroup = factor(AgeGroup, levels = AGE_ORDER)
  )


library(ggplot2)
BASE_DIR <- "D:/skripsi" 
OUT_DIR <- file.path(BASE_DIR, "Output_Grafik") # Subfolder untuk menyimpan grafik

# Buat folder jika belum ada
if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE) # 'recursive = TRUE' akan membuat folder induk jika tidak ada
}


x<-ggplot(pop_long, aes(x = AgeGroup, y = Population, fill = Gender)) +
  geom_bar(data = filter(pop_long, Gender == "Female"), stat = "identity") +
  geom_bar(data = filter(pop_long, Gender == "Male"), aes(y = -Population), stat = "identity") +
  facet_wrap(~ Year, ncol = 4) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) abs(x),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    y = "Jumlah Penduduk (dalam ribuan)",
    x = "Kelompok Umur",
    title = "Proyeksi Piramida Populasi Indonesia 2020-2070"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF"))
ggsave(
  filename = file.path(OUT_DIR, "piramida_populasi_indonesia.png"), # Path lengkap + nama file
  plot = x,          # Objek plot yang ingin disimpan
  width = 15,        # Lebar plot dalam inci (sesuaikan agar tidak terlalu padat)
  height = 9,       # Tinggi plot dalam inci
  units = "in",      # Satuan
  dpi = 300          # Resolusi tinggi untuk publikasi/skripsi
)

library(ggplot2)
library(dplyr)
library(scales)

# 1. Tentukan daftar tahun unik yang akan di-loop
# Saya asumsikan Anda menggunakan pop_df_labelled (hasil dari langkah sebelumnya)
unique_years <- unique(pop_df$Year)

# 2. Tentukan direktori output (gunakan variabel yang sama dengan kode Anda sebelumnya)
# Pastikan variabel OUT_DIR sudah didefinisikan, misalnya:
# OUT_DIR <- "C:/Path/To/Output/Folder"
if (!exists("OUT_DIR")) {
  stop("Variabel OUT_DIR belum didefinisikan. Mohon definisikan path output Anda.")
}


# LOOP UNTUK MEMBUAT DAN MENYIMPAN SETIAP PLOT
for (tahun in unique_years) {
  
  # 2. Filter data untuk tahun saat ini
  data_tahun <- pop_long%>%
    filter(Year == tahun)
  
  # 3. Buat Plot Piramida Populasi
  plot_tahun <- ggplot(data_tahun, aes(x = AgeGroup, y = Population, fill = Gender)) +
    
    # Female (positif)
    geom_bar(data = filter(data_tahun, Gender == "Female"), stat = "identity") +
    
    # Male (negatif)
    geom_bar(data = filter(data_tahun, Gender == "Male"), aes(y = -Population), stat = "identity") +
    
    coord_flip() +
    
    # Atur skala Y (memastikan label sumbu tetap positif)
    scale_y_continuous(
      labels = function(x) abs(x), # Dibagi 1000 agar unitnya 'ribuan'
      breaks = scales::pretty_breaks(n = 6)
    ) +
    
    # Atur Judul dan Label
    labs(
      y = "Jumlah Penduduk (dalam ribuan)",
      x = "Kelompok Umur",
      title = paste("Piramida Populasi Indonesia Tahun", tahun) # Judul disesuaikan per tahun
    ) +
    
    # PERUBAHAN UTAMA 1: Base size diperbesar menjadi 18 untuk teks umum (tick labels)
    theme_minimal(base_size = 18) +
    scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF"),
                      labels = c("Female" = "Wanita", "Male" = "Pria")) +
    # PERUBAHAN UTAMA 2: Atur ukuran teks secara spesifik untuk Judul, Sumbu, dan Legenda
    theme(
      # Judul Grafik
      plot.title = element_text(size = 24, face = "bold"),
      # Label Sumbu X dan Y (Judul Sumbu)
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      # Judul dan Teks Legenda
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18) # Sama dengan base_size, tapi diulangi agar eksplisit
    )
  
  # 4. Simpan Plot
  filename <- paste0("piramida_populasi_LC1", tahun, ".png")
  ggsave(
    filename = file.path(OUT_DIR, filename),
    plot = plot_tahun,
    width = 10, # Perbesar width untuk mengakomodasi teks yang lebih besar
    height = 10,# Perbesar height
    units = "in",
    dpi = 300
  )
  
  cat(paste("Berhasil menyimpan", filename, "\n"))
}

