library(tidyverse)
library(ggplot2)
library(scales) # Untuk scales::comma
#cewe
# --- 1. Persiapan Data Frame Awal ---
# Memperbaiki penamaan variabel di tibble
total_pop_data_c <- tibble(
  Tahun = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070),
  LC_ = c(133542.1, 142791.10, 149229.53, 154494.09, 158819.48, 162554.85,
          165609.89, 169996.36, 174355.20, 178617.77, 182513.23),
  LC_Bayesian = c(133542.1,140157.1,147195.9,153093.4,157908.9,161606,165370,169215.7,173006.3,176764.6,180777.5)
)

# --- 2. Transformasi ke Format Panjang (Long Format) ---
# Mengubah kolom model (LC_Standard, LC_Bayesian) menjadi satu kolom 'Population'
pop_long <- total_pop_data_c %>%
  pivot_longer(
    cols = starts_with("LC_"),
    names_to = "Model",
    values_to = "Population_Ribuan"
  )

comparison_plot_enlarged <- ggplot(pop_long, aes(x = Tahun, y = Population_Ribuan, color = Model)) +
  
  # Garis dan Titik
  geom_line(linewidth = 1.5) + # Garis lebih tebal
  geom_point(size = 4) +      # Titik lebih besar
  
  # Label dan Judul
  labs(
    title = "Komparasi Proyeksi Total Populasi Wanita: Bayesian vs. Non-Bayesian",
    x = "Tahun Proyeksi",
    y = "Total Populasi (Ribuan)",
    color = "Metode Estimasi" # Memperjelas legend
  ) +
  
  # --- Penataan Tema (Pembesaran Font Menyeluruh) ---
  theme_minimal(base_size = 18) + # FONT DASAR DIBESARKAN
  
  # Penyesuaian Sumbu dan Legend
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2020, 2070, by = 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 20), # Judul utama lebih besar
    axis.title = element_text(size = 18),               # Label sumbu X/Y lebih besar
    axis.text = element_text(size = 16),                # Angka sumbu lebih besar
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )

print(comparison_plot_enlarged)
#cowo
total_pop_data <- tibble( Tahun = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070), LC_ = c( 136662.00, 143240.24, 149212.71, 153966.36, 157381.02, 159587.65, 167258.58, 170993.93, 174395.26, 177938.79, 180918.89), LC_Bayesian = c(136662.0, 143054.6, 150066.3, 156122.4, 161222.5, 165419.1, 167198.3, 171160.6, 174942.6, 178798.5, 181206.3) )
# --- 2. Transformasi ke Format Panjang (Long Format) ---
# Mengubah kolom model (LC_Standard, LC_Bayesian) menjadi satu kolom 'Population'
pop_long <- total_pop_data %>%
  pivot_longer(
    cols = starts_with("LC_"),
    names_to = "Model",
    values_to = "Population_Ribuan"
  )

comparison_plot_enlarged <- ggplot(pop_long, aes(x = Tahun, y = Population_Ribuan, color = Model)) +
  
  # Garis dan Titik
  geom_line(linewidth = 1.5) + # Garis lebih tebal
  geom_point(size = 4) +      # Titik lebih besar
  
  # Label dan Judul
  labs(
    title = "Komparasi Proyeksi Total Populasi Laki-Laki: Bayesian vs. Non-Bayesian",
    x = "Tahun Proyeksi",
    y = "Total Populasi (Ribuan)",
    color = "Metode Estimasi" # Memperjelas legend
  ) +
  
  # --- Penataan Tema (Pembesaran Font Menyeluruh) ---
  theme_minimal(base_size = 18) + # FONT DASAR DIBESARKAN
  
  # Penyesuaian Sumbu dan Legend
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2020, 2070, by = 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 20), # Judul utama lebih besar
    axis.title = element_text(size = 18),               # Label sumbu X/Y lebih besar
    axis.text = element_text(size = 16),                # Angka sumbu lebih besar
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )

print(comparison_plot_enlarged)
#TOTAL
total_pop_data <- tibble(
  Tahun = total_pop_data$Tahun,
  LC_ = total_pop_data$LC_ + total_pop_data_c$LC_,
  LC_Bayesian = total_pop_data$LC_Bayesian + total_pop_data_c$LC_Bayesian
)# --- 2. Transformasi ke Format Panjang (Long Format) ---
# Mengubah kolom model (LC_Standard, LC_Bayesian) menjadi satu kolom 'Population'
pop_long <- total_pop_data %>%
  pivot_longer(
    cols = starts_with("LC_"),
    names_to = "Model",
    values_to = "Population_Ribuan"
  )

comparison_plot_enlarged <- ggplot(pop_long, aes(x = Tahun, y = Population_Ribuan, color = Model)) +
  
  # Garis dan Titik
  geom_line(linewidth = 1.5) + # Garis lebih tebal
  geom_point(size = 4) +      # Titik lebih besar
  
  # Label dan Judul
  labs(
    title = "Komparasi Proyeksi Total Populasi: Bayesian vs. Non-Bayesian",
    x = "Tahun Proyeksi",
    y = "Total Populasi (Ribuan)",
    color = "Metode Estimasi" # Memperjelas legend
  ) +
  
  # --- Penataan Tema (Pembesaran Font Menyeluruh) ---
  theme_minimal(base_size = 18) + # FONT DASAR DIBESARKAN
  
  # Penyesuaian Sumbu dan Legend
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2020, 2070, by = 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 20), # Judul utama lebih besar
    axis.title = element_text(size = 18),               # Label sumbu X/Y lebih besar
    axis.text = element_text(size = 16),                # Angka sumbu lebih besar
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )

print(comparison_plot_enlarged)
#2025
library(dplyr)
library(tibble)
library(knitr)

# --- 1. DATA INPUT ---
# Data populasi wanita tahun 2025 (dalam ribuan)
pop_comparison_df <- tibble(
  AgeGroup = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  T = c(11146.1, 10730.2, 10766.1, 10706.3, 10760.0, 10981.9, 10870.5, 10704.9, 
        10246.7, 9794.5, 8825.3, 7651.8, 6286.9, 4748.8, 3308.0, 3363.1), # Data Asli
  P1 = c(11404.94, 10479.87, 10774.08, 10724.99, 10778.63, 11001.96, 10888.83, 10723.22, 
         10256.05, 9791.78, 8808.45, 7628.50, 6268.71, 4750.55, 3335.73, 1918.76), # Lee-Carter
  P2 = c(11463.05, 10520.00, 10773.26, 10723.67, 10775.81, 10998.38, 10885.02, 10718.96, 
         10251.17, 9785.77, 8801.58, 7621.28, 6260.53, 4741.90, 3327.02, 1912.24) )# Lee-Carter Bayesian)
# --- 2. PERHITUNGAN METRIK PER KELOMPOK UMUR ---

# Hitung error absolut dan persentase error untuk LC Non-Bayesian (P1)
pop_comparison_df <- pop_comparison_df %>%
  mutate(
    # MAE adalah Error Absolut per Baris
    MAE_LC = abs(P1 - T),
    # MAPE adalah Persentase Error Absolut per Baris
    MAPE_LC = abs(P1 - T) / T * 100,
    
    # Hitung error absolut dan persentase error untuk LC Bayesian (P2)
    MAE_LCB = abs(P2 - T),
    MAPE_LCB = abs(P2 - T) / T * 100
  )

# Pilih kolom yang relevan dan rapikan tampilan
local_metrics_df <- pop_comparison_df %>%
  select(
    Kelompok_Umur = AgeGroup,
    Data_Asli = T,
    MAE_LC, MAE_LCB,
    MAPE_LC, MAPE_LCB
  )

cat("== TABEL MAE DAN MAPE PER KELOMPOK UMUR (Wanita 2025) ==\n")
print(local_metrics_df %>% knitr::kable(format = "markdown", digits = 2))

calculate_metrics <- function(df, pred_col) {
  # Ambil error
  errors <- df[[pred_col]] - df$T

  mae = mean(abs(errors))
  mape = mean(abs(errors / df$T)) * 100
  
  return(tibble(
    MAE = mae,
    MAPE = mape
  ))
}

# --- 3. PERHITUNGAN METRIK GLOBAL (Semua Usia 0-74) ---
# Filter baris ekstrem (0-4 dan 75+) karena memiliki error yang sangat besar
df_stable <- pop_comparison_df %>% filter(AgeGroup != "0-4", AgeGroup != "75+")

metrics_total_stable_P1 <- calculate_metrics(df_stable, "P1")
metrics_total_stable_P2 <- calculate_metrics(df_stable, "P2")

# Gabungkan hasil untuk tabel
metrics_summary <- tibble(
  Metrik = c( "MAE (Ribuan)", "MAPE (%)"),
  LC_Non_Bayesian = unlist(metrics_total_stable_P1),
  LC_Bayesian = unlist(metrics_total_stable_P2)
) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

# --- 4. Tampilkan Hasil ---
cat("\n== TABEL AKURASI PROYEKSI (USIA 5-74, Paling Stabil) ==\n")
print(metrics_summary %>% knitr::kable(format = "markdown"))
#cowo
Kelompok_Umur <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75+")

# Data Asli (Actual Data)
Data_Asli <- c(11606.4, 11225.3, 11250.0, 11389.4, 11400.7, 11532.3,
               11323.8, 11013.4, 10435.8, 9840.1, 8775.0, 7517.9,
               6074.4, 4518.6, 3035.7, 2609.2)

Lee_Carter <- c(
  12560.29, 10482.95, 11217.50, 11391.97, 11392.01, 11495.64,
  11282.33, 10968.76, 10383.19, 9781.73, 8710.42, 7449.71,
  6007.56, 4443.07, 2974.73, 1555.61
)

# Proyeksi Model Lee-Carter Bayesian (tahun 2025)
Lee_Carter_Bayesian <- c(
  12539.78, 10954.83, 11264.49, 11421.85, 11432.42, 11549.65,
  11333.29, 11022.84, 10444.58, 9853.58, 8787.72, 7528.75,
  6076.99, 4505.45, 3024.75, 1595.24
)

# Membuat Data Frame
df_populasi <- data.frame(Kelompok_Umur, Data_Asli, Lee_Carter, Lee_Carter_Bayesian)

# Menampilkan struktur dan beberapa baris pertama data frame
print(head(df_populasi))
print(str(df_populasi))

# ------------------------------------------------------------------------------

# ==============================================================================
# 2. DEFINISI FUNGSI METRIK KESALAHAN
# ==============================================================================

# Fungsi Root Mean Square Error (RMSE)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Fungsi Mean Absolute Error (MAE)
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Fungsi Mean Absolute Percentage Error (MAPE)
# Catatan: MAPE sensitif terhadap nilai aktual yang mendekati nol.
# Di sini, kita berasumsi Data_Asli (actual) tidak nol.
mape <- function(actual, predicted) {
  # Menghitung selisih absolut dibagi nilai aktual, lalu rata-rata dan dikalikan 100
  mean(abs((actual - predicted) / actual)) * 100
}

# ------------------------------------------------------------------------------

# ==============================================================================
# 3. MENGHITUNG DAN MENAMPILKAN METRIK KESALAHAN
# ==============================================================================

# Inisialisasi data frame untuk menyimpan hasil metrik
results <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE (%)"),
  Lee_Carter = numeric(3),
  Lee_Carter_Bayesian = numeric(3)
)

# --- A. Hitung Metrik untuk Model Lee-Carter ---
lc_rmse <- rmse(df_populasi$Data_Asli, df_populasi$Lee_Carter)
lc_mae <- mae(df_populasi$Data_Asli, df_populasi$Lee_Carter)
lc_mape <- mape(df_populasi$Data_Asli, df_populasi$Lee_Carter)

# --- B. Hitung Metrik untuk Model Lee-Carter Bayesian ---
lcb_rmse <- rmse(df_populasi$Data_Asli, df_populasi$Lee_Carter_Bayesian)
lcb_mae <- mae(df_populasi$Data_Asli, df_populasi$Lee_Carter_Bayesian)
lcb_mape <- mape(df_populasi$Data_Asli, df_populasi$Lee_Carter_Bayesian)

# --- C. Isi Data Frame Hasil ---
results$Lee_Carter <- c(lc_rmse, lc_mae, lc_mape)
results$Lee_Carter_Bayesian <- c(lcb_rmse, lcb_mae, lcb_mape)

# Tampilkan Hasil
cat("\n======================================================\n")
cat("HASIL METRIK KESALAHAN MODEL\n")
cat("======================================================\n")
print(results, row.names = FALSE)
df_populasi <- data.frame(Kelompok_Umur, Data_Asli, Lee_Carter, Lee_Carter_Bayesian)

# ==============================================================================
# 2. PERHITUNGAN KOMPONEN KESALAHAN PER KELOMPOK UMUR
# ==============================================================================

# --- Hitung Selisih (Error) ---
error_LC <- df_populasi$Data_Asli - df_populasi$Lee_Carter
error_LCB <- df_populasi$Data_Asli - df_populasi$Lee_Carter_Bayesian

# --- 1. Absolute Error (AE) -> Komponen MAE ---
df_populasi$AE_LC <- abs(error_LC)
df_populasi$AE_LCB <- abs(error_LCB)

# --- 2. Squared Error (SE) -> Komponen RMSE ---
df_populasi$SE_LC <- error_LC^2
df_populasi$SE_LCB <- error_LCB^2

# --- 3. Absolute Percentage Error (APE) -> Komponen MAPE ---
# Asumsi Data_Asli tidak nol
df_populasi$APE_LC <- abs(error_LC / df_populasi$Data_Asli) * 100
df_populasi$APE_LCB <- abs(error_LCB / df_populasi$Data_Asli) * 100

# ==============================================================================
# 3. MENAMPILKAN DATA FRAME HASIL
# ==============================================================================

# Pilih kolom yang relevan untuk ditampilkan
df_error_age <- df_populasi[c("Kelompok_Umur", "Data_Asli",
                              "AE_LC", "SE_LC", "APE_LC",
                              "AE_LCB", "SE_LCB", "APE_LCB")]

# Ubah nama kolom agar lebih ringkas
names(df_error_age) <- c("Kelompok Umur", "Data Asli", 
                         "AE (LC)", "SE (LC)", "APE (LC) %", 
                         "AE (LCB)", "SE (LCB)", "APE (LCB) %")

cat("==================================================================================\n")
cat("Komponen Kesalahan (AE, SE, APE) Proyeksi Populasi Per Kelompok Umur\n")
cat("==================================================================================\n")
# Menampilkan dengan pembulatan yang wajar
print(round(df_error_age, 3), row.names = FALSE)
Kelompok_Umur <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                  "60-64", "65-69", "70-74", "75+")

# Data Asli (Actual Data)
Data_Asli <- c(22752.6, 21955.5, 22016.1, 22095.7, 22160.6, 22514.2,
               22194.3, 21718.3, 20682.5, 19634.6, 17600.2, 15169.7,
               12361.3, 9267.4, 6343.6, 5972.3)

# Proyeksi Model Lee-Carter (LC)
Lee_Carter = c(23965.23, 20962.82, 21991.58, 22016.96, 22170.64, 22497.60, 22171.16, 21691.98,
               20639.24, 19573.51, 17518.87, 15078.21, 12276.27, 9193.62, 6310.46, 3474.37)
Lee_Carter_Bayesian = c(23992.83, 21474.83, 22037.75, 22145.52, 22208.23, 22548.03, 22218.31, 21741.80,
                        20695.75, 19639.35, 17589.30, 15149.03, 12337.52, 9247.35, 6351.77, 3507.48)
# Membuat Data Frame
df_populasi_total <- data.frame(Kelompok_Umur, Data_Asli, Lee_Carter, Lee_Carter_Bayesian)

# Menampilkan data frame yang telah dibuat
cat("Data Frame Populasi Total:\n")
print(head(df_populasi_total, 16))
cat("\n------------------------------------------------------\n")


# --- 2. DEFINISI FUNGSI METRIK KESALAHAN AGREGAT ---
rmse <- function(actual, predicted) {
  # Root Mean Square Error
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  # Mean Absolute Error
  mean(abs(actual - predicted))
}

mape <- function(actual, predicted) {
  # Mean Absolute Percentage Error (dinyatakan dalam persen)
  mean(abs((actual - predicted) / actual)) * 100
}


# --- 3. PERHITUNGAN METRIK KESALAHAN AGREGAT (Rata-rata) ---
lc_rmse <- rmse(df_populasi_total$Data_Asli, df_populasi_total$Lee_Carter)
lc_mae <- mae(df_populasi_total$Data_Asli, df_populasi_total$Lee_Carter)
lc_mape <- mape(df_populasi_total$Data_Asli, df_populasi_total$Lee_Carter)

lcb_rmse <- rmse(df_populasi_total$Data_Asli, df_populasi_total$Lee_Carter_Bayesian)
lcb_mae <- mae(df_populasi_total$Data_Asli, df_populasi_total$Lee_Carter_Bayesian)
lcb_mape <- mape(df_populasi_total$Data_Asli, df_populasi_total$Lee_Carter_Bayesian)

# Membuat data frame hasil agregat
df_agregat <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE (%)"),
  Lee_Carter = c(lc_rmse, lc_mae, lc_mape),
  Lee_Carter_Bayesian = c(lcb_rmse, lcb_mae, lcb_mape)
)

# Menampilkan Hasil Agregat
cat("HASIL METRIK KESALAHAN AGREGAT (RATA-RATA):\n")
print(df_agregat, row.names = FALSE)
cat("\n------------------------------------------------------\n")


# --- 4. PERHITUNGAN KOMPONEN KESALAHAN PER KELOMPOK UMUR ---
# (AE = Komponen MAE; sqrt(SE) = Komponen RMSE; APE = Komponen MAPE)

df_populasi_total <- data.frame(Kelompok_Umur, Data_Asli, Lee_Carter, Lee_Carter_Bayesian)

# Hitung Selisih (Error)
error_LC <- df_populasi_total$Data_Asli - df_populasi_total$Lee_Carter
error_LCB <- df_populasi_total$Data_Asli - df_populasi_total$Lee_Carter_Bayesian

# --- 1. MAE (Mean Absolute Error) per kelompok umur -> Absolute Error ---
df_populasi_total$MAE_LC <- abs(error_LC)
df_populasi_total$MAE_LCB <- abs(error_LCB)

# --- 2. RMSE (Root Mean Square Error) per kelompok umur -> sqrt(Squared Error) ---
# Secara numerik, ini sama dengan Absolute Error (MAE) per baris
df_populasi_total$RMSE_LC <- sqrt(error_LC^2)
df_populasi_total$RMSE_LCB <- sqrt(error_LCB^2)

# --- 3. MAPE (Mean Absolute Percentage Error) per kelompok umur -> APE ---
df_populasi_total$MAPE_LC <- abs(error_LC / df_populasi_total$Data_Asli) * 100
df_populasi_total$MAPE_LCB <- abs(error_LCB / df_populasi_total$Data_Asli) * 100


# Memilih kolom untuk ditampilkan
df_error_age_split <- df_populasi_total[c("Kelompok_Umur", "Data_Asli",
                                          "MAE_LC", "RMSE_LC", "MAPE_LC",
                                          "MAE_LCB", "RMSE_LCB", "MAPE_LCB")]

# Mengganti nama kolom untuk kejelasan
names(df_error_age_split) <- c("Kelompok Umur", "Data Asli",
                               "MAE (LC)", "RMSE (LC)", "MAPE (LC) %",
                               "MAE (LCB)", "RMSE (LCB)", "MAPE (LCB) %")

# Menampilkan Hasil Per Kelompok Umur
cat("KOMPONEN KESALAHAN PER KELOMPOK UMUR (MAE dan RMSE Dipisah):\n")
cat(" (Catatan: Nilai MAE dan RMSE per baris data akan identik)\n")

