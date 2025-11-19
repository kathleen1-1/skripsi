#female------------------------------------------------------------------
# --- LIBRARIES ---
fert<-read.csv("D:/skripsi/fertility.csv",sep=";")
# -------------------------------
# FERTILITAS: Lee-Carter & ARIMA(0,2,0)
# -------------------------------
# --- Library ---
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)
library(forecast)
library(StMoMo)

load("D:/skripsi/fit_LC.RData")

# Ambil data kt dari fit_LC
kt_values <- as.numeric(fit_LC_fert$kt[1, ])   # Ambil baris pertama (karena matrix 1x11)

# Ambil tahun dari objek fit_LC$data
years <- fit_LC_fert$data$years   # ini vector tahun: 1971, 1975, 1980 ...

# Buat time series tahunan
kt_fert <- ts(kt_values, start = min(years), frequency = 1)  # tahunan

# Forecast ARIMA(0,2,0)
arima_fert <- Arima(kt_fert, order = c(0,1,0))

# Tentukan horizon proyeksi (misalnya 10 periode ke depan)
forecast_kt_fert <- forecast(arima_fert, h = 10)

# Ambil mean forecast sebagai numeric
kt_fert_proj <- as.numeric(forecast_kt_fert$mean)
kt_all_df<-c(kt_values,kt_fert_proj)
# Cek hasil
print(kt_fert_proj)
ax_hat<-fit_LC_fert$ax
bx_hat<-fit_LC_fert$bx
# --- ASUMSI: ax_fert dan bx_fert sudah didefinisikan ---
I_fert <- length(ax_hat)
T_fert_proj <- length(kt_all_df)

# 1. Menghitung Intersep (Alpha_x)
# Gunakan fungsi as.vector() pada ax_fert untuk menghilangkan ambiguitas
ax_matrix <- outer(as.vector(ax_hat), rep(1, T_fert_proj)) 

# 2. Menghitung Efek Periode (Beta_x * Kappa_t)
# Gunakan fungsi as.vector() pada bx_fert untuk menghilangkan ambiguitas
bx_kt_matrix <- outer(as.vector(bx_hat), kt_all_df) 

# 3. Hasil Akhir (Sekarang Dimensi DIJAMIN cocok: I x T + I x T)
log_fert_hat <- ax_matrix + bx_kt_matrix
mx_hat <- exp(log_fert_hat)

# --- 7) Label kolom dengan tahun, dan baris dengan usia (jika ada nama) ---
colnames(mx_hat) <- c(1971,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070)
rownames(mx_hat)<-c(15,20,25,30,35,40,45)


# --- MORTALITAS ---

load("D:/skripsi/fit_LC_female.RData")
LC_fit_female<-LC_fit
kt_mort <- ts(as.vector(LC_fit_female$kt), start = min(LC_fit_female$years))
tahun_proyeksi <- seq(from = 2025, by = 5, length.out = 10)

# STEP 3: Forecast ARIMA(1,1,0) dengan drift
arima_drift <- Arima(kt_mort, order = c(2,1,0), include.drift = TRUE)
forecast_drift_mort <- forecast(arima_drift, h = 10)
kt_mort_full <- as.numeric(forecast_drift_mort$mean)
years_full <- (max(LC_fit$years) + 1):(max(LC_fit$years) + length(kt_mort_full))
kt_mort_proj <- forecast_drift_mort$mean[1:length(tahun_proyeksi)]
kt_all_df<-c(kt_mort,kt_mort_proj)

ax_hat<-LC_fit$ax
bx_hat<-LC_fit$bx
# --- ASUMSI: ax_fert dan bx_fert sudah didefinisikan ---
I_fert <- length(ax_hat)
T_fert_proj <- length(kt_all_df)

# 1. Menghitung Intersep (Alpha_x)
# Gunakan fungsi as.vector() pada ax_fert untuk menghilangkan ambiguitas
ax_matrix <- outer(as.vector(ax_hat), rep(1, T_fert_proj)) 

# 2. Menghitung Efek Periode (Beta_x * Kappa_t)
# Gunakan fungsi as.vector() pada bx_fert untuk menghilangkan ambiguitas
bx_kt_matrix <- outer(as.vector(bx_hat), kt_all_df) 

# 3. Hasil Akhir (Sekarang Dimensi DIJAMIN cocok: I x T + I x T)
log_fert_hat <- ax_matrix + bx_kt_matrix
mx_hat <- exp(log_fert_hat)

# --- 7) Label kolom dengan tahun, dan baris dengan usia (jika ada nama) ---
colnames(mx_hat) <- c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070)
rownames(mx_hat)<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)



# --- POPULASI PEREMPUAN 2020 ---

pop_2020 <- data.frame(
  Kelompok.Umur = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75+"),
  Perempuan = c(10778.8, 10799, 10746.1, 10816.9, 11050.1, 10945.2,
                10795.5, 10354.3, 9928.5, 8996.9, 7874, 6574.5,
                5117.8, 3772.6, 2374.9, 2617)
)

data_plus <- pop_2020 %>%
  filter(grepl("\\+", Kelompok.Umur)) %>%
  mutate(
    umur = as.numeric(sub("\\+", "", Kelompok.Umur)) + 2.5,
    pop = Perempuan * 1000
  )

data_range <- pop_2020 %>%
  filter(grepl("-", Kelompok.Umur)) %>%
  mutate(
    umur = sapply(Kelompok.Umur, function(x) {
      parts <- as.numeric(unlist(strsplit(x, "-")))
      mean(parts)
    }),
    pop = Perempuan * 1000
  )

pop_perempuan <- bind_rows(data_range, data_plus) %>%
  select(Kelompok.Umur, umur, pop) %>%
  arrange(umur)

pop_vector <- pop_perempuan$pop
umur_vector <- pop_perempuan$umur
n_age <- length(pop_vector)

# --- SETUP PROYEKSI TAHUNAN ---
tahun_awal <- 2020
tahun_proyeksi <- seq(2025, 2070, by = 5)
n_proj <- length(tahun_proyeksi)

kt_mort_proj <- forecast_drift_mort$mean[1:n_proj]

pop_matrix <- matrix(0, nrow = n_age, ncol = n_proj + 1)
pop_matrix[, 1] <- pop_vector

leslie_list <- list()
ax_mort <- as.vector(LC_fit_female$ax)
bx_mort <- as.vector(LC_fit_female$bx)

# --- Fertilitas ---
ax_fert <- as.vector(fit_LC_fert$ax)
bx_fert <- as.vector(fit_LC_fert$bx)
valid_fert_ages <- c(17, 22, 27, 32, 37, 42, 47)

for (i in seq_len(n_proj)) {
  tahun_t <- tahun_proyeksi[i]
  
  ## 1. Proyeksi mortalitas 5-tahun
  kt_m <- kt_mort_proj[i]
  log_mx <- ax_mort + bx_mort * kt_m
  mx <- exp(log_mx)
  qx <- 1 - exp(- 5*mx)             # ini beda
  px <- pmax(0, 1 - qx)
  px[is.na(px)] <- 0
  
  ## 2. Proyeksi fertilitas 5-tahun
  kt_f <- ifelse(i <= length(kt_fert_proj) && !is.na(kt_fert_proj[i]),
                 kt_fert_proj[i],
                 tail(kt_fert_proj[!is.na(kt_fert_proj)], 1))
  
  log_asfr <- ax_fert + bx_fert * kt_f
  asfr_data <- exp(log_asfr)
  
  if (any(is.na(asfr_data)) || sum(asfr_data) == 0) {
    asfr_data[is.na(asfr_data)] <- mean(asfr_data, na.rm = TRUE)
    if (sum(asfr_data) == 0) asfr_data <- rep(0.0001, length(asfr_data))
  }
  
  # konversi tahunan → 5 tahunan
  asfr_data <- asfr_data*5 
  # normalisasi TFR 5-tahun
  asfr_data <- (asfr_data / sum(asfr_data)) * (2.18)
  
  asfr <- rep(0, n_age)
  idx_subur <- which(umur_vector %in% valid_fert_ages)
  if (length(asfr_data) == length(idx_subur)) {
    asfr[idx_subur] <- asfr_data
  } else {
    asfr[idx_subur] <- rep(mean(asfr_data), length(idx_subur))
  }
  
  # Sesuaikan rasio anak lahir perempuan
  f1 <- asfr / 2.06
  
  ## 3. Bangun Leslie matrix
  L <- matrix(0, nrow = n_age, ncol = n_age)
  L[1, idx_subur] <- f1[idx_subur]
  for (j in 2:n_age) {
    L[j, j - 1] <- px[j - 1]
  }
  
  if (any(!is.finite(L))) {
    warning(paste("Leslie matrix tahun", tahun_t, "invalid."))
    next
  }
  
  leslie_list[[i]] <- L
}

# --- Jalankan Proyeksi Populasi 5-tahunan ---
for (t in 1:n_proj) {
  pop_matrix[, t + 1] <- leslie_list[[t]] %*% pop_matrix[, t]
}

colnames(pop_matrix) <- c(tahun_awal, tahun_proyeksi)
pop_df_female <- data.frame(Umur = umur_vector, round(pop_matrix))
total_pop <- colSums(pop_matrix)

plot(c(tahun_awal, tahun_proyeksi), total_pop,
     type = "l", col = "darkred", lwd = 2,
     main = "Proyeksi 5-Tahunan Populasi Perempuan (2020–2070)",
     xlab = "Tahun", ylab = "Jumlah Penduduk Perempuan")

# Ambil data dari hasil proyeksi
pop_2040f <- pop_df_female$X2040
pop_2070f <- pop_df_female$X2070

umur_breaks <- c(seq(0, max(umur_vector) + 5, by = 5))
umur_labels <- paste(head(umur_breaks, -1), tail(umur_breaks, -1) - 1, sep = "-")
umur_interval <- cut(umur_vector, breaks = umur_breaks,
                     labels = umur_labels, include.lowest = TRUE, right = FALSE)

# Gabungkan ke data.frame
piramidaf <- data.frame(
  Umur = umur_interval,
  `2040` = pop_2040f,
  `2070` = pop_2070f
)


# Urutkan dari kelompok umur tertua ke termuda
piramidaf <- piramidaf %>%
  mutate(Umur = factor(Umur, levels = rev(unique(Umur))))

# Buat grafik piramida (perempuan saja, kiri dan kanan untuk tahun berbeda)
library(ggplot2)
# Pastikan level umur dari termuda ke tertua
piramidaf$Umur <- factor(piramidaf$Umur, 
                         levels = unique(piramidaf$Umur))  # atau gunakan sort manual

ggplot(piramidaf, aes(x = Umur)) +
  geom_bar(aes(y = -`X2040` / 1000, fill = "2040"), stat = "identity", width = 0.8) +
  geom_bar(aes(y = `X2070` / 1000, fill = "2070"), stat = "identity", width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = abs, name = "Jutaan Penduduk (Perempuan)") +
  scale_fill_manual(values = c("2040" = "lightblue", "2070" = "salmon")) +
  labs(title = "Piramida Populasi Perempuan: 2040 vs 2070",
       x = "Kelompok Umur", fill = "Tahun") +
  theme_minimal()

#male---------------------------------------------------------------------
nMx_raw <- read_excel("D:/skripsi/nMx_Male.xlsx")

# --- FERTILITAS ---
load("D:/skripsi/fit_LC.RData")
# Ambil data kt dari fit_LC
kt_values <- as.numeric(fit_LC_fert$kt[1, ])   # Ambil baris pertama (karena matrix 1x11)

# Ambil tahun dari objek fit_LC$data
years <- fit_LC_fert$data$years   # ini vector tahun: 1971, 1975, 1980 ...

# Buat time series tahunan
kt_fert <- ts(kt_values, start = min(years), frequency = 5)  # tahunan

# Forecast ARIMA(0,2,0)
arima_fert <- Arima(kt_fert, order = c(0,1,0))

# Tentukan horizon proyeksi (misalnya 10 periode ke depan)
forecast_kt_fert <- forecast(arima_fert, h = 10)

# Ambil mean forecast sebagai numeric
kt_fert_proj <- as.numeric(forecast_kt_fert$mean)

# Cek hasil
print(kt_fert_proj)
#mortalita
load("D:/skripsi/fit_LC_male.RData")
kt_mort <- ts(as.vector(LC_fit_male$kt), start = min(LC_fit_male$years))

# STEP 3: Forecast ARIMA(0,1,1) dengan drift
arima_mort <- Arima(kt_mort, order = c(2,1,1))
forecast_mort <- forecast(arima_mort, h = 10)
kt_mort_proj <- as.numeric(forecast_mort$mean[1:10])


years_full <- (max(LC_fit$years) + 1):(max(LC_fit$years) + length(kt_mort_proj))
kt_mort_proj <- forecast_mort$mean[1:length(tahun_proyeksi)]
kt_all_df<-c(kt_mort,kt_mort_proj)

ax_hat<-LC_fit_male$ax
bx_hat<-LC_fit_male$bx
# --- ASUMSI: ax_fert dan bx_fert sudah didefinisikan ---
I_fert <- length(ax_hat)
T_fert_proj <- length(kt_all_df)

# 1. Menghitung Intersep (Alpha_x)
# Gunakan fungsi as.vector() pada ax_fert untuk menghilangkan ambiguitas
ax_matrix <- outer(as.vector(ax_hat), rep(1, T_fert_proj)) 

# 2. Menghitung Efek Periode (Beta_x * Kappa_t)
# Gunakan fungsi as.vector() pada bx_fert untuk menghilangkan ambiguitas
bx_kt_matrix <- outer(as.vector(bx_hat), kt_all_df) 

# 3. Hasil Akhir (Sekarang Dimensi DIJAMIN cocok: I x T + I x T)
log_fert_hat <- ax_matrix + bx_kt_matrix
mx_hat <- exp(log_fert_hat)

# --- 7) Label kolom dengan tahun, dan baris dengan usia (jika ada nama) ---
colnames(mx_hat) <- c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070)
rownames(mx_hat)<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)


# Baca data populasi 2020
pop_2020 <-data.frame(
  Kelompok.Umur = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75+"),
  Laki_Laki = c(11293.7, 11295.3, 11449.8, 11495.7, 11632.2, 11410.8,
                11109.1, 10556.7, 10014.6, 9025.6, 7872.4, 6546.3,
                5091.7, 3681.5, 2179.1, 2007.5)
)
# Pisahkan data umur "+", misal "75+"
data_plus <- pop_2020 %>%
  filter(grepl("\\+", Kelompok.Umur)) %>%
  mutate(
    umur = as.numeric(sub("\\+", "", Kelompok.Umur)) + 2.5,
    pop = Laki_Laki * 1000
  )

# Pisahkan data umur range "0-4", "5-9", ...
data_range <- pop_2020 %>%
  filter(grepl("-", Kelompok.Umur)) %>%
  mutate(
    umur = sapply(Kelompok.Umur, function(x) {
      parts <- as.numeric(unlist(strsplit(x, "-")))
      mean(parts)
    }),
    pop = Laki_Laki * 1000
  )

# Gabungkan data dan urutkan
pop_perempuan <- bind_rows(data_range, data_plus) %>%
  select(Kelompok.Umur, umur, pop) %>%
  arrange(umur)

# Ambil vektor populasi dan umur
pop_vector <- pop_perempuan$pop
umur_vector <- pop_perempuan$umur
n_age <- length(pop_vector)

# --- SETUP ---
valid_fert_ages <- c(17, 22, 27, 32, 37, 42, 47)  # umur rata-rata kelompok subur
tahun_awal <- 2020
tahun_proyeksi <- seq(2025, 2070, by = 5)        # proyeksi tiap 5 tahun
n_proj <- length(tahun_proyeksi)

pop_matrix <- matrix(0, nrow = n_age, ncol = n_proj + 1)
pop_matrix[, 1] <- pop_vector  # populasi awal laki-laki

leslie_list_male <- list()
ax_mort <- as.vector(LC_fit_male$ax)
bx_mort <- as.vector(LC_fit_male$bx)
ax_fert <- as.vector(fit_LC_fert$ax)
bx_fert <- as.vector(fit_LC_fert$bx)

# --- LOOP PROYEKSI ---
# --- DEFENISI LIST DI LUAR LOOP (Wajib) ---
mx_list_male <- list()
px_list_male <- list()
# ----------------------------------------

for (i in seq_len(n_proj)) {
  tahun_t <- tahun_proyeksi[i]
  
  # --- 1. Proyeksi mortalitas 5-tahun ---
  kt_m <- kt_mort_proj[i]
  log_mx <- ax_mort + bx_mort * kt_m
  mx <- exp(log_mx)# mx tahunan
  
  # --- SIMPAN OUTPUT MORTALITAS SENTRAL (mx) ---
  mx_list_male[[i]] <- mx
  
  qx <- 1 - exp(-5 * mx)# peluang mati 5 tahun
  px <- pmax(0, 1 - qx)# peluang selamat 5 tahun
  px[is.na(px)] <- 0
  
  # --- SIMPAN OUTPUT PROBABILITAS SURVIVAL (px) ---
  px_list_male[[i]] <- px
  
  # --- 2. Fertilitas 5 tahun ---
  kt_f <- ifelse(i <= length(kt_fert_proj) && !is.na(kt_fert_proj[i]),
                 kt_fert_proj[i],
                 tail(kt_fert_proj[!is.na(kt_fert_proj)], 1))
  
  log_asfr <- ax_fert + bx_fert * kt_f
  asfr_data <- exp(log_asfr)
  
  if (any(is.na(asfr_data)) || sum(asfr_data) == 0) {
    asfr_data[is.na(asfr_data)] <- mean(asfr_data, na.rm = TRUE)
    if (sum(asfr_data) == 0) asfr_data <- rep(0.0001, length(asfr_data))
  }
  
  # konversi tahunan → 5 tahunan
  asfr_data <- asfr_data * 5
  # normalisasi TFR 5 tahun
  asfr_rate_final <- (asfr_data / sum(asfr_data)) * (2.18)
  
  asfr <- rep(0, n_age)
  idx_subur <- which(umur_vector %in% valid_fert_ages)
  if (length(asfr_rate_final) == length(idx_subur)) {
    asfr[idx_subur] <- asfr_rate_final
  } else {
    asfr[idx_subur] <- rep(mean(asfr_rate_final), length(idx_subur))
  }
  
  # Rasio bayi laki-laki 1.06
  prop_male <- 1.06 / (1 + 1.06)# ≈ 0.5146
  f1 <- asfr * prop_male# kelahiran laki-laki
  
  # --- 3. Bangun Leslie matrix ---
  L <- matrix(0, nrow = n_age, ncol = n_age)
  L[1, idx_subur] <- f1[idx_subur]
  for (j in 2:n_age) {
    L[j, j - 1] <- px[j - 1]
  }
  
  if (any(!is.finite(L))) {
    warning(paste("Leslie matrix tahun", tahun_t, "invalid."))
    next
  }
  
  leslie_list_male[[i]] <- L
}
library(dplyr)
library(tidyr)
library(tibble)
library(knitr) # Untuk membuat tabel output yang rapi

# --- ASUMSI: mx_list sudah terisi dari for loop ---
# ASUMSI: tahun_proyeksi dan umur_vector sudah didefinisikan

# 1. Gabungkan List mx menjadi Matriks (Age x Year)
mx_matrix_proj <- do.call(cbind, mx_list) 

# 2. Beri Label
colnames(mx_matrix_proj) <- tahun_proyeksi # Label kolom dengan tahun proyeksi
rownames(mx_matrix_proj) <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95) # Label baris dengan usia

# --- Jalankan Proyeksi Populasi 5 Tahunan ---
for (t in 1:n_proj) {
  pop_matrix[, t + 1] <- leslie_list_male[[t]] %*% pop_matrix[, t]
}

colnames(pop_matrix) <- c(tahun_awal, tahun_proyeksi)
pop_df_male <- data.frame(Umur = umur_vector, round(pop_matrix))
total_pop <- colSums(pop_matrix)

plot(c(tahun_awal, tahun_proyeksi), total_pop,
     type = "l", col = "blue", lwd = 2,
     main = "Proyeksi 5-Tahunan Populasi Laki-Laki (2020–2070)",
     xlab = "Tahun", ylab = "Jumlah Penduduk Laki-Laki")

# Ambil data dari hasil proyeksi
pop_2040f <- pop_df_male$X2040
pop_2070f <- pop_df_male$X2070

umur_breaks <- c(seq(0, max(umur_vector) + 5, by = 5))
umur_labels <- paste(head(umur_breaks, -1), tail(umur_breaks, -1) - 1, sep = "-")
umur_interval <- cut(umur_vector, breaks = umur_breaks,
                     labels = umur_labels, include.lowest = TRUE, right = FALSE)

# Gabungkan ke data.frame
piramidam <- data.frame(
  Umur = umur_interval,
  `2040` = pop_2040f,
  `2070` = pop_2070f
)


# Urutkan dari kelompok umur tertua ke termuda
piramidam <- piramidam %>%
  mutate(Umur = factor(Umur, levels = rev(unique(Umur))))

# Buat grafik piramida (perempuan saja, kiri dan kanan untuk tahun berbeda)
library(ggplot2)

ggplot(piramidam, aes(x = Umur)) +
  geom_bar(aes(y = -`X2040` / 1000, fill = "2040"), stat = "identity", width = 0.8) +
  geom_bar(aes(y = `X2070` / 1000, fill = "2070"), stat = "identity", width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = abs, name = "Jutaan Penduduk (Laki-Laki)") +
  scale_fill_manual(values = c("2040" = "lightblue", "2070" = "salmon")) +
  labs(title = "Piramida Populasi Perempuan: 2040 vs 2070",
       x = "Kelompok Umur", fill = "Tahun") +
  theme_minimal()

#combine-----------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

AGE_ORDER <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
               "60-64", "65-69", "70-74", "75+")

# --- ASUMSI: pop_long_female sudah dibuat di langkah sebelumnya ---

# 1A. Transformasi Data Pria (Menjadi Long Format dan Negatif)

pop_long_male <- pop_df_male %>%
  rownames_to_column(var = "AgeGroup") %>% 
  select(-Umur) %>% 
  
  # KOREKSI 1: Gunakan 'cols = -AgeGroup' untuk memivot SEMUA kolom selain AgeGroup
  pivot_longer(
    cols = -AgeGroup, 
    names_to = "Year", 
    values_to = "Population"
  ) %>%
  mutate(
    # KOREKSI: Tambahkan kembali gsub("X", "", ...) untuk membersihkan prefiks 'X'
    Year = as.numeric(gsub("X", "", Year)), 
    Gender = "Male",
    # CRUCIAL: Buat nilai populasi negatif untuk plot di sisi kiri
    Population = -Population, 
    AgeGroup = factor(AgeGroup, levels = AGE_ORDER) 
  )
pop_long_female <- pop_df_female %>%
  rownames_to_column(var = "AgeGroup") %>% 
  
  # KOREKSI 1: HAPUS kolom 'Umur' (menggunakan minus)
  select(-Umur) %>% 
  
  # KOREKSI 2: PIVOT semua kolom SELAIN AgeGroup (menggunakan minus)
  pivot_longer(
    cols = -AgeGroup, 
    names_to = "Year", 
    values_to = "Population"
  ) %>%
  mutate(
    # KOREKSI 3: Tambahkan kembali gsub untuk membersihkan 'X' dan perbaiki typo
    Year = as.numeric(gsub("X", "", Year)), 
    Gender = "Female", 
    Population = Population, # Pertahankan nilai positif untuk wanita
    AgeGroup = factor(AgeGroup, levels = AGE_ORDER) 
  )
# 2. Gabungkan Data Final (Wanita dan Pria)
pop_long_combined <- bind_rows(pop_long_female, pop_long_male)


# --- 3. Plotting Piramida Populasi (Faceted by Year) ---
final_pyramid <- ggplot(pop_long_combined, aes(x = AgeGroup, y = Population, fill = Gender)) +
  geom_bar(stat = "identity") +
  # FACET: Memecah plot per tahun (wajib untuk melihat perubahan struktur)
  facet_wrap(~ Year, ncol = 4) + 
  coord_flip() + # Memutar plot menjadi horizontal
  scale_y_continuous(
    # Label menunjukkan nilai absolut dalam ribuan
    labels = function(x) abs(x / 1000), 
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    y = "Jumlah Penduduk (dalam ribuan)",
    x = "Kelompok Umur",
    title = "Proyeksi Piramida Populasi Indonesia 2020-2070",
    fill = "Jenis Kelamin"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF"))
print(final_pyramid)
projection_years <- unique(pop_long_combined$Year)
pop_long_combined <- pop_long_combined %>%
  # Menggunakan mutate dan ifelse/replace_na untuk mengganti nilai NA
  mutate(AgeRange = replace_na(AgeGroup, "75+"))

# --- 2. Buat Loop untuk Setiap Tahun ---
for (current_year in projection_years) {
  
  # A. FILTER DATA: Ambil data hanya untuk tahun saat ini
  pop_filtered <- pop_long_combined %>%
    filter(Year == current_year)
  
  # B. BUAT PLOT (Menghapus facet_wrap)
  pyramid_plot <- ggplot(pop_filtered, aes(x = AgeRange, y = Population, fill = Gender)) +
    geom_bar(stat = "identity") +
    
    # C. HAPUS FACET_WRAP: Karena hanya ada 1 tahun per plot
    coord_flip() +
    scale_y_continuous(
      labels = function(x) abs(x / 1000),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    labs(
      y = "Jumlah Penduduk (dalam ribuan)",
      x = "Kelompok Umur",
      # Ganti judul agar mencantumkan tahun spesifik
      title = paste("Piramida Populasi Indonesia Tahun", current_year)
    ) +
    # Gunakan base_size yang lebih kecil (misalnya 12) atau hapus,
    # lalu atur setiap elemen secara manual di theme() untuk kontrol maksimum.
    theme_minimal(base_size = 14) + # Atur base size untuk teks umum (tick labels)
    scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF"),
                      labels = c("Female" = "Wanita", "Male" = "Pria")) +
    # >>> PERUBAHAN SPESIFIK: Atur ukuran teks untuk setiap elemen
    theme(
      # Judul Grafik
      plot.title = element_text(size = 22, face = "bold"),
      # Label Sumbu X dan Y (Judul Sumbu)
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      # Label Sumbu X dan Y (Angka/Teks pada Sumbu)
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      # Judul dan Teks Legenda
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16)
    )
  
  # D. SIMPAN FILE dengan Nama Unik (misal: piramida_2025.png)
  filename <- paste0("piramida_populasi_", current_year, ".png")
  
  ggsave(
    filename = file.path("D:/skripsi/Output_Grafik", filename), # Ganti dengan FINAL_OUTPUT_PATH Anda
    plot = pyramid_plot,
    width = 10, # Perbesar width sedikit untuk mengakomodasi teks yang lebih besar
    height = 8,# Perbesar height sedikit
    units = "in",
    dpi = 300
  )
  
  cat(paste("✅ Plot", current_year, "selesai disimpan.\n"))
}
print(final_pyramid)

FINAL_OUTPUT_PATH <- "D:/skripsi/Output_Grafik/Piramida_Proyeksi_Final.png"

# --- 2. Simpan Plot Menggunakan ggsave() ---
# Simpan objek plot 'final_pyramid' ke lokasi yang ditentukan.
ggsave(
  filename = FINAL_OUTPUT_PATH, # Path lengkap file Anda
  plot = final_pyramid,        # Objek plot yang ingin disimpan
  width = 15,                  # Lebar plot dalam inci (Ideal untuk facet wrap)
  height = 9,                  # Tinggi plot dalam inci
  units = "in",                # Satuan
  dpi = 300                      # Resolusi tinggi (300 DPI)
)

