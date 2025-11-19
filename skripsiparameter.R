#FEMALE----------------------------------------------------
data <- read.csv("C:/Users/kathl/OneDrive/mortality.csv",sep=";")
fert<-read.csv("C:/Users/kathl/OneDrive/fertility.csv",sep=";")
pop<-read.csv("D:/downloads/Jumlah Penduduk Menurut Kelompok Umur dan Jenis Kelamin, 2020.csv")
summary(mort)
summary(fert)
summary(pop)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(StMoMo)
# Load the data

colnames(data) <- c(
  "Year", "Country","AgeGroup", "Sex", "nMx", "nqx", "lx", "ndx", "nLx", "Tx", "ex"
)
data$ndx <- as.numeric(data$ndx)
data$Year <- as.numeric(data$Year)

# --- Step 1: Define age midpoints (keep as requested) ---
age_midpoints <- c(
  "<1 year"     = NA,
  "1-4 years"   = 2,
  "5-9 years"   = 7,
  "10-14 years" = 12,
  "15-19  years" = 17,
  "20-24 years" = 22,
  "25-29 years" = 27,
  "30-34 years" = 32,
  "35-39 years" = 37,
  "40-44 years" = 42,
  "45-49 years" = 47,
  "50-54 years" = 52,
  "55-59 years" = 57,
  "60-64 years" = 62,
  "65-69 years" = 67,
  "70-74 years" = 72,
  "75-79 years" = NA,
  "80-84 years" = NA,
  "85+ years"   = NA
)

# --- Step 2: Filter and prepare female mortality data ---
library(dplyr)

mort <- data %>%
  filter(Sex == "Female") %>%
  mutate(
    nMx = as.numeric(nMx),
    lx  = as.numeric(lx),
    ndx = as.numeric(ndx),
    Year = as.numeric(Year),
    age = age_midpoints[AgeGroup]
  ) %>%
  filter(!is.na(age), !is.na(nMx), !is.na(lx), !is.na(ndx), !is.na(Year)) %>%
  filter(age >= 1, age <= 84) %>%
  arrange(Year, age)


# --- Step 3: Estimate nqx for 2020 using nMx from 2019 ---
mort_2019 <- mort %>% filter(Year == 2019)

mort_2020 <- mort_2019 %>%
  mutate(
    n = ifelse(age == 0.5, 1, 5),
    nqx = 1 - exp(-nMx * n),
    Year = 2020
  ) %>%
  select(Year, age, nqx, nMx)

# --- Step 4: Combine with all nqx values and remove 2019 ---
data_nqx <- mort %>% select(Year, age, nqx)

data_nqx_full <- bind_rows(data_nqx, mort_2020 %>% select(Year, age, nqx)) %>%
  arrange(Year, age)

data_nqx_clean <- data_nqx_full %>% filter(Year != 2019)

# --- Step 5: Build lx and dx matrices ---
age_levels <- sort(unique(data_nqx_clean$age))
year_levels <- sort(unique(data_nqx_clean$Year))

lx_matrix <- matrix(NA, nrow = length(age_levels), ncol = length(year_levels),
                    dimnames = list(as.character(age_levels), as.character(year_levels)))
dx_matrix <- lx_matrix

# --- Step 6: Clean female population for 2020 ---
pop_clean <- pop %>%
  rename(
    AgeGroup = Kelompok.Umur,
    Female = `Penduduk..Perempuan...Ribu.`
  ) %>%
  mutate(
    Female = as.numeric(gsub(",", "", Female)),
    age = c(
      "0-4"   = 2,
      "5-9"   = 7,
      "10-14" = 12,
      "15-19" = 17,
      "20-24" = 22,
      "25-29" = 27,
      "30-34" = 32,
      "35-39" = 37,
      "40-44" = 42,
      "45-49" = 47,
      "50-54" = 52,
      "55-59" = 57,
      "60-64" = 62,
      "65-69" = 67,
      "70-74" = 72,
      "75+" = NA
    )[AgeGroup]
  ) %>%
  filter(!is.na(Female), !is.na(age), age >= 1, age <= 84)

# --- Step 7: Build dx_2020_vec using aligned numeric names ---
pop_2020_vec <- setNames(pop_clean$Female * 1000, pop_clean$age)  # indiv
nqx_2020_vec <- setNames(mort_2020$nqx, mort_2020$age)
ages_char <- formatC(age_levels, format = "f", digits = 1)
pop_names <- names(pop_2020_vec)
nqx_names <- names(nqx_2020_vec)

cat("pop:\n"); print(pop_names)
cat("nqx:\n"); print(nqx_names)

missing_from_pop <- setdiff(nqx_names, pop_names)
cat("Missing from pop:\n"); print(missing_from_pop)

ages_char <- as.character(age_levels)

dx_2020_vec <- numeric(length(age_levels))
names(dx_2020_vec) <- ages_char

common_ages <- intersect(as.character(names(pop_2020_vec)), as.character(names(nqx_2020_vec)))

if (length(common_ages) == 0) stop("No matching ages between pop and nqx for 2020.")

for (age in common_ages) {
  pop_val <- pop_2020_vec[[age]]
  nqx_val <- nqx_2020_vec[[age]]
  dx_2020_vec[age] <- ifelse(is.na(pop_val) | is.na(nqx_val), 0, pop_val * nqx_val)
}

# --- Step 8: Fill matrices year-by-year ---
# --- Step 8: Fill matrices year-by-year (with consistent dx and lx[1] logic) ---
for (y in year_levels) {
  df_year <- data_nqx_clean %>% filter(Year == y) %>% arrange(age)
  
  nqx <- df_year$nqx
  lx <- numeric(length(age_levels))
  
  if (y == 2020) {
    # Pakai lx dari 2015 sebagai awal
    prev_lx <- lx_matrix[, as.character(y - 5)]
    lx[1] <- prev_lx[1]
    for (i in 2:length(age_levels)) {
      lx[i] <- lx[i - 1] * (1 - nqx[i - 1])
      if (is.na(lx[i]) || lx[i] < 0) lx[i] <- 0
    }
  } else {
    lx[1] <- 100000
    for (i in 2:length(age_levels)) {
      lx[i] <- lx[i - 1] * (1 - nqx[i - 1])
      if (is.na(lx[i]) || lx[i] < 0) lx[i] <- 0
    }
  }
  
  # Simpan lx
  lx_matrix[, as.character(y)] <- lx
  
  # dx selalu dihitung dari lx * nqx (konsisten)
  dx <- lx * pmin(pmax(nqx, 0), 1)
  dx <- pmax(dx, 0)
  dx_matrix[, as.character(y)] <- dx
}

# --- Step 9: Convert matrices to data.frame for StMoMo input ---
death_df <- as.data.frame(dx_matrix)
exposure_df <- as.data.frame(lx_matrix)
ages <- as.numeric(rownames(death_df))
years <- as.numeric(colnames(death_df))


# === Load library ===
library(StMoMo)

# === Step 1: Convert Dxt and Ext to matrix (if not already) ===
Dxt <- as.matrix(death_df)       # Replace with your actual death matrix
Ext <- as.matrix(exposure_df)   # Replace with your actual exposure matrix

# === Step 2: Define age and year vectors ===
ages <- as.numeric(rownames(Dxt))
years <- as.numeric(colnames(Dxt))

# === Step 3: Define weight matrix (optional, here all set to 1) ===
wxt <- matrix(1, nrow = length(ages), ncol = length(years),
              dimnames = list(as.character(ages), as.character(years)))

# === Step 4: Create Lee-Carter model object ===
LC <- lc(link = "log")  # or lc() with default log link

# === Step 5: Fit the Lee-Carter model safely ===
fit_LC <- tryCatch({
  fit(
    LC,
    Dxt = Dxt,
    Ext = Ext,
    ages = ages,
    years = years,
    ages.fit = ages,
    years.fit = years,
    wxt = wxt,
    gc.ctrl = list(maxit = 100)  # optional: max iterations for convergence
  )
}, error = function(e) {
  cat("Lee-Carter fitting failed:\n")
  print(e)
  return(NULL)
})

# === Step 6: Extract and save model parameters if successful ===
if (!is.null(fit_LC)) {
  ax <- fit_LC$ax  # Intercept per age
  bx <- fit_LC$bx  # Sensitivity per age
  kt <- fit_LC$kt  # Time trend per year
  
  # Optionally: check the kt plot
  plot(years, kt, type = "b", main = "Lee-Carter kt", xlab = "Year", ylab = "kt")
}

  # Print the extracted parameters
  cat("a (age-specific intercepts) for LC model:\n")
  print(ax)
  
  cat("b (sensitivity to period effects) for LC model:\n")
  print(bx)
  
  cat("kt (time trend for period) for LC model:\n")
  print(kt)
else {
  cat("The Lee-Carter model fitting was unsuccessful.\n")
}
# Load necessary library for plotting
library(ggplot2)

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
  labs(title = "Age-Specific Intercepts (a) for LC Model", 
       x = "Age", 
       y = "Intercepts") +
  theme_minimal()

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
    labs(title = "Sensitivity to Period Effects (b) for LC Model", 
         x = "Age", 
         y = "Sensitivity") +
    theme_minimal()
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
    title = "Period Effect (kt) from Lee-Carter Model",
    subtitle = "Represents the mortality trend over time",
    x = "Year",
    y = expression(kappa[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Load necessary libraries for time series analysis
library(forecast)
library(ggplot2)

# Convert the data to a time series object
kt_ts <- ts(kt_df$Trend, start = 2000, frequency = 1)

# --- Model Fitting ---

# Fit ARIMA and ETS models
arima_model <- auto.arima(kt_ts)
ets_model <- ets(kt_ts)

# Print model summaries
cat("🔹 ARIMA Model Summary:\n")
summary(arima_model)

cat("\n🔹 ETS Model Summary:\n")
summary(ets_model)

# --- AIC Comparison ---
arima_aic <- AIC(arima_model)
ets_aic <- AIC(ets_model)

cat("\n📊 Model AIC Comparison:\n")
cat("ARIMA AIC:", arima_aic, "\n")
cat("ETS AIC:", ets_aic, "\n")

# --- Forecasts ---
horizon <- 5  # Forecast horizon
arima_forecast <- forecast(arima_model, h = horizon)
ets_forecast <- forecast(ets_model, h = horizon)

# --- Forecast Plots ---
autoplot(arima_forecast) +
  ggtitle("ARIMA Forecast of κₜ") +
  theme_minimal()

autoplot(ets_forecast) +
  ggtitle("ETS Forecast of κₜ") +
  theme_minimal()

# --- ACF Plots ---
par(mfrow = c(1, 3))  # Arrange plots in one row
pacf(kt_ts, main = "ACF: Original κₜ Series")
pacf(residuals(arima_model), main = "ACF: ARIMA Residuals")
pacf(residuals(ets_model), main = "ACF: ETS Residuals")
par(mfrow = c(1, 1))  # Reset layout
library(tseries)
adf.test(kt_ts)


#male-----------------------------------------------------------------------------------
data <- read.csv("C:/Users/kathl/OneDrive/mortality.csv",sep=";")
fert<-read.csv("C:/Users/kathl/OneDrive/fertility.csv",sep=";")
pop<-read.csv("D:/downloads/Jumlah Penduduk Menurut Kelompok Umur dan Jenis Kelamin, 2020.csv")
summary(mort)
summary(fert)
summary(pop)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(StMoMo)
# Load the data
# Rename columns manually
colnames(data) <- c(
  "Year", "Country","AgeGroup", "Sex", "nMx", "nqx", "lx", "ndx", "nLx", "Tx", "ex"
)
data$ndx <- as.numeric(data$ndx)
data$Year <- as.numeric(data$Year)

# Define the custom order for AgeGroup
age_group_order <- c("<1 year", "1-4 years", "5-9 years", "10-14 years", 
                     "15-19  years", "20-24 years", "25-29 years", "30-34 years", 
                     "35-39 years", "40-44 years", "45-49 years", "50-54 years", 
                     "55-59 years", "60-64 years", "65-69 years", "70-74 years", 
                     "75-79 years", "80-84 years", "85+ years")
library(zoo)
mort<- data %>%
  filter(Sex == "Male") %>%
  mutate(
    nMx = as.numeric(nMx),
    lx = as.numeric(lx),
    ndx = as.numeric(ndx),
    Year = as.numeric(Year),
    Age = as.numeric(sub("^<1.*", "0", sub("^([0-9]+).*", "\\1", AgeGroup)))
  ) %>%
  filter(!is.na(Age), !is.na(ndx), !is.na(lx), !is.na(nMx), !is.na(Year))
qxt_interpolated <- mort %>%
  select(AgeGroup, Year, nqx) %>%
  mutate(AgeGroup = factor(AgeGroup, levels = age_group_order)) %>%
  complete(Year = full_seq(c(min(Year), 2020), 1), nesting(AgeGroup)) %>%
  filter(!is.na(AgeGroup)) %>%  # ✅ REMOVE any NA group
  arrange(Year, AgeGroup) %>%
  group_by(AgeGroup) %>%
  mutate(nqx = zoo::na.approx(nqx, Year, rule = 2, maxgap = Inf)) %>%
  ungroup()


# Set lx for age <1 year to 100,000
initial_population <- 100000

qxt_interpolated <- qxt_interpolated %>%
  arrange(Year, AgeGroup) %>%
  group_by(Year) %>%
  mutate(
    lx = ifelse(AgeGroup == "<1 year", initial_population, NA)
  ) %>%
  ungroup()

# Propagate lx using qxt
for (i in 2:nrow(qxt_interpolated)) {
  if (is.na(qxt_interpolated$lx[i]) && !is.na(qxt_interpolated$lx[i - 1])) {
    qxt_interpolated$lx[i] <- qxt_interpolated$lx[i - 1] * (1 - qxt_interpolated$nqx[i - 1])
  }
}

# Calculate dx
qxt_interpolated <- qxt_interpolated %>%
  mutate(dx = lx * nqx)
final_data <- qxt_interpolated %>%
  select(Year, AgeGroup, lx, dx, nqx)
final_data %>%
  filter(grepl("^\\d+", AgeGroup)) %>%
  mutate(
    extracted = sub("^(\\d+).*", "\\1", AgeGroup),
    numeric_check = as.numeric(extracted)
  ) %>%
  filter(is.na(numeric_check)) %>%
  distinct(AgeGroup)

final_data <- final_data %>%
  mutate(
    Age = case_when(
      grepl("^<1", AgeGroup) ~ 1,
      grepl("^85\\+", AgeGroup) ~ 85,
      grepl("^\\d+", AgeGroup) ~ as.numeric(trimws(gsub("[^0-9].*", "", AgeGroup))),
      TRUE ~ NA_real_
    )
  )
final_data <- final_data %>%
  mutate(
    AgeGroupCategory = case_when(
      Age < 15             ~ "Young",
      Age >= 15 & Age < 65 ~ "Working Age",
      Age >= 65            ~ "Old",
      TRUE                 ~ NA_character_
    )
  )


# Now build the matrices using only 'Young' category
deaths_clean <- final_data %>%
  select(Year, AgeGroup, dx) %>%
  spread(key = Year, value = dx)

exposure_clean <- final_data %>%
  select(Year, AgeGroup, lx) %>%
  spread(key = Year, value = lx)

deaths_matrix <- deaths_clean %>%
  select(-AgeGroup) %>%
  as.matrix()

exposure_matrix <- exposure_clean %>%
  select(-AgeGroup) %>%
  as.matrix()

ages <- c(1,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)  # safer for filtered age
years <- as.numeric(colnames(deaths_matrix))

# Create Lee-Carter model object
LC <- lc()

# Fit the Lee-Carter model
fit_LC <- tryCatch({
  fit(
    LC,
    Dxt = deaths_matrix,
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

# Check if the model fitting was successful and extract parameters
if (!is.null(fit_LC)) {
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
  labs(title = "Age-Specific Intercepts (a) for LC Model", 
       x = "Age", 
       y = "Intercepts") +
  theme_minimal()

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
    labs(title = "Sensitivity to Period Effects (b) for LC Model", 
         x = "Age", 
         y = "Sensitivity") +
    theme_minimal()
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
    title = "Period Effect (kt) from Lee-Carter Model",
    subtitle = "Represents the mortality trend over time",
    x = "Year",
    y = expression(kappa[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Load necessary libraries for time series analysis
library(forecast)
library(ggplot2)

# Convert the data to a time series object
kt_ts <- ts(kt_df$Trend, start = 2000, frequency = 1)

# --- Model Fitting ---

# Fit ARIMA and ETS models
arima_model <- auto.arima(kt_ts)
ets_model <- ets(kt_ts)

# Print model summaries
cat("🔹 ARIMA Model Summary:\n")
summary(arima_model)

cat("\n🔹 ETS Model Summary:\n")
summary(ets_model)

# --- AIC Comparison ---
arima_aic <- AIC(arima_model)
ets_aic <- AIC(ets_model)

cat("\n📊 Model AIC Comparison:\n")
cat("ARIMA AIC:", arima_aic, "\n")
cat("ETS AIC:", ets_aic, "\n")

# --- Forecasts ---
horizon <- 5  # Forecast horizon
arima_forecast <- forecast(arima_model, h = horizon)
ets_forecast <- forecast(ets_model, h = horizon)

# --- Forecast Plots ---
autoplot(arima_forecast) +
  ggtitle("ARIMA Forecast of κₜ") +
  theme_minimal()

autoplot(ets_forecast) +
  ggtitle("ETS Forecast of κₜ") +
  theme_minimal()

# --- ACF Plots ---
par(mfrow = c(1, 3))  # Arrange plots in one row
acf(kt_ts, main = "ACF: Original κₜ Series")
acf(residuals(arima_model), main = "ACF: ARIMA Residuals")
acf(residuals(ets_model), main = "ACF: ETS Residuals")
par(mfrow = c(1, 1))  # Reset layout




#fertility---------------------------------------------
fert<-read.csv("C:/Users/kathl/OneDrive/fertility.csv",sep=";")
summary(mort)
library(dplyr)
library(tidyr)
library(zoo)
library(dplyr)
library(tidyr)
library(zoo)

# --- Step 1: Mapping age ke titik tengah ---
age_midpoints <- c(
  "15-19" = 17, "20-24" = 22, "25-29" = 27,
  "30-34" = 32, "35-39" = 37, "40-44" = 42, "45-49" = 47
)

# --- Step 2: Clean data ---
fert <- fert %>%
  mutate(
    population = gsub(",", ".", population),
    population = as.numeric(population),
    age = age_midpoints[age]
  ) %>%
  filter(!is.na(age), !is.na(population))

# --- Step 3: Hitung lx_raw dan nqx per usia & tahun asli ---
fert_clean <- fert %>%
  group_by(age, year) %>%
  summarise(lx_raw = mean(population), .groups = "drop") %>%
  arrange(age, year) %>%
  group_by(age) %>%
  mutate(
    lx_next = lead(lx_raw),
    nqx = 1 - (lx_next / lx_raw)
  ) %>%
  ungroup() %>%
  filter(!is.na(nqx))

# --- Step 4: Interpolasi CFM untuk tahun tengah (per 5 tahun) ---
# --- Step 4: Interpolasi CFM untuk tahun-tahun di antara observasi (per tahun) ---
original_years <- sort(unique(fert_clean$year))
target_years <- seq(min(original_years), max(original_years), by = 1)  # TAHUNAN

cfm_interp <- data.frame()

for (a in unique(fert_clean$age)) {
  df_age <- fert_clean %>% filter(age == a)
  
  for (i in 1:(length(original_years) - 1)) {
    y0 <- original_years[i]
    y1 <- original_years[i + 1]
    
    q0 <- df_age$nqx[df_age$year == y0]
    q1 <- df_age$nqx[df_age$year == y1]
    
    # Ambil satu nilai saja, pastikan valid
    if (length(q0) > 0 && length(q1) > 0) {
      q0 <- q0[1]
      q1 <- q1[1]
      if (is.na(q0) || is.na(q1) || q0 <= 0 || q1 <= 0) next
    } else {
      next
    }
    
    # Interpolasi semua tahun di antara y0 dan y1 (kecuali batas)
    for (yt in (y0 + 1):(y1 - 1)) {
      t <- (yt - y0) / (y1 - y0)
      log_interp <- (1 - t) * log(1 - q0) + t * log(1 - q1)
      q_interp <- 1 - exp(log_interp)
      
      cfm_interp <- bind_rows(cfm_interp, data.frame(age = a, year = yt, nqx = q_interp))
    }
  }
}


# Gabungkan dengan data asli
fert_nqx_full <- fert_clean %>%
  select(age, year, nqx) %>%
  bind_rows(cfm_interp) %>%
  arrange(age, year)

# (Opsional) Lihat hasilnya
print(fert_nqx_full)

# --- Step: Extrapolasi tahun 2015 dan 2020 pakai CFM ---
# --- Step: Ekstrapolasi CFM untuk 2011–2020 (tahunan) ---
cfm_extra <- data.frame()

for (a in unique(fert_nqx_full$age)) {
  q2000 <- fert_nqx_full$nqx[fert_nqx_full$age == a & fert_nqx_full$year == 2000]
  q2010 <- fert_nqx_full$nqx[fert_nqx_full$age == a & fert_nqx_full$year == 2010]
  
  if (length(q2000) > 0 && length(q2010) > 0) {
    q2000 <- q2000[1]
    q2010 <- q2010[1]
    if (is.na(q2000) || is.na(q2010) || q2000 <= 0 || q2010 <= 0) next
  } else {
    next
  }
  
  for (yt in 2011:2020) {
    t <- (yt - 2000) / (2010 - 2000)
    log_interp <- (1 - t) * log(1 - q2000) + t * log(1 - q2010)
    q_interp <- 1 - exp(log_interp)
    
    cfm_extra <- bind_rows(cfm_extra, data.frame(age = a, year = yt, nqx = q_interp))
  }
}
fert_nqx_full <- fert_clean %>%
  select(age, year, nqx) %>%
  bind_rows(cfm_interp) %>%
  bind_rows(cfm_extra) %>%
  arrange(age, year)
fert_nmx_full <- fert_nqx_full %>%
  mutate(
    n = 5,
    nMx = -log(1 - nqx) / n
  ) %>%
  filter(is.finite(nMx), nMx >= 0) %>%  # hanya ambil nilai valid
  select(age, year, nMx) %>%
  arrange(age, year)
library(dplyr)
library(tidyr)
library(zoo)

# Pastikan data lengkap dulu
full_grid <- expand.grid(
  age = unique(fert_nmx_full$age),
  year = min(fert_nmx_full$year):max(fert_nmx_full$year)
)

# Gabungkan dengan data asli
fert_nmx_filled <- full_grid %>%
  left_join(fert_nmx_full, by = c("age", "year")) %>%
  arrange(age, year)

# Interpolasi log(nMx) → lalu kembalikan ke nMx
fert_nmx_filled <- fert_nmx_filled %>%
  group_by(age) %>%
  arrange(year) %>%
  mutate(
    log_nMx = log(nMx),
    log_nMx = zoo::na.approx(log_nMx, na.rm = FALSE),  # CFM-style interpolasi
    log_nMx = zoo::na.locf(log_nMx, na.rm = FALSE),    # isi dari bawah
    log_nMx = zoo::na.locf(log_nMx, fromLast = TRUE, na.rm = FALSE),  # isi dari atas
    nMx = exp(log_nMx)
  ) %>%
  select(age, year, nMx) %>%
  ungroup()



valid_ages <- sort(unique(fert_nmx_full$age))
valid_years <- sort(unique(fert_nmx_full$year))
nMx_matrix <- matrix(NA, nrow = length(valid_ages), ncol = length(valid_years),
                     dimnames = list(as.character(valid_ages), as.character(valid_years)))

library(tidyr)
library(dplyr)
nMx_wide <- fert_nmx_filled %>%
  pivot_wider(names_from = year, values_from = nMx) %>%
  arrange(age)

nMx_matrix <- as.matrix(nMx_wide[,-1])
rownames(nMx_matrix) <- nMx_wide$age



valid_fert_ages <- c(17, 22, 27, 32, 37, 42, 47)

valid_ages <- rownames(nMx_matrix)
valid_years <- colnames(nMx_matrix)

# Exposure dummy: 100000 untuk semua kombinasi umur × tahun
Ext <- matrix(100000,
              nrow = length(valid_ages),
              ncol = length(valid_years),
              dimnames = list(valid_ages, valid_years))
exposure_matrix<-Ext
Dxt <- nMx_matrix * Ext
death_matrix<-Dxt

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

# Check if the model fitting was successful and extract parameters
if (!is.null(fit_LC)) {
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
  labs(title = "Age-Specific Intercepts (a) for LC Model", 
       x = "Age", 
       y = "Intercepts") +
  theme_minimal()

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
    labs(title = "Sensitivity to Period Effects (b) for LC Model", 
         x = "Age", 
         y = "Sensitivity") +
    theme_minimal()
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
    title = "Period Effect (kt) from Lee-Carter Model",
    subtitle = "Represents the mortality trend over time",
    x = "Year",
    y = expression(kappa[t])
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
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
