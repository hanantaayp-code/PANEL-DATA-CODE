#===============================================================================
# BERLATIH OLAH DATA PANEL STATIS MENGGUNAKAN R
# Script Manage by HANANTA ANGGER YUGA PRAWIRA, 11 MEI 2026
# LAST UPDATED 11 MEI 2026 6:04 WIB
#===============================================================================
rm(list = ls()) #Membersihkan Enviroment
graphics.off() #Membersihkan Tab grafik yang terbuka
gc() #mengelola dan membebaskan memori RAM yang sudah tidak digunakan lagi oleh sistem
cat("\014") #membersihkan console

#===============================================================================
#---------------Inventory Package yang akan digunakan---------------------------
#===============================================================================
# 1. MANAJEMEN PACKAGES (Otomatis Install jika belum ada)
packages <- c("plm", "readxl", "dplyr", "modelsummary", "corrplot", "ggplot2", 
              "GGally", "psych", "lmtest", "sandwich", "car", "stargazer")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. IMPORT & MANIPULASI DATA
library(readxl)       # Membaca file Excel (.xlsx)
library(dplyr)        # Manipulasi dan transformasi data (wrangling)

# 3. ANALISIS DATA PANEL
library(plm)          # Library utama untuk regresi data panel (FE, RE, Pooling)

# 4. UJI DIAGNOSTIK & ASUMSI KLASIK
library(lmtest)       # Uji hipotesis model linear (Breusch-Pagan, dll)
library(car)          # Uji Multikolinearitas (VIF) dan uji lanjut
library(sandwich)     # Estimasi Robust Standard Error (mengatasi heteroskedastisitas)

# 5. STATISTIK DESKRIPTIF & EKSPLORASI
library(psych)        # Ringkasan statistik deskriptif yang detail (fungsi describe)
library(corrplot)     # Visualisasi matriks korelasi dalam bentuk grafik
library(ggplot2)      # Membuat grafik (scatter plot, line chart, dll)
library(GGally)       # Matriks plot korelasi yang lebih kompleks (fungsi ggpairs)

# 6. PENYAJIAN HASIL (REPORTING)
library(modelsummary) # Membuat tabel ringkasan model yang modern & estetis
library(stargazer)    # Membuat output regresi format jurnal (Latex/HTML/Text)

#===============================================================================
# 1. SETTING DIRECTORY & LOAD DATA
#===============================================================================

# Mengatur folder kerja
setwd("C:/Users/windows/Downloads/TUTORIAL PANEL DATA WITH NANTA")

# Membaca dataset dari Excel
dfinvesment <- read_xlsx("Latihan_Panel_R.xlsx")

#===============================================================================
# 2. DATA INSPECTION (PREVIEW)
#===============================================================================
#melihat data 
head(dfinvesment, 15)  # Melihat 15 baris pertama
tail(dfinvesment, 20)  # Melihat 20 baris terakhir

#melihat karakter data
glimpse(dfinvesment)   # Ringkasan struktur data (ala dplyr)
str(dfinvesment)       # Cek karakteristik/tipe data tiap kolom

#===============================================================================
# 3. DATA CLEANING & TRANSFORMATION
#===============================================================================

# Memastikan kolom ID terbaca sebagai numerik
dfinvesment$ID <- as.numeric(dfinvesment$ID)

# Cek jumlah Missing Value (NA) secara keseluruhan
total_na <- sum(is.na(dfinvesment))
print(paste("Total NA:", total_na))

# Cek jumlah NA per kolom
colSums(is.na(dfinvesment))

# Mencari koordinat (baris & kolom) posisi NA
lokasi_na <- which(is.na(dfinvesment), arr.ind = TRUE)
print(lokasi_na)

#===============================================================================
# Tips: Jika ingin menghapus baris yang mengandung NA (Opsional)
# dfinvesment <- na.omit(dfinvesment)
#===============================================================================
#===============================================================================
# 4. ANALISIS DESKRIPTIF
#===============================================================================

#--- Statistik Ringkasan ---
# Menampilkan statistik dasar (Mean, Median, Min, Max, Kuartil) untuk semua variabel
summary(dfinvesment)         

# Membuat tabel ringkasan yang lebih rapi, mencakup standar deviasi dan histogram kecil (inline)
datasummary_skim(dfinvesment) 

#--- Analisis Korelasi ---
# Membuat vektor berisi nama-nama kolom yang akan dianalisis korelasinya
vars_pilihan <- c("Investment", "Profit", "Assets", "MarketShare")

# Menghitung koefisien korelasi Pearson. 
# use = "pairwise.complete.obs" memastikan korelasi tetap dihitung meskipun ada data kosong (NA)
cor_matrix <- cor(dfinvesment[, vars_pilihan], use = "pairwise.complete.obs")
print(cor_matrix) # Menampilkan angka korelasi di console

# Membuat tabel korelasi formal untuk kebutuhan laporan/skripsi
# fmt = 3 artinya membatasi angka di belakang koma menjadi 3 digit
datasummary_correlation(dfinvesment[, vars_pilihan], 
                        fmt = 3, 
                        title = "Tabel 2.1 Matriks Korelasi Variabel Investasi")

# Visualisasi Korelasi (Heatmap)
corrplot(cor_matrix, 
         method = "color",       # Menampilkan korelasi dalam bentuk blok warna
         type = "upper",         # Hanya menampilkan bagian segitiga atas agar tidak duplikasi
         addCoef.col = "black",  # Menampilkan angka koefisien korelasi di dalam kotak warna
         tl.col = "black",       # Mengatur warna teks label variabel menjadi hitam
         tl.srt = 45,            # Memutar teks label 45 derajat agar tidak tumpang tindih
         diag = FALSE,           # Menghilangkan angka korelasi 1 (diagonal) agar lebih bersih
         title = "\n\nMatriks Korelasi Variabel", # Memberikan judul pada grafik
         mar = c(0,0,3,0))       # Mengatur margin (Bawah, Kiri, Atas, Kanan) agar judul tidak terpotong

#===============================================================================
# 5. VISUALISASI DATA (EDA - Exploratory Data Analysis)
#===============================================================================

#--- Scatter Plot (Hubungan dua variabel secara visual) ---
#scetterplot sederhana
plot(dfinvesment$Profit, dfinvesment$Investment)
plot(dfinvesment$Profit, dfinvesment$Investment,
     main = "Scatterplot Profit vs Investment",
     xlab = "Profit (X1)",
     ylab = "Investment (Y)",
     pch = 16,            # Titik bulat padat
     col = "steelblue",   # Warna titik
     cex = 0.8)           # Ukuran titik (biar gak terlalu numpuk kalau N=5000)
abline(lm(Investment ~ Profit, data = dfinvesment), 
       col = "darkred",   # Warna garis
       lwd = 2)           # Ketebalan garis

ggplot(dfinvesment, aes(x = Investment, y = Profit)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) + # Membuat titik data (scatter)
  geom_smooth(method = "lm", color = "darkred", se = TRUE) + # Menambahkan garis tren linear (regresi)
  theme_minimal() + # Menggunakan tema latar belakang yang bersih/minimalis
  labs(title = "Hubungan Investment terhadap Profit", # Memberi judul dan keterangan sumbu
       x = "Investment",
       y = "Profit")

#--- Matriks Plot (Visualisasi hubungan banyak variabel secara simultan) ---
# Menampilkan korelasi, scatter plot, dan densitas antar variabel dalam satu frame
ggpairs(dfinvesment[, vars_pilihan]) + 
  theme_bw() # Menggunakan tema border hitam putih

#--- Histogram (Melihat distribusi data tunggal) ---
# Versi Base R: Cepat dan sederhana untuk pengecekan awal
hist(dfinvesment$Investment, 
     main = "Distribusi Investment", 
     xlab = "Nilai Investment", 
     col = "steelblue", 
     breaks = 10) # Membagi data menjadi 10 kelompok batang

# Versi ggplot2: Lebih estetis untuk kebutuhan presentasi
ggplot(dfinvesment, aes(x = Investment)) +
  geom_histogram(bins = 15, fill = "seagreen", color = "white") + # Mengatur jumlah batang (bins)
  theme_minimal() +
  labs(title = "Histogram Investment", x = "Investment", y = "Frekuensi")

# Menggunakan library psych untuk melihat histogram banyak variabel sekaligus
# Sangat berguna untuk mengecek normalitas data sebelum masuk ke model regresi
multi.hist(dfinvesment[, vars_pilihan], main = "Distribusi Variabel Pilihan")

#===============================================================================
#--------- 4. ESTIMASI MODEL DATA PANEL ----------------------------------------
#===============================================================================
library(plm)

# Pastikan data sudah didefinisikan sebagai pdata.frame
Paneldata <- pdata.frame(dfinvesment, index = c("ID", "Year"))

# A. Common Effect Model (Pooled OLS)
ModelCEM <- plm(Investment ~ Profit + Assets + MarketShare, data = Paneldata, model = "pooling")

# B. Fixed Effect Model (Within)
ModelFEM <- plm(Investment ~ Profit + Assets + MarketShare, data = Paneldata, model = "within")

# C. Random Effect Model
ModelREM <- plm(Investment ~ Profit + Assets + MarketShare, data = Paneldata, model = "random")

#===============================================================================
#--------- 5. UJI PEMILIHAN MODEL (DIAGNOSTIK) ---------------------------------
#===============================================================================

# 1. Chow Test (CEM vs FEM) -> H0: CEM
pooltest(ModelCEM, ModelFEM)

# 2. Hausman Test (FEM vs REM) -> H0: REM
phtest(ModelFEM, ModelREM)

# 3. LM Test (CEM vs REM) -> H0: CEM
plmtest(ModelCEM, type=c("bp"))


#===============================================================================
#--------- 5. UJI ASUMSI KLASIK (DETEKSI PENYAKIT DATA) ------------------------
#===============================================================================
# Kita menguji asumsi pada model terbaik hasil Hausman (ModelREM)

# A. Uji Multikolinearitas (VIF)
# Menghitung korelasi antar variabel independen
library(car)
model_vif <- lm(Investment ~ Profit + Assets + MarketShare, data = dfinvesment)
vif(model_vif) 

# B. Uji Heteroskedastisitas (Breusch-Pagan Test)
# Mengecek apakah varians error bersifat konstan
library(lmtest)
bptest(Investment ~ Profit + Assets + MarketShare + factor(ID), 
       data = dfinvesment, studentize = TRUE)
# Interpretasi: Jika p-value < 0.05, terdapat masalah Heteroskedastisitas.

# C. Uji Autokorelasi (Wooldridge Test)
# Mengecek korelasi antar waktu dalam satu individu
pbgtest(ModelREM)
# Interpretasi: Jika p-value < 0.05, terdapat masalah Autokorelasi (Serial Correlation).

# D. Uji Normalitas (Visual Residual)
# Mengingat N=5000, kita gunakan pendekatan visual (Asimtotik)
qqnorm(residuals(ModelREM))
qqline(residuals(ModelREM), col = "red")

#===============================================================================
#--------- 6. KOREKSI (ROBUST STANDARD ERROR) ----------------------------------
#===============================================================================
# Jika hasil B atau C di atas menunjukkan p-value < 0.05, 
# maka wajib menggunakan Robust Standard Error.

library(sandwich)
robust_results <- coeftest(ModelREM, 
                           vcov = vcovHC(ModelREM, 
                                         method = "arellano", # Menangani Hetero & Auto
                                         type = "HC1", 
                                         cluster = "group"))

#===============================================================================
#--------- 7. EXPORT KOMPARASI FINAL -------------------------------------------
#===============================================================================
library(stargazer)

stargazer(ModelCEM, ModelFEM, ModelREM, ModelREM,
          se = list(NULL, NULL, NULL, robust_results[,2]), # Suntik Robust SE ke kolom 4
          type = "text", 
          column.labels = c("Pooled (CEM)", "Fixed (FEM)", "Random (REM)", "REM Robust"),
          model.numbers = TRUE,
          dep.var.labels = "Investment (Y)",
          covariate.labels = c("Profit (X1)", "Assets (X2)", "Market Share (X3)"),
          add.lines = list(c("Uji Asumsi Klasik", "Dipenuhi", "Dipenuhi", "Dipenuhi", "Koreksi Robust")),
          notes = "Diolah oleh Hananta Angger Yuga Prawira (2026)",
          out = "Hasil_Regresi_Final.txt")
