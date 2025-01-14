# =================================================================
# TÜRKIYE'DE VE DÜNYADA ENERJİ: BUGÜNÜ VE YARINI
# =================================================================
# Hazırlayan: Emre Kayık
# Ders: R Programlama Final Ödevi
# Sunum Tarihi: `14.01.2025`
# =================================================================

# ----- 1. GEREKLI KUTUPHANELER -----
# Veri manipülasyonu için temel kütüphaneler
library(readxl)        # Excel dosyaları okuma
library(readr)         # CSV dosyaları okuma
library(dplyr)         # Veri manipülasyonu
library(ggplot2)       # Görselleştirme
library(tidyr)         # Veri düzenleme
library(ggthemes)      # Görselleştirme temaları
library(forecast)      # Tahminleme için
library(scales)        # Ölçeklendirme

# ----- 2. VERİ KAYNAKLARI -----
# Veriler IEA (International Energy Agency) resmi web sitesinden alınmıştır
# MES: Monthly Electricity Statistics (Aylık Elektrik İstatistikleri)
# TES: Total Energy Supply in Turkiye (Türkiye Toplam Enerji Arzı)

# MES verisini oku
mesdata <- read_excel("datas/MES.xlsx", sheet = 1)

# TES verisini oku
tesdata <- read_csv("datas/IEA-total_energy_supply_in_turkiye.csv", 
                   show_col_types = FALSE)

# ----- 3. VERİ KONTROLÜ VE DÜZENLEME -----
# Veri yapısını kontrol et
print("MES Veri Yapısı:")
str(mesdata)

print("TES Veri Yapısı:")
str(tesdata)

# ----- 4. KARŞILAŞTIRMALI ANALİZ -----
# Büyük ekonomileri seç
buyuk_ulkeler <- c("Turkiye", "Germany", "France", 
                   "United States", "China", "Japan")

# Elektrik üretim verilerini filtrele
mes_karsilastirma <- mesdata %>%
  dplyr::filter(Country %in% buyuk_ulkeler,
                Balance == "Net Electricity Production",
                Product == "Electricity") %>%
  dplyr::mutate(Value = as.numeric(Value)) %>%
  dplyr::select(dplyr::all_of(c("Country", "Value")))

# Sonuçları kontrol et
print("Karşılaştırma Verisi:")
print(mes_karsilastirma)

# ----- 5. GÖRSELLEŞTİRMELER -----

# 5.1 Ülkeler Arası Karşılaştırma
uretim_karsilastirma <- ggplot(mes_karsilastirma, 
       aes(x = reorder(Country, Value), y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Büyük Ekonomilerin Elektrik Üretimi Karşılaştırması",
       subtitle = "Net Elektrik Üretimi (GWh)",
       x = "Ülke",
       y = "Elektrik Üretimi (GWh)",
       caption = "Kaynak: IEA, 2024") +
  theme(text = element_text(size = 12))

# 5.2 Türkiye'nin Enerji Trendi
turkiye_trend <- ggplot(tesdata, aes(x = Year, y = Value)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  theme_minimal() +
  labs(title = "Türkiye'nin Toplam Enerji Arzı Trendi",
       subtitle = "Yıllık Değişim",
       x = "Yıl",
       y = "Enerji Arzı",
       caption = "Kaynak: IEA") +
  scale_x_continuous(breaks = seq(min(tesdata$Year), 
                                max(tesdata$Year), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.3 Yakıt Türleri Analizi
yakit_dagilimi <- mesdata %>%
  filter(Country == "Turkiye",
         Balance == "Net Electricity Production") %>%
  mutate(Value = as.numeric(Value)) %>%
  group_by(Product) %>%
  summarise(Toplam = sum(Value, na.rm = TRUE)) %>%
  filter(Toplam > 0)

yakit_grafik <- ggplot(yakit_dagilimi, 
       aes(x = reorder(Product, Toplam), y = Toplam, fill = Product)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Turkiye'de Yakit Turlerine Gore Elektrik Üretimi",
       x = "Yakit Turu",
       y = "Uretim Miktari (GWh)",
       fill = "Yakit Turu") +
  theme(legend.position = "none")

# ----- 6. GRAFİKLERİ KAYDET -----
library(svglite)

ggsave("grafikler/1_uretim_karsilastirma.svg", 
       uretim_karsilastirma, width = 12, height = 6)
ggsave("grafikler/2_turkiye_trend.svg", 
       turkiye_trend, width = 12, height = 6)
ggsave("grafikler/3_yakit_dagilimi.svg", 
       yakit_grafik, width = 12, height = 8)

# ----- 7. SONUÇLAR VE YORUMLAR -----
# Analiz Sonuçları:
# 1. Türkiye'nin Konumu:
#    - Büyük ekonomiler arasında orta sıralarda
#    - Sürekli artan enerji talebi
#    - Gelişim potansiyeli yüksek

# 2. Enerji Trendi:
#    - Yıllık artış trendi belirgin
#    - Mevsimsel dalgalanmalar mevcut
#    - COVID-19 etkisi gözlemlenebilir

# 3. Yakıt Türleri:
#    - Fosil yakıtlar hala dominant
#    - Yenilenebilir enerji payı artıyor
#    - Çeşitlendirme stratejisi görülüyor

# ----- 8. ÖNERİLER -----
# 1. Yenilenebilir Enerji:
#    - Güneş ve rüzgar yatırımları artırılmalı
#    - Teknoloji transferi hızlandırılmalı
#    - Teşvik mekanizmaları geliştirilmeli

# 2. Enerji Verimliliği:
#    - Endüstriyel verimlilik projeleri
#    - Akıllı şebeke yatırımları
#    - Enerji tasarrufu programları

# 3. Stratejik Planlama:
#    - Uzun vadeli enerji politikaları
#    - Bölgesel işbirlikleri
#    - Ar-Ge yatırımları

# ----- 4. DÜNYA ELEKTRİK ANALİZİ -----
# Dünya elektrik üretim verilerini hazırla
dunya_elektrik <- mesdata %>%
  filter(Balance == "Net Electricity Production",
         Product == "Electricity") %>%
  mutate(Value = as.numeric(Value)) %>%
  group_by(Country) %>%
  summarise(Toplam_Uretim = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Toplam_Uretim)) %>%
  head(10)  # İlk 10 ülke

# Dünya elektrik üretimi görselleştirme
dunya_elektrik_grafik <- ggplot(dunya_elektrik, 
       aes(x = reorder(Country, Toplam_Uretim), y = Toplam_Uretim)) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Dünya Elektrik Üretimi - İlk 10 Ülke",
       subtitle = "Net Elektrik Üretimi (GWh)",
       x = "Ülke",
       y = "Elektrik Üretimi (GWh)",
       caption = "Kaynak: IEA, 2024") +
  theme(text = element_text(size = 12))

# ----- 5. REGRESYON ANALİZLERİ -----

# 5.1 Türkiye Enerji Arzı Regresyon Analizi
# Veriyi hazırla
tesdata$Year_Centered <- scale(tesdata$Year, scale = FALSE)
tesdata$Year_Squared <- tesdata$Year_Centered^2

# Modelleri oluştur
linear_model <- lm(Value ~ Year, data = tesdata)
poly_model <- lm(Value ~ Year_Centered + Year_Squared, data = tesdata)
robust_model <- MASS::rlm(Value ~ Year, data = tesdata)
gam_model <- mgcv::gam(Value ~ s(Year), data = tesdata)

# Model performanslarını hesapla
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# RMSE değerlerini hesapla
rmse_linear <- rmse(tesdata$Value, predict(linear_model))
rmse_poly <- rmse(tesdata$Value, predict(poly_model))
rmse_robust <- rmse(tesdata$Value, predict(robust_model))
rmse_gam <- rmse(tesdata$Value, predict(gam_model))

# Regresyon görselleştirmeleri
reg_linear <- ggplot(tesdata, aes(x = Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Doğrusal Regresyon: Türkiye Enerji Arzı",
       x = "Yıl", y = "Enerji Arzı",
       caption = paste("RMSE:", round(rmse_linear, 2)))

reg_poly <- ggplot(tesdata, aes(x = Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  theme_minimal() +
  labs(title = "Polinom Regresyon: Türkiye Enerji Arzı",
       x = "Yıl", y = "Enerji Arzı",
       caption = paste("RMSE:", round(rmse_poly, 2)))

# ----- 6. ZAMAN SERİSİ ANALİZİ -----

# 6.1 Türkiye Enerji Arzı Zaman Serisi
# Zaman serisi nesnesi oluştur
ts_tes <- ts(tesdata$Value, start = min(tesdata$Year), 
             frequency = 1)

# Ayrıştırma analizi
decomp_tes <- decompose(ts_tes)

# Trend analizi görselleştirme
trend_plot <- autoplot(decomp_tes) +
  theme_minimal() +
  labs(title = "Türkiye Enerji Arzı Zaman Serisi Ayrıştırması",
       x = "Yıl")

# ARIMA modeli
arima_model <- auto.arima(ts_tes)
forecast_tes <- forecast(arima_model, h = 10)  # 10 yıllık tahmin

# Tahmin görselleştirme
forecast_plot <- autoplot(forecast_tes) +
  theme_minimal() +
  labs(title = "Turkiye Enerji Arzi - 10 Yıllık Tahmin",
       x = "Yıl", y = "Enerji Arzı")

# ----- 7. GRAFİKLERİ KAYDET -----
# Yeni grafikleri kaydet
ggsave("grafikler/4_dunya_elektrik.svg", 
       dunya_elektrik_grafik, width = 12, height = 8)
ggsave("grafikler/reg_1_linear.svg", 
       reg_linear, width = 10, height = 6)
ggsave("grafikler/reg_2_polynomial.svg", 
       reg_poly, width = 10, height = 6)
ggsave("grafikler/ts_1_trend_analizi.svg", 
       trend_plot, width = 12, height = 8)
ggsave("grafikler/ts_2_forecast.svg", 
       forecast_plot, width = 12, height = 6)

# ----- 8. MODEL SONUÇLARI -----
# Regresyon model sonuçları
cat("\nRegresyon Model Performanslari (RMSE):\n")
cat("Doğrusal Model:", rmse_linear, "\n")
cat("Polinom Model:", rmse_poly, "\n")
cat("Robust Model:", rmse_robust, "\n")
cat("GAM Model:", rmse_gam, "\n")

# ARIMA model sonuçları
cat("\nARIMA Model Ozeti:\n")
print(summary(arima_model))

# =================================================================
# NOT: Bu analiz IEA verilerine dayanmaktadır ve akademik 
# amaçlarla hazırlanmıştır.
# ================================================================


