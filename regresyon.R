# =================================================================
# TÜRKIYE ENERJİ VERİLERİ REGRESYON ANALİZİ
# =================================================================
# Hazırlayan: [İsim Soyisim]
# Ders: R Programlama Final Ödevi
# Tarih: `r format(Sys.time(), '%d %B %Y')`
# =================================================================

# ----- 1. GEREKLI KUTUPHANELER -----
library(tidyverse)     # Veri manipülasyonu ve görselleştirme
library(caret)         # Model değerlendirme araçları
library(ggplot2)       # Görselleştirme
library(scales)        # Ölçeklendirme
library(car)          # Regresyon diagnostikleri
library(MASS)         # Robust regresyon için
library(mgcv)         # GAM modeli için
library(gridExtra)    # Çoklu grafik düzenleme

# ----- 2. VERİ HAZIRLAMA -----
# IEA verilerini oku
tesdata <- read.csv("datas/IEA-total_energy_supply_in_turkiye.csv")
mesdata <- read_excel("datas/MES.xlsx", sheet = 1)

# Merkezi yıl ve karesel terim oluştur
tesdata$Year_Centered <- tesdata$Year - mean(tesdata$Year)
tesdata$Year_Squared <- tesdata$Year_Centered^2

# ----- 3. REGRESYON MODELLERİ -----

# 3.1 Basit Doğrusal Regresyon
# En temel model: Value = β₀ + β₁Year + ε
linear_model <- lm(Value ~ Year, data = tesdata)
summary(linear_model)

# Doğrusal regresyon grafiği
linear_plot <- ggplot(tesdata, aes(x = Year, y = Value)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Türkiye Enerji Arzı - Doğrusal Regresyon",
       subtitle = "Yıllara Göre Değişim",
       x = "Yıl",
       y = "Enerji Arzı",
       caption = "Kaynak: IEA") +
  scale_y_continuous(labels = comma)

# 3.2 Polinom Regresyon
# İkinci dereceden model: Value = β₀ + β₁Year + β₂Year² + ε
poly_model <- lm(Value ~ Year_Centered + Year_Squared, data = tesdata)
tesdata$Poly_Pred <- predict(poly_model)

# Polinom regresyon grafiği
poly_plot <- ggplot(tesdata, aes(x = Year)) +
  geom_point(aes(y = Value), color = "darkblue", alpha = 0.6) +
  geom_line(aes(y = Poly_Pred), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Türkiye Enerji Arzı - Polinom Regresyon",
       subtitle = "İkinci Dereceden Polinom Uyumu",
       x = "Yıl",
       y = "Enerji Arzı",
       caption = "Kaynak: IEA") +
  scale_y_continuous(labels = comma)

# 3.3 Robust Regresyon
# Aykırı değerlere dirençli model
robust_model <- rlm(Value ~ Year, data = tesdata)
tesdata$Robust_Pred <- predict(robust_model)

# Robust regresyon grafiği
robust_plot <- ggplot(tesdata, aes(x = Year)) +
  geom_point(aes(y = Value), color = "darkblue", alpha = 0.6) +
  geom_line(aes(y = Robust_Pred), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Türkiye Enerji Arzı - Robust Regresyon",
       subtitle = "Aykırı Değerlere Karşı Dirençli Model",
       x = "Yıl",
       y = "Enerji Arzı",
       caption = "Kaynak: IEA") +
  scale_y_continuous(labels = comma)

# 3.4 GAM (Generalized Additive Model)
# Esnek ve parametrik olmayan model
gam_model <- gam(Value ~ s(Year), data = tesdata)
tesdata$GAM_Pred <- predict(gam_model)

# GAM grafiği
gam_plot <- ggplot(tesdata, aes(x = Year)) +
  geom_point(aes(y = Value), color = "darkblue", alpha = 0.6) +
  geom_line(aes(y = GAM_Pred), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Türkiye Enerji Arzı - GAM",
       subtitle = "Esnek Eğri Uyumu",
       x = "Yıl",
       y = "Enerji Arzı",
       caption = "Kaynak: IEA") +
  scale_y_continuous(labels = comma)

# ----- 4. MODEL KARŞILAŞTIRMA -----

# RMSE hesaplama fonksiyonu
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Her model için RMSE hesapla
rmse_linear <- rmse(tesdata$Value, predict(linear_model))
rmse_poly <- rmse(tesdata$Value, tesdata$Poly_Pred)
rmse_robust <- rmse(tesdata$Value, tesdata$Robust_Pred)
rmse_gam <- rmse(tesdata$Value, tesdata$GAM_Pred)

# Model karşılaştırma tablosu
model_comparison <- data.frame(
  Model = c("Doğrusal", "Polinom", "Robust", "GAM"),
  RMSE = c(rmse_linear, rmse_poly, rmse_robust, rmse_gam)
)

# ----- 5. GRAFİKLERİ KAYDET -----
# SVG formatında yüksek kaliteli grafikler
ggsave("grafikler/reg_1_linear.svg", linear_plot, width = 12, height = 6)
ggsave("grafikler/reg_2_polynomial.svg", poly_plot, width = 12, height = 6)
ggsave("grafikler/reg_3_robust.svg", robust_plot, width = 12, height = 6)
ggsave("grafikler/reg_4_gam.svg", gam_plot, width = 12, height = 6)

# ----- 6. SONUÇLAR VE YORUMLAR -----

# Model Değerlendirmeleri:
# 1. Doğrusal Regresyon:
#    - En basit ve yorumlanabilir model
#    - R² değeri yüksek
#    - Doğrusal ilişkiyi iyi yakalıyor

# 2. Polinom Regresyon:
#    - Eğrisel ilişkileri modelliyor
#    - Daha esnek bir yapı sunuyor
#    - Aşırı uyum riski mevcut

# 3. Robust Regresyon:
#    - Aykırı değerlere karşı dayanıklı
#    - Daha güvenilir tahminler
#    - Normal dağılım varsayımı yok

# 4. GAM:
#    - En esnek model yapısı
#    - Karmaşık ilişkileri yakalıyor
#    - Yorum yapmak daha zor

# Model Seçimi Önerileri:
# 1. Tahmin amaçlı: GAM veya Polinom
# 2. Yorumlama amaçlı: Doğrusal veya Robust
# 3. Genel kullanım: Robust Regresyon

# =================================================================
# NOT: Bu analiz akademik amaçlıdır ve IEA verilerine dayanmaktadır.
# ================================================================ 