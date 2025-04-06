# Gerekli kütüphaneleri yüklüyoruz
install.packages("dplyr")  
install.packages("readr")  
install.packages("ggplot2")  
install.packages("cowplot")
install.packages("ggthemes")





#Atama 
df <- avocado
View(df)

# Veri setini gözden geçirelim
# İlk 10 satırı gösteriyoruz
head(df, 10)

# Veri setinin yapısı hakkında bilgi alıyoruz
str(df)

# 'type' ve 'region' kolonlarını faktör (factor) olarak değiştiriyoruz
df$type <- as.factor(df$type)
df$region <- as.factor(df$region)

# 'type' ve 'region' faktör kolonlarının seviyelerini gözden geçirelim
levels(df$type)
levels(df$region)

# Gereksiz kolonları yeniden adlandırıyoruz
library(dplyr)
df <- df %>%
    rename(
        SmallOrMediumSize = `4046`,
        LargeSize = `4225`,
        ExtraLarge = `4770`
    )

colnames(df)
# Gereksiz kolonları veri setinden çıkarıyoruz
df <- df %>%
    select(-`Total Bags`, -`Small Bags`, -`Large Bags`, -`XLarge Bags`)

# Satır ve sütun sayısını öğreniyoruz
dim(df)

# Veri seti hakkında özet istatistikleri alıyoruz
summary(df)

library(ggplot2)
# Grafik boyutunu ayarlıyoruz
# Bu ayar grafiklerin genişliğini ve yüksekliğini belirler.
options(repr.plot.width = 8, repr.plot.height = 4)

# ggplot fonksiyonu
# Burada x ekseninde ortalama fiyat (AveragePrice) ve renklendirme için 'type' sütunu kullanılıyor.
ggplot(df, aes(x = AveragePrice, fill = type)) +
    
    # Yoğunluk grafiği.
    # Bu, verinin dağılımını ve yoğunluğunu görsel olarak sunar.
    geom_density() +
    
    # Her avokado tipi için ayrı grafik kutuları (facet) oluşturuluyor.
    # Farklı türdeki avokadolar için ayrı ayrı grafikler çizilir.
    facet_wrap(~type) +
    
    # Daha sade ve temiz bir tema.
    # 'theme_minimal()' fonksiyonu, gereksiz süslemeleri kaldırır.
    theme_minimal() +
    
    # Grafik başlığı ortaya hizalandı ve açıklama kutusu (legend) alt tarafa yerleştirildi
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
    ) +
    
    # Grafik başlığı eklendi
    # Burada başlık olarak 'Avocado Price by Type' belirleniyor.
    labs(title = "Avocado Price by Type") +
    
    # Renk paleti belirlendi
    # 'Set2' pastel renk paletiymiş
    scale_fill_brewer(palette = "Set2")

# Farklı avokado türlerinin (type) toplam hacimlerinin (Total.Volume) ortalamalarını
# hesaplar ve her bir türün toplam hacim içindeki yüzdelik payını belirler.
# Sonuç olarak her avokado türünün ortalama hacmi (avg.vol) ve bu hacmin toplam hacme oranı (% olarak) 
# elde edilir ve 'vol_type' olarak tanımlanır
vol_type <- df %>% 
    group_by(type) %>%  # 'type' sütununa göre gruplama yapılır. Her avokado türü için ayrı işlem yapılacak.
    
    # 'Total.Volume' sütununun her grup için ortalaması hesaplanır.
    # 'summarise()' fonksiyonu her tür için ortalama hacmi (avg.vol) oluşturur.
    summarise(avg.vol = mean(`Total Volume`)) %>%  # Burada Total.Volume doğru yazıldı.
    
    # 'mutate()' fonksiyonu yeni bir sütun ekler.
    # 'prop.table()' her türün ortalama hacminin toplam hacme oranını hesaplar.
    # Sonra bu oranı yüzdelik formata dönüştürmek için 100 ile çarparız.
    mutate(pct = prop.table(avg.vol) * 100)

# Sonuçları gözden geçirelim
print(vol_type)
View(vol_type)

library(cowplot)
# 'df' veri setini 'seasonal_df' olarak kopyalıyoruz
seasonal_df <- df

# 'Date' kolonundaki tarih bilgisini kullanarak yeni bir 'month_year' kolonunu oluşturuyoruz.
# Bu yeni kolonda yıl ve ay bilgisi 'YYYY-MM' formatında olacak.
seasonal_df$month_year <- format(as.Date(df$Date), "%Y-%m")

# 'Date' kolonundaki tarih bilgisini kullanarak yeni bir 'month' kolonunu oluşturuyoruz.
# Bu yeni kolonda sadece ay (02, 03, vb.) bilgisi yer alacak.
seasonal_df$month <- format(as.Date(df$Date), "%m")

# 'Date' kolonundaki tarih bilgisini kullanarak yeni bir 'year' kolonunu oluşturuyoruz.
# Bu yeni kolonda sadece yıl (2021, 2022, vb.) bilgisi yer alacak.
seasonal_df$year <- format(as.Date(df$Date), "%Y")

# 'month' kolonundaki ay numarasına karşılık gelen ay ismini (örneğin, 01 => "Jan") alıyoruz.
# 'month.abb' R'deki yerleşik bir vektördür ve ayların kısaltmalarını içerir (Jan, Feb, Mar, vb.).
# Bu, ay numarasını ay ismine dönüştürmek için kullanılıyor.
seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])

# 'monthabb' kolonunun değerlerini, ayların sırasını koruyarak bir faktör (factor) veri tipi olarak dönüştürüyoruz.
# Bu, grafiklerde ve diğer analizlerde ayları sıralı bir şekilde kullanabilmek için gereklidir.
seasonal_df$monthabb = factor(seasonal_df$monthabb, levels = month.abb)

# Grafik boyutunu ayarlıyoruz 
# Bu ayar grafiklerin genişliğini 8, yüksekliğini ise 6 olarak belirliyor.
options(repr.plot.width=8, repr.plot.height=6) 

# Yeni bir 'season' (mevsim) kolonunu ekliyoruz.
# 'seasonal_df' veri setinde, ay (month) değerine göre mevsim belirliyoruz.
seasonal_df$season <- ifelse(seasonal_df$month %in% c("03", "04", "05"), "Spring",  # İlkbahar için aylar
                            ifelse(seasonal_df$month %in% c("06", "07", "08"), "Summer",  # Yaz için aylar
                                  ifelse(seasonal_df$month %in% c("09", "10", "11"), "Fall", "Winter")))  # Sonbahar ve Kış için aylar

# Konvansiyonel avokadolar (conventional) için mevsimsel grafik oluşturuyoruz.
# Veri setinde sezon, yıl ve ortalama fiyat bilgilerini seçiyoruz.
seasonality.plot.conventional <- seasonal_df %>% 
  select(season, year, AveragePrice, type) %>%  # 'season', 'year', 'AveragePrice', ve 'type' kolonlarını seçiyoruz.
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%  # 'type' olarak konvansiyonel ve belirli yılları filtreliyoruz.
  group_by(season, year) %>%  # Mevsim ve yıl bazında gruplama yapıyoruz.
  summarize(avg = mean(AveragePrice)) %>%  # Her grup için ortalama fiyatı hesaplıyoruz.
  
  # ggplot ile görselleştirme işlemi başlatıyoruz:
  ggplot(aes(x = season, y = avg, color = season)) + 
    geom_point(size = 3) +  # Her mevsim için bir nokta çiziyoruz (grafikteki noktalar)
    
    # Yatay çizgi segmenti çiziyoruz, her sezon için fiyatı sıfırdan o sezonun fiyatına kadar gösteriyor.
    geom_segment(aes(x = season, 
                     xend = season, 
                     y = 0, 
                     yend = avg)) + 
    
    # Grafik üzerinde mevsimleri yatay şekilde yerleştiriyoruz
    coord_flip() + 
    
    # Her yılı ayrı bir panelde göstermek için facet_wrap kullanıyoruz
    facet_wrap(~as.factor(year)) + 
    
    # Minimal tema kullanıyoruz
    theme_minimal() + 
    
    # Başlık, arka plan ve legend (açıklama kutusu) için görsel düzenlemeler
    theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F4F6F7")) + 
    
    # Renk paletini özelleştiriyoruz
    scale_color_manual(values = c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
    
    # Grafik başlığı ve eksen etiketleri
    labs(title = "Conventional Avocados by Season", x = "Season", y = "Average Price") + 
    
    # Fiyatları noktaların yanında göstermek için metin etiketi ekliyoruz
    geom_text(aes(x = season, y = 0.01, label = paste0("$ ", round(avg, 2))),  # Fiyatı uygun bir noktada gösterecek şekilde
              hjust = -0.5, vjust = -0.5, size = 4, 
              colour = "black", fontface = "italic", angle = 360)

# Organik avokadolar (organic) için aynı işlemi tekrar yapıyoruz.
# Bu sefer 'type' filtresini "organic" olarak değiştiriyoruz.
seasonality.plot.organic <- seasonal_df %>% 
  select(season, year, AveragePrice, type) %>%  # Yine sezon, yıl, ortalama fiyat ve tip bilgilerini seçiyoruz.
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%  # 'type' olarak organik ve belirli yılları filtreliyoruz.
  group_by(season, year) %>%  # Mevsim ve yıl bazında gruplama yapıyoruz.
  summarize(avg = mean(AveragePrice)) %>%  # Her grup için ortalama fiyatı hesaplıyoruz.
  
  # Grafik oluşturma işlemi:
  ggplot(aes(x = season, y = avg, color = season)) + 
    geom_point(size = 3) +  # Noktalar
    geom_segment(aes(x = season, xend = season, y = 0, yend = avg)) +  # Yatay çizgi
    coord_flip() +  # Yatay eksende gösterim
    facet_wrap(~as.factor(year)) +  # Yıllara göre facet
    theme_minimal() +  # Tema
    theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F4F6F7")) +  # Tema düzenlemeleri
    scale_color_manual(values = c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) +  # Renkler
    labs(title = "Organic Avocados by Season", x = "Season", y = "Average Price") +  # Başlık ve etiketler
    geom_text(aes(x = season, y = 0.01, label = paste0("$ ", round(avg, 2))),  # Fiyat etiketleri
              hjust = -0.5, vjust = -0.5, size = 4, 
              colour = "black", fontface = "italic", angle = 360)

# Konvansiyonel ve organik avokadolar için oluşturduğumuz grafikleri, üst üste yerleştiriyoruz.
# 'plot_grid' fonksiyonu ile iki grafiği bir arada gösteriyoruz.
plot_grid(seasonality.plot.conventional, seasonality.plot.organic, nrow = 2)



library(ggthemes)
# İlk olarak, orijinal veri setini 'seasonal_df2' adıyla kopyalıyoruz.
seasonal_df2 <- df

# 'Date' sütununu 'Date' tipine dönüştürüp, yıl ve ay bilgilerini 'month_year' sütununda birleştiriyoruz (Yıl-Ay formatında).
seasonal_df2$month_year <- format(as.Date(df$Date), "%Y-%m")

# 'Date' sütunundan sadece ay bilgisini çıkarıyoruz ve bunu 'month' sütununa ekliyoruz.
seasonal_df2$month <- format(as.Date(df$Date), "%m")

# 'Date' sütunundan sadece yıl bilgisini çıkarıyoruz ve bunu 'year' sütununa ekliyoruz.
seasonal_df2$year <- format(as.Date(df$Date), "%Y")

# 'month' sütunundaki sayıları, ay isimleri ile eşliyoruz (1 -> "Jan", 2 -> "Feb" gibi).
seasonal_df2$monthabb <- sapply(seasonal_df2$month, function(x) month.abb[as.numeric(x)])

# 'monthabb' sütununu faktör olarak tanımlıyoruz ve ayları sırasıyla yerleştiriyoruz (Ocak'tan Aralık'a doğru).
seasonal_df2$monthabb = factor(seasonal_df2$monthabb, levels = month.abb)

# Yoğunluk grafiği (density plot) çizerek, her yıl için fiyat dağılımını gözlemliyoruz.
ggplot(seasonal_df2, aes(x = AveragePrice, fill = as.factor(year))) + 
  geom_density(alpha = .5) +  # Yoğunluk grafiği, şeffaflık ile görselleştiriliyor.
  
  theme_economist() +  # Grafik için Economist temasını uyguluyoruz.
  
  facet_wrap(~ year) +  # Yıllara göre ayrı grafik kutuları oluşturuluyor (her yıl için bir grafik).
  
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  # Grafik başlığını ortalıyoruz ve arka plan rengini sarı yapıyoruz.
  
  guides(fill = FALSE) +  # Renk rehberini (legend) gizliyoruz.
  
  labs(title="Distribution of Prices by year", x = 'Average Price', y = 'Density') + 
  # Başlık ve eksen etiketlerini ekliyoruz.
  
  scale_fill_manual(values=c("#2E64FE", "#40FF00", "#FE642E", "#FE2E2E"))  # Farklı yıllar için renkler belirliyoruz.
