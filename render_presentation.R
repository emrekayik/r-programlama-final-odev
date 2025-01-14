# Sunum oluşturma script'i

# Gerekli kütüphaneler
library(rmarkdown)
library(officer)

# PowerPoint sunumunu oluştur
render("sunum.Rmd", 
       output_format = "powerpoint_presentation",
       output_file = "Turkiye_Enerji_Analizi_Sunumu.pptx") 