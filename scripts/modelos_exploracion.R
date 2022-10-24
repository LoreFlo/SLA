# Cargar bases

getwd()
library(readxl)
peso_seco <- read.csv("./db/peso_seco.csv")
tricomas <- read.csv("./db/tricomas.csv")




#################################################################################
###############  PROBANDO GRAFICAS EN R  ########################################
#################################################################################
head(peso_seco)
hist(peso_seco$sla)
hist(log(peso_seco$sla))

head(tricomas)

plot(log(peso_seco$sla), tricomas$leafMean)
### https://www.youtube.com/watch?v=BffAQZzbBms&t=3873s