# Cargar bases

getwd()
library(readxl)
if(!require(tidyverse)){install.packages("tidyverse")}; library(tidyverse)
### es una funcion util para instalar y cargar paketes que se pueden necesitar o no. 
peso_seco <- read.csv("./db/peso_seco.csv")
tricomas <- read.csv("./db/tricomas.csv")




#################################################################################
###############  PROBANDO GRAFICAS EN R  ########################################
#################################################################################
head(peso_seco)
summary(peso_seco)
hist(peso_seco$sla) ### así queda con distribución sesgada
hist(log(peso_seco$sla)) ### así queda con distribucion normal

head(tricomas) 
summary(tricomas)

peso_seco %>% count(ID)
tricomas %>% count(ID)

tricomas %>% 
  group_by(ID, ciudad, ambiente) %>% 
  summarise(mean_trico = mean(leafMean, na.rm = T))

write.table(
  prom_trico, "./db/prom_tricomas.csv", 
  sep = ",", 
  col.names = T, row.names = F)

prom_tri <- read.csv("./db/prom_tricomas.csv")

plot(log(peso_seco$sla), prom_tri$tricomas)

### https://www.youtube.com/watch?v=BffAQZzbBms&t=3873s

########## Linear Regresion ##########

prueba1 <- glm(log(peso_seco$sla)~prom_tri$tricomas)
summary(prueba1)

prueba2 <- lm(log(peso_seco$sla)~prom_tri$tricomas)
summary(prueba2)
# residuals: la mediana está cerca del cero, parece que el cuartil 1 y 3 estan equidistantes.
# el modelo tiene intercepto (por eso salio significativo, lo cual es importante si me interesa hacer un modelo predictivo)
# el peso seco es marginalmente significativo (p>0.05)
# 2% de la variabilidad de los tricomas se explica por el peso seco (Multiple R-squared:  0.020).
anova(prueba2)
# sale el mismo vaor de F
confint(prueba2)
