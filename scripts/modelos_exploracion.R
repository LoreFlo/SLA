# Cargar bases

getwd()
library(readxl)

if(!require(tidyverse)){install.packages("tidyverse")}; library(tidyverse)
### es una funcion util para instalar y cargar paketes que se pueden necesitar o no. 
peso_seco <- read.csv("./db/peso_seco.csv")
tricomas <- read.csv("./db/tricomas.csv")
npk <- read.csv(file = './db/npk.csv')


#################################################################################
###############  PROBANDO GRAFICAS EN R  ########################################
#################################################################################

## PESO SECO
head(peso_seco)
peso_seco %>% count(ID)
summary(peso_seco)
hist(peso_seco$sla) ### así queda con distribución sesgada
hist(log(peso_seco$sla)) ### así queda con distribucion normal


## TRICOMAS
head(tricomas)
tricomas %>% count(ID)
summary(tricomas)

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

## NPK
head(npk)
npk %>% count(ID)
summary(npk)
hist(log(npk$P_mg_Kg))
hist(log(npk$C_porcentaje))
hist(npk$K_Cmol_Kg)
hist(npk$N_porcentaje)

qqnorm(npk$C_porcentaje)
qqnorm(npk$N_porcentaje)


########## Linear Regresion ##########

plot(npk$C_porcentaje, log(peso_seco$sla))

# grafico de correlaciones
c1<-cor(npk[,9:12])
corrplot(c1)


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

#pairs.panels() para visualizar multiples datos

