#############################################################################
#### CALCULO DE SPECIFIC LEAF AREA EN MUESTRAS DE CAMPO DE R. nudiflora #####
#############################################################################

# Cargar bases

getwd()
library(readxl)
peso_seco <- read_xlsx("./db/peso_seco_accesorias.xlsx")
tricomas <- read_xlsx("./db/TRICOMA_LORENA.xlsx")

#install.packages("googledrive")
#install.packages("googlesheets4")
#library(googlesheets4)
#leer base de datos desde un google sheet
#x <- read_sheet('https://docs.google.com/spreadsheets/d/15qjj8LPQstf_vaI0A3xTrmIB4Zfl5dVR/edit#gid=555512928')
# código en proceso. Necesito convertir a csv para continuar

head(peso_seco)

# Calcular promedio del peso por lugar de colecta 

library(dplyr)

peso_seco$ind1_gr <- as.double(peso_seco$ind1_gr) ##todo esto es para cambiar el
peso_seco$ind2_gr <- as.double(peso_seco$ind2_gr) ##tipo de dato que tenian las var
peso_seco$ind3_gr <- as.double(peso_seco$ind3_gr) ##por default al cargar las bd
peso_seco$ind4_gr <- as.double(peso_seco$ind4_gr)
peso_seco$ind5_gr <- as.double(peso_seco$ind5_gr)
peso_seco$sitio <- as.character(peso_seco$sitio)
peso_seco$parche <- as.character(peso_seco$parche)

head(peso_seco)

peso_seco$promedio <- rowMeans(peso_seco[ ,c(7,8,9,10,11)], na.rm = TRUE)
head(peso_seco)

# calcular SLA por lugar de colecta
### el área de todas las muestras es la misma = 0.211 cm2

peso_seco <- mutate(peso_seco, sla = promedio/0.211)
head(peso_seco)

# calcular Prueba de T entre ambientes

amb_t_sla <- t.test(peso_seco$sla ~ peso_seco$ambiente, var.equal = T) 
amb_t_sla
### https://www.youtube.com/watch?v=NlYgJJR2Qzc   ### virgulilla alt + 126

# grafica de cajas SLA-ambiente
boxplot(sla~ambiente, 
        data = peso_seco,
        main = "Specific Leaf Area por ambiente",
        xlab = "ambiente", 
        ylab = "sla")

# calcular Prueba T entre ciudades
ciudad_lm_sla <- lm(sla ~ ciudad, data = peso_seco, na.action = na.exclude)
summary(ciudad_lm_sla)                                                

# grafica de cajas SLA-ciudad
boxplot(sla~ciudad,
        data = peso_seco,
        main = "SLA por ciudad",
        xlab = "ciudad",
        ylab = "sla")


###############################################################################
# RESUMEN DE LA BASE DE DATOS CON TRICOMAS DE PLANTAS DE CAMPO DE R.nudiflora
###############################################################################

head(tricomas)


tricomas$uno <- as.double(tricomas$uno) ##todo esto es para cambiar el
tricomas$dos <- as.double(tricomas$dos)  ##tipo de dato que tenian las var
tricomas$tres <- as.double(tricomas$tres) ##por default al cargar las bd
tricomas$cuatro <- as.double(tricomas$cuatro)
tricomas$sitio <- as.character(tricomas$sitio) 
tricomas$parche <- as.character(tricomas$parche)

head(tricomas)
tricomas$leafMean <- rowMeans(subset(tricomas, select = c(uno, dos, tres,cuatro)), na.rm = T)
head(tricomas)

#graficar los datos
boxplot(leafMean~ambiente, 
        data = tricomas,
        main = "Promedio de tricomas por ambiente",
        xlab = "ambiente", 
        ylab = "tricomas")


boxplot(leafMean~ciudad, 
        data = tricomas,
        main = "Promedio de tricomas por ciudad",
        xlab = "ciudad", 
        ylab = "tricomas")


# calcular Prueba de T entre ambientes

t_huanjing <- t.test(tricomas$leafMean ~ tricomas$ambiente, var.equal = T) 
t_huanjing
### https://www.youtube.com/watch?v=NlYgJJR2Qzc   ### virgulilla alt + 126


lm_chengshi <- lm(leafMean ~ ciudad, data = tricomas, na.action = na.exclude)
lm_chengshi
summary(lm_chengshi)

lm_interaccion <- lm(leafMean ~ ambiente*ciudad, data = tricomas, na.action = na.exclude)
lm_interaccion
summary(lm_interaccion)



