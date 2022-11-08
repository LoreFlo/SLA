#############################################################################
#### CALCULO DE SPECIFIC LEAF AREA EN MUESTRAS DE CAMPO DE R. nudiflora #####
#############################################################################

# Cargar bases

getwd()
library(readxl)
peso_seco <- read_xlsx("./db/peso_seco_accesorias.xlsx")
tricomas <- read_xlsx("./db/TRICOMA_LORENA.xlsx")
nutri <- read_xlsx("./db/macronutrientes_vegetales.xlsx")

#install.packages("googledrive")
#install.packages("googlesheets4")
#library(googlesheets4)
#leer base de datos desde un google sheet
#x <- read_sheet('https://docs.google.com/spreadsheets/d/15qjj8LPQstf_vaI0A3xTrmIB4Zfl5dVR/edit#gid=555512928')
# código en proceso

head(peso_seco)

# Calcular promedio del peso por lugar de colecta 

library(dplyr)
library(tidyr)

peso_seco$ciudad <- as.factor(peso_seco$ciudad)
peso_seco$ambiente <- as.factor(peso_seco$ambiente)
peso_seco$sitio <- as.factor(peso_seco$sitio)
peso_seco$parche <- as.factor(peso_seco$parche)
peso_seco$planta <- as.factor(peso_seco$planta)

peso_seco$ind1_gr <- as.double(peso_seco$ind1_gr) ##todo esto es para cambiar el
peso_seco$ind2_gr <- as.double(peso_seco$ind2_gr) ##tipo de dato que tenian las var
peso_seco$ind3_gr <- as.double(peso_seco$ind3_gr) ##por default al cargar las bd
peso_seco$ind4_gr <- as.double(peso_seco$ind4_gr)
peso_seco$ind5_gr <- as.double(peso_seco$ind5_gr)


head(peso_seco)

peso_seco$promedio <- rowMeans(peso_seco[ ,c(7,8,9,10,11)], na.rm = TRUE)
head(peso_seco)

################## calcular SLA por lugar de colecta ###########################
### el área de todas las muestras es la misma = 0.211 cm2
### sla es la razón: area de hoja / masa seca ; Jennifer Firn et al. 2019
### https://www.nature.com/articles/s41559-018-0790-1

peso_seco <- mutate(peso_seco, sla = 0.211/promedio)
peso_seco <- rename(peso_seco, ID = id)
head(peso_seco)

### calcular Prueba de T entre ambientes ###########################
amb_t_sla <- t.test(peso_seco$sla ~ peso_seco$ambiente, var.equal = T) 
amb_t_sla
### https://www.youtube.com/watch?v=NlYgJJR2Qzc   ### virgulilla alt + 126

# grafica de cajas SLA-ambiente
caja_sla_amb<-boxplot(sla~ambiente, 
        data = peso_seco,
        main = "Specific Leaf Area por ambiente",
        xlab = "ambiente", 
        ylab = "sla")

### ver los outliers, forma 1:
#outliers <- peso_seco[order(peso_seco$ambiente, -(peso_seco$sla)), ] ### es la 
### base peso_seco en orden descendente para encontrar el ID de los outliers
#outliers ### se abre en la consola y ademas con la fn view. Ahí se ve que los dos valores más altos
### en los datos urbanos son: mid_u21m2= 0.13483412 y cam_u31m2= 0.07402844

### ver los outliers, forma 2:
caja_sla_amb$out #para ver los outliers más fácil -.-
peso_seco_out <- peso_seco
peso_seco_out <- peso_seco_out[!(peso_seco_out$sla %in% caja_sla_amb$out), ]
caja_sla <-boxplot(sla~ambiente, 
                      data = peso_seco_out,
                      main = "Specific Leaf Area por ambiente",
                      xlab = "ambiente", 
                      ylab = "sla")

# calcular Prueba de T entre ambientes con la base sin outliers
#amb_t_sla2 <- t.test(peso_seco_out$sla ~ peso_seco_out$ambiente, var.equal = T) 
#amb_t_sla2


# ajustar modelo lineal entre ciudades ########################################
ciudad_lm_sla <- lm(sla ~ ciudad, data = peso_seco, na.action = na.exclude)
summary(ciudad_lm_sla)                                                

cd_amb_lm_sla <- lm(sla ~ ciudad*ambiente, data = peso_seco, na.action = na.exclude)
summary(cd_amb_lm_sla)

# grafica de caja SLA-ciudad
caja_sla_ciudad <- boxplot(sla~ciudad,
        data = peso_seco,
        main = "SLA por ciudad",
        xlab = "ciudad",
        ylab = "sla")

caja_sla_cd_amb <- boxplot(sla~ciudad*ambiente,
        data = peso_seco,
        main = "SLA por ciudad por ambiente",
        xlab = "ciudad",
        ylab = "sla")

# ver outliers 
caja_sla_ciudad$out #resultado: 0.07402844, 0.05943128, 0.05545024, 0.06085308, 0.13483412 
# cam, cun, cun, cun, mid, respectivamente.
caja_sla_cd_amb$out #result: 0.05355450, 0.07402844, 0.13483412
# tizimin, campeche, merida

# quitar outliers de la base y volver a graficar
#peso_seco_out2 <- peso_seco
#peso_seco_out2 <- peso_seco_out2[!(peso_seco_out2$sla %in% caja_sla_ciudad$out), ]
#caja_sla_ciudad2 <- boxplot(sla~ciudad, data = peso_seco_out2, main = "SLA por ciudad", xlab = "ciudad", ylab = "sla")

# con la base sin outliers, volver a ajustar a modelo lineal
#ciudad_lm_sla2 <- lm(sla ~ ciudad, data = peso_seco_out2, na.action = na.exclude)
#summary(ciudad_lm_sla2)

write.table(
  peso_seco, 
  file = "./db/peso_seco.csv",
  sep = ",", 
  col.names = T,
  row.names = F
)

###############################################################################
# RESUMEN DE LA BASE DE DATOS CON TRICOMAS DE PLANTAS DE CAMPO DE R.nudiflora
###############################################################################

head(tricomas)

### todo esto es para cambiar la clase que tenias las variables por default
###a la hora de cargar las bases de datos
tricomas$ciudad <- as.factor(tricomas$ciudad)
tricomas$ambiente <- as.factor(tricomas$ambiente)
tricomas$sitio <- as.factor(tricomas$sitio)
tricomas$parche <- as.factor(tricomas$parche)
tricomas$planta <- as.factor(tricomas$planta)

tricomas$uno <- as.double(tricomas$uno)
tricomas$dos <- as.double(tricomas$dos)  
tricomas$tres <- as.double(tricomas$tres) 
tricomas$cuatro <- as.double(tricomas$cuatro)
head(tricomas)

# la columna leaf es un individuo; del uno al cuatro, son conteos de tricomas en 
# hojas de cada individuo. 
# En esta sección se hará el promedio de tricomas de cada individuo en leaf
tricomas$leafMean <- rowMeans(subset(tricomas, select = c(uno, dos, tres,cuatro)), na.rm = T)
head(tricomas)

#graficar los datos
caja_tric_amb <- boxplot(leafMean~ambiente, 
        data = tricomas,
        main = "Promedio de tricomas por ambiente",
        xlab = "ambiente", 
        ylab = "tricomas")
caja_tric_amb$out


caja_tric_cd <- boxplot(leafMean~ciudad, 
        data = tricomas,
        main = "Promedio de tricomas por ciudad",
        xlab = "ciudad", 
        ylab = "tricomas")
caja_tric_cd$out # hay muchos outlier tanto si se ven los datos agrupados por ciudad como por ambiente.
# tal vez habrá que transformarlos


# calcular Prueba de T entre ambientes ############################################

t_tric_amb <- t.test(tricomas$leafMean ~ tricomas$ambiente, var.equal = T) 
t_tric_amb
### virgulilla alt + 126


# ajustar modelo lineal a los tricomas entre ciudades y amb*cd #############################

lm_tric_cd <- lm(leafMean ~ ciudad, data = tricomas, na.action = na.exclude)
summary(lm_tric_cd)

lm_interaccion <- lm(leafMean ~ ambiente*ciudad, data = tricomas, na.action = na.exclude)
summary(lm_interaccion)

# arreglar el ID

tricomas <- tricomas %>% mutate(ciudad2 = ciudad)
tricomas <- unite(tricomas, ciudad2, ID, col= "ID", sep="_")


# guardar la base tricomas con la columna nueva leafMean y el ID arreglado

write.table(
  tricomas, "./db/tricomas.csv", 
  sep = ",", 
  col.names = T, row.names = F)


######### explorando diferencias entre sitio, parche y planta #######################
library(ggplot2)

ps <- read.csv(file = './db/peso_seco.csv')
tric <- read.csv(file = "./db/tricomas.csv")

install.packages("xlsx")
library(xlsx)


# desplegar heatmap (porque sí se observaron diferencias entre sitios, etc)
# forma 1
ggplot(ps, aes(ambiente, planta, fill= sla)) + #probar cambiando los dos primeros campos
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red")

# forma 2
# preparar set de datos
data <- read.table(file = "./db/peso_seco.csv",
                 sep = ",",
                 header = T)
data
rownames(data) <- data[ ,6] # extraer toda la columna 6 y que la asigne como nombre
# filas de los datos
rownames(data)
samp2 <- data[ ,-6] # eliminar la columna 6
samp2
mat_data <- data.matrix(samp2[ ,1:ncol(samp2)]) # objeto para contener el set de 
# datos en una matriz
library(pheatmap)
require(ggplot2)
library(colorspace)
require(colorspace)
library(grid)

pheatmap(mat_data, fontsize_row = 5, fontsize_col = 5)


# sla - sitio
ps$sitio <- as.factor(ps$sitio)
lm_sla_sitio <- lm(sla ~ sitio, data = ps, na.action = na.exclude)
summary(lm_sla_sitio)

# sla - parche

# sla - parche*planta

# tricomas - sitio

# tricomas - parche

# tricomas - parche*planta

###############################################################################
# RESUMEN DE LA BASE DE DATOS CON MACRONUTRIENTES DE PLANTAS DE CAMPO DE R.nudiflora
###############################################################################

head(nutri)


nutri$ciudad <- as.factor(nutri$ciudad)
nutri$ambiente <- as.factor(nutri$ambiente)
nutri$sitio <- as.factor(nutri$sitio)
nutri$parche <- as.factor(nutri$parche)
nutri$planta <- as.factor(nutri$planta)

nutri$P_mg_Kg <- as.double(nutri$P_mg_Kg) ##todo esto es para cambiar el
nutri$C_porcentaje <- as.double(nutri$C_porcentaje) ##tipo de dato que tenian las var
nutri$K_Cmol_Kg <- as.double(nutri$K_Cmol_Kg) ##por default al cargar las bd
nutri$N_porcentaje <- as.double(nutri$N_porcentaje)

head(nutri)

# guardar la base tricomas con la columna nueva leafMean y el ID arreglado

write.table(
  nutri , "./db/npk.csv", 
  sep = ",", 
  col.names = T, row.names = F)

npk <- read.csv(file = './db/npk.csv')

head(npk)

#################### UNIFICACION DE BASES #################################

cap1 <- read.csv("./db/capitulo1.csv")

cap1$ciudad <- as.factor(cap1$ciudad)
cap1$ambiente <- as.factor(cap1$ambiente)
cap1$sitio <- as.factor(cap1$sitio)
cap1$parche <- as.factor(cap1$parche)
cap1$planta <- as.factor(cap1$planta)

cap1$ind1_gr <- as.double(cap1$ind1_gr) ##todo esto es para cambiar el
cap1$ind2_gr <- as.double(cap1$ind2_gr) ##tipo de dato que tenian las var
cap1$ind3_gr <- as.double(cap1$ind3_gr) ##por default al cargar las dbs
cap1$ind4_gr <- as.double(cap1$ind4_gr)
cap1$ind5_gr <- as.double(cap1$ind5_gr)

cap1$promedio <- as.double(cap1$promedio)
cap1$sla <- as.double(cap1$sla)
cap1$tricomas <- as.double(cap1$tricomas)

cap1$P_mg_Kg <- as.double(cap1$P_mg_Kg) ##todo esto es para cambiar el
cap1$C_porcentaje <- as.double(cap1$C_porcentaje) ##tipo de dato que tenian las var
cap1$K_Cmol_Kg <- as.double(cap1$K_Cmol_Kg) ##por default al cargar las bd
cap1$N_porcentaje <- as.double(cap1$N_porcentaje)


write.table(
  cap1, "./db/cap1_str.csv", 
  sep = ",", 
  col.names = T, row.names = F)

cap1_1 <- read.csv(file = './db/cap1_str.csv')

head(cap1_1)

str(cap1_1) # no se guardó la str en el cvs :(

str(cap1)
