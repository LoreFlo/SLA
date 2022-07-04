#############################################################################
#### CALCULO DE SPECIFIC LEAF AREA EN MUESTRAS DE CAMPO DE R. nudiflora #####
#############################################################################

# Cargar bases

library(readxl)

peso_seco <- read_excel("./data/peso_seco_accesorias.xlsx")
tricomas <- read_excel("./data/TRICOMA_LORENA.xlsx")

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

t_test <- t.test(peso_seco$sla ~ peso_seco$ambiente, var.equal = T) 
### https://www.youtube.com/watch?v=NlYgJJR2Qzc   ### virgulilla alt + 126

                                                
# resumir los datos
group_by(peso_seco, ciudad) %>%
  summarise(
    count = n(),
    M = mean(sla), 
    SD = sd(sla),
    median = median(sla),
    IQR = IQR(sla, na.rm = T)
  )
    

###############################################################################
# RESUMEN DE LA BASE DE DATOS CON TRICOMAS DE PLANTAS DE CAMPO DE R.nudiflora
###############################################################################

head(tricomas)


tricomas$uno <- as.double(tricomas$uno) ##todo esto es para cambiar el
tricomas$dos <- as.double(tricomas$dos) ##tipo de dato que tenian las var
tricomas$tres <- as.double(tricomas$tres) ##por default al cargar las bd
tricomas$cuatro <- as.double(tricomas$cuatro)
tricomas$sitio <- as.character(tricomas$sitio)
tricomas$parche <- as.character(tricomas$parche)

head(tricomas)



#falta la variable "total"




# calcular Prueba de T entre ambientes

t_test <- t.test(tricomas$total ~ tricomas$ambiente, var.equal = T) 
### https://www.youtube.com/watch?v=NlYgJJR2Qzc   ### virgulilla alt + 126


# resumir los datos
group_by(tricomas, ciudad) %>%
  summarise(
    count = n(),
    M = mean(total), 
    SD = sd(total),
    median = median(total),
    IQR = IQR(total, na.rm = T)
  )





