#####ADVERTENCIA: para usar este codigo necesito cambiar las variables del primer modelo pero no lo haré ahorita

# Cargar bases

#getwd()
#library()

if(!require(tidyverse)){install.packages("tidyverse")}; library(tidyverse)
### es una funcion util para instalar y cargar paketes que se pueden necesitar o no. 

peso_seco <- read.csv("./db/peso_seco.csv")
#tricomas <- read.csv("./db/tricomas.csv") #esta base se modificó y mas adelante se abre la version que sirve para este script
npk <- read.csv(file = './db/npk.csv')


#################################################################################
###############  PROBANDO GRAFICAS EN R  ########################################
#################################################################################

## PESO SECO generales
#head(peso_seco)
#peso_seco %>% count(ID)
#summary(peso_seco)
hist(peso_seco$sla) ### así queda con distribución sesgada
hist(log(peso_seco$sla)) ### así queda con distribucion normal
qqnorm(log(peso_seco$sla))

## TRICOMAS
#head(tricomas)
#tricomas %>% count(ID)
#summary(tricomas)

#tricomas %>% 
#  group_by(ID, ciudad, ambiente) %>% 
#  summarise(mean_trico = mean(leafMean, na.rm = T))

#write.table(
#  mean_trico, "./db/prom_tricomas.csv", 
#  sep = ",", 
#  col.names = T, row.names = F)

trico <- read.csv("./db/prom_tricomas.csv")
hist(log(trico$tricomas)) #con log queda menos sesgada la distribucion
qqnorm(log(trico$tricomas))

### fuente https://www.youtube.com/watch?v=BffAQZzbBms&t=3873s

## NPK generales
#head(npk)
#npk %>% count(ID)
#summary(npk)
hist(log(npk$P_mg_Kg))
hist(log(npk$C_porcentaje))
hist(npk$K_Cmol_Kg)
hist(npk$N_porcentaje)

qqnorm(npk$C_porcentaje)
qqnorm(npk$N_porcentaje)


########## Gráfica de dispersión ##########

plot(log(peso_seco$sla), prom_tri$tricomas)
plot(npk$C_porcentaje, log(peso_seco$sla))
plot(npk$N_porcentaje, log(peso_seco$sla))


# grafico de correlaciones
c1<-cor(npk[,9:12], use = "complete.obs")
c1
#corrplot(c1)  #falta abrir paqute, no se cual es.


########## Linear Regresion ##########
# regresion lineal <- dependiente (cuantitativa continua); independiente (mixtas)
# objetivo: predecir la variable depediente Y en funcion de un conjunto de variables independientes Xj
# funcion liga <- identidad, gaussiana
# requisito de normalidad es en los residuos: que el modelo se equivoque de forma predecible
# residuo: (error) Y-y


m_a <- lm(log(peso_seco$sla)~ peso_seco$ambiente)
summary(m_a)
# residuals: la mediana está cerca del cero, parece que el cuartil 1 y 3 estan equidistantes.
# el modelo tiene intercepto (por eso salio significativo, lo cual es importante si me interesa hacer un modelo predictivo)
# las diferencias entre ambientes son significativas (p>0.001)
# 15% de la variabilidad del sla se explica por el ambienteu (Multiple R-squared:  0.1559).
# F-statistic: 26.79 on 1 and 145 DF,  p-value: 7.443e-07

#NOTAS sobre lm
# Residual standard error: 0.3213 (la variabilidad que no predice el modelo)
# R^2<-variabilidad explicada por el modelo / variabilidad total
# Multiple R-squared:  0.1559 (la ajustada es para mas de una predictora)
# Para interpretar más fácil : 0.1559*100= 15.59% de variabilidad explicada por el modelo
# el estadistico F (o prueba de hipótesis) dice si la variailidad de grupos es mayor que
# la variabilidad intragrupos.

# EJERCICIO: Predecir un valor de sla para una planta con sla de 20
m_a #coeficientes: intercept 3.3045; ambienteu 0.2746
# beta0 es el intercepto
# beta1 es el coeficiente de mi primer variable independiente, que en este caso es ambienteu
# siguiendo el modelo y=beta1*x1+beta0 :
#sla_20<- 0.2746*20+3.3045
#sla_20 Me sale 8.7, pero no tiene ningun sentido, tendría que predecir a que categoria pertenece ese tamaño de sla...

anova(m_a)
#anova descompone la variabilidad, por ejemplo la var que predice el valor de sla
# y la que no predice (variabilidad residual; residual standard error)
confint(m_a) # da los intervalos de confianza del beta0 y beta1.
# En summary obtenemos los valores de beta cuando los predictores son cero. beta0 es igual a Y cuando x1 es 0
#nd<-data.frame(peso_seco$sla = c(70,90))
#nd
#predict(m_a, newdata = nd)
# esta funcion predice valores de la variable de respuesta de una manera más 
# precisa para cualquier tipo de modelo, pero no me salió.


########## Modelo de regresión múltiple ##########

?glm # regresion lineal, logistica, gaussiana, binomial...

m_ac <- lm(sla ~ ambiente+ciudad, data = peso_seco)
m_ac
summary(m_ac)
# mediana negativa, q 1 y 3 no son equidistantes
# coeficiente beta para ambienteU y para ciudadcam 
# es estadísticamente significativo con un alfa de 0.001
# Adjusted R-squared:  0.2153 
# F-statistic: 9.011, p-value: 1.883e-07

########### Contribuira mucho la var. ciudad o no? Comparar modelos con anova #####

anova(m_a, m_ac) # deberia dar una tabla de analisis de varianza comparando modelos, pero no me salió

# está comparando si la diferencia en la suma de cuadrados es estadisticamente 
# significativa. El modelo que sale significativo es mejor que el otro en términos de 
# disminucion de l suma de cuadrados.


########## Comparación de modelos ################

# AIC Criterio de informacion de Akaike - penaliza por # de variables
# BIC Criterio de informacion Bayesian de Schwartz - penaliza por num
# de variables y por num de muestra. Entre mas pequeño, indica un mejor modelo
# Estos valores tienen sentido cuando se contrastan

delta.AIC<- AIC(m_a)-AIC(m_ac)
delta.AIC
delta.BIC<- BIC(m_a)-BIC(m_ac)
delta.BIC

############ Validacion de supuestos ###############

#  Supuestos sobre los predictores
## 1. linealidad entre y y xj
## 2. Ausencia de muticolinealidad (matriz de diseño de rango completo)


#  Supuestos sobre los errores 
## 1. Varianza constante (Homocedasticidad; que el modelo se equivoque igual para valores altos o bajos)
## 2. Normalidad conjunta (de los errores)
## 3. Independencia de los errores 






