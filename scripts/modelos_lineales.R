
# Cargar bases

#getwd()

library(tidyverse)
library(ggplot2)
library(corrplot)
library(lmtest)
 
#peso_seco <- read.csv("./db/peso_seco.csv", colClasses = c("ciudad"="factor", "ambiente"="factor", "sitio"="factor", "parche"="factor"))

#tricomas <- read.csv("./db/tricomas.csv") #esta base se modificó y mas adelante se abre la version que sirve para este script
#npk <- read.csv(file = './db/npk.csv', colClasses = c("ciudad"="factor", "ambiente"="factor", "sitio"="factor", "parche"="factor"))


#################################################################################
###############  PROBANDO GRAFICAS EN R  ########################################
#################################################################################

#### PESO SECO generales
#head(peso_seco)
#peso_seco %>% count(ID)
#summary(peso_seco)
#hist(peso_seco$sla) ### así queda con distribución sesgada
#hist(log(peso_seco$sla)) ### así queda con distribucion normal
#qqnorm(log(peso_seco$sla))

#### TRICOMAS
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

#trico <- read.csv("./db/prom_tricomas.csv", colClasses = c("ciudad"="factor", "ambiente"="factor"))
#str(trico)
#hist(log(trico$tricomas)) #con log queda menos sesgada la distribucion
#qqnorm(log(trico$tricomas))

#### fuente https://www.youtube.com/watch?v=BffAQZzbBms&t=3873s

#### NPK generales
#head(npk)
#npk %>% count(ID)
#summary(npk)
#hist(log(npk$P_mg_Kg))
#hist(log(npk$C_porcentaje))
#hist(npk$K_Cmol_Kg)
#hist(npk$N_porcentaje)

#qqnorm(npk$C_porcentaje)
#qqnorm(npk$N_porcentaje)


########## Gráfica de dispersión ##########

#plot(log(peso_seco$sla), prom_tri$tricomas)
#plot(npk$C_porcentaje, log(peso_seco$sla))
#plot(npk$N_porcentaje, log(peso_seco$sla))


####################  GRAFICO DE CORRELACIONES  ############################### 
#### https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

cap1 <- read.csv("./db/cap1_str.csv", colClasses = 
                   c("ciudad"="factor", "ambiente"="factor", "sitio"="factor", "parche"="factor",
                     "sla"="double"))
c1<-cor(cap1[,13:18], use = "complete.obs")
c1
corrplot(c1, method = "number")  # si está muy bajito el color de los numeros, quitar method

#########################################################################################
################################### Linear Regresion ##########
#########################################################################################

# regresion lineal <- dependiente (cuantitativa continua); independiente (mixtas)
# objetivo: predecir la variable depediente Y en funcion de un conjunto de variables independientes Xj
# funcion liga <- identidad, gaussiana
# requisito de normalidad es en los residuos: que el modelo se equivoque de forma predecible
# residuo: (error) Y-y


m_a <- lm(log(sla)~ ambiente, data = cap1)
summary(m_a)
# residuals: la mediana está cerca del cero 0.00618, parece que el cuartil 1 (-0.20283) y 3 (0.19112) estan equidistantes.
# el modelo tiene intercepto (por eso salio significativo, lo cual es importante si me interesa hacer un modelo predictivo)
# las diferencias entre ambientes son significativas (p>0.001)
# 15% de la variabilidad del log(sla) se explica por el ambienteu (Multiple R-squared: 0.1559).
# F-statistic: 26.79 on 1 and 145 DF,  p-value: 7.443e-07

#NOTAS sobre lm
# Residual standard error: 0.3213 (la variabilidad que no predice el modelo)
# R^2<-variabilidad explicada por el modelo / variabilidad total
# Multiple R-squared:  0.1623 (la ajustada es para mas de una predictora)
# Para interpretar más fácil : 0.1623*100= 16.23% de variabilidad explicada por el modelo
# el estadistico F (o prueba de hipótesis) dice si la variabilidad de grupos es mayor que
# la variabilidad intragrupos.

# EJERCICIO VALIDO PARA VAR CONTINUAS: Predecir un valor de sla para una planta con sla de 20
# m_a #coeficientes: intercept 28.254; ambienteu 9.812
# beta0 es el intercepto
# beta1 es el coeficiente de mi primer variable independiente, que en este caso es ambienteu
# siguiendo el modelo y=beta1*x1+beta0 :
#sla_20<- 9.812*20+28.254
#sla_20 Me sale 224.494, pero no tiene ningun sentido, tendría que predecir a que categoria pertenece ese tamaño de sla...

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

m_ac <- lm(log(sla) ~ ambiente+ciudad, data = cap1)
m_ac
summary(m_ac)
# mediana cerca de cero (0.02753), q 1 (-0.17886) y 3 (0.18364)  equidistantes
# ambienteU, y el intercepto, estadísticamente significativo con un alfa de 0.001
# Adjusted R-squared:  0.1848 
# F-statistic: 7.621 on 5 and 141 DF,  p-value: 2.295e-06

########### Contribuira mucho la var. ciudad o no? Comparar modelos con anova #####

anova(m_a, m_ac) # da una tabla de analisis de varianza comparando modelos. 
# está comparando si la diferencia en la suma de cuadrados es estadisticamente 
# significativa. El modelo que sale significativo es mejor que el otro en términos de 
# disminucion de la suma de cuadrados.
# en este caso fue el modelo 2 (m_ac) p<0.05

########## Comparación de modelos ################

# AIC Criterio de informacion de Akaike - penaliza por # de variables
# BIC Criterio de informacion Bayesian de Schwartz - penaliza por num
# de variables y por num de muestra. Entre mas pequeño, indica un mejor modelo
# Estos valores tienen sentido cuando se contrastan

delta.AIC<- AIC(m_a)-AIC(m_ac)
delta.AIC # 2.246209
delta.BIC<- BIC(m_a)-BIC(m_ac)
delta.BIC # -9.715522

############ Validacion de supuestos ###############

#  Supuestos sobre los predictores
## 1. linealidad entre y y xj
## 2. Ausencia de muticolinealidad (matriz de diseño de rango completo)


#  Supuestos sobre los errores 
## 1. Varianza constante (Homocedasticidad; que el modelo se equivoque igual para valores altos o bajos)
## 2. Normalidad conjunta (de los errores)
## 3. Independencia de los errores 

# Linealidad
## Prueba de falta de ajuste
## Explorar términos polinómicos mediante polinomios ortogonales
#m1<-lm(sla~poly(ambiente,3), data = cap1) # expresion no apta para mis variables predictoras que son categoricas
#summary(m1)
#### se supone que si ningun polinomio funciona tan bien como el lineal, es una confirmación de 
#### la exploracion que se hizo previamente con graficas como la siguiente:
#cap1 %>%
  #ggplot(aes(x=ambiente, y=log(sla)))+geom_point()+geom_smooth(method="lm")+
  #theme_classic()+ylab("specific leaf area")+xlab(ambiente)
#?cor
#c1<-cor(cap1$sla, cap1$ambiente, method = "spearman")

######## FIN DEL CURSO (EL RESTO SE PUEDE SEGUIR SOLO CON VAR continuas) ###########
######## CONTINUO CON LAS VARIABLES DE RESPUESTA QUE ME FALTAN #######################

m_acac <- lm(log(sla) ~ ambiente+ciudad+(ambiente*ciudad), data = cap1)
summary(m_acac) # m 0.03334 1Q  -0.20489 3Q 0.16153
# intercepto p<0.001 y ambienteU p<0.01
# Residual standard error: 0.3182; Multiple R-squared:  0.2177,	Adjusted R-squared:  0.1663 
# F-statistic: 4.237 on 9 and 137 DF,  p-value: 7.367e-05


##################### LM TRICOMAS ##########################
t_a <- lm(log(tricomas)~ambiente, data = cap1)
t_a # intercepto 9.099 , ambienteU -4.169
summary(t_a) # med -0.02073, 1Q -0.51321, 3Q 0.44753
# ambienteU p<0.001; Residual standard error: 0.8044; Multiple R-squared:  0.1001
#F-statistic: 15.68 on 1 and 141 DF,  p-value: 0.0001187

t_ac <- lm(log(tricomas)~ambiente+ciudad, data = cap1)
summary(t_ac) # med 0.03467; 1Q -0.47695; 3Q 0.47792
# ciudad cam, ambienteU, ciudadMid, ciudadVal, todos con p<0.001
# Residual standard error: 0.71
#F-statistic: 12.82 on 5 and 137 DF,  p-value: 3.161e-10

t_acac <- lm(log(tricomas)~ambiente+ciudad+(ambiente*ciudad), data = cap1)
summary(t_acac)
# med 0.04853; 1Q -0.39717; 3Q 0.42373
# intercepto, ambienteU, cun, tiz, val, ambienteU*cun tienen p<0.001
# cun, ambienteU*tiz, tienen p<0.01
# Residual standard error: 0.6397 on 133 degrees of freedom
# Multiple R-squared:  0.463,	Adjusted R-squared:  0.4267 
# F-statistic: 12.74 on 9 and 133 DF,  p-value: 1.832e-14


#####################  PLANT C/N RATIO  #########################
plot(cap1$C_porcentaje, cap1$N_porcentaje)
hist(log(cap1$CN_ratio))
cn_a <- lm(log(CN_ratio)~ambiente, data = cap1)
summary(cn_a)


cn_ac <- lm(log(CN_ratio)~ambiente+ciudad, data = cap1)
summary(cn_ac)


cn_acac <- lm(log(CN_ratio)~ambiente+ciudad+(ambiente*ciudad), data = cap1)
summary(cn_acac)




