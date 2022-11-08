## Fuentes importantes:
## https://fhernanb.github.io/libro_modelos_mixtos/pac-lme4.html
## https://github.com/oyaxbell/curso_R/blob/main/Clase_9.R
## https://www.youtube.com/watch?v=GskZZ6eUqNM

##########Modelo lineal generalizado############

## Modelo lineal Y~beta0+beta1*x1+beta2*x2+...+betan*xn

# Cargar bases

pacman:: p_load(MixAll, lmtest, car, gvlma, MASS, NHSRdatasets, ResourceSelection, 
               rcompanion, nortest,tidyverse, caret,mgcv, rms, jtools)

library(lme4)
library(performance) # para comparar modelos
#compare_performance(modelo 1, modelo 2,... modelo n)
library(ggplot2)

cap1 <- read.csv("./db/cap1_str.csv")
str(cap1)
cap1<-(as.data.frame(unclass(cap1),
                     stringsAsFactors=T))

## inspeccionar los datos

nrow(cap1)
str(cap1)
sample_n(cap1, 5)



