rm(list=ls(all=TRUE))
library(dplyr)
library("rio")
library(magrittr)
library(readxl)
#library(xlsx)
setwd("C:/Users/jenny/Documents/Data_final/Data TFM")
#lectura base
dir()
Data_TFM_comp <- read.csv("Data_TFM_17122021_.csv", sep=";",dec = ".")
Data_TFM_comp <- Dataset
###CONVERTIR A FACTOR VARIABLE TARJET Y VARIABLES CATEGORICAS ORDINALES
Dataset$Reincidencia <- factor(Dataset$Reincidencia, levels = c("No_Reincide", "Si_Reincide"))
Dataset$sexo_f <- factor(Dataset$sexo_f, levels = c("Hombre", "Mujer"))
Dataset$Trabaja_f1 <- factor(Dataset$Trabaja_f1, levels = c("No", "Si"))

##TRANSFORMAR A DUMMYS VARIABLES CATEGÓRICAS NOMINALES
Dataset <- fastDummies::dummy_cols(Data_TFM_comp,  select_columns = c("grupo_edad_UD", "Ultimo_Delito_f", "Region_UD_f", "Sit_Actual_f", "Estado_civil_UD_f", "Nivel_Instrucción_UD_f"),remove_first_dummy = TRUE) %>% 
  select(-c("grupo_edad_UD", "Ultimo_Delito_f", "Region_UD_f", "Sit_Actual_f", "Estado_civil_UD_f", "Nivel_Instrucción_UD_f")) %>% 

str(Dataset1)
#### SELECCIONAR LAS 24 VARIABLES TOTAL
Dataset1 <- Dataset%>%select(Reincidencia, Promedio_tiempo_reincidencia, Prom_Tiempo_sentencia_f, Nivel_Instrucción_UD_f, grupo_edad_UD, Sit_Actual_f_No_ingreso_CPL, Sit_Actual_f_Libre, Sit_Actual_f_Presente, Sit_Actual_f_Otros_Fallecido_Fugado, Estado_civil_UD_f_Casado, Estado_civil_UD_f_Soltero, Estado_civil_UD_f_Viudo, Estado_civil_UD_f_Divorciado, sexo_f_Mujer, sexo_f_Hombre, Region_UD_f_Sierra, Region_UD_f_Costa, Region_UD_f_Oriente, Region_UD_f_Insular, Ultimo_Delito_f_Delito_Sexual, Ultimo_Delito_f_Delito_Drogas, Ultimo_Delito_f_Delito_CP, Ultimo_Delito_f_Delito_CV, Trabaja_f1_No, Trabaja_f1_Si)

## DIVIDIR CONJUNTO DE ENTRENAMIENTO Y TEST

##Datos de entrenamiento y test (80 /20)
#library(rsample)
set.seed(777)
library(caTools)
split = sample.split(Dataset1$Reincidencia, SplitRatio = 0.80)
data_train = subset(Dataset1, split == TRUE)
data_test = subset(Dataset1, split == FALSE)

# Escalar las variables numericas
data_train[,c(2:3)] = scale(data_train[,c(2:3)], center = T, scale = T)
data_test[,c(2:3)] = scale(data_test[,c(2:3)], center = T, scale = T)

##Balancear data de entrenamiento

###  PRIMER CONJUNTO - ROSE
library(ROSE)
set.seed(950)
data_train_bal <- ovun.sample(Reincidencia ~ ., 
                              data = data_train, 
                              method = "both") 

prop.table(table(data_train_bal$data$Reincidencia))
##Primera Data Balanceada con both (combination of over- and under-sampling)
train_bal <- data_train_bal$data

###SEGUNDO CONJUNTO - ROSE
#library(ROSE)
set.seed(950)
data_train_bal1 <- ovun.sample(Reincidencia ~ ., 
                               data = data_train, 
                               method = "over") 

prop.table(table(data_train_bal1$data$Reincidencia))
##Segunda Data Balanceada con over sampling
train_bal1 <- data_train_bal1$data

###TERCER CONJUNTO - ROSE
#library(ROSE)
set.seed(950)
data_train_bal2 <- ovun.sample(Reincidencia ~ ., 
                               data = data_train, 
                               method = "under") 

prop.table(table(data_train_bal2$data$Reincidencia))
##Primera Data Balanceada con under-sampling)
train_bal2 <- data_train_bal2$data

##Exportar data de entrenamiento (SIN BALANCEAR y BALANCEADA) y data prueba o test 
#data train
write.csv(train_bal,"Data_train_Rose_Both.csv",sep=";",dec=".") # Data Balanceada Both
write.csv(train_bal1,"Data_train_Rose_Over.csv",sep=";",dec=".") # Data Balanceada Over
write.csv(train_bal2,"Data_train_Rose_Under.csv",sep=";",dec=".") # Data Balanceada Under
write.csv(data_test,"Data_test.csv",sep=";",dec=".") # Data de test
write.csv(data_train,"Data_train.csv",sep=";",dec=".") # Data entrenamiento No Balanceada
