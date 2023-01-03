# Librerias:
#install.packages("tidyverse")
#install.packages("class")
#install.packages("gmodels")
#install.packages("caret")
library(tidyverse)
library(class)
library(gmodels)
library(caret)
#-----------------------------------------------------------------------------------
# Funciones:
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}
#-----------------------------------------------------------------------------------
# Carga de datos
wisc_data <- read.csv(file = "wisc_bc_data.csv")
wisc_data <- wisc_data[,-1]                                #Se elimina ID para evitar sobreajuste
wisc_data <- mutate(wisc_data,                             #Cambiamos el nombre de la clase  
                    diagnosis = fct_recode(wisc_data$diagnosis,
                                           "Benigno" = "B",
                                           "Maligno" = "M"))
#-----------------------------------------------------------------------------------
# Sumarizacion: Obtenemos los porcentajes de tumores benignos y malignos
round(prop.table(table(wisc_data$diagnosis)) * 100, 1)

#-----------------------------------------------------------------------------------
# Normalizacion
wisc_data_norm <- as.data.frame(lapply(wisc_data[2:31], normalize))

#-----------------------------------------------------------------------------------
# Dividimos la muestra en dos datasets, uno para tranining y otro para test. 
# Podemos escoger las primeras 469 para el primero, y las �ltimas 100 para 
# el segundo, puesto que todas las observaciones de este dataset ya vienen 
# en orden aleatorio.
wisc_training <- wisc_data_norm[1:469,]
wisc_test <- wisc_data_norm[470:569,]

#-----------------------------------------------------------------------------------
# Por otro lado vamos a guardar las etiquetas de diagn�stico de todas las 
# observaciones en dos vectores por separado
wisc_training_labels <- wisc_data[1:469,1]
wisc_test_labels <- wisc_data[470:569,1]

#-----------------------------------------------------------------------------------
# Aplicamos el modelo
# Los ejemplos de prueba sin etiqueta se comparan con los registros m�s 
# similares en el conjunto de entrenamiento utilizando una funci�n de 
# distancia, y al ejemplo sin etiqueta se le asigna la etiqueta de sus
# vecinos. Como n�mero k escogeremos la raiz cuadrada del total de 
# observaciones, y redondeamos a un n�mero impar para evitar un empate 
# entre categor�as.    
wisc_test_predicho <- knn(wisc_training, wisc_test, cl = wisc_training_labels, k = 23)

#Con esta operaci�n hemos obtenido un vector con la predicci�n de 
#categor�as que hemos hecho, y que despu�s compararemos con las categor�as reales

#-----------------------------------------------------------------------------------    
# Validacion cruzada
# Compararemos con una tabla cruzada los valores para el dataset de Test 
#  que hemos predicho con nuestro modelo, con los valores reales de Test    
confusionMatrix(data = wisc_test_predicho, reference = wisc_test_labels)

library(ggplot2)                                     #Comentar:  
wisc_test$pred <- wisc_test_predicho
wisc_test$Diagnostico <- wisc_test_labels
ggplot(wisc_test, aes(Diagnostico, pred, color = Diagnostico)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Visual de la matriz de confusion", 
       subtitle="Predicho vs. real del conjunto de datos", 
       y="Predichos", 
       x="Reales",
       caption="Metodos predictivos")


