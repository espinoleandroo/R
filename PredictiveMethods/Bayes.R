#     Profesor Noe Amir Rodriguez Olivares
#     Metodos de deteccion de patrones                                     
#
#      Ejemplo: Ejemplo de clasificador Naive-Bayes
#
#
# NOTA: Ejecuta estas lineas solo una vez!!!!
#install.packages("e1071")
#install.packages("klaR")
#install.packages("bnlearn")
#install.packages("caret")
#install.packages("plotly")
#----------------------------------------
#link del paquete https://cran.r-project.org/web/packages/klaR/klaR.pdf  
#link del paquete https://cran.r-project.org/web/packages/caret/caret.pdf
library(klaR)
library(caret)
library(plotly)
library(ggplot2)
library(ggpubr)
rm (list = ls ())           #Borra el ambiente global
#-----------------------------------------------------------------------------------
#Paso 1.  Datos para la prueba
#1) Carga de datos tipo CSV
#  Links recomendados:  https://conceptosclaros.com/importar-datos-r/
datos <- read.csv("Datos.csv", header = TRUE)     # Importa la informacion a RStudio
attach(datos)                                      #Carga los datos al espacio de trabajo
datos$Slot <- as.factor(datos$Slot)
#-----------------------------------------------------------------------------------
# Paso 2. Histograma de datos por clase
p1 <- ggplot(data = datos, aes(x = Altura, fill = Slot)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = Ancho, fill = Slot)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, nrow = 2, common.legend = TRUE)
#-----------------------------------------------------------------------------------
# Matriz de correlacion
pairs(datos)
#-----------------------------------------------------------------------------------
# Representaci�n mediante Histograma de cada variable para cada especie 
par(mfcol = c(2, 2))
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 10)
  for (i in 1:5) {
    i0 <- levels(datos$Slot)[i]
    x <- datos[datos$Slot == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("Slot", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}
#-----------------------------------------------------------------------------------
# Uso del clasificador por Naive Bayes utilizando la libreria klaR
datos$Slot<-as.factor(datos$Slot)
nb_mod <- NaiveBayes(Slot ~ Ancho+Altura, data=datos)
plot(nb_mod)
#-----------------------------------------------------------------------------------
# Grafica de la prediccion y el real
library(ggplot2)
pred <- predict(nb_mod, datos)
datos$pred <- pred$class
ggplot(datos, aes(Slot, pred , color = Slot)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Visual de la matriz de confusion", 
       subtitle="Predicho vs. real del conjunto de datos de slots", 
       y="Predichos", 
       x="Reales",
       caption="Deteccion de patrones")
#-----------------------------------------------------------------------------------
nueva_observacion <- data.frame(Altura = 0.15, Ancho = 0.18)
predict(object = nb_mod, newdata = nueva_observacion)
