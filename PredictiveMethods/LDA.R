install.packages('knitr')
install.packages('MVN')
install.packages('biotools')
install.packages('SpatialEpi')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('scatterplot3d')
#----------------------------------------------------------
# Librerias
library(ggplot2)
library(ggpubr)
#-----------------------------------------------------------------------------------
#Paso 1.  Datos para la prueba
input <- ("Especie pata abdomen organo_sexual 
          a 191 131 53
          a 185 134 50
          a 200 137 52
          a 173 127 50
          a 171 128 49
          a 160 118 47
          a 188 134 54
          a 186 129 51
          a 174 131 52
          a 163 115 47
          b 186 107 49
          b 211 122 49
          b 201 144 47
          b 242 131 54
          b 184 108 43
          b 211 118 51
          b 217 122 49
          b 223 127 51
          b 208 125 50
          b 199 124 46
          ")
datos <- read.table(textConnection(input), header = TRUE)
datos$Especie <- as.factor(datos$Especie)
#-----------------------------------------------------------------------------------
# Paso 2. Histograma de datos por clase
p1 <- ggplot(data = datos, aes(x = pata, fill = Especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = abdomen, fill = Especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = datos, aes(x = organo_sexual, fill = Especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)
#-----------------------------------------------------------------------------------
# Matriz de correlacion
pairs(datos)
#-----------------------------------------------------------------------------------
# Grafica en 3D
library(scatterplot3d)
scatterplot3d(datos$pata, datos$abdomen, datos$organo_sexual,
              color = c("firebrick", "green3")[datos$Especie], pch = 19,
              grid = TRUE, xlab = "pata", ylab = "abdomen",
              zlab = "organo sexual", angle = 65, cex.axis = 0.6)
legend("topleft", bty = "n", cex = 0.8, title = "Especie", c("a", "b"), fill = c("firebrick", "green3"))
#-----------------------------------------------------------------------------------
# Representaci�n mediante Histograma de cada variable para cada especie 
par(mfcol = c(2, 3))
for (k in 2:4) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$Especie)[i]
    x <- datos[datos$Especie == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("Especie", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}
#-----------------------------------------------------------------------------------
# Prueba de normalidad univariante
# Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(datos, value.name = "valor")
kable(datos_tidy %>% group_by(Especie, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))
#-----------------------------------------------------------------------------------
# Prueba de normalidad multivariante
# Link de paquete https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
library(MVN)
outliers <- mvn(data = datos[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")
royston_test <- mvn(data = datos[,-1], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality
hz_test <- mvn(data = datos[,-1], mvnTest = "hz")
hz_test$multivariateNormality
#-----------------------------------------------------------------------------------
# Prueba de igualdad de matriz de covarianza
# Link    https://www.rdocumentation.org/packages/heplots/versions/1.3-5/topics/boxM
# Link    https://cran.r-project.org/web/packages/biotools/biotools.pdf
# Link    https://cran.r-project.org/web/packages/SpatialEpi/SpatialEpi.pdf
library(biotools)
#library(SpatialEpi)
boxM(data = datos[, 2:4], grouping = datos[, 1])
#-----------------------------------------------------------------------------------
# Calculo del discriminante lineal
library(MASS)
datos$Especie<-as.factor(datos$Especie)
modelo_lda <- lda(formula = Especie ~ pata + abdomen + organo_sexual,
                  data = datos)
#-----------------------------------------------------------------------------------
# Grafica de la prediccion y el real
library(ggplot2)
pred <- predict(modelo_lda, datos)
datos$pred <- pred$class
ggplot(datos, aes(Especie, pred , color = Especie)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Visual de la matriz de confusion", 
       subtitle="Predicho vs. real del conjunto de datos de insectos", 
       y="Predichos", 
       x="Reales",
       caption="Modelado")
#-----------------------------------------------------------------------------------
# Estimaci�n de nuevos casos
nueva_observacion <- data.frame(pata = 194, abdomen = 124,
                                organo_sexual = 49)
predict(object = modelo_lda, newdata = nueva_observacion)

