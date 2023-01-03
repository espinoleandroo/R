#-----------------------------------------------------------------------------------
#
#     Profesor Noe Amir Rodriguez Olivares
#     Codigo : Ejemplo 1 de Red Neuronal
#
#-----------------------------------------------------------------------------------
# NOTA: Instala este paquete, si el codigo no funciona!!
install.packages("ggplot")
install.packages('neuralnet')
install.packages("gridExtra", dependencies = TRUE) 
# -----------------------------------------------------
library(MASS); library(neuralnet); library(ggplot2)
# -----------------------------------------------------
#  Carga de datos

file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE) 
plot(datos)
head(datos) 
# -----------------------------------------------------
# Grafica de valores
ggplot(datos, aes(x=Edad, y=Resistencia)) + geom_point()
# -----------------------------------------------------
# Normalizacion de datos
maxs <- apply(datos, 2, max) 
mins <- apply(datos, 2, min) 
scaled <- as.data.frame(scale(datos, center=mins, scale=maxs-mins))
require(gridExtra)                                         # Se carga cada sesion
plot1 <- ggplot(datos, aes(x=Edad, y=Resistencia)) + geom_point()
plot2 <- ggplot(scaled, aes(x=Edad, y=Resistencia)) + geom_point()
grid.arrange(plot1, plot2, ncol=2)
# -----------------------------------------------------
# Se entrena la red neuronal

mod1 <- neuralnet(Resistencia ~ Edad, 
                  data=scaled, 
                  stepmax = 1e+05,
                  algorithm='rprop+',
                  learningrate=0.01,
                  rep=10,
                  hidden=c(15,5),           
                  threshold=0.001)         
# -----------------------------------------------------
# Informacion de la red

plot(mod1, rep="best")                   
names(mod1)                              
mod1$act.fct                             
unlist(mod1$weights)                     

# -----------------------------------------------------
# Evaluacion de la red
test <- data.frame(Edad = scaled$Edad)
myprediction <- compute(x=mod1, covariate=test)

# -----------------------------------------------------
# Re escalado a valor real

yhat_red <- myprediction$net.result * (max(datos$Resistencia)-min(datos$Resistencia))+min(datos$Resistencia)
datos$yhat_red <- yhat_red
yhat_red[1:5] 

# -----------------------------------------------------
# Graficas de resultados
plot(datos$Resistencia)
lines(yhat_red,col="red")
ggplot(datos, aes(x=Resistencia, y=yhat_red)) + geom_point() +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dashed", size=1.5)

