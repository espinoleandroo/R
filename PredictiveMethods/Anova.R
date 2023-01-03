
rm(list = ls())
Tiempo.t0 = c(108, 109, 99, 103, 107, 95, 102)
Tiempo.t1 = c(84, 82, 85, 92, 87, 78, 90)
Tiempo.t2 = c(76, 85, 74, 78, 82, 75, 82)
Tiempo.t3 = c(57, 67, 64, 61, 63, 55, 63)
#Esta no es la forma adecuada para trabajar con un programa estadistico. Mejor ponemos los datos
#en un unico vector y anadimos una variable cualitativa o factor que nos indique la poblacion de cada
#dato
Tiempo <- c(Tiempo.t0, Tiempo.t1, Tiempo.t2, Tiempo.t3)
PorcSal <- rep(1:4, each = 7)
PorcSal <- factor(PorcSal, labels = c("0%", "1%", "2%", "3%"))
# ------------------------------------------------------------------------------------
#En R es imprescindible definir el vector especie como un factor, ya que en caso contrario se podria
#confundir con un vector numerico. #Una unica instruccion realiza los dos pasos:
PorcSal <- gl(4, 7, labels = c("0%", "1%", "2%", "3%"))
# ------------------------------------------------------------------------------------
#Con la instruccion split podemos separar los datos:
split(Tiempo, PorcSal)
# ------------------------------------------------------------------------------------
# Resumen de los datos y el grafico
tapply(Tiempo, PorcSal, summary)
plot(Tiempo ~ PorcSal, main = 'Boxplot para tiempo de coccion debido a la sal')
# ------------------------------------------------------------------------------------
# ANOVA
p.aov <- aov(Tiempo ~ PorcSal)
summary(p.aov)


# ------------------------------------------------------------------------------------
#A partir de los residuos del modelo comprobaremos si el modelo
# ANOVA es adecuado. Los supuestos que se deben cumplir son
# tres: independencia, homocedasticidad y normalidad.
plot(p.aov$residuals)
summary(p.aov$residuals)
boxplot(p.aov$residuals)
hist(p.aov$residuals)
qqnorm(p.aov$residuals)
qqline(p.aov$residuals)
# El test de Shapiro-Wilk indica que no tenemos evidencia suficiente
# para rechazar la hip�tesis nula (normalidad de los residuos)
shapiro.test(p.aov$residuals)
