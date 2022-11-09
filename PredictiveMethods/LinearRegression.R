#-----------------------------------------------------------------------------------
#1) Generacion de datos para explicacion
ejex = seq(1, 100)

ruido = runif(100, min = -6, max = 6)
datos = -15 + 0.7 * ejex + ruido

plot(ejex,
     datos,
     type = "l",
     col = "blue",
     main = "Senal real")
#----------------------------------------
#2) Se aplica el metodo de minimos cuadrados
c1 = rep(1, 100)
c2 = seq(0, 99, 1)
c3 = seq(0, 99, 1) ^ 2
c4 = seq(0, 99, 1) ^ 3

#H = cbind(c1, c2, c3, c4)
H = cbind(c1, c2)

#Obtenemos B0 y B1
poli = (solve(t(H) %*% H)) %*% t(H) %*% datos  #Theta=(H^T * H)^-1  H^T x
tendencia = 0 * seq(1, 100, 1)

for (i in 1:100)
  #{tendencia[i]=poli[1]+poli[2]*i+(poli[3]*i*i)+(poli[4]*i*i*i)}
{
  tendencia[i] = poli[1] + poli[2] * i
}
#-----------------------------------------------------------------------------------
lines(ejex, tendencia, col = "red")
poli
