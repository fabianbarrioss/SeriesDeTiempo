# Paquetes Utilizados 

library(tseries)
library(forecast)
library(readxl)
library(RcmdrMisc)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(TSstudio)
library(xts)
library(TSA)
library(lmtest)
library(car)
library(FinTS)

### Cargar datos
### Gasto_consumo_final
PIB <- read_excel("PIB.xlsx", sheet = "Hoja1")
sc <- tsclean(ts(PIB$Gasto_consumo_final, start = c(2005, 1), frequency = 4))

### Gráfico de la serie temporal
ts.plot(sc, title = "",
        Ytitle = "Gasto Consumo Final",
        Xtitle = "",
        slider = F,
        line.mode = "lines+markers")


### Generar gráficos de autocorrelación
ts_cor(sc)

### Varianza 
lambda <- BoxCox.lambda(sc)

sc.bc <- BoxCox(sc, lambda)
ts.plot(sc.bc)

par(mfrow = c(1,2))

## Dividimos los datos
h = 2
sc.div <- ts_split(sc.bc, sample.out = h)
sc.e <- sc.div$train ## Datos de entrenamiento
sc.p <- sc.div$test ## Datos de prueba 
plot(sc.e)
plot(sc.p)

#--------------------------------
## Fase 1 
# D y d 
ts.cor(sc.e)

## Diferenciación 
D = nsdiffs(sc.e) 
d = ndiffs(sc.e)
s = 4

plot(sc.e)
seriedD <- diff(sc.e, differences = 1, lag = 4)
ts_plot(seriedD)

ts.plot(seriedD)

par(mfrow = c(1, 2))

### Estacionariedad de la serie diferenciada 
### Identificación P y Q: ACF y PACF
ts.cor(seriedD, lag.max = 60)
kpss.test(seriedD) ##

acf(seriedD, lag.max = 60)

### Identificacion los p y q usando los datos de entrenamiento 
y <- seriedD
orden <- eacf(y, ar.max = 10, ma.max = 10)

## Posibles modelos 
Umbral <-  2/sqrt(length(y)); Umbral
#orden$eacf

selec <- ifelse(abs(orden$eacf) < Umbral + 0.07, 1, 0) ## 0.07 
colnames(selec) <- 0:10
row.names(selec) <- 0:10
print(selec) ## Matrix con más posibles modelos 


### Modelos ARMA Candidatos Con datos de entrenemiento (sc.e) 

## 1,0 Si ML
## 1,1 NO
## 1,2 No
## 1,3 Si ML - CSS mas o menos
## 1,4 No

## 2,1 No
## 2,2 No
## 2,3 No
## 2,4 Si CSS - ML NO

## 3,1 No
## 3,2 No
## 3,3 No
## 3,4 No

## 4,1 No
## 4,2 No 
## 4,3 No
## 4,4 No 

## 0,1 Si ML
## 0,2 Si ML
## 0,3 No
## 0,4 No
m1 <- Arima(sc.e, order = c(2, 1, 3),
              seasonal = list(order = c(1, 1, 0), period = 4),
              method = 'ML'); coeftest(m1)


m2 <- Arima(sc.e, order = c(2, 1, 3),
            seasonal = list(order = c(1, 1, 0), period = 4),
            method = 'CSS'); coeftest(m2)

m3 <- Arima(sc.e, order = c(2, 1, 3),
            seasonal = list(order = c(1, 1, 0), period = 4),
            method = 'CSS-ML'); coeftest(m3)


forecast(modelo, h = 4, bootstrap = T) ## Cuando los ei no son normales


typeof(sc.e)

### Modelos seleccionados 
modelo1 <- Arima(sc.e, order = c(1, 1, 0),
                 seasonal = list(order = c(1, 1, 0), period = 4),
                 method = 'ML'); coeftest(modelo1)



modelo2 <- Arima(sc.e, order = c(0, 1, 1),
                 seasonal = list(order = c(1, 1, 0), period = 4),
                 method = 'ML'); coeftest(modelo2)


## h = 2
modelo3 <- Arima(sc.e, order = c(0, 1, 2),
                 seasonal = list(order = c(1, 1, 0), period = 4),
                 method = 'ML'); coeftest(modelo1)


modelo4 <- Arima(sc.e, order = c(1, 1, 3),
                 seasonal = list(order = c(1, 1, 0), period = 4),
                 method = 'ML'); coeftest(modelo2)


arima1 <- auto.arima(sc.e, trace = T) # h = 4
arima2 <- auto.arima(sc.e, trace = T) # h = 2

arima1
arima2

length(sc.e)
AIC(modelo1, modelo2)

## Suma de los coeficientes diferente de 1
coef(modelo1)
coef(modelo2)


## Media de los Residuos: One Sample t-test
t.test(residuals(modelo1)) ## 1, 0
t.test(residuals(modelo2)) ## 0, 1 

## Normalidad de los Residuos: Kolmogorov-Smirnov test
ks.test(residuals(modelo1), "pnorm")
ks.test(residuals(modelo2), "pnorm")
ks.test(residuals(modelo3), "pnorm")
ks.test(residuals(modelo4), "pnorm")
ks.test(residuals(arima), "pnorm")


par(mfrow = c(1, 2))
qqnorm(residuals(modelo1))
qqline(residuals(modelo1))
hist(residuals(modelo1),ylim = c(0, 0.4),  breaks = 10, freq = F, main = "Histogram of resifuals", xlab = "")
lines(density(residuals(modelo1)), col = "red", lwd = "2")
par(mfrow = c(1, 2))


## Homocedasticidad de los Residuos: ARCH LM-test
ArchTest(residuals(modelo1))
ArchTest(residuals(modelo2))

## Independencia de los Residuos: Ljung-Box
par(mfrow = c(1, 2))

n <- length(arima1$x)
k <- seq(6, n, 15)
resultados <- data.frame(k = numeric(),
                         Estadistico = numeric(),
                         p_valor = numeric(),
                         stringsAsFactors = FALSE)
for (i in seq_along(k)) {
  result <- Box.test(residuals(arima1), lag = k[i], type = "Ljung-Box")
  resultados <- rbind(resultados, 
                      data.frame(k = k[i],
                                 Estadistico = result$statistic,
                                 p_valor = result$p.value))
}
resultados$p_valor[resultados$p_valor > 1.2] <- 1.2
altura_barras1 <- barplot(resultados$p_valor,
                          main = "ARIMA(1,0,2)(0,1,1)[4]",
                          ylab = "Valores p",
                          xlab = "Rezagos",
                          names.arg = resultados$k,
                          col = "skyblue",
                          ylim = c(0, 1.2))


abline(h = 0.05, col = "red", lwd = 2)
text(x = altura_barras1,  
     y = resultados$p_valor + 0.02,
     labels = round(resultados$p_valor, 2),
     pos = 3,
     col = "black")
#legend("topright", legend = c("Valor p = 0.05"), col = c("red"), lwd = 2, bty = "n")
legend("topright", legend = c("Valor p = 0.05"), col = c("red"), lwd = 2, bty = "n", inset = c(0, -0.05))



## Dependencias No Lineales: Prueba BDS

bds.test(residuals(modelo1))
bds.test(residuals(modelo2))
bds.test(residuals(modelo3))
bds.test(residuals(modelo4))

r1 <- bds.test(residuals(modelo1))
r2 <- bds.test(residuals(modelo2))
r3 <- bds.test(residuals(modelo3))
r4 <- bds.test(residuals(modelo4))
r5 <- bds.test(residuals(arima1))
r6 <- bds.test(residuals(arima2))

resul1 <- data.frame(cbind(t(r1$p.value)), 
                     round(r1$parameter$eps, 2))
colnames(resul1) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul1) <- NULL

resul2 <- data.frame(cbind(t(r2$p.value)), 
                     round(r2$parameter$eps, 2))
colnames(resul2) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul2) <- NULL

resul3 <- data.frame(cbind(t(r3$p.value)), 
                     round(r3$parameter$eps, 2))
colnames(resul3) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul3) <- NULL

resul4 <- data.frame(cbind(t(r4$p.value)), 
                     round(r4$parameter$eps, 2))
colnames(resul4) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul4) <- NULL

resul5 <- data.frame(cbind(t(r5$p.value)), 
                     round(r5$parameter$eps, 2))
colnames(resul5) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul5) <- NULL

resul6 <- data.frame(cbind(t(r6$p.value)), 
                     round(r6$parameter$eps, 2))
colnames(resul6) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul6) <- NULL

gra <- barplot(
  t(resul5[, -3]),  
  beside = TRUE,    
  names.arg = resul1$Epsilon,  
  col = c("#87CEEB", "#FFB6C1"),   
  ylim = c(0, 1.2),  
  xlab = "Epsilon", 
  ylab = "P-valor",  
  main = "ARIMA(1,0,2)(0,1,1)[4]" 
)
abline(h = 0.05, col = "red", lty = 1, lwd = 2)

legend("top", 
       legend = c("Dim 2", "Dim 3", "Valor p = 0.05"),
       fill = c(colores, "red"), 
       bty = "n",
       inset = c(0, -0.12),
       title = "",
       horiz = TRUE  
)


### Pronostico 

h=c(2,4)
pronos1 <- forecast(modelo1, h[2])
pronos3 <- forecast(arima2, h[1])

autoplot(pronos3)
f1 <- predict(modelo3, h)


## Graficar los datos de entrenamiento y el ajuste del modelo
plot(sc.e, col = "#FFB6C1", lwd = 5, ylim = range(c(sc.e, fitted(arima2))),
     xlab = "Tiempo", ylab = "Valores",
     main = "")
lines(fitted(arima2), col = "blue")
legend("topleft", legend = c("Datos de Entrenamiento", "Pronóstico"), 
       col = c("#FFB6C1", "blue", rgb(0, 0, 1, 0.2)), lwd = 2, pch = c(16, 16, NA), 
       fill = c(NA, NA, rgb(0, 0, 1, 0.2)), border = c(NA, NA, "blue"), cex = 0.8)



plot(sc.p, col = "red", lwd = 3, ylab = "Valores", xlab = "Tiempo", 
     main = "", type = "o", pch = 16, 
     xlim = c(time(sc.p)[1], time(pronos3$mean)[length(pronos3$mean)]), 
     ylim = range(c(sc.p, pronos3$mean, pronos3$lower, pronostico$upper)))

lines(pronos3$mean, col = "blue", lwd = 2, type = "o", pch = 16)
polygon(c(time(pronos3$mean), rev(time(pronos3$mean))), 
        c(pronos3$lower[,2], rev(pronos3$upper[,2])), 
        col = rgb(0, 0, 1, 0.2), border = NA)
legend("topleft", legend = c("Datos de Prueba", "Pronóstico", "Intervalo de Confianza"), 
       col = c("red", "blue", rgb(0, 0, 1, 0.2)), lwd = 2, pch = c(16, 16, NA), 
       fill = c(NA, NA, rgb(0, 0, 1, 0.2)), border = c(NA, NA, "blue"), cex = 0.8)





### accuracy(y.est, y)
## --------------

pronos3 <- forecast(modelo3, h[1])

accuracy(InvBoxCox(pronos3$fitted, lambda), InvBoxCox(sc.e, lambda)) ## Prueba 
accuracy(InvBoxCox(pronos3$mean, lambda), InvBoxCox(sc.p, lambda)) ## Entrenamiento 

accuracy(InvBoxCox(pronos1$fitted, lambda), InvBoxCox(sc.e, lambda)) ## Prueba 
accuracy(InvBoxCox(pronos1$mean, lambda), InvBoxCox(sc.p, lambda)) ## Entrenamiento 


### ARIMA 

h = 2
sc.div <- ts_split(sc.bc, sample.out = h)
sc.e <- sc.div$train ## Datos de entrenamiento
sc.p <- sc.div$test ## Datos de prueba 


## Diferenciación
# ndiffs(sc.e)
y <- diff(sc.e, d = 1)
yp <- diff(sc.p, d = 1)

# Verificación de estacionariedad de la serie diferenciada
par(mfrow = c(1, 2))
plot(sc, xlab = "Año")
title("Serie Original")
plot(y, xlab = "Año"); abline( h = 0)
title("Serie Diferenciada(1)")

orden <- eacf(y, ar.max = 10, ma.max = 10)
Umbral <-  2/sqrt(length(y)); Umbral

selec <- ifelse(abs(orden$eacf) < Umbral + 0.07, 1, 0) ## 0.07 
colnames(selec) <- 0:10
row.names(selec) <- 0:10
print(selec) ## Matrix con más posibles modelos 


# 3,1 si
# 3,2 si
# 3,3 si

# 2,3 si CSS-ML
# 2,2 Si
# 2.1 no
ts_plot(y)
ts_cor(y)

m33m <- Arima(y, order = c(3, 1, 3), method = 'ML')
m33c <- Arima(y, order = c(3, 1, 3), method = 'CSS')
m33cm <- Arima(y, order = c(3, 1, 3), method = 'CSS-ML')

coeftest(m33m) ## Mejor 
### 

ml1 <- m33m


pronos <- forecast(ml1, h)
autoplot(pronos)

# Calcular las medidas de precisión para las predicciones ajustadas (Prueba) y las predicciones en el conjunto de entrenamiento (Entrenamiento)

accuracy(InvBoxCox(pronos$fitted, lambda), InvBoxCox(sc.e, lambda))  # Prueba
accuracy(InvBoxCox(pronos$mean, lambda), InvBoxCox(y, lambda))

accuracy(InvBoxCox(pronos$mean, lambda), InvBoxCox(sc.p, lambda))
accuracy(InvBoxCox(pronos$fitted, lambda), InvBoxCox(sc.e, lambda))







par(mfrow = c(1, 2))

## Serie original 

ts.plot(AirPassengers)
## CAlculamos el lambda 
lambda <- BoxCox.lambda(AirPassengers,lower=0)
## Serie transformada
Yt_B <- BoxCox(AirPassengers, lambda)


par(mfrow = c(1, 2))
ts.plot(AirPassengers)
ts.plot(Yt_B)

## manual 
ytb <- ( AirPassengers^(lambda) - 1 )/lambda
ts.plot(ytb)




