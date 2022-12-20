table(Rendimiento$Marca)
plot(table(Rendimiento$Marca))

plot(table(Rendimiento$Marca),cex.axis=0.5)

table(Rendimiento$CLASS)

plot(table(Rendimiento$CLASS))

table(Rendimiento$TRANSMISSION)

table(Rendimiento$`FUEL TYPE`)

plot(table(Rendimiento$`FUEL TYPE`), xlab = "Tipo de combustible", ylab = "Frecuencia", main = "Frecuencias de Tipos de Gasolinas")

AUDI <- Rendimiento[Rendimiento$Marca == "AUDI",]
table(AUDI$CLASS)

NISSAN <- Rendimiento[Rendimiento$Marca == "NISSAN",]
table(NISSAN$CLASS)

mean(AUDI$`CONSUMPTION  CITY (L/100 KM)`, na.rm = TRUE)

mean(NISSAN$`CONSUMPTION  CITY (L/100 KM)`, na.rm = TRUE)

mean(Rendimiento$`CONSUMPTION  CITY (L/100 KM)`, na.rm = TRUE)

var(Rendimiento$`CONSUMPTION  CITY (L/100 KM)`, na.rm = TRUE)

var(AUDI$`CONSUMPTION  CITY (L/100 KM)`, na.rm = TRUE)

var(NISSAN$`CONSUMPTION  CITY (L/100 KM)`, na.rm = TRUE)

hist(Rendimiento$`CONSUMPTION  CITY (L/100 KM)`)
hist(Rendimiento$`CONSUMPTION  CITY (L/100 KM)`, breaks = 12)


summary(Rendimiento$`CONSUMPTION  COMBINED (L/100 KM)`)



DODGE <- Rendimiento[Rendimiento$Marca == "DODGE",]
summary(DODGE$`CONSUMPTION  COMBINED (L/100 KM)`)
mean(Rendimiento$`CONSUMPTION  CITY (L/100 KM)`)
var(DODGE$`CONSUMPTION COMBINED (L/100 KM)`, na.rm = TRUE)
