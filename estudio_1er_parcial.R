### Cargamos librerías
library(readxl)
library(dplyr)
library(tidyr)

## Leemos la base
av <- read_xlsx("Psicoestadistica_19_A-Primer parcial 2019-calificaciones.xlsx")
attributes(av)$names[1] <- "Apellido"


asistencia <- read_xls("013-A5 (1).xls")

colnames(asistencia) <- c("Apellido", "Nombre", "Documento", "1ra entrega", "11_abr", "2_may", "9_may", "16_may",
       "sin_fecha", "sin_fecha2", "6_jun", "horas_estudio", "n_asistencias", "ritmo_estudio", "orden1", "orden2",
       "orden3", "orden4", "orden5", "orden6", "apoyo")


## Juntamos las tablas
datos <- right_join(av, asistencia, by = c("Apellido", "Nombre"))

datos <- datos[!datos$`2_may` > 1, ]

## Quitamos los casos que no tienen apellido
datos <- datos[!is.na(datos$Apellido), ]


## 


hist(as.numeric(datos$horas_estudio), breaks = 30)

plot(as.numeric(datos$horas_estudio), datos$`Calificación/10`, col = rgb(0, 0, 0.9, alpha = 0.2))
plot(as.numeric(datos$practico), datos$`Calificación/10`, col = rgb(0, 0, 0.9, alpha = 0.05))

cor(as.numeric(datos$horas_estudio), as.numeric(datos$`Calificación/10`), use = "complete.obs")
cor(as.numeric(datos$n_asistencias), as.numeric(datos$`Calificación/10`), use = "complete.obs")

lm(`Calificación/10`)


convertir_a_bin <- name <- function(x) {
  x[x == "P"] <- 1
  x[is.na(x)] <- 0
  x
}

datos$`11_abr` <- convertir_a_bin(datos$`11_abr`)
datos$`2_may` <- convertir_a_bin(datos$`2_may`)
datos$`9_may` <- convertir_a_bin(datos$`9_may`)
datos$`6_jun` <- convertir_a_bin(datos$`6_jun`)

datos$`11_abr` <- as.numeric(datos$`11_abr`)
datos$`2_may`  <- as.numeric(datos$`2_may`)
datos$`9_may`  <- as.numeric(datos$`9_may`)
datos$`6_jun`  <- as.numeric(datos$`6_jun`)

datos$practico <- datos$`11_abr` + datos$`2_may`  + datos$`9_may`  + datos$`6_jun` 

hist(datos$practico)


fit <- lm(`Calificación/10` ~ as.numeric(horas_estudio) + as.numeric(n_asistencias) + as.numeric(practico), data = datos)
fit2 <- lm(`Calificación/10` ~ as.numeric(horas_estudio) + as.numeric(n_asistencias) + as.numeric(practico) + I(as.numeric(practico)^2), data = datos)
summary(fit)
summary(fit2)

datos$ritmo_estudio <- toupper(datos$ritmo_estudio)
datos$ritmo_estudio <- factor(datos$ritmo_estudio,
          levels = LETTERS[1:3],
          labels = c("He estudiado la materia regularmente desde el inicio de clases",
          "He comenzado a estudiar esta semana", 
          "Aún no comencé a estudiar la materia"))

table(datos$ritmo_estudio)
barplot(datos$ritmo_estudio)

fit3 <- lm(`Calificación/10` ~ datos$ritmo_estudio, data = datos)
summary(fit3)
unique(fit3$fitted.values)
fit3$rank
fit3$contrasts

aggregate(datos$`Calificación/10`, by = list(datos$ritmo_estudio), FUN = mean, na.rm = TRUE)
tapply(as.numeric(datos$`Calificación/10`), list(datos$ritmo_estudio), mean, na.rm = TRUE)

anova(fit3)


fit4 <- lm(`Calificación/10` ~ as.numeric(horas_estudio) + as.numeric(n_asistencias) + as.numeric(practico) + datos$ritmo_estudio, data = datos)
summary(fit4)
