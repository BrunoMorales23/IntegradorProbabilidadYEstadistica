rm(list = ls())

if (!require(readxl)) install.packages("readxl")
library(readxl)
if (!require(agricolae)) install.packages("agricolae")
library(agricolae)
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)
if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)
#--------------------------------------------------------------------------------------------------------------------------------------
#A)
sheets <- c("planilla", "nivel de satisfacción")
xlsx_file <- file.choose()
lista_hojas <- lapply(sheets, function(x) read.xlsx(xlsx_file, sheet = x))
names(lista_hojas) <- sheets

base = data.frame(lista_hojas$"planilla")
satisfaccion = data.frame(lista_hojas$"nivel de satisfacción")
str(base)
str(satisfaccion)

table(base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`)
n = length(base$TIEMPO.SEMANAL.en.HS..DEDIC..EST.)
n_sturges = ceiling(log2(n) + 1)

t=hist(base$TIEMPO.SEMANAL.en.HS..DEDIC..EST., breaks=n_sturges, plot=FALSE)
tf=table.freq(t)
tf_summary = tf[, c("Frequency", "Percentage", "CF", "CPF")]
colnames(tf_summary) <- c("Frec_Abs", "Frec_Rel", "Frec_Acu_Abs", "Frec_Acu_Rel")
tf_summary
print(paste("El número óptimo de intervalos para el tiempo de horas semanales dedicadas al estudio es: ", n_sturges))
#--------------------------------------------------------------------------------------------------------------------------------------
#B)
satisfaccion_value <- table(base$`SATISFACCIÓN.CON.LA.CARRERA`)
Frecuencia_Absoluta <- as.numeric(satisfaccion_value)
Frecuencia_Relativa <- round(Frecuencia_Absoluta / sum(Frecuencia_Absoluta) * 100, 2)
Frecuencia_Acumulada_Absoluta <- cumsum(Frecuencia_Absoluta)
Frecuencia_Acumulada_Relativa <- cumsum(Frecuencia_Relativa)
tf_summary <- data.frame(
  Categoria = names(satisfaccion_value),
  Frecuencia_Absoluta,
  Frecuencia_Relativa,
  Frecuencia_Acumulada_Absoluta,
  Frecuencia_Acumulada_Relativa
)

tf_summary
top_k <- names(which.max(satisfaccion_value))
combinado <- paste(satisfaccion$Nivel.de.satisfacción.con.la.carrera, satisfaccion$X2, sep = " - ")
combinado

resultado_final <- switch(top_k,
                          "1" = combinado[1],
                          "2" = combinado[2],
                          "3" = combinado[3],
                          "4" = combinado[4]
)

print(paste("El nivel de satisfacción con la carrera es: ", resultado_final))

#--------------------------------------------------------------------------------------------------------------------------------------
#C)
t=hist(base$TIEMPO.SEMANAL.en.HS..DEDIC..EST., plot=FALSE)
tf=table.freq(t)
tf_summary = tf[, c("Frequency", "Percentage", "CF", "CPF")]
tf_summary
cuarto_intervalo <- tf_summary[4, ]
colnames(cuarto_intervalo) <- c("Frec_Abs", "Frec_Rel", "Frec_Acu_Abs", "Frec_Acu_Rel")
extracted_value <- cuarto_intervalo$"Frec_Abs"
print(paste("En función de las horas semanales, se agrupa a: ", round(extracted_value,4), " estudiantes."))
extracted_value <- cuarto_intervalo$"Frec_Rel"
print(paste("Que representa un: ", round(extracted_value,4), "% del total de la muestra"))
extracted_value <- cuarto_intervalo$"Frec_Acu_Abs"
print(paste("Entendemos, que en el intervalo 4, en su frecuencia acumulada, hay un total de: ", round(extracted_value,4), " estudiantes."))
extracted_value <- cuarto_intervalo$"Frec_Acu_Rel"
print(paste("Esto supone entonces, un: ", round(extracted_value,4), "% total para la frecuencia acumulada."))

#--------------------------------------------------------------------------------------------------------------------------------------
#D)
satisfaccion_value <- base[base$`SATISFACCIÓN.CON.LA.CARRERA` == 2, ]
satisfaccion_value <- table(satisfaccion_value$`SATISFACCIÓN.CON.LA.CARRERA`)
satisfaccion_total <- table(base$`SATISFACCIÓN.CON.LA.CARRERA`)

Frecuencia_Absoluta <- as.numeric(satisfaccion_value)
Frecuencia_Relativa <- as.numeric(satisfaccion_value) / sum(satisfaccion_total) * 100
Frecuencia_Acumulada_Absoluta <- cumsum(Frecuencia_Absoluta)
Frecuencia_Acumulada_Relativa <- cumsum(Frecuencia_Relativa)
tf_summary <- data.frame(
  Categoria = names(satisfaccion_value),
  Frecuencia_Absoluta,
  Frecuencia_Relativa,
  Frecuencia_Acumulada_Absoluta,
  Frecuencia_Acumulada_Relativa
)


print(paste("En función de las satisfacción general, se agrupa a: ", Frecuencia_Absoluta, " estudiantes."))
print(paste("Que representa un: ", round(Frecuencia_Relativa,4), "% del total de la muestra"))
print(paste("Entendemos, que su frecuencia acumulada, hay un total de: ", Frecuencia_Acumulada_Absoluta, " estudiantes."))
print(paste("Esto supone entonces, un: ", round(Frecuencia_Acumulada_Relativa, 4), "% total para la frecuencia acumulada."))

#--------------------------------------------------------------------------------------------------------------------------------------
#3)

print(paste("A continuación, los valores de tendencia central, posición y dispersión para la variable 'Tiempo Semanal en Horas'"))
print(paste("Empezando por la tendencia central"))

media <- round(mean(base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`, na.rm = TRUE),4)
print(paste("Media: ",media))
mediana <- round(median(base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`, na.rm = TRUE),4)
print(paste("Mediana: ",mediana))

getmoda <- function(num) {
  uniqv <- unique(num)                 
  uniqv[which.max(tabulate(match(num, uniqv)))]
}

moda <- getmoda(base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`)
print(paste("Moda: ", moda))

print(paste("Ahora, los valores de posición"))
print(paste("Para cuartiles: "))
cuartiles <- quantile((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`), probs = c(0.25, 0.5, 0.75))
cuartiles

print(paste("Para deciles: "))
deciles <- quantile((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`), probs = seq(0.1, 0.9, by = 0.1))
deciles

print(paste("Para percentiles: "))
percentiles <- quantile((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`), probs = seq(0.01, 0.99, by = 0.01))
percentiles


print(paste("Finalmente, la dispersión."))


varianza <- round(var((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`)),4)
print(paste("Para la varianza: ", varianza))

desviacion <- round(sd((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`)),4)
print(paste("Para la desviación:", desviacion))

rango <- round(range((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`)),4)
print(paste("El valor del rango es: "))
rango
rango_diff <- round(diff(range((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`))),4)
print(paste("El valor del rango como diferencia es: "))
rango_diff  

print(paste("El coeficiente de variación es: "))
coeficiente <- sd((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`)) / mean((base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`))
coeficiente

print(paste("Finalmente, los valores de moda, mediana y cuartiles para 'Satisfacción con la carrera'"))

moda <- getmoda(base$`SATISFACCIÓN.CON.LA.CARRERA`)
resultado_final <- switch(moda,
                          "1" = "1 - Muy satisfecho",
                          "2" = "2 - Satisfecho",
                          "3" = "3 - Insatisfecho",
                          "4" = "4 - Muy insatisfecho"
)
print(paste("Moda: ", resultado_final))

mediana <- round(median(base$`SATISFACCIÓN.CON.LA.CARRERA`, na.rm = TRUE),4)
resultado_final <- switch(mediana,
                          "1" = "1 - Muy satisfecho",
                          "2" = "2 - Satisfecho",
                          "3" = "3 - Insatisfecho",
                          "4" = "4 - Muy insatisfecho"
)
print(paste("Mediana: ", resultado_final))

print(paste("Por último, cuartiles: "))
cuartiles <- quantile((base$`SATISFACCIÓN.CON.LA.CARRERA`), probs = c(0.25, 0.5, 0.75))
cuartiles

#--------------------------------------------------------------------------------------------------------------------------------------
#4)

hist(base$`TIEMPO.SEMANAL.en.HS..DEDIC..EST.`,
     breaks = "Sturges",
     main = "Horas semanales de estudio",
     xlab = "Horas semanales",
     ylab = "Frecuencia Absoluta",
     col = "orange",
     border = "black")

frecuencias <- factor(base$`SATISFACCIÓN.CON.LA.CARRERA`,
                      levels = c(1,2,3,4),
                      labels = c("1 - Muy satisfecho", "2 - Satisfecho", "3 - Insatisfecho", "4 - Muy Insatisfecho"))

frecuencias <- table(frecuencias)
nombres <- names(frecuencias)
colores <- brewer.pal(n = length(frecuencias), name = "Set3")
pie(frecuencias, labels = nombres, col = colores, main = "Nivel de satisfacción con la carrera")





