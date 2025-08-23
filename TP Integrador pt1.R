rm(list = ls())

if (!require(readxl)) install.packages("readxl")
library(readxl)
if (!require(agricolae)) install.packages("agricolae")
library(agricolae)
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)
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
colnames(tf_summary) <- c("Frecuencia_Absoluta", "Frecuencia_Relativa", "Frecuencia_Acumulada_Absoluta", "Frecuencia_Acumulada_Relativa")
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
colnames(cuarto_intervalo) <- c("Frecuencia_Absoluta", "Frecuencia_Relativa", "Frecuencia_Acumulada_Absoluta", "Frecuencia_Acumulada_Relativa")
extracted_value <- cuarto_intervalo$"Frecuencia_Absoluta"
print(paste("En función de las horas semanales, se agrupa a: ", extracted_value, " estudiantes."))
extracted_value <- cuarto_intervalo$"Frecuencia_Relativa"
print(paste("Que representa un: ", extracted_value, "% del total de la muestra"))
extracted_value <- cuarto_intervalo$"Frecuencia_Acumulada_Absoluta"
print(paste("Entendemos, que en el intervalo 4, en su frecuencia acumulada, hay un total de: ", extracted_value, " estudiantes."))
extracted_value <- cuarto_intervalo$"Frecuencia_Acumulada_Relativa"
print(paste("Esto supone entonces, un: ", extracted_value, "% total para la frecuencia acumulada."))

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
print(paste("Que representa un: ", Frecuencia_Relativa, "% del total de la muestra"))
print(paste("Entendemos, que su frecuencia acumulada, hay un total de: ", Frecuencia_Acumulada_Absoluta, " estudiantes."))
print(paste("Esto supone entonces, un: ", Frecuencia_Acumulada_Relativa, "% total para la frecuencia acumulada."))





