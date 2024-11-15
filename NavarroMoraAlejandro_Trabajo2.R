# 1. Cargar los datos y examinarlos en R. ¿Cuántas variables hay?¿Cuántos tratamientos?
# Especifica la ruta del archivo
data <- read.table("datos-trabajoR.txt", header = TRUE)
summary(data)
head(data)
dim(data)
str(data)

numerovariables <- dim(data)[2]
cat("Variables:", numerovariables, "\n")
numero_tratamiento <- length(unique(data$Tratamiento))
cat("Variables:", numero_tratamiento, "\n")

par(mfrow = c(1, 3))

# 2. Haz un boxplot para nuestros datos. Uno para cada condición. Elige un color para cada condición y guárdalo para las siguientes gráficas.
boxplot(Wildtype~Tratamiento, data = data, col = "lightblue", main = "Wildtype", xlab = "Tratamiento", ylab = "Value")

boxplot(Sequia~Tratamiento, data = data, col = "lightgreen", main = "Sequia", xlab = "Tratamiento", ylab = "Value")

boxplot(ExcesoRiego~Tratamiento, data = data, col = "orange", main = "ExcesoRiego", xlab = "Tratamiento", ylab = "Value")

# 3. Haz dos gráficos de dispersión. El primero debe comparar Sequía con Wildtype, y el segundo ExcesoRiego con Wildtype. Cada tratamiento debe de ir de un color distinto. 


# La leyenda hay que ponerla en la esquina inferior derecha, pero como se me solapa con los datos, la he puesto en la esquina superior izquierda
data$Tratamiento <- as.factor(data$Tratamiento)

plot(data$Wildtype, data$Sequia, col = data$Tratamiento, pch = 19, xlab = "Wildtype", ylab = "Sequia", main = "Sequia-Wildtype")

legend("topleft", legend = levels(data$Tratamiento), col = 1:length(levels(data$Tratamiento)), pch = 19, title = "Tratamiento")

plot(data$Wildtype, data$ExcesoRiego, col = data$Tratamiento, pch = 19, xlab = "Wildtype", ylab = "ExcesoRiego", main = "ExcesoRiego-Wildtype")

legend("topleft", legend = levels(data$Tratamiento), col = 1:length(levels(data$Tratamiento)), pch = 19, title = "Tratamiento")

# 4. Poner la leyenda para cada gráfico
# 5. Haz un histograma para cada variable. Recuerda mantener los colores.

par(mfrow = c(3, 1))

hist(data$Wildtype, col = "lightblue", main = "Histograma Wildtype", xlab = "Wildtype", ylab = "Frecuencia")

hist(data$Sequia, col = "lightgreen", main = "Histograma Sequia", xlab = "Sequia", ylab = "Frecuencia")

hist(data$ExcesoRiego, col = "orange", main = "Histograma ExcesoRiego", xlab = "ExcesoRiego", ylab = "Frecuencia")

# 6. Haz un factor en la columna tratamiento y guárdalo en una variable.

tratamiento2 <- factor(data$Tratamiento)
print(tratamiento2)

# 7. Calcula la media y la desviación estándar para cada tratamiento. 

mediaWildtype <- aggregate(Wildtype ~ Tratamiento, data, mean)
print(mediaWildtype)
mediaSequia <- aggregate(Sequia ~ Tratamiento, data, mean)
print(mediaSequia)
mediaExcesoRiego <- aggregate(ExcesoRiego ~ Tratamiento, data, mean)
print(mediaExcesoRiego)

desviacion_estandar_Wildtype <- aggregate(Wildtype ~ Tratamiento, data, sd)
print(desviacion_estandar_Wildtype)
desviacion_estandar_Sequia <- aggregate(Sequia ~ Tratamiento, data, sd)
print(desviacion_estandar_Sequia)
desviacion_estandar_ExcesoRiego <- aggregate(ExcesoRiego ~ Tratamiento, data, sd)
print(desviacion_estandar_ExcesoRiego)

# 8. Averigua cuántos elementos tiene cada tratamiento.

conteo_tratamiento <- table(tratamiento2)
print(conteo_tratamiento)

# 9. Extrae los daos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variables diferente.

trat1 <- subset(data, Tratamiento == 1)
print(trat1)

trat4 <- subset(data, Tratamiento == 4)
print(trat4)


# 10. Queremos comprobar que hay diferencias significativas para el tratamiento 1 y el tratamiento 5 entre Wildtype y Sequía, y entre Wildtype y ExcesoRiego. Primero, necesitaríamos comprobar si los datos se distribuyen de forma normal. En función de los resultados de la prueba de normalidad, ¿qué test usarías para cada comparativa?¿Puedes comparar también Sequía con ExcesoRiego en ambos tratamientos?

trat5 <- subset(data, Tratamiento == 5)
print(trat5)

normalidad_trat1_Wildtype <- shapiro.test(trat1$Wildtype)
print(normalidad_trat1_Wildtype)
normalidad_trat1_Sequia <- shapiro.test(trat1$Sequia)
print(normalidad_trat1_Sequia)
normalidad_trat1_ExcesoRiego <- shapiro.test(trat1$ExcesoRiego)
print(normalidad_trat1_ExcesoRiego)

normalidad_trat5_Wildtype <- shapiro.test(trat5$Wildtype)
print(normalidad_trat5_Wildtype)
normalidad_trat5_Sequia <- shapiro.test(trat5$Sequia)
print(normalidad_trat5_Sequia)
normalidad_trat5_ExcesoRiego <- shapiro.test(trat5$ExcesoRiego)
print(normalidad_trat5_ExcesoRiego)

var.test(trat1$Wildtype, trat1$Sequia)
var.test(trat1$Wildtype, trat1$ExcesoRiego)
var.test(trat1$Sequia, trat1$ExcesoRiego)
var.test(trat5$Wildtype, trat5$Sequia)
var.test(trat5$Wildtype, trat5$ExcesoRiego)
var.test(trat5$Sequia, trat5$ExcesoRiego)

if (shapiro.test(trat1$Wildtype)$p.value > 0.05 && shapiro.test(trat1$Sequia)$p.value > 0.05) {
	if (var.test(trat1$Wildtype, trat1$Sequia)$p.value > 0.05) {
		t.test(trat1$Wildtype, trat1$Sequia, var.equal = TRUE)
	} else {
		t.test(trat1$Wildtype, trat1$Sequia, var.equal = FALSE)
	}
} else {
	wilcox.test(trat1$Wildtype, trat1$Sequia)
}

if (shapiro.test(trat1$Wildtype)$p.value > 0.05 && shapiro.test(trat1$ExcesoRiego)$p.value > 0.05) {
	if (var.test(trat1$Wildtype, trat1$ExcesoRiego)$p.value > 0.05) {
		t.test(trat1$Wildtype, trat1$ExcesoRiego, var.equal = TRUE)
	} else {
		t.test(trat1$Wildtype, trat1$ExcesoRiego, var.equal = FALSE)
	}
} else {
	wilcox.test(trat1$Wildtype, trat1$ExcesoRiego)
}

if (shapiro.test(trat1$Sequia)$p.value > 0.05 && shapiro.test(trat1$ExcesoRiego)$p.value > 0.05) {
	if (var.test(trat1$Sequia, trat1$ExcesoRiego)$p.value > 0.05) {
		t.test(trat1$Sequia, trat1$ExcesoRiego, var.equal = TRUE)
	} else {
		t.test(trat1$Sequia, trat1$ExcesoRiego, var.equal = FALSE)
	}
} else {
	wilcox.test(trat1$Sequia, trat1$ExcesoRiego)
}

if (shapiro.test(trat5$Wildtype)$p.value > 0.05 && shapiro.test(trat5$Sequia)$p.value > 0.05) {
	if (var.test(trat5$Wildtype, trat5$Sequia)$p.value > 0.05) {
		t.test(trat5$Wildtype, trat5$Sequia, var.equal = TRUE)
	} else {
		t.test(trat5$Wildtype, trat5$Sequia, var.equal = FALSE)
	}
} else {
	wilcox.test(trat5$Wildtype, trat5$Sequia)
}

if (shapiro.test(trat5$Wildtype)$p.value > 0.05 && shapiro.test(trat5$ExcesoRiego)$p.value > 0.05) {
	if (var.test(trat5$Wildtype, trat5$ExcesoRiego)$p.value > 0.05) {
		t.test(trat5$Wildtype, trat5$ExcesoRiego, var.equal = TRUE)
	} else {
		t.test(trat5$Wildtype, trat5$ExcesoRiego, var.equal = FALSE)
	}
} else {
	wilcox.test(trat5$Wildtype, trat5$ExcesoRiego)
}

if (shapiro.test(trat5$Sequia)$p.value > 0.05 && shapiro.test(trat5$ExcesoRiego)$p.value > 0.05) {
	if (var.test(trat5$Sequia, trat5$ExcesoRiego)$p.value > 0.05) {
		t.test(trat5$Sequia, trat5$ExcesoRiego, var.equal = TRUE)
	} else {
		t.test(trat5$Sequia, trat5$ExcesoRiego, var.equal = FALSE)
	}
} else {
	wilcox.test(trat5$Sequia, trat5$ExcesoRiego)
}

# 11. Realiza un ANOVA para comparar el tratamiento 1 en las tres condiciones.

trat_uno_Wildtype <- trat1$Wildtype
trat_uno_Sequia <- trat1$Sequia
trat_uno_ExcesoRiego <- trat1$ExcesoRiego

print(trat_uno_Wildtype)
print(trat_uno_Sequia)
print(trat_uno_ExcesoRiego)

datosAnova <- data.frame("Condicion" = c("W","W","W","W","W","W","W","W","W","W","S","S","S","S","S","S","S","S","S","S","E","E","E","E","E","E","E","E","E","E"),"Valor"=c(trat_uno_Wildtype, trat_uno_Sequia, trat_uno_ExcesoRiego))
anova <- aov(Valor ~ Condicion, data=datosAnova)
summary(anova)


