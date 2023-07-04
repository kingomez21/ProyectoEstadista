# TALLER FINAL DE ESTADISTICA


#Primera Pregunta

# a)
# Cálculo de la probabilidad utilizando la distribución normal

media <- 250
desviacion <- 2
rango_inferior <- 245
rango_superior <- 255

# Estandarización de los valores de los extremos del rango
z_inferior <- (rango_inferior - media) / desviacion
z_superior <- (rango_superior - media) / desviacion

# Cálculo de la probabilidad acumulada de los valores estandarizados
prob_inferior <- pnorm(z_inferior)
prob_superior <- pnorm(z_superior)

# Cálculo de la probabilidad de que el próximo producto sea clasificado como disconforme
prob_disconforme <- prob_superior - prob_inferior

# Imprimir resultado
prob_disconforme


# b)
# Cálculo del valor límite para el 0.2% superior

media <- 250
desviacion <- 2
prob_superior <- 0.002  # 0.2% convertido a probabilidad

# Cálculo del valor Z correspondiente al percentil 99.8
z_superior <- qnorm(1 - prob_superior)

# Cálculo del límite superior de peso
limite_superior <- media + (z_superior * desviacion)

# Imprimir resultado
limite_superior


# c)
# Cálculo de la probabilidad utilizando la distribución binomial

tamaño_muestra <- 10
probabilidad_disconforme <- 0.9876  # Probabilidad obtenida anteriormente

# Calcular la probabilidad de máximo 2 productos disconformes
prob_max_2_disconformes <- sum(dbinom(0:2, size = tamaño_muestra, prob = probabilidad_disconforme))

# Imprimir resultado
prob_max_2_disconformes



#Segunda Pregunta

prob_tres_lleguen_tarde <- dbinom(3, size = 35, prob = 0.05)
prob_tres_lleguen_tarde

prob_ninguno_llegue_tarde <- dbinom(0, size = 35, prob = 0.05)
prob_ninguno_llegue_tarde

prob_max_uno_llegue_tarde <- sum(dbinom(0:1, size = 35, prob = 0.05))
prob_max_uno_llegue_tarde

numero_esperado_llegan_tiempo <- 35 * 0.95
numero_esperado_llegan_tiempo



#Tercera pregunta

# Datos de muestra
n <- 150
x_media <- 628
s <- 30
intervalo_de_confianza <- 0.95

#Con un intervalo de confianza del 95% el valor de Z sera igual 
#a 1.96

nuevo_interval_de_confianza <- 1.96

# Cálculo del intervalo de confianza
margen_ <- nuevo_interval_de_confianza * (s / sqrt(n))
limite_inferior <- x_media - margen_
limite_superior <- x_media + margen_

# Resultado
nivel_de_confianza <- c(limite_inferior, limite_superior)
nivel_de_confianza

#cuarta pregunta
# Datos de tiempo para fallar del componente
data <- c(13.5518731, 0.3752651, 5.3344134, 1.7478818, 3.8793485, 4.1063320,
          4.0648515, 3.0796885, 0.3805058, 4.5867865, 1.7191893, 5.3809083,
          1.3784754, 4.8355531, 1.3360657, 4.7943754, 4.7052784, 9.8394008,
          6.3776400, 3.8306637)

# Intervalo de confianza del 90%
confidence_level <- 0.90
n <- length(data)
mean_data <- mean(data)
sd_data <- sd(data)

# Cálculo del intervalo de confianza utilizando la distribución t de Student
interval <- t.test(data, conf.level = confidence_level)$conf.int

# Resultados del intervalo de confianza
lower <- interval[1]
upper <- interval[2]

#Mostrar
cat("Intervalo de confianza del", confidence_level * 100, "%:\n")
cat("Límite inferior:", lower, "\n")
cat("Límite superior:", upper, "\n\n")
