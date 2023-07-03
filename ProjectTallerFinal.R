
#Primera Pregunta

prob_disconforme <- pnorm(245, mean = 250, sd = 2, lower.tail = FALSE) +
  pnorm(255, mean = 250, sd = 2, lower.tail = TRUE)

prob_disconforme

# Cálculo del valor
valor_peso_muy_alto <- qnorm(0.002, mean = 250, sd = 2)

# Resultado
valor_peso_muy_alto

prob_max_2_disconformes <- sum(dbinom(0:2, size = 10, prob = prob_disconforme))

# Resultado
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



x <- 248
mu <- 250
sd <- 2
pnorm(x,mean=mu,sd)
plot(density(rnorm(5000,mean=mu,sd)), main="Distribución normal")

