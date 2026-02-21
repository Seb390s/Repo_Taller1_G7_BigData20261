# SECCIÓN 1: Perfilamiento edad-ingreso laboral
# Requisitos cubiertos:
# (1) Limpieza de datos, con el data frame de base

# =============================================================================
#Modelo simple age y age al cuadrado
# -----------------------------------------------------------------------------
modelo1 <- lm(
  logw ~ age + agecua,
  data = base)

stargazer(modelo1, type = "text")

b1m1 <- coef(modelo1)["age"]
b2m1 <- coef(modelo1)["agecua"]

age_pick_m1 <- -b1m1 / (2*b2m1)
age_pick_m1

base$yhatm1 <- predict(modelo1, newdata = base)

# Este dato solo nos sirve como referencia del máximo, para tener la curva real 
# se procede con el bootstrap

ggplot(base,aes(x = age, y = yhatm1)) +
  geom_line()

ggplot(base, aes(x = age)) +
  geom_line(aes(y = yhatm1), color = "blue", linewidth = 1) +
  labs(
    title = "Perfil Edad–Ingreso",
    x = "Edad",
    y = "Log ingreso"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# En esta sección se procede a realizar el bootstrap, con una muestra de m=1000,
# y de esta manera poder estimar los valores de la edad pico

# Función bootstrap modelo 1

boot_fn_m1 <- function(data, indices) {
  d <- data[indices, ] 
  model <- lm(logw ~ age + agecua, data = d)
  b1 <- coef(model)["age"]
  b2 <- coef(model)["agecua"]
  return(-b1 / (2*b2))
}
set.seed(1013)
boot_m1 <- boot(
  data = base,
  statistic = boot_fn_m1,
  R = 1000
)
#------------------------------------------------------------------------------
# Intervalo percentil
# Se procede a calcular de confianza del 95%, de las realizaciones producidas por 
# el bootstrap
ci_m1 <- boot.ci(boot_m1, type = "norm")

ci_m1 

# Se procede a realizar el elaborar la  gráfica de la distribución muestral del
# bootstrap

boot_df <- data.frame(
  pico = c(boot_m1$t),
  modelo = rep(c("Modelo 1"),
               each = length(boot_m1$t))
)
ggplot(boot_df, aes(x = pico, fill = modelo)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = age_pick_m1, color = "blue", linetype = "dashed") +
  labs(
  title = "Distribución Bootstrap de la Edad Pico",
  x = "Edad pico",
  y = "Densidad"
  ) +
  theme_minimal()


# =============================================================================
#Perfil age-labor condicional
# =============================================================================

#Modelo conidiconal con totalhoursworked y employmenttype
modelo2 <- lm(
  logw ~ age + agecua + totalHoursWorked + relab,
  data = base
)
modelo2
stargazer(modelo2, type = "text")

# Se verifican la edad pico, sin embargo este valor no puede ser tenido en cuenta,
# Ya que es una relación lineal

b1m2 <- coef(modelo2)["age"] 
b2m2 <- coef(modelo2)["agecua"] 
age_pick_m2 <- -b1m2 / (2*b2m2)
age_pick_m2

# 

base$yhatm2 <- predict(modelo2, newdata = base)
#------------------------------------------------------------------------------
# bootstrap para la función dependiente, por medio de 1000 realizaciones:
#------------------------------------------------------------------------------

boot_fn_m2 <- function(data, indices) {
d <- data[indices, ]
model <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = d)
b1 <- coef(model)["age"]
b2 <- coef(model)["agecua"]
return(-b1 / (2*b2))
}

set.seed(1013)
# Se genera la lista de 1000 con los datos de las 1000 realizaciones 
boot_m2 <- boot(
  data = base,
  statistic = boot_fn_m2,
  R = 1000
)

# Se calculan los intervalos de confianza.

ci_m2 <- boot.ci(boot_m2, type = "perc")
ci_m2

boot_df <- data.frame(
  pico = c(boot_m2$t),
  modelo = rep(c("Modelo 2"),
               each = length(boot_m1$t))
)

# Se procede a graficar la distribución para así tener la distribución muestral
# del boostrap. 

boot_df <- data.frame(
  pico = c(boot_m2$t),
  modelo = rep(c("Modelo 2"),
               each = length(boot_m1$t))
)

ggplot(boot_df, aes(x = pico, fill = modelo)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = age_pick_m2, color = "red", linetype = "dashed") +
  labs(
    title = "Distribución Bootstrap de la Edad Pico",
    x = "Edad pico",
    y = "Densidad"
  ) +
  theme_minimal()




