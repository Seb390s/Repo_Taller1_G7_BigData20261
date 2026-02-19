#punto1

base <- base %>% mutate (
  logw=log(y_total_m),
  agecua = age^2
)

#Modelo simple age y age al cuadrado
modelo1 <- lm(
  logw ~ age + agecua,
  data = base)

stargazer(modelo1, type = "text")

b1m1 <- coef(modelo1)["age"]
b2m1 <- coef(modelo1)["agecua"]

age_pick_m1 <- -b1m1 / (2*b2m1)
age_pick_m1

base$yhatm1 <- predict(modelo1, newdata = base)

# ===============================================

#Modelo con total hours worked y employment type
modelo2 <- lm(
  logw ~ age + agecua + totalHoursWorked + relab,
  data = base
)
stargazer(modelo2, type = "text")

b1m2 <- coef(modelo2)["age"]
b2m2 <- coef(modelo2)["agecua"]

age_pick_m2 <- -b1m2 / (2*b2m2)
age_pick_m2

#Parámetros
coefs <- coef(modelo2)
mean_hours <- mean(base$totalHoursWorked, na.rm = TRUE)
mean_relab <- mean(base$relab, na.rm = TRUE)

base$yhatm2 <- 
  coefs["(Intercept)"] +
  coefs["age"] * base$age +
  coefs["agecua"] * base$agecua +
  coefs["totalHoursWorked"] * mean_hours +
  coefs["relab"] * mean_relab

ggplot(base, aes(x = age, y = yhatm2)) +
  geom_line()

ggplot(base, aes(x = age)) +
  geom_line(aes(y = yhatm1), color = "blue", linewidth = 1) +
  geom_line(aes(y = yhatm2), color = "red", linewidth = 1) +
  labs(
    title = "Perfil Edad–Ingreso",
    x = "Edad",
    y = "Log ingreso"
  ) +
  theme_minimal()



# ====

library(boot)

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

# Intervalo percentil
ci_m1 <- boot.ci(boot_m1, type = "perc")

ci_m1




# 
boot_fn_m2 <- function(data, indices) {
d <- data[indices, ]
model <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = d)
b1 <- coef(model)["age"]
b2 <- coef(model)["agecua"]
return(-b1 / (2*b2))
}

set.seed(1013)

boot_m2 <- boot(
  data = base,
  statistic = boot_fn_m2,
  R = 1000
)

ci_m2 <- boot.ci(boot_m2, type = "perc")

ci_m2


boot_df <- data.frame(
  pico = c(boot_m1$t, boot_m2$t),
  modelo = rep(c("Modelo 1", "Modelo 2"),
               each = length(boot_m1$t))
)


library(ggplot2)

ggplot(boot_df, aes(x = pico, fill = modelo)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = age_pick_m1, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = age_pick_m2, color = "red", linetype = "dashed") +
  labs(
    title = "Distribución Bootstrap de la Edad Pico",
    x = "Edad pico",
    y = "Densidad"
  ) +
  theme_minimal()

