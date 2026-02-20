# ==============================================================
# SECCIÓN 2: Brecha salarial (Hombres vs Mujeres) - Mínimos
# Usa: base
# Mantiene: modelo3 (incond) y modelo4 (cond)
# Agrega: modelo_pref (condicional preferido para perfiles por edad y picos)

# --------------------------------------------------------------
# 1) MODELO 3: Incondicional
# --------------------------------------------------------------
modelo3 <- lm(logw ~ sex, data = base)

# --------------------------------------------------------------
# 2) MODELO 4: Condicional (controles)
# --------------------------------------------------------------

modelo4 <- lm(
  logw ~ sex + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base
)

# Tabla “clásica” (SE analíticos)
stargazer(modelo3, modelo4, type = "text",
          title = "Brecha salarial por sexo: Modelo 3 (incondicional) vs Modelo 4 (condicional)")

# --------------------------------------------------------------
# 3) Requisito (1): Tabla con t in-sample + SE analítico y bootstrap
#     (bootstrap SE = sd de los betas bootstrap)
# --------------------------------------------------------------
boot_fn_m3 <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(logw ~ sex, data = d)
  return(coef(m)["sex"])
}

boot_fn_m4 <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(logw ~ sex + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked, data = d)
  return(coef(m)["sex"])
}

set.seed(1013)
boot_m3 <- boot(data = base, statistic = boot_fn_m3, R = 1000)
boot_m4 <- boot(data = base, statistic = boot_fn_m4, R = 1000)

sum_m3 <- summary(modelo3)$coefficients
sum_m4 <- summary(modelo4)$coefficients

tabla_gap <- data.frame(
  especificacion = c("Modelo 3 (incond.)", "Modelo 4 (cond.)"),
  beta_sex       = c(coef(modelo3)["sex"], coef(modelo4)["sex"]),
  se_analitico   = c(sum_m3["sex","Std. Error"], sum_m4["sex","Std. Error"]),
  t_in_sample    = c(sum_m3["sex","t value"],   sum_m4["sex","t value"]),
  se_bootstrap   = c(sd(boot_m3$t, na.rm = TRUE), sd(boot_m4$t, na.rm = TRUE))
)

cat("\n=== Tabla requerida (beta, t in-sample, SE analítico y bootstrap) ===\n")
print(tabla_gap)

# (Opcional útil) brecha en % desde log-points
tabla_gap$gap_pct <- 100 * (exp(tabla_gap$beta_sex) - 1)
cat("\n=== Misma tabla con brecha en % (exp(beta)-1) ===\n")
print(tabla_gap)

# --------------------------------------------------------------
# 4) Requisito (2) y (3): Especificación condicional preferida
#     Necesita age y age^2, y para picos distintos por grupo:
#     interacciones sex:age y sex:age^2
# --------------------------------------------------------------
modelo_pref <- lm(
  logw ~ sex + age + agecua + sex:age + sex:agecua +
    maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base
)

# --------------------------------------------------------------
# 5) Visualización: perfiles edad–ingreso predicho por sexo
#     Fijamos controles a valores "típicos":
#       - totalHoursWorked: media
#       - controles categóricos: moda (valor más frecuente)
# --------------------------------------------------------------
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

age_min <- floor(min(base$age, na.rm = TRUE))
age_max <- floor(max(base$age, na.rm = TRUE))

grid <- data.frame(age = seq(age_min, age_max, by = 1))
grid$agecua <- grid$age^2

grid$totalHoursWorked <- mean(base$totalHoursWorked, na.rm = TRUE)

grid$maxEducLevel <- mode_value(base$maxEducLevel)
grid$relab        <- mode_value(base$relab)
grid$sizeFirm     <- mode_value(base$sizeFirm)
grid$regSalud     <- mode_value(base$regSalud)

grid0 <- grid; grid0$sex <- 0
grid1 <- grid; grid1$sex <- 1

grid0$yhat <- predict(modelo_pref, newdata = grid0)
grid1$yhat <- predict(modelo_pref, newdata = grid1)

plot_df <- rbind(
  transform(grid0, grupo = "sex=0"),
  transform(grid1, grupo = "sex=1")
)

ggplot(plot_df, aes(x = age, y = yhat, color = grupo)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Perfiles predichos Edad–Ingreso por sexo (especificación preferida)",
    x = "Edad",
    y = "log(ingreso) predicho"
  ) +
  theme_minimal()

# --------------------------------------------------------------
# 6) Requisito (3): Edad pico por grupo + IC bootstrap
#     Pico(sex=0): -b_age / (2*b_agecua)
#     Pico(sex=1): -(b_age + b_sex:age) / (2*(b_agecua + b_sex:agecua))
# --------------------------------------------------------------
peak_from_model <- function(m) {
  b <- coef(m)
  
  b_age  <- b["age"]
  b_age2 <- b["agecua"]
  b_i1   <- b["sex:age"]
  b_i2   <- b["sex:agecua"]
  
  peak_0 <- -b_age / (2 * b_age2)
  peak_1 <- -(b_age + b_i1) / (2 * (b_age2 + b_i2))
  
  c(peak_sex0 = peak_0, peak_sex1 = peak_1)
}

peaks_point <- peak_from_model(modelo_pref)

cat("\n=== Edad pico (punto estimado) ===\n")
print(peaks_point)

boot_fn_peaks <- function(data, indices) {
  d <- data[indices, ]
  
  m <- lm(
    logw ~ sex + age + agecua + sex:age + sex:agecua +
      maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
    data = d
  )
  
  pk <- peak_from_model(m)
  pk[!is.finite(pk)] <- NA_real_
  return(pk)
}

set.seed(1013)
boot_peaks <- boot(data = base, statistic = boot_fn_peaks, R = 1000)

colnames(boot_peaks$t) <- c("peak_sex0", "peak_sex1")

ci_peak0 <- quantile(boot_peaks$t[, "peak_sex0"], probs = c(0.025, 0.975), na.rm = TRUE)
ci_peak1 <- quantile(boot_peaks$t[, "peak_sex1"], probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap (percentil 2.5%–97.5%) para edad pico ===\n")
cat("sex=0: ", ci_peak0[1], " a ", ci_peak0[2], "\n", sep = "")
cat("sex=1: ", ci_peak1[1], " a ", ci_peak1[2], "\n", sep = "")