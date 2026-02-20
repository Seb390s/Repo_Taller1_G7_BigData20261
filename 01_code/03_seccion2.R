# ==============================================================
# SECCIÓN 2: Brecha salarial (Hombres vs Mujeres) - Mínimos
# ==============================================================

# --------------------------------------------------------------
# 0) Definir muestra común para comparar Modelo 3 vs Modelo 4 y FWL
#    (NO modifica base; solo define un índice lógico)
# --------------------------------------------------------------
controls_formula <- ~ maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked

fwl_vars <- c("logw", "sex", "maxEducLevel", "relab", "sizeFirm", "regSalud", "totalHoursWorked")
idx_gap <- stats::complete.cases(base[, fwl_vars])

# --------------------------------------------------------------
# 1) MODELO 3: Incondicional (misma muestra idx_gap para comparabilidad)
# --------------------------------------------------------------
modelo3 <- lm(logw ~ sex, data = base, subset = idx_gap, na.action = stats::na.fail)

# --------------------------------------------------------------
# 2) MODELO 4: Condicional (controles) - SIN oficio y SIN estrato1, CON relab
# --------------------------------------------------------------
modelo4 <- lm(
  logw ~ sex + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base,
  subset = idx_gap,
  na.action = stats::na.fail
)

stargazer::stargazer(
  modelo3, modelo4, type = "text",
  title = "Brecha salarial por sexo: Modelo 3 (incondicional) vs Modelo 4 (condicional)"
)

# --------------------------------------------------------------
# 2.1) FWL explícito para el coeficiente de sex en el condicional
#      (usa la misma muestra idx_gap para evitar longitudes distintas)
# --------------------------------------------------------------
m_y_m4 <- lm(update(controls_formula, logw ~ .), data = base, subset = idx_gap, na.action = stats::na.fail)
m_d_m4 <- lm(update(controls_formula, sex  ~ .), data = base, subset = idx_gap, na.action = stats::na.fail)

y_tilde_m4 <- resid(m_y_m4)
d_tilde_m4 <- resid(m_d_m4)

modelo4_fwl <- lm(y_tilde_m4 ~ d_tilde_m4)

cat("\n=== Verificación FWL (coeficiente de sex) ===\n")
cat("coef sex (modelo4 completo): ", coef(modelo4)["sex"], "\n", sep = "")
cat("coef sex (FWL explícito):    ", coef(modelo4_fwl)["d_tilde_m4"], "\n", sep = "")
cat("SE sex (modelo4 completo):   ", summary(modelo4)$coefficients["sex", "Std. Error"], "\n", sep = "")
cat("SE sex (FWL explícito):      ", summary(modelo4_fwl)$coefficients["d_tilde_m4", "Std. Error"], "\n", sep = "")

# --------------------------------------------------------------
# 3) Requisito (1): Tabla con t in-sample + SE analítico y bootstrap + fit in-sample
#     Condicional: beta(sex) por FWL explícito
#     Bootstrap estilo clase: subset=indices + muestra común dentro del resample
# --------------------------------------------------------------
boot_fn_m3 <- function(data, indices) {
  d <- data[indices, ]
  idx <- stats::complete.cases(d[, fwl_vars])
  m <- lm(logw ~ sex, data = d, subset = idx)
  unname(coef(m)["sex"])
}

boot_fn_m4_fwl <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  idx <- stats::complete.cases(d[, fwl_vars])
  
  # Si el resample queda muy pequeño, devuelve NA (para que sd(..., na.rm=TRUE) funcione)
  if (sum(idx) < 10) return(NA_real_)
  
  # Clave: fijar el entorno de la fórmula al entorno local de la función
  f_y <- update(controls_formula, logw ~ .)
  environment(f_y) <- environment()
  
  f_d <- update(controls_formula, sex ~ .)
  environment(f_d) <- environment()
  
  m_y <- lm(f_y, data = d, subset = idx)
  m_d <- lm(f_d, data = d, subset = idx)
  
  y_tilde <- resid(m_y)
  d_tilde <- resid(m_d)
  
  m_fwl <- lm(y_tilde ~ d_tilde)
  unname(coef(m_fwl)["d_tilde"])
}

set.seed(1013)
boot_m3 <- boot::boot(data = base, statistic = boot_fn_m3,     R = 1000)
boot_m4 <- boot::boot(data = base, statistic = boot_fn_m4_fwl, R = 1000)

sum_m3 <- summary(modelo3)$coefficients
sum_m4_fwl <- summary(modelo4_fwl)$coefficients

fit_stats <- function(m) {
  s <- summary(m)
  c(
    n      = nobs(m),
    r2     = s$r.squared,
    adj_r2 = s$adj.r.squared,
    rmse   = sqrt(mean(resid(m)^2, na.rm = TRUE))
  )
}

fs3 <- fit_stats(modelo3)
fs4 <- fit_stats(modelo4)

tabla_gap <- data.frame(
  especificacion = c("Modelo 3 (incond.)", "Modelo 4 (cond., FWL)"),
  beta_sex       = c(coef(modelo3)["sex"], coef(modelo4_fwl)["d_tilde_m4"]),
  se_analitico   = c(sum_m3["sex","Std. Error"], sum_m4_fwl["d_tilde_m4","Std. Error"]),
  t_in_sample    = c(sum_m3["sex","t value"],   sum_m4_fwl["d_tilde_m4","t value"]),
  se_bootstrap   = c(sd(boot_m3$t, na.rm = TRUE), sd(boot_m4$t, na.rm = TRUE)),
  n              = c(fs3["n"], fs4["n"]),
  r2             = c(fs3["r2"], fs4["r2"]),
  adj_r2         = c(fs3["adj_r2"], fs4["adj_r2"]),
  rmse           = c(fs3["rmse"], fs4["rmse"])
)

cat("\n=== Tabla requerida (beta, t in-sample, SE analítico y bootstrap + fit in-sample) ===\n")
print(tabla_gap)

tabla_gap$gap_pct <- 100 * (exp(tabla_gap$beta_sex) - 1)
cat("\n=== Misma tabla con brecha en % (exp(beta)-1) ===\n")
print(tabla_gap)

# --------------------------------------------------------------
# 4) Requisito (2) y (3): Especificación preferida para perfiles y picos
#     SIN oficio y SIN estrato1, CON relab
# --------------------------------------------------------------
pref_vars <- c(fwl_vars, "age", "agecua")
idx_pref <- stats::complete.cases(base[, pref_vars])

modelo_pref <- lm(
  logw ~ sex + age + agecua + sex:age + sex:agecua +
    maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base,
  subset = idx_pref,
  na.action = stats::na.fail
)

# --------------------------------------------------------------
# 5) Visualización: perfiles edad–ingreso predicho por sexo
#     Controles fijos: totalHoursWorked (media), categóricos (moda)
# --------------------------------------------------------------
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

typical_like_base <- function(val, base_col) {
  if (is.factor(base_col)) return(factor(as.character(val), levels = levels(base_col)))
  if (is.character(base_col)) return(as.character(val))
  if (is.logical(base_col)) return(as.logical(val))
  val
}

age_min <- floor(min(base$age, na.rm = TRUE))
age_max <- floor(max(base$age, na.rm = TRUE))

grid <- data.frame(age = seq(age_min, age_max, by = 1))
grid$agecua <- grid$age^2

grid$totalHoursWorked <- mean(base$totalHoursWorked, na.rm = TRUE)

grid$maxEducLevel <- typical_like_base(mode_value(base$maxEducLevel), base$maxEducLevel)
grid$relab        <- typical_like_base(mode_value(base$relab),        base$relab)
grid$sizeFirm     <- typical_like_base(mode_value(base$sizeFirm),     base$sizeFirm)
grid$regSalud     <- typical_like_base(mode_value(base$regSalud),     base$regSalud)

grid0 <- grid; grid0$sex <- 0
grid1 <- grid; grid1$sex <- 1

grid0$yhat <- predict(modelo_pref, newdata = grid0)
grid1$yhat <- predict(modelo_pref, newdata = grid1)

plot_df <- rbind(
  transform(grid0, grupo = "sex=0"),
  transform(grid1, grupo = "sex=1")
)

ggplot2::ggplot(plot_df, ggplot2::aes(x = age, y = yhat, color = grupo)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(
    title = "Perfiles predichos Edad–Ingreso por sexo (especificación preferida)",
    x = "Edad",
    y = "log(ingreso) predicho"
  ) +
  ggplot2::theme_minimal()

# --------------------------------------------------------------
# 6) Requisito (3): Edad pico por grupo + IC bootstrap
# --------------------------------------------------------------
peak_from_model <- function(m) {
  b <- coef(m)
  need <- c("age", "agecua", "sex:age", "sex:agecua")
  if (!all(need %in% names(b))) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  b_age  <- b["age"]
  b_age2 <- b["agecua"]
  b_i1   <- b["sex:age"]
  b_i2   <- b["sex:agecua"]
  
  denom0 <- 2 * b_age2
  denom1 <- 2 * (b_age2 + b_i2)
  
  peak_0 <- if (is.finite(denom0) && denom0 != 0) -b_age / denom0 else NA_real_
  peak_1 <- if (is.finite(denom1) && denom1 != 0) -(b_age + b_i1) / denom1 else NA_real_
  
  c(peak_sex0 = peak_0, peak_sex1 = peak_1)
}

peaks_point <- peak_from_model(modelo_pref)

cat("\n=== Edad pico (punto estimado) ===\n")
print(peaks_point)

boot_fn_peaks <- function(data, indices) {
  d <- data[indices, ]
  idx <- stats::complete.cases(d[, pref_vars])
  
  m <- lm(
    logw ~ sex + age + agecua + sex:age + sex:agecua +
      maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
    data = d,
    subset = idx
  )
  
  pk <- peak_from_model(m)
  pk[!is.finite(pk)] <- NA_real_
  pk
}

set.seed(1013)
boot_peaks <- boot::boot(data = base, statistic = boot_fn_peaks, R = 1000)

if (is.null(dim(boot_peaks$t))) {
  boot_peaks$t <- matrix(boot_peaks$t, ncol = 2, byrow = TRUE)
}
colnames(boot_peaks$t) <- c("peak_sex0", "peak_sex1")

ci_peak0 <- quantile(boot_peaks$t[, "peak_sex0"], probs = c(0.025, 0.975), na.rm = TRUE)
ci_peak1 <- quantile(boot_peaks$t[, "peak_sex1"], probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap (percentil 2.5%–97.5%) para edad pico ===\n")
cat("sex=0: ", ci_peak0[1], " a ", ci_peak0[2], "\n", sep = "")
cat("sex=1: ", ci_peak1[1], " a ", ci_peak1[2], "\n", sep = "")

diff_peaks <- boot_peaks$t[, "peak_sex1"] - boot_peaks$t[, "peak_sex0"]
ci_diff <- quantile(diff_peaks, probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap para diferencia de picos (sex=1 - sex=0) ===\n")
cat("diff: ", ci_diff[1], " a ", ci_diff[2], "\n", sep = "")