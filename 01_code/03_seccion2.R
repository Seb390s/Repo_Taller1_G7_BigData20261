# ==============================================================
# SECCIÓN 2: Brecha salarial (Hombres vs Mujeres) - Mínimos (CORREGIDO)
# Requisitos cubiertos:
# (1) Tabla: incond vs cond (FWL) + t in-sample + SE analítico y bootstrap + fit
# (2) Perfiles edad–ingreso predicho por sexo (especificación preferida)
# (3) Edades pico por sexo + IC bootstrap (percentil) + diferencia de picos
# ==============================================================

# --------------------------------------------------------------
# 1) Definir muestra común para comparar Modelo 3 vs Modelo 4 y FWL
#    Incluye edad y edad^2 para alinear con “age–income profile” (Sección 1)
# --------------------------------------------------------------
controls_formula <- ~ age + agecua + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked

fwl_vars <- c("logw", "sex", "age", "agecua",
              "maxEducLevel", "relab", "sizeFirm", "regSalud", "totalHoursWorked")

idx_gap <- stats::complete.cases(base[, fwl_vars])

# --------------------------------------------------------------
# 2) MODELO 3: Incondicional (misma muestra idx_gap para comparabilidad)
# --------------------------------------------------------------
modelo3 <- lm(logw ~ sex, data = base, subset = idx_gap, na.action = stats::na.fail)

# --------------------------------------------------------------
# 3) MODELO 4: Condicional (controles) + edad y edad^2
# --------------------------------------------------------------
modelo4 <- lm(
  logw ~ sex + age + agecua + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base,
  subset = idx_gap,
  na.action = stats::na.fail
)

stargazer::stargazer(
  modelo3, modelo4, type = "text",
  title = "Brecha salarial por sexo: Modelo 3 (incondicional) vs Modelo 4 (condicional)"
)

# --------------------------------------------------------------
# 3.1) FWL explícito para el coeficiente de sex en el condicional
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
# 4) Requisito (1): Bootstrap SE para beta(sex)
#     Bootstrap sobre muestra completa fija (idx_gap) para inferencia limpia.
# --------------------------------------------------------------
base_gap <- base[idx_gap, , drop = FALSE]

boot_fn_m3 <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  # Si en el resample no hay variación en sex, el coeficiente no es identificable
  if (stats::var(d$sex) == 0) return(NA_real_)
  
  m <- tryCatch(
    lm(logw ~ sex, data = d),
    error = function(e) NULL
  )
  if (is.null(m)) return(NA_real_)
  
  unname(coef(m)["sex"])
}

boot_fn_m4_fwl <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  if (stats::var(d$sex) == 0) return(NA_real_)
  
  f_y <- update(controls_formula, logw ~ .)
  f_d <- update(controls_formula, sex  ~ .)
  environment(f_y) <- environment()
  environment(f_d) <- environment()
  
  m_y <- tryCatch(lm(f_y, data = d), error = function(e) NULL)
  m_d <- tryCatch(lm(f_d, data = d), error = function(e) NULL)
  if (is.null(m_y) || is.null(m_d)) return(NA_real_)
  
  y_tilde <- resid(m_y)
  d_tilde <- resid(m_d)
  
  m_fwl <- tryCatch(lm(y_tilde ~ d_tilde), error = function(e) NULL)
  if (is.null(m_fwl)) return(NA_real_)
  
  # El coeficiente está en la segunda posición (intercepto + pendiente)
  unname(coef(m_fwl)[2])
}

set.seed(1013)
boot_m3 <- boot::boot(data = base_gap, statistic = boot_fn_m3,     R = 1000)
boot_m4 <- boot::boot(data = base_gap, statistic = boot_fn_m4_fwl, R = 1000)

# --------------------------------------------------------------
# 5) Tabla requerida: beta, t in-sample, SE analítico y bootstrap + fit in-sample
#    - Para el condicional, beta(sex) se toma del FWL explícito (modelo4_fwl)
#    - Fit se reporta del modelo completo (modelo4), no del FWL.
# --------------------------------------------------------------
sum_m3      <- summary(modelo3)$coefficients
sum_m4_fwl  <- summary(modelo4_fwl)$coefficients

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
  se_bootstrap   = c(stats::sd(boot_m3$t, na.rm = TRUE), stats::sd(boot_m4$t, na.rm = TRUE)),
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
# 6) Requisito (2) y (3): Especificación preferida para perfiles y picos
#     Incluye interacciones para permitir formas distintas por sexo.
# --------------------------------------------------------------
pref_vars <- c("logw", "sex", "age", "agecua",
               "maxEducLevel", "relab", "sizeFirm", "regSalud", "totalHoursWorked")

idx_pref <- stats::complete.cases(base[, pref_vars])
base_pref <- base[idx_pref, , drop = FALSE]

modelo_pref <- lm(
  logw ~ sex + age + agecua + sex:age + sex:agecua +
    maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base,
  subset = idx_pref,
  na.action = stats::na.fail
)

# --------------------------------------------------------------
# 7) Visualización: perfiles edad–ingreso predicho por sexo
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

age_min <- floor(min(base_pref$age, na.rm = TRUE))
age_max <- floor(max(base_pref$age, na.rm = TRUE))

grid <- data.frame(age = seq(age_min, age_max, by = 1))
grid$agecua <- grid$age^2

grid$totalHoursWorked <- mean(base_pref$totalHoursWorked, na.rm = TRUE)

grid$maxEducLevel <- typical_like_base(mode_value(base_pref$maxEducLevel), base_pref$maxEducLevel)
grid$relab        <- typical_like_base(mode_value(base_pref$relab),        base_pref$relab)
grid$sizeFirm     <- typical_like_base(mode_value(base_pref$sizeFirm),     base_pref$sizeFirm)
grid$regSalud     <- typical_like_base(mode_value(base_pref$regSalud),     base_pref$regSalud)

grid0 <- grid; grid0$sex <- 0
grid1 <- grid; grid1$sex <- 1

grid0$yhat <- predict(modelo_pref, newdata = grid0)
grid1$yhat <- predict(modelo_pref, newdata = grid1)

plot_df <- rbind(
  transform(grid0, grupo = "Hombres"),
  transform(grid1, grupo = "Mujeres")
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
# 8) Requisito (3): Edad pico por grupo + IC bootstrap
#     - Valida concavidad (coef cuadrático < 0) y rango [age_min, age_max]
# --------------------------------------------------------------
peak_from_model <- function(m, age_min, age_max) {
  b <- coef(m)
  need <- c("age", "agecua", "sex:age", "sex:agecua")
  if (!all(need %in% names(b))) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  b_age  <- b["age"]
  b_age2 <- b["agecua"]
  b_i1   <- b["sex:age"]
  b_i2   <- b["sex:agecua"]
  
  # Curvaturas: deben ser negativas para que exista un "pico"
  curv0 <- b_age2
  curv1 <- b_age2 + b_i2
  
  peak_0 <- if (is.finite(curv0) && curv0 < 0) -b_age / (2 * curv0) else NA_real_
  peak_1 <- if (is.finite(curv1) && curv1 < 0) -(b_age + b_i1) / (2 * curv1) else NA_real_
  
  # Debe caer dentro del rango de edades usado en el gráfico / muestra preferida
  if (!is.na(peak_0) && (peak_0 < age_min || peak_0 > age_max)) peak_0 <- NA_real_
  if (!is.na(peak_1) && (peak_1 < age_min || peak_1 > age_max)) peak_1 <- NA_real_
  
  c(peak_sex0 = peak_0, peak_sex1 = peak_1)
}

peaks_point <- peak_from_model(modelo_pref, age_min, age_max)

cat("\n=== Edad pico (punto estimado; NA si no hay pico válido) ===\n")
print(peaks_point)

boot_fn_peaks <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  # Si no hay variación en sex, el modelo con interacciones puede degenerar
  if (stats::var(d$sex) == 0) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  m <- tryCatch(
    lm(
      logw ~ sex + age + agecua + sex:age + sex:agecua +
        maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
      data = d
    ),
    error = function(e) NULL
  )
  
  if (is.null(m)) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  pk <- peak_from_model(m, age_min, age_max)
  pk[!is.finite(pk)] <- NA_real_
  pk
}

set.seed(1013)
boot_peaks <- boot::boot(data = base_pref, statistic = boot_fn_peaks, R = 1000)

# Asegura forma matricial y nombres de columnas
if (is.null(dim(boot_peaks$t))) {
  boot_peaks$t <- matrix(boot_peaks$t, ncol = 2, byrow = TRUE)
}
colnames(boot_peaks$t) <- c("peak_sex0", "peak_sex1")

ci_peak0 <- stats::quantile(boot_peaks$t[, "peak_sex0"], probs = c(0.025, 0.975), na.rm = TRUE)
ci_peak1 <- stats::quantile(boot_peaks$t[, "peak_sex1"], probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap (percentil 2.5%–97.5%) para edad pico (NA si no hay pico válido) ===\n")
cat("sex=0: ", ci_peak0[1], " a ", ci_peak0[2], "\n", sep = "")
cat("sex=1: ", ci_peak1[1], " a ", ci_peak1[2], "\n", sep = "")

diff_peaks <- boot_peaks$t[, "peak_sex1"] - boot_peaks$t[, "peak_sex0"]
ci_diff <- stats::quantile(diff_peaks, probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap para diferencia de picos (sex=1 - sex=0) ===\n")
cat("diff: ", ci_diff[1], " a ", ci_diff[2], "\n", sep = "")