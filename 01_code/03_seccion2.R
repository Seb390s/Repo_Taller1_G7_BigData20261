# ==============================================================
# SECCIÓN 2: Brecha salarial (Hombres vs Mujeres) - Mínimos (CORREGIDO)
# Objetivo general:
# - Estimar la brecha de ingreso por sexo sin controles y con controles.
# - Comparar ambos resultados en una misma muestra para que sean comparables.
# - Validar el coeficiente condicional con FWL (misma estimación vista desde otra descomposición).
# - Reportar incertidumbre por dos vías (analítica y bootstrap).
# - Pasar de una brecha promedio a perfiles por edad (trayectorias) y edades pico con rangos.
# ==============================================================

# --------------------------------------------------------------
# 1) Definir muestra común para comparar Modelo 3 vs Modelo 4 y FWL
#    - Usar la misma muestra evita que la brecha cambie solo por NAs o por selección de filas.
#    - age y age^2 permiten una forma cóncava del perfil edad–ingreso (crece y luego cae).
# --------------------------------------------------------------
controls_formula <- ~ age + agecua + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked

# Lista mínima de variables necesarias para estimar ambos modelos en la misma muestra
# (dependiente, sexo y controles)
fwl_vars <- c("logw", "sex", "age", "agecua",
              "maxEducLevel", "relab", "sizeFirm", "regSalud", "totalHoursWorked")

# Indicador lógico: TRUE si la fila tiene datos completos en todas las variables usadas
idx_gap <- stats::complete.cases(base[, fwl_vars])

# --------------------------------------------------------------
# 2) MODELO 3: Incondicional
#    - Estima la brecha "cruda": solo compara promedios por sexo en logw.
#    - subset = idx_gap fuerza a usar la misma muestra que el modelo condicional.
# --------------------------------------------------------------
modelo3 <- lm(logw ~ sex, data = base, subset = idx_gap, na.action = stats::na.fail)

# --------------------------------------------------------------
# 3) MODELO 4: Condicional
#    - Estima la brecha "ajustada": compara por sexo manteniendo constantes los controles.
#    - Incluye edad y edad^2 para capturar la forma del ciclo de vida del ingreso.
# --------------------------------------------------------------
modelo4 <- lm(
  logw ~ sex + age + agecua + maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
  data = base,
  subset = idx_gap,
  na.action = stats::na.fail
)

# Exporta tabla LaTeX de los dos modelos (salida "presentable" para el informe/slides)
stargazer::stargazer(
  modelo3, modelo4,
  type  = "latex",
  title = "Brecha salarial por sexo: Modelo 3 (incondicional) vs Modelo 4 (condicional)",
  out   = "02_output/tables/tabla_brecha_m3_m4.tex"
)

# --------------------------------------------------------------
# 3.1) FWL explícito para el coeficiente de sex en el condicional
#    Lógica:
#    - Primero "quitamos" el efecto de los controles en logw (y_tilde).
#    - Luego "quitamos" el efecto de los controles en sex (d_tilde).
#    - Finalmente, regresamos y_tilde sobre d_tilde.
#    Interpretación:
#    - El coeficiente obtenido debe coincidir con el coeficiente de sex en modelo4
#      si todo (muestra, controles y especificación) está alineado correctamente.
# --------------------------------------------------------------
m_y_m4 <- lm(update(controls_formula, logw ~ .), data = base, subset = idx_gap, na.action = stats::na.fail)
m_d_m4 <- lm(update(controls_formula, sex  ~ .), data = base, subset = idx_gap, na.action = stats::na.fail)

# Residuos: parte de logw/sex que NO queda explicada por los controles
y_tilde_m4 <- resid(m_y_m4)
d_tilde_m4 <- resid(m_d_m4)

# Regresión final del paso FWL: coeficiente asociado a la parte "limpia" de sex
modelo4_fwl <- lm(y_tilde_m4 ~ d_tilde_m4)

# Verificación rápida:
# - El coeficiente de sex del modelo completo y el obtenido por FWL deben coincidir.
# - Los SE impresos pueden diferir si se usan grados de libertad distintos (se ajusta más adelante).
cat("\n=== Verificación FWL (coeficiente de sex) ===\n")
cat("coef sex (modelo4 completo): ", coef(modelo4)["sex"], "\n", sep = "")
cat("coef sex (FWL explícito):    ", coef(modelo4_fwl)["d_tilde_m4"], "\n", sep = "")
cat("SE sex (modelo4 completo):   ", summary(modelo4)$coefficients["sex", "Std. Error"], "\n", sep = "")
cat("SE sex (FWL explícito):      ", summary(modelo4_fwl)$coefficients["d_tilde_m4", "Std. Error"], "\n", sep = "")

# --------------------------------------------------------------
# 4) Bootstrap SE para beta(sex)
#    Lógica:
#    - Re-muestreamos filas con reemplazo dentro de la muestra fija (base_gap).
#    - En cada remuestreo re-estimamos el coeficiente de sex.
#    - La desviación estándar de esos coeficientes es el SE bootstrap.
#    Detalle práctico:
#    - Si en un remuestreo sex no varía, no se puede identificar el efecto y devolvemos NA.
# --------------------------------------------------------------
base_gap <- base[idx_gap, , drop = FALSE]

boot_fn_m3 <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  # Sin variación en sex no hay comparación posible (coeficiente no identificable)
  if (stats::var(d$sex) == 0) return(NA_real_)
  
  # tryCatch evita que un remuestreo "raro" detenga el proceso completo
  m <- tryCatch(
    lm(logw ~ sex, data = d),
    error = function(e) NULL
  )
  if (is.null(m)) return(NA_real_)
  
  unname(coef(m)["sex"])
}

boot_fn_m4_fwl <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  # Misma lógica: sin variación en sex, el modelo no puede estimar el efecto
  if (stats::var(d$sex) == 0) return(NA_real_)
  
  # Fórmulas para residualizar dentro de cada remuestreo
  f_y <- update(controls_formula, logw ~ .)
  f_d <- update(controls_formula, sex  ~ .)
  environment(f_y) <- environment()
  environment(f_d) <- environment()
  
  # Paso 1: residualizar logw y sex por controles dentro del remuestreo
  m_y <- tryCatch(lm(f_y, data = d), error = function(e) NULL)
  m_d <- tryCatch(lm(f_d, data = d), error = function(e) NULL)
  if (is.null(m_y) || is.null(m_d)) return(NA_real_)
  
  y_tilde <- resid(m_y)
  d_tilde <- resid(m_d)
  
  # Paso 2: regresión final (FWL) dentro del remuestreo
  m_fwl <- tryCatch(lm(y_tilde ~ d_tilde), error = function(e) NULL)
  if (is.null(m_fwl)) return(NA_real_)
  
  # Pendiente asociada a d_tilde (segunda posición: intercepto + pendiente)
  unname(coef(m_fwl)[2])
}

set.seed(1013)
boot_m3 <- boot::boot(data = base_gap, statistic = boot_fn_m3,     R = 1000)
boot_m4 <- boot::boot(data = base_gap, statistic = boot_fn_m4_fwl, R = 1000)

# --------------------------------------------------------------
# 5) Tabla resumen: beta, t, SE analítico y bootstrap + fit
#    Lógica:
#    - Modelo 3: todo se toma directo de la regresión.
#    - Modelo 4: beta se toma del FWL explícito (misma estimación que el modelo completo).
#    - Para SE/t del FWL usamos la varianza residual del modelo completo (sigma^2),
#      de modo que el cálculo sea consistente con la especificación con controles.
# --------------------------------------------------------------
sum_m3 <- summary(modelo3)$coefficients

# Coeficiente condicional por FWL (debe coincidir con coef(modelo4)["sex"])
beta_fwl <- unname(coef(modelo4_fwl)["d_tilde_m4"])

# Varianza residual del modelo completo condicional: resume el "ruido" restante luego de controles
sigma2_full <- summary(modelo4)$sigma^2

# SE analítico para el beta FWL usando la variación de d_tilde:
# - Si d_tilde tiene poca variación, el SE crece; si tiene mucha, el SE baja.
se_fwl_correct <- sqrt(sigma2_full / sum(d_tilde_m4^2))

# Estadístico t asociado al beta condicional
t_fwl_correct <- beta_fwl / se_fwl_correct

# Check numérico:
# - Compara SE y t calculados con los reportados por el modelo completo.
# - Si coinciden (hasta tolerancia numérica), el armado es consistente.
cat("\n=== Check SE/t FWL vs modelo4 completo (deberían coincidir) ===\n")
cat("SE sex (modelo4 completo): ", summary(modelo4)$coefficients["sex", "Std. Error"], "\n", sep = "")
cat("SE sex (FWL correcto):     ", se_fwl_correct, "\n", sep = "")
cat("t  sex (modelo4 completo): ", summary(modelo4)$coefficients["sex", "t value"], "\n", sep = "")
cat("t  sex (FWL correcto):     ", t_fwl_correct, "\n", sep = "")

# Métricas de ajuste en la muestra: útiles para comparar capacidad explicativa entre modelos
fit_stats <- function(m) {
  s <- summary(m)
  c(
    n      = nobs(m),
    r2     = s$r.squared,
    adj_r2 = s$adj.r.squared,
    rmse   = sqrt(mean(resid(m)^2, na.rm = TRUE))
  )
}

# Fit del modelo incondicional vs condicional (con controles)
fs3 <- fit_stats(modelo3)
fs4 <- fit_stats(modelo4)  # En condicional se reporta fit del modelo completo, no del FWL

# SE bootstrap: dispersión de los coeficientes remuestreados
se_boot_m3 <- stats::sd(boot_m3$t, na.rm = TRUE)
se_boot_m4 <- stats::sd(boot_m4$t, na.rm = TRUE)

# Tabla final: junta estimación, inferencia y fit en un solo objeto para exportar/mostrar
tabla_gap <- data.frame(
  especificacion = c("Modelo 3 (incond.)", "Modelo 4 (cond., FWL)"),
  beta_sex       = c(unname(coef(modelo3)["sex"]), beta_fwl),
  se_analitico   = c(sum_m3["sex","Std. Error"],   se_fwl_correct),
  t_in_sample    = c(sum_m3["sex","t value"],      t_fwl_correct),
  se_bootstrap   = c(se_boot_m3,                   se_boot_m4),
  n              = c(fs3["n"],                     fs4["n"]),
  r2             = c(fs3["r2"],                    fs4["r2"]),
  adj_r2         = c(fs3["adj_r2"],                fs4["adj_r2"]),
  rmse           = c(fs3["rmse"],                  fs4["rmse"])
)

cat("\n=== Tabla requerida (beta, t in-sample, SE analítico y bootstrap + fit in-sample) ===\n")
print(tabla_gap)

# Conversión a porcentaje: interpretación multiplicativa en niveles del ingreso
tabla_gap$gap_pct <- 100 * (exp(tabla_gap$beta_sex) - 1)
cat("\n=== Misma tabla con brecha en % (exp(beta)-1) ===\n")
print(tabla_gap)

# --------------------------------------------------------------
# 6) Especificación preferida para perfiles y picos
#    - La brecha promedio no describe cómo cambia la diferencia con la edad.
#    - Las interacciones permiten que la forma del perfil edad–ingreso difiera por sexo.
# --------------------------------------------------------------
pref_vars <- c("logw", "sex", "age", "agecua",
               "maxEducLevel", "relab", "sizeFirm", "regSalud", "totalHoursWorked")

# Muestra (posiblemente distinta) para el modelo preferido: completa en variables del perfil
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
# 7) Visualización: perfiles predichos por sexo
#    Lógica:
#    - Se construye una "grilla" de edades para trazar una curva suave.
#    - Se fijan los controles en valores típicos para comparar por sexo "ceteris paribus".
# --------------------------------------------------------------
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Asegura que el valor típico respete el tipo/clase de la variable original (factor/character/etc.)
typical_like_base <- function(val, base_col) {
  if (is.factor(base_col)) return(factor(as.character(val), levels = levels(base_col)))
  if (is.character(base_col)) return(as.character(val))
  if (is.logical(base_col)) return(as.logical(val))
  val
}

# Rango de edad observado en la muestra preferida (evita extrapolar fuera del soporte de datos)
age_min <- floor(min(base_pref$age, na.rm = TRUE))
age_max <- floor(max(base_pref$age, na.rm = TRUE))

# Grilla de edades: una fila por edad
grid <- data.frame(age = seq(age_min, age_max, by = 1))
grid$agecua <- grid$age^2

# Control continuo fijado en la media
grid$totalHoursWorked <- mean(base_pref$totalHoursWorked, na.rm = TRUE)

# Controles categóricos fijados en su valor más frecuente (moda)
grid$maxEducLevel <- typical_like_base(mode_value(base_pref$maxEducLevel), base_pref$maxEducLevel)
grid$relab        <- typical_like_base(mode_value(base_pref$relab),        base_pref$relab)
grid$sizeFirm     <- typical_like_base(mode_value(base_pref$sizeFirm),     base_pref$sizeFirm)
grid$regSalud     <- typical_like_base(mode_value(base_pref$regSalud),     base_pref$regSalud)

# Dos datasets para predicción: uno por sexo
grid0 <- grid; grid0$sex <- 0
grid1 <- grid; grid1$sex <- 1

# Predicciones del modelo preferido por edad y sexo
grid0$yhat <- predict(modelo_pref, newdata = grid0)
grid1$yhat <- predict(modelo_pref, newdata = grid1)

# Data final para ggplot (una columna "grupo" para colorear por sexo)
plot_df <- rbind(
  transform(grid0, grupo = "Mujeres"),
  transform(grid1, grupo = "Hombres")
)

# Gráfico: dos perfiles predichos (comparación directa por sexo a lo largo de la edad)
ggplot2::ggplot(plot_df, ggplot2::aes(x = age, y = yhat, color = grupo)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(
    title = "Perfiles predichos Edad–Ingreso por sexo (especificación preferida)",
    x = "Edad",
    y = "log(ingreso) predicho"
  ) +
  ggplot2::theme_minimal()

# Exporta la figura para su uso en el reporte/presentación
ggplot2::ggsave(
  filename = "02_output/figures/perfiles_predichos_edad_ingreso_por_sexo.png",
  plot     = ggplot2::last_plot(),
  width    = 10, height = 6, units = "in",
  dpi      = 300
)

# --------------------------------------------------------------
# 8) Edad pico por grupo + rangos por bootstrap
#    Lógica:
#    - Si el perfil es cuadrático y cóncavo, existe un máximo interior (pico).
#    - Se calcula el pico para cada sexo usando los coeficientes relevantes.
#    - Se valida que el pico tenga sentido: concavidad y que caiga dentro del rango observado.
#    - Bootstrap: re-estima el modelo y recalcula el pico muchas veces para obtener rangos plausibles.
# --------------------------------------------------------------
peak_from_model <- function(m, age_min, age_max) {
  b <- coef(m)
  need <- c("age", "agecua", "sex:age", "sex:agecua")
  if (!all(need %in% names(b))) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  b_age  <- b["age"]
  b_age2 <- b["agecua"]
  b_i1   <- b["sex:age"]
  b_i2   <- b["sex:agecua"]
  
  # Curvaturas por sexo: deben ser negativas para que exista un máximo (forma "∩")
  curv0 <- b_age2
  curv1 <- b_age2 + b_i2
  
  # Pico por sexo (si hay concavidad)
  peak_0 <- if (is.finite(curv0) && curv0 < 0) -b_age / (2 * curv0) else NA_real_
  peak_1 <- if (is.finite(curv1) && curv1 < 0) -(b_age + b_i1) / (2 * curv1) else NA_real_
  
  # Evita reportar picos fuera del soporte de edad observado
  if (!is.na(peak_0) && (peak_0 < age_min || peak_0 > age_max)) peak_0 <- NA_real_
  if (!is.na(peak_1) && (peak_1 < age_min || peak_1 > age_max)) peak_1 <- NA_real_
  
  c(peak_sex0 = peak_0, peak_sex1 = peak_1)
}

# Pico puntual (estimado en la muestra)
peaks_point <- peak_from_model(modelo_pref, age_min, age_max)

cat("\n=== Edad pico (punto estimado; NA si no hay pico válido) ===\n")
print(peaks_point)

boot_fn_peaks <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  
  # Si sex no varía en el remuestreo, el modelo con interacciones puede quedar mal identificado
  if (stats::var(d$sex) == 0) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  # Re-estima el mismo modelo preferido en la muestra remuestreada
  m <- tryCatch(
    lm(
      logw ~ sex + age + agecua + sex:age + sex:agecua +
        maxEducLevel + relab + sizeFirm + regSalud + totalHoursWorked,
      data = d
    ),
    error = function(e) NULL
  )
  
  if (is.null(m)) return(c(peak_sex0 = NA_real_, peak_sex1 = NA_real_))
  
  # Recalcula picos dentro del rango de edad observado
  pk <- peak_from_model(m, age_min, age_max)
  pk[!is.finite(pk)] <- NA_real_
  pk
}

set.seed(1013)
boot_peaks <- boot::boot(data = base_pref, statistic = boot_fn_peaks, R = 1000)

# Asegura que boot_peaks$t sea una matriz con dos columnas (pico por sexo)
if (is.null(dim(boot_peaks$t))) {
  boot_peaks$t <- matrix(boot_peaks$t, ncol = 2, byrow = TRUE)
}
colnames(boot_peaks$t) <- c("peak_sex0", "peak_sex1")

# Rangos percentil (2.5% y 97.5%) por sexo
ci_peak0 <- stats::quantile(boot_peaks$t[, "peak_sex0"], probs = c(0.025, 0.975), na.rm = TRUE)
ci_peak1 <- stats::quantile(boot_peaks$t[, "peak_sex1"], probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap (percentil 2.5%–97.5%) para edad pico (NA si no hay pico válido) ===\n")
cat("sex=0: ", ci_peak0[1], " a ", ci_peak0[2], "\n", sep = "")
cat("sex=1: ", ci_peak1[1], " a ", ci_peak1[2], "\n", sep = "")

# Diferencia de picos: útil para evaluar si los máximos ocurren a edades distintas
diff_peaks <- boot_peaks$t[, "peak_sex1"] - boot_peaks$t[, "peak_sex0"]
ci_diff <- stats::quantile(diff_peaks, probs = c(0.025, 0.975), na.rm = TRUE)

cat("\n=== IC bootstrap para diferencia de picos (sex=1 - sex=0) ===\n")
cat("diff: ", ci_diff[1], " a ", ci_diff[2], "\n", sep = "")