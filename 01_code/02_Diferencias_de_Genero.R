# ==============================================================
# 02_gender_gap.R - Sección 2 (Diferencias salariales por género)
# Taller 1 BDML
#
# Supuestos:
# - GEIH2018 (o base) ya existe en el entorno (viene de webscraping + cleaner)
# - Se guardan outputs en 02_output/figures y 02_output/tables (relativo al getwd)
# ==============================================================

suppressPackageStartupMessages(library(tidyverse))

# --------------------------------------------------------------
# 0) Helpers: selección robusta de nombres en datasets scrapeados
# --------------------------------------------------------------

# Devuelve el primer nombre que exista en el df desde un conjunto de candidatos.
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

# Crea carpetas de output si no existen (figuras/tablas)
ensure_dirs <- function() {
  dir.create("02_output/figures", recursive = TRUE, showWarnings = FALSE)
  dir.create("02_output/tables",  recursive = TRUE, showWarnings = FALSE)
}
ensure_dirs()

# --------------------------------------------------------------
# 1) Base de análisis: mayores de edad y ocupados
#    (si 'base' ya existe, lo usamos tal cual; si no, lo construimos)
# --------------------------------------------------------------

if (exists("base")) {
  df0 <- as_tibble(base)
} else if (exists("GEIH2018")) {
  df0 <- as_tibble(GEIH2018) %>%
    filter(age >= 18, ocu == 1)
} else {
  stop("No existe 'base' ni 'GEIH2018'. Ejecuta primero webscraping/cleaner.")
}

# --------------------------------------------------------------
# 2) Identificación de variables clave (ingreso y género/sexo)
#    (se deja robusto porque el scraping a veces cambia nombres)
# --------------------------------------------------------------

income_var <- pick_first_existing(df0, c("y_total_m", "ingtot", "y_salary_m"))
if (is.na(income_var)) {
  stop("No encontré una variable de ingreso (y_total_m / ingtot / y_salary_m). Ajusta candidates.")
}

sex_var <- pick_first_existing(df0, c("female", "Female", "sex", "Sexo"))
if (is.na(sex_var)) {
  stop("No encontré variable de sexo/género (female/sex). Ajusta candidates.")
}

# --------------------------------------------------------------
# 3) Construcción de variables de trabajo
#    - w: ingreso (numérico)
#    - female: 1 si mujer, 0 si hombre (definición robusta)
#    - age: edad (numérico)
# --------------------------------------------------------------

df <- df0 %>%
  mutate(
    w = as.numeric(.data[[income_var]]),
    
    # Regla típica en encuestas: sex=1 hombre, sex=2 mujer.
    # Si ya viene una dummy "female", se respeta.
    female = case_when(
      sex_var %in% c("female", "Female") ~ as.integer(as.numeric(.data[[sex_var]]) == 1),
      TRUE ~ as.integer(as.numeric(.data[[sex_var]]) == 2)
    ),
    
    age = as.numeric(age)
  )

# --------------------------------------------------------------
# 4) Dataset para regresiones con log(w)
#    - Para log estricto se requiere w > 0
#    - Se filtra también por finitud y NAs en variables esenciales
# --------------------------------------------------------------

df_reg_difsal <- df %>%
  mutate(logw = log(w)) %>%
  filter(
    is.finite(logw),
    !is.na(female),
    !is.na(age)
  )

# --------------------------------------------------------------
# 5) Sección 2.1: Brecha incondicional
#    log(w) ~ female
#    - beta_female en log-puntos
#    - interpretación %: 100*(exp(beta)-1)
# --------------------------------------------------------------

m_uncond <- lm(logw ~ female, data = df_reg_difsal)

uncond_beta <- coef(m_uncond)[["female"]]
uncond_se   <- summary(m_uncond)$coefficients["female", "Std. Error"]
uncond_pct  <- 100 * (exp(uncond_beta) - 1)

# --------------------------------------------------------------
# 6) Sección 2.2: Brecha condicional por FWL
#    - Residualizar logw y female contra controles
#    - Regresar resid(logw) ~ resid(female)
#    - Reportar SE analítico y bootstrap
# --------------------------------------------------------------

# Controles sugeridos (se usan solo si existen en el df)
control_candidates <- c("totalHoursWorked", "totalhoursworked", "relab", "maxEducLevel", "maxeduclevel", "estrato1")
controls <- intersect(control_candidates, names(df_reg_difsal))

if (length(controls) == 0) {
  stop("No hay controles detectados. Ajusta control_candidates según tu base.")
}

# Implementación FWL (devuelve beta y SE analítico)
fwl_estimate <- function(data) {
  f_y <- as.formula(paste0("logw ~ ", paste(controls, collapse = " + ")))
  f_f <- as.formula(paste0("female ~ ", paste(controls, collapse = " + ")))
  
  r_y <- resid(lm(f_y, data = data))
  r_f <- resid(lm(f_f, data = data))
  
  m <- lm(r_y ~ r_f)
  
  list(
    beta = coef(m)[["r_f"]],
    se   = summary(m)$coefficients["r_f", "Std. Error"]
  )
}

cond_res  <- fwl_estimate(df_reg_difsal)
cond_beta <- cond_res$beta
cond_se   <- cond_res$se
cond_pct  <- 100 * (exp(cond_beta) - 1)

# Bootstrap del beta (FWL) para SE empírico
set.seed(123)
B <- 500
boot_betas <- numeric(B)

for (b in 1:B) {
  idx <- sample.int(nrow(df_reg_difsal), size = nrow(df_reg_difsal), replace = TRUE)
  db  <- df_reg_difsal[idx, , drop = FALSE]
  boot_betas[b] <- fwl_estimate(db)$beta
}

cond_boot_se <- sd(boot_betas, na.rm = TRUE)

# --------------------------------------------------------------
# 7) Tabla resumen para slides (brecha incondicional vs condicional)
# --------------------------------------------------------------

results_tbl <- tibble(
  model = c(
    "Incondicional: log(w) ~ Female",
    "Condicional (FWL): log(w) ~ Female | controles"
  ),
  beta_female    = c(uncond_beta, cond_beta),
  se_analytic    = c(uncond_se,  cond_se),
  se_bootstrap   = c(NA_real_,   cond_boot_se),
  approx_pct     = c(uncond_pct, cond_pct),
  n              = c(nobs(m_uncond), nrow(df_reg_difsal))
)

write.csv(results_tbl, "02_output/tables/section2_gap_summary.csv", row.names = FALSE)

# --------------------------------------------------------------
# 8) Perfil edad–ingreso por género (especificación "preferida")
#    - Permite curvas distintas por género:
#      log(w) ~ female + age + age^2 + female*age + female*age^2 + controles
#    - Se grafica el log(ingreso) predicho por edad para hombres vs mujeres
# --------------------------------------------------------------

controls_no_age <- setdiff(controls, c("age", "I(age^2)"))

form_profile <- as.formula(
  paste0(
    "logw ~ female + age + I(age^2) + female:age + female:I(age^2) + ",
    paste(controls_no_age, collapse = " + ")
  )
)

m_profile <- lm(form_profile, data = df_reg_difsal)

age_min <- floor(min(df_reg_difsal$age, na.rm = TRUE))
age_max <- floor(max(df_reg_difsal$age, na.rm = TRUE))
age_grid <- tibble(age = seq(age_min, age_max, by = 1))

# Fijar controles en valores típicos (media numérica / moda categórica)
typical_value <- function(x) {
  if (is.numeric(x)) return(mean(x, na.rm = TRUE))
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA)
  as.character(names(sort(table(ux), decreasing = TRUE))[1])
}

control_grid <- map_dfc(controls_no_age, ~{
  v <- df_reg_difsal[[.x]]
  tibble(!!.x := typical_value(v))
})

new_male <- bind_cols(age_grid, control_grid) %>% mutate(female = 0)
new_fem  <- bind_cols(age_grid, control_grid) %>% mutate(female = 1)

pred_male <- predict(m_profile, newdata = new_male, se.fit = TRUE)
pred_fem  <- predict(m_profile, newdata = new_fem,  se.fit = TRUE)

profile_df <- bind_rows(
  tibble(age = age_grid$age, female = 0, fit = pred_male$fit, se = pred_male$se.fit),
  tibble(age = age_grid$age, female = 1, fit = pred_fem$fit,  se = pred_fem$se.fit)
) %>%
  mutate(
    group = if_else(female == 1, "Mujeres", "Hombres"),
    lo = fit - 1.96 * se,
    hi = fit + 1.96 * se
  )

p_profile <- ggplot(profile_df, aes(x = age, y = fit, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = group), alpha = 0.15, color = NA) +
  labs(
    title = "Perfil edad–ingreso (log) por género",
    x = "Edad",
    y = "log(ingreso laboral)"
  ) +
  theme_minimal()

ggsave(
  "02_output/figures/section2_age_income_profile.png",
  p_profile, width = 9, height = 5, dpi = 300
)

# --------------------------------------------------------------
# 9) Edad pico por género + IC por bootstrap
#    - Se deriva del máximo de la función cuadrática en edad
#    - Se bootstrappea el modelo para intervalos empíricos
# --------------------------------------------------------------

peak_age_from_model <- function(mod) {
  b <- coef(mod)
  
  # Hombre: b_age + 2*b_age2*age = 0  => age* = -b_age/(2*b_age2)
  b1_m <- b[["age"]]
  b2_m <- b[["I(age^2)"]]
  peak_m <- -b1_m / (2 * b2_m)
  
  # Mujer: (b_age + b_f:age) + 2*(b_age2 + b_f:age2)*age = 0
  b1_f <- b1_m + b[["female:age"]]
  b2_f <- b2_m + b[["female:I(age^2)"]]
  peak_f <- -b1_f / (2 * b2_f)
  
  c(peak_m = peak_m, peak_f = peak_f)
}

set.seed(123)
B2 <- 500
boot_peaks <- matrix(NA_real_, nrow = B2, ncol = 2)
colnames(boot_peaks) <- c("peak_m", "peak_f")

for (b in 1:B2) {
  idx <- sample.int(nrow(df_reg_difsal), size = nrow(df_reg_difsal), replace = TRUE)
  db  <- df_reg_difsal[idx, , drop = FALSE]
  
  mod_b <- tryCatch(lm(form_profile, data = db), error = function(e) NULL)
  if (!is.null(mod_b)) boot_peaks[b, ] <- peak_age_from_model(mod_b)
}

peaks_hat <- peak_age_from_model(m_profile)

peaks_tbl <- tibble(
  group = c("Hombres", "Mujeres"),
  peak_age = c(peaks_hat["peak_m"], peaks_hat["peak_f"]),
  ci_low = c(
    quantile(boot_peaks[, "peak_m"], 0.025, na.rm = TRUE),
    quantile(boot_peaks[, "peak_f"], 0.025, na.rm = TRUE)
  ),
  ci_high = c(
    quantile(boot_peaks[, "peak_m"], 0.975, na.rm = TRUE),
    quantile(boot_peaks[, "peak_f"], 0.975, na.rm = TRUE)
  )
)

write.csv(peaks_tbl, "02_output/tables/section2_peak_ages.csv", row.names = FALSE)

cat("\nListo ✅ Tablas en 02_output/tables y figura en 02_output/figures\n")
print(results_tbl)
print(peaks_tbl)
