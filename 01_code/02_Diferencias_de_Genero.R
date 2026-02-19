# ==============================================================
# SECCIÓN 2: Brecha salarial (Hombres vs Mujeres)
# ==============================================================

df_difsal <- base

# --------------------------------------------------------------
# 1) Variable de género (female)
#    Importante: en tu caso 'sex' toma valores 0/1.
#    Este bloque crea female y verifica que queden ambos grupos.
# --------------------------------------------------------------
df_difsal <- df_difsal %>%
  dplyr::mutate(
    # Si sex es 0/1, tomamos female = 1 cuando sex == 1.
    # (Si luego ves que las etiquetas están al revés, se cambia a sex == 0.)
    female = dplyr::case_when(
      sex %in% c(0, 1) ~ as.integer(sex == 1),
      sex %in% c(1, 2) ~ as.integer(sex == 2),  # por si viene codificado 1/2
      TRUE ~ NA_integer_
    )
  )

cat("\nChequeo rápido de sex vs female:\n")
print(table(df_difsal$sex,    useNA = "ifany"))
print(table(df_difsal$female, useNA = "ifany"))

# --------------------------------------------------------------
# 2) Variable dependiente: log salario/ingreso
#    - Usamos log(y_total_m) como en tu sección 1
#    - log() requiere valores > 0; los <=0 se ponen como NA
# --------------------------------------------------------------
df_difsal <- df_difsal %>%
  dplyr::mutate(
    logw  = dplyr::if_else(y_total_m > 0, log(y_total_m), NA_real_),
    age2  = age^2
  )

head(df_difsal$y_total_m)

# --------------------------------------------------------------
# 3) CONTROLES (W) PARA EL MODELO CONDICIONAL (modelo4)
#    La idea es comparar salarios entre hombres/mujeres manteniendo constantes
#    características "job-relevant" (Mincer-like): educación, experiencia (proxy),
#    región y dummies ocupacionales/sectoriales. :contentReference[oaicite:2]{index=2}
#
#    En GEIH (tu base) usamos como controles:
#      - age y age^2: proxy flexible de experiencia potencial
#      - maxEducLevel: educación (categorías)
#      - oficio: ocupación
#      - estrato1: proxy socioeconómico / contexto (y a veces correlaciona con región)
#      - relab: relación laboral / tipo de empleo
#      - sizeFirm: tamaño de firma (estructura del empleo)
#      - regSalud: régimen de salud (proxy de formalidad/condición laboral)
#      - totalHoursWorked (si existe): intensidad laboral (si estás en wage mensual)
# --------------------------------------------------------------
cat_vars <- c("maxEducLevel", "oficio", "estrato1", "relab", "sizeFirm", "regSalud")
cat_vars <- cat_vars[cat_vars %in% names(df_difsal)]

df_difsal <- df_difsal %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cat_vars), as.factor))

has_hours <- "totalHoursWorked" %in% names(df_difsal)

# --------------------------------------------------------------
# 4) MODELO 3: Brecha INCONDICIONAL
#    logw ~ female
# --------------------------------------------------------------
modelo3 <- lm(logw ~ female, data = df_difsal)
print(summary(modelo3))

# % gap incondicional (aprox): 100*(exp(beta_female)-1)
gap_incond_pct <- 100 * (exp(coef(modelo3)["female"]) - 1)
cat("\nGap incondicional (%):", gap_incond_pct, "\n")

# --------------------------------------------------------------
# 5) MODELO 4: Brecha CONDICIONAL (con controles W)
# --------------------------------------------------------------
if (has_hours) {
  modelo4 <- lm(
    logw ~ female + age + age2 + totalHoursWorked +
      maxEducLevel + oficio + estrato1 + relab + sizeFirm + regSalud,
    data = df_difsal
  )
} else {
  modelo4 <- lm(
    logw ~ female + age + age2 +
      maxEducLevel + oficio + estrato1 + relab + sizeFirm + regSalud,
    data = df_difsal
  )
}

print(summary(modelo4))

gap_cond_pct <- 100 * (exp(coef(modelo4)["female"]) - 1)
cat("\nGap condicional (%):", gap_cond_pct, "\n")

# --------------------------------------------------------------
# 6) (Solo una figura simple de diagnóstico) Boxplot por género
#    y guardado en 02_output/figures
# --------------------------------------------------------------
if (!dir.exists("02_output/figures")) dir.create("02_output/figures", recursive = TRUE)

p_box <- ggplot2::ggplot(df_difsal, ggplot2::aes(x = as.factor(female), y = logw)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    title = "Log ingreso (y_total_m) por género",
    x = "female (0 = referencia, 1 = otro grupo)",
    y = "log(y_total_m)"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = "02_output/figures/s2_box_logw_by_gender.png",
  plot = p_box,
  width = 7, height = 4, dpi = 300
)

