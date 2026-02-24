# ==============================================================
# Cleaner: 
# Selecciona las variables, relevantes, agrega filtros, arregla variables categoricas,
# y hace una tabla que relaciona el tipo de variable y los NAs

# --------------------------------------------------------------
# 1) Selección de variables necesarias
# --------------------------------------------------------------

base <- GEIH2018 %>%
  select(age, ocu, y_total_m, sex, totalHoursWorked, maxEducLevel, oficio,estrato1, relab,
          sizeFirm, regSalud, chunk, p6050, p6426)

# --------------------------------------------------------------
# 2) Filtros
# --------------------------------------------------------------

base <- base %>% filter(age>18, ocu ==1) 

# --------------------------------------------------------------
# 3) Variables creadas
# --------------------------------------------------------------

base <- base %>% mutate (
  logw=log(y_total_m),
  agecua = age^2,
  female = 1 - sex
)

# --------------------------------------------------------------
# 4.1) Correción categoricas 
# --------------------------------------------------------------

# VARIABLE RELAB NIVELES 6 Y 7
#En la exploración tuvimos problemas con los niveles 6 y 7 de la variable categorica Relab,
#Al revisar con más detalle estas observaciones tienen NA sistemático en la variable de ingreso
prueba <- base %>%
  select(y_total_m,  relab, female)
prueba <- prueba %>% filter(relab == 6 | relab == 7)
nrow(prueba)
sum(is.na(prueba))
#luego revisamos bien el diccionario de variables y estos dos niveles corresponden a trabajo no remunerado
#particularmente, 6 =Trabajador familiar sin remuneración, 7=Trabajador sin remuneración en empresas o negocios
#de otros hogares, por tal motivo decidimos eliminar estos dos niveles, pues no hay una variable dependiente que estimar 
#o con la cual podamos comparar una predicción.

base <- base %>%
  filter(!relab %in% c(6,7)) #Eliminandolas 

#Haciendolas categoricas
base <- base %>%
  mutate(
    maxEducLevel = as.factor(maxEducLevel),
    p6050        = as.factor(p6050),
    oficio       = as.factor(oficio),
    estrato1     = as.factor(estrato1),
    relab        = as.factor(relab),
    regSalud     = as.factor(regSalud),
    sizeFirm     = as.factor(sizeFirm)
  )



# --------------------------------------------------------------
# 4.2) Tabla con NAs y tipo de variable
# --------------------------------------------------------------

summary_table <- tibble(
  variable   = names(base),
  type       = sapply(base, function(x) class(x)[1]),
  n_missing  = sapply(base, function(x) sum(is.na(x))),
  p_missing  = sapply(base, function(x) mean(is.na(x))),
  n_unique   = sapply(base, function(x) length(unique(x)))
) %>%
  arrange(desc(p_missing))

cat("\nResumen completo de variables:\n")
print(summary_table)

# ==============================================================
# 5) DESCRIPTIVAS (TABLAS Y GRAFICAS)
# Este bloque se agrega DESPUES del cleaner original (sin alterarlo)
# ==============================================================

# --------------------------------------------------------------
# 5.1) Base auxiliar para descriptivas (solo agrega etiquetas)
# --------------------------------------------------------------
base_desc <- base %>%
  mutate(
    genero = case_when(
      sex == 1 ~ "Hombre",
      sex == 0 ~ "Mujer",
      TRUE     ~ "Sin dato"
    ),
    rango_edad = cut(
      age,
      breaks = c(18, 25, 35, 45, 55, 65, Inf),
      labels = c("19-25", "26-35", "36-45", "46-55", "56-65", "66+"),
      right = TRUE,
      include.lowest = FALSE
    )
  )

# --------------------------------------------------------------
# 5.2) Tabla descriptiva general (muestra e ingreso)
# --------------------------------------------------------------
tabla_desc_general <- base_desc %>%
  summarise(
    n_obs                 = n(),
    n_ingreso_no_na       = sum(!is.na(y_total_m)),
    n_ingreso_na          = sum(is.na(y_total_m)),
    edad_media            = mean(age, na.rm = TRUE),
    edad_mediana          = median(age, na.rm = TRUE),
    edad_sd               = sd(age, na.rm = TRUE),
    ingreso_media         = mean(y_total_m, na.rm = TRUE),
    ingreso_mediana       = median(y_total_m, na.rm = TRUE),
    ingreso_sd            = sd(y_total_m, na.rm = TRUE),
    ingreso_p25           = quantile(y_total_m, 0.25, na.rm = TRUE),
    ingreso_p75           = quantile(y_total_m, 0.75, na.rm = TRUE),
    ingreso_p90           = quantile(y_total_m, 0.90, na.rm = TRUE),
    horas_media           = mean(totalHoursWorked, na.rm = TRUE),
    horas_mediana         = median(totalHoursWorked, na.rm = TRUE)
  )

cat("\n================ TABLA DESCRIPTIVA GENERAL ================\n")
print(tabla_desc_general)

# --------------------------------------------------------------
# 5.3) Distribución de relab (frecuencia y porcentaje)
# --------------------------------------------------------------
tabla_relab_dist <- base_desc %>%
  count(relab, name = "n") %>%
  mutate(
    porcentaje = 100 * n / sum(n)
  ) %>%
  arrange(desc(n))

cat("\n================ DISTRIBUCION DE RELAB ====================\n")
print(tabla_relab_dist)

# --------------------------------------------------------------
# 5.4) Salarios por relab (datos crudos y_total_m)
# --------------------------------------------------------------
tabla_salario_relab <- base_desc %>%
  group_by(relab) %>%
  summarise(
    n_ingresos       = sum(!is.na(y_total_m)),
    ingreso_media    = mean(y_total_m, na.rm = TRUE),
    ingreso_mediana  = median(y_total_m, na.rm = TRUE),
    ingreso_sd       = sd(y_total_m, na.rm = TRUE),
    ingreso_p25      = quantile(y_total_m, 0.25, na.rm = TRUE),
    ingreso_p75      = quantile(y_total_m, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(relab)

cat("\n================ SALARIOS POR RELAB =======================\n")
print(tabla_salario_relab)

# --------------------------------------------------------------
# 5.5) Rangos de edad (frecuencia + porcentaje)
# --------------------------------------------------------------
tabla_rangos_edad <- base_desc %>%
  count(rango_edad, name = "n") %>%
  mutate(
    porcentaje = 100 * n / sum(n)
  ) %>%
  arrange(rango_edad)

cat("\n================ RANGOS DE EDAD ===========================\n")
print(tabla_rangos_edad)

# --------------------------------------------------------------
# 5.6) Ingreso por genero (tabla resumen)
# --------------------------------------------------------------
tabla_ingreso_genero <- base_desc %>%
  group_by(genero) %>%
  summarise(
    n_obs            = n(),
    n_ingresos       = sum(!is.na(y_total_m)),
    ingreso_media    = mean(y_total_m, na.rm = TRUE),
    ingreso_mediana  = median(y_total_m, na.rm = TRUE),
    ingreso_sd       = sd(y_total_m, na.rm = TRUE),
    ingreso_p25      = quantile(y_total_m, 0.25, na.rm = TRUE),
    ingreso_p75      = quantile(y_total_m, 0.75, na.rm = TRUE),
    edad_media       = mean(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(genero)

cat("\n================ INGRESO POR GENERO =======================\n")
print(tabla_ingreso_genero)

# --------------------------------------------------------------
# 5.7) (Opcional) Redondear tablas para reporte
# --------------------------------------------------------------
tabla_desc_general_r <- tabla_desc_general %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tabla_relab_dist_r <- tabla_relab_dist %>%
  mutate(porcentaje = round(porcentaje, 2))

tabla_salario_relab_r <- tabla_salario_relab %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tabla_rangos_edad_r <- tabla_rangos_edad %>%
  mutate(porcentaje = round(porcentaje, 2))

tabla_ingreso_genero_r <- tabla_ingreso_genero %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

cat("\n================ TABLAS REDONDEADAS (REPORTE) =============\n")
print(tabla_relab_dist_r)
print(tabla_salario_relab_r)
print(tabla_rangos_edad_r)
print(tabla_ingreso_genero_r)

# ==============================================================
# 6) GRAFICAS DESCRIPTIVAS
# ==============================================================

# Para mejorar lectura de boxplots sin eliminar datos, se usa un tope visual (p99)
tope_visual <- quantile(base_desc$y_total_m, 0.99, na.rm = TRUE)

# --------------------------------------------------------------
# 6.1) Histograma de ingresos (datos crudos)
# --------------------------------------------------------------
g_ingreso_hist <- base_desc %>%
  filter(!is.na(y_total_m), y_total_m >= 0) %>%
  ggplot(aes(x = y_total_m)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribución del ingreso laboral mensual (y_total_m)",
    x = "Ingreso laboral mensual",
    y = "Frecuencia"
  ) +
  theme_minimal()

print(g_ingreso_hist)

# --------------------------------------------------------------
# 6.2) Histograma de ingresos en escala log10 (solo visual)
# Útil porque los ingresos suelen estar muy sesgados a la derecha.
# --------------------------------------------------------------
g_ingreso_hist_log <- base_desc %>%
  filter(!is.na(y_total_m), y_total_m > 0) %>%
  ggplot(aes(x = y_total_m)) +
  geom_histogram(bins = 50) +
  scale_x_log10(labels = scales::label_comma()) +
  labs(
    title = "Distribución del ingreso (escala log10 en eje X)",
    x = "Ingreso laboral mensual (escala log10)",
    y = "Frecuencia"
  ) +
  theme_minimal()

print(g_ingreso_hist_log)

# --------------------------------------------------------------
# 6.3) Boxplot de ingreso por relab (datos crudos, con tope visual)
# --------------------------------------------------------------
g_ingreso_relab <- base_desc %>%
  filter(!is.na(y_total_m), !is.na(relab), y_total_m >= 0) %>%
  ggplot(aes(x = relab, y = y_total_m)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, tope_visual)) +
  labs(
    title = "Ingreso laboral por relab (boxplot, tope visual p99)",
    x = "relab",
    y = "Ingreso laboral mensual"
  ) +
  theme_minimal()

print(g_ingreso_relab)

# --------------------------------------------------------------
# 6.4) Barras de rangos de edad
# --------------------------------------------------------------
g_rangos_edad <- tabla_rangos_edad %>%
  ggplot(aes(x = rango_edad, y = n)) +
  geom_col() +
  labs(
    title = "Distribución por rangos de edad",
    x = "Rango de edad",
    y = "Frecuencia"
  ) +
  theme_minimal()

print(g_rangos_edad)

# --------------------------------------------------------------
# 6.5) Boxplot de ingreso por genero (datos crudos, con tope visual)
# --------------------------------------------------------------
g_ingreso_genero <- base_desc %>%
  filter(!is.na(y_total_m), genero %in% c("Hombre", "Mujer"), y_total_m >= 0) %>%
  ggplot(aes(x = genero, y = y_total_m)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, tope_visual)) +
  labs(
    title = "Ingreso laboral por género (boxplot, tope visual p99)",
    x = "Género",
    y = "Ingreso laboral mensual"
  ) +
  theme_minimal()

print(g_ingreso_genero)

# --------------------------------------------------------------
# 6.6) Barras de ingreso promedio y mediano por genero
# (muy útil para mostrar diferencia sin depender solo del boxplot)
# --------------------------------------------------------------
tabla_genero_long <- tabla_ingreso_genero %>%
  filter(genero %in% c("Hombre", "Mujer")) %>%
  select(genero, ingreso_media, ingreso_mediana) %>%
  pivot_longer(
    cols = c(ingreso_media, ingreso_mediana),
    names_to = "estadistico",
    values_to = "valor"
  )

g_genero_barras <- tabla_genero_long %>%
  ggplot(aes(x = genero, y = valor, fill = estadistico)) +
  geom_col(position = "dodge") +
  labs(
    title = "Ingreso promedio y mediano por género",
    x = "Género",
    y = "Ingreso laboral mensual",
    fill = "Estadístico"
  ) +
  theme_minimal()

print(g_genero_barras)

# ==============================================================
# 7) GUARDAR TABLAS EN TEXTO (.txt) EN 02_output/tables/
# ==============================================================

# Función auxiliar: convierte tabla/data frame a texto y la guarda
save_table_txt <- function(tabla, file_path, titulo = NULL) {
  txt <- c()
  
  if (!is.null(titulo)) {
    txt <- c(txt, titulo, strrep("=", nchar(titulo)), "")
  }
  
  # capture.output(print(...)) convierte la impresión de R en texto
  txt <- c(txt, capture.output(print(tabla)))
  
  writeLines(txt, con = file_path)
}

# Guardar tablas descriptivas (redondeadas para reporte)
save_table_txt(
  tabla_desc_general_r,
  "02_output/tables/tabla_desc_general.txt",
  "TABLA DESCRIPTIVA GENERAL"
)

save_table_txt(
  tabla_relab_dist_r,
  "02_output/tables/tabla_relab_distribucion.txt",
  "DISTRIBUCION DE RELAB"
)

save_table_txt(
  tabla_salario_relab_r,
  "02_output/tables/tabla_salario_por_relab.txt",
  "SALARIOS POR RELAB"
)

save_table_txt(
  tabla_rangos_edad_r,
  "02_output/tables/tabla_rangos_edad.txt",
  "RANGOS DE EDAD"
)

save_table_txt(
  tabla_ingreso_genero_r,
  "02_output/tables/tabla_ingreso_por_genero.txt",
  "INGRESO POR GENERO"
)

cat("\nTablas guardadas en: 02_output/tables/\n")

