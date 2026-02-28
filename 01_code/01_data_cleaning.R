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
# SECCIÓN 2: ESTADÍSTICAS DESCRIPTIVAS
# base nunca se modifica — todo trabaja sobre subconjuntos locales
# ==============================================================

# --------------------------------------------------------------
# 2.1) Tabla descriptiva
# --------------------------------------------------------------
tabla_desc <- base %>%
  summarise(
    `Salario (media)`                    = round(mean(y_total_m, na.rm = TRUE), 1),
    `Salario (sd)`                       = round(sd(y_total_m, na.rm = TRUE), 1),
    `Salario (p25)`                      = round(quantile(y_total_m, 0.25, na.rm = TRUE), 1),
    `Salario (p75)`                      = round(quantile(y_total_m, 0.75, na.rm = TRUE), 1),
    `Edad (media)`                           = round(mean(age, na.rm = TRUE), 1),
    `Edad (sd)`                              = round(sd(age, na.rm = TRUE), 1),
    `Horas trabajadas (media)`               = round(mean(totalHoursWorked, na.rm = TRUE), 1),
    `Horas trabajadas (sd)`                  = round(sd(totalHoursWorked, na.rm = TRUE), 1),
    `Antigüedad en el empleo, meses (media)` = round(mean(p6426, na.rm = TRUE), 1),
    `Antigüedad en el empleo, meses (sd)`    = round(sd(p6426, na.rm = TRUE), 1),
    `Proporción mujeres (%)`                 = round(100 * mean(female, na.rm = TRUE), 1),
    `Proporción salud contributiva (%)`      = round(100 * mean(regSalud == "1", na.rm = TRUE), 1),
    `N observaciones`                        = n()
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor")

print(xtable(tabla_desc,
             caption = "Estadísticas descriptivas de la muestra (GEIH Bogotá 2018)",
             label   = "tab:desc",
             digits  = c(0, 0, 1)),
      include.rownames = FALSE,
      booktabs = TRUE)

print(xtable(tabla_desc,
             caption = "Estadísticas descriptivas de la muestra (GEIH Bogotá 2018)",
             label   = "tab:desc",
             digits  = c(0, 0, 1)),
      include.rownames = FALSE,
      booktabs = TRUE,
      file = "02_output/tables/descriptivas.tex")

# --------------------------------------------------------------
# 2.2) Distribución del log salario
# --------------------------------------------------------------
ggplot(base, aes(x = logw)) +
  geom_histogram(aes(y = ..density..), bins = 50,
                 fill = "#2c5f8a", alpha = 0.7, color = NA) +
  geom_density(color = "red", linewidth = 0.8) +
  geom_vline(xintercept = mean(base$logw, na.rm = TRUE),
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  annotate("text",
           x = mean(base$logw, na.rm = TRUE) + 0.3,
           y = 0.45,
           label = paste0("Media = ", round(mean(base$logw, na.rm = TRUE), 2)),
           color = "darkred", size = 3.5, hjust = 0) +
  labs(title = "Distribución del log salario mensual",
       x = "Log salario mensual",
       y = "Densidad") +
  theme_minimal()

ggsave("02_output/figures/dist_logw.jpg", width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------
# 2.3) Composición por relación laboral
# --------------------------------------------------------------
etiquetas_relab <- c(
  "1" = "Obrero empresa particular",
  "2" = "Obrero gobierno",
  "3" = "Empleado doméstico",
  "4" = "Cuenta propia",
  "5" = "Patrón o empleador",
  "9" = "Otro"
)

base %>%
  filter(!is.na(relab), as.character(relab) %in% names(etiquetas_relab)) %>%
  mutate(relab_label = etiquetas_relab[as.character(relab)]) %>%
  count(relab_label) %>%
  mutate(
    porcentaje  = 100 * n / sum(n),
    relab_label = reorder(relab_label, porcentaje)
  ) %>%
  ggplot(aes(x = relab_label, y = porcentaje)) +
  geom_col(fill = "#2c5f8a", alpha = 0.8) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip(ylim = c(0, 65)) +
  labs(title = "Composición Relación Laboral",
       x = "",
       y = "Porcentaje (%)") +
  theme_minimal()

ggsave("02_output/figures/composicion_relab.jpg", width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------
# 2.4) Perfil salario-edad por género
# --------------------------------------------------------------
base %>%
  filter(!is.na(female), !is.na(age), !is.na(logw)) %>%
  mutate(genero = ifelse(female == 1, "Mujer", "Hombre")) %>%
  ggplot(aes(x = age, y = logw, color = genero)) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.2, alpha = 0.15) +
  scale_color_manual(values = c("Hombre" = "#2c5f8a", "Mujer" = "#c0392b")) +
  labs(title = "Perfil salario-edad por género",
       x = "Edad",
       y = "Log salario mensual",
       color = "") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("02_output/figures/perfil_edad_genero.jpg", width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------
# 2.5) Log salario por nivel educativo (boxplot)
# --------------------------------------------------------------
base %>%
  filter(!is.na(maxEducLevel), !as.character(maxEducLevel) %in% c("2", "9")) %>%
  mutate(educ_label = case_when(
    maxEducLevel == "1" ~ "Ninguno",
    maxEducLevel == "3" ~ "Primaria\nincompleta",
    maxEducLevel == "4" ~ "Primaria\ncompleta",
    maxEducLevel == "5" ~ "Secundaria\nincompleta",
    maxEducLevel == "6" ~ "Secundaria\ncompleta",
    maxEducLevel == "7" ~ "Terciaria"
  )) %>%
  mutate(educ_label = factor(educ_label, levels = c(
    "Ninguno", "Primaria\nincompleta", "Primaria\ncompleta",
    "Secundaria\nincompleta", "Secundaria\ncompleta", "Terciaria"
  ))) %>%
  ggplot(aes(x = educ_label, y = logw, fill = educ_label)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.15, outlier.size = 0.8) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log salario por nivel educativo",
       x = "",
       y = "Log salario mensual") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

ggsave("02_output/figures/logw_por_educ.jpg", width = 9, height = 5, dpi = 300)


# --------------------------------------------------------------
# 2.X) Parentesco con el jefe(a) por género (proporciones)
# --------------------------------------------------------------

base %>%
  filter(!is.na(p6050), !is.na(female)) %>%
  
  # Crear variable género
  mutate(genero = ifelse(female == 1, "Mujer", "Hombre")) %>%
  
  # Etiquetar p6050
  mutate(parentesco = case_when(
    p6050 == 1 ~ "Jefe(a)",
    p6050 == 2 ~ "Cónyuge",
    p6050 == 3 ~ "Hijo(a)",
    p6050 == 4 ~ "Nieto(a)",
    p6050 == 5 ~ "Otro pariente",
    p6050 == 6 ~ "Servicio doméstico",
    p6050 == 7 ~ "Pensionista",
    p6050 == 8 ~ "Trabajador",
    p6050 == 9 ~ "No pariente"
  )) %>%
  
  # Definir orden correcto
  mutate(parentesco = factor(parentesco, levels = c(
    "Jefe(a)", "Cónyuge", "Hijo(a)", "Nieto(a)",
    "Otro pariente", "Servicio doméstico",
    "Pensionista", "Trabajador", "No pariente"
  ))) %>%
  
  # Calcular proporciones dentro de cada parentesco
  group_by(parentesco, genero) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(parentesco) %>%
  mutate(prop = n / sum(n)) %>%
  
  # Graficar
  ggplot(aes(x = parentesco, y = prop, fill = genero)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Hombre" = "#2c5f8a",
                               "Mujer" = "#c0392b")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribución por parentesco según género",
       x = "Parentesco con el jefe(a) del hogar",
       y = "Proporción por género",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
ggsave("02_output/figures/parentescojefehogar.jpg", width = 9, height = 5, dpi = 300)

