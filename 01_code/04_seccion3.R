#Seccion 3

# --------------------------------------------------------------
# 1) training y valid data
# --------------------------------------------------------------
set.seed(1013)

train <- base %>% filter(chunk %in% 1:7)
valid <- base %>% filter(chunk %in% 8:10)
nrow(train)
nrow(valid)

# --------------------------------------------------------------
# 2) Los 4 modelos de las secciones anteriores
# --------------------------------------------------------------
#Los modelos son:

#a) Sección 1
#     - modelo1 <- lm(logw ~ age + agecua, data = base)
#     - modelo2 <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = base)
#     - modelo3 <- lm(logw ~ female, data = base)
#     - modelo4 <- lm(logw ~ female + age + age2 + totalHoursWorked + maxEducLevel + relab + sizeFirm + regSalud, data = base)

#Entrenando los modelos 

m1_train <- lm(logw ~ age + agecua, data = train)
m2_train <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = train)
m3_train <- lm(logw ~ female, data = train)
m4_train <- lm(logw ~ female + age + agecua + totalHoursWorked + maxEducLevel + relab + sizeFirm + regSalud, data = train)

#predecir en validación
pred_m1 <- predict(m1_train, newdata = valid)
pred_m2 <- predict(m2_train, newdata = valid)
pred_m3 <- predict(m3_train, newdata = valid)
pred_m4 <- predict(m4_train, newdata = valid)


#calcular el RMSE
rmse_m1 <- RMSE(pred = pred_m1, obs = valid$logw, na.rm = TRUE)
rmse_m2 <- RMSE(pred = pred_m2, obs = valid$logw, na.rm = TRUE)
rmse_m3 <- RMSE(pred = pred_m3, obs = valid$logw, na.rm = TRUE)
rmse_m4 <- RMSE(pred = pred_m4, obs = valid$logw, na.rm = TRUE)


# --------------------------------------------------------------
# 3) 5 Modelos adicionales
# --------------------------------------------------------------
#Entrenando los modelos 
m5_train <- lm(logw ~ age + agecua + I(age^3) + maxEducLevel + 
                 totalHoursWorked + relab + p6050+ p6426, data = train)
#Este modelo pretende captar no linealidades respecto la edad (la literatura lo ha usado) y controles

m6_train <- lm(logw ~ age + I(age^2)  + maxEducLevel * female + 
                 totalHoursWorked + relab, data = train)
#Este modelo se baja un grado de polinomio para disminuir el sobreajuste e incluye
#una interacción con mujer y educación alcanzada esperando capturar diferencias entre
#los retornos de la educación a distintos niveles por género

m7_train <- lm(logw ~ age * female +
                 (age + I(age^2)) * maxEducLevel +
                 totalHoursWorked + relab, data = train)
#Este modelo espera capturar distintos retornos en la acumulación de capital
#por edades


m8_train <- lm(logw ~ (age) * (female + maxEducLevel) +
                 I(age^2) + I(age^3)+
                 totalHoursWorked * relab +
                 sizeFirm ,
               data = train)

#Este modelo pretende capturar 1)diferencias en el perfil salario -edad
#2) retornos que dependen del nivel educativo


m9_train <- lm(logw ~ female + age + 
                 totalHoursWorked + maxEducLevel + 
                 relab + p6426 + sizeFirm + regSalud+ I(age^2)  + age* (female + maxEducLevel) + p6050, data = train)


#predecir en validación
pred_m5 <- predict(m5_train, valid)
pred_m6 <- predict(m6_train, valid)
pred_m7 <- predict(m7_train, valid)
pred_m8 <- predict(m8_train, valid)
pred_m9 <- predict(m9_train, valid)

#calcular el RMSE
rmse_m5 <- RMSE(pred_m5, valid$logw, na.rm = TRUE)
rmse_m6 <- RMSE(pred_m6, valid$logw, na.rm = TRUE)
rmse_m7 <- RMSE(pred_m7, valid$logw, na.rm = TRUE)
rmse_m8 <- RMSE(pred_m8, valid$logw, na.rm = TRUE)
rmse_m9 <- RMSE(pred_m9, valid$logw, na.rm = TRUE)

# --------------------------------------------------------------
# 4) Modelo con mejor desempeño fuera de muestra
# --------------------------------------------------------------

rmse_values <- c(
  m1 = rmse_m1,
  m2 = rmse_m2,
  m3 = rmse_m3,
  m4 = rmse_m4,
  m5 = rmse_m5,
  m6 = rmse_m6,
  m7 = rmse_m7,
  m8 = rmse_m8,
  m9 = rmse_m9
)

best_model_name <- names(which.min(rmse_values))
best_model_name
best_rmse <- min(rmse_values)
best_rmse

cat("El modelo con menor RMSE es:", best_model_name,
    "con un RMSE de", best_rmse)

rmse_table <- data.frame(
  Modelo = c("Modelo 1", 
             "Modelo 2",
             "Modelo 3",
             "Modelo 4",
             "Modelo 5",
             "Modelo 6",
             "Modelo 7",
             "Modelo 8",
             "Modelo 9"),
  RMSE = round(rmse_values, 4),
  Seccion = c("S1","S1","S2","S2","S3","S3","S3","S3","S3")
)

# Consola
print(
  xtable(rmse_table,
         caption = "Validación RMSE por modelo",
         label = "tab:rmse",
         digits = c(0, 0, 2, 0)),
  include.rownames = FALSE,
  booktabs = TRUE
)

# Archivo
print(
  xtable(rmse_table,
         caption = "Validación RMSE por modelo",
         label = "tab:rmse",
         digits = c(0, 0, 2, 0)),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "02_output/tables/rmse_validacion.tex"
)
# --------------------------------------------------------------
# 5) LOOCV del mejor modelo 
# --------------------------------------------------------------

full_model <- m9_train  

# A) Extraer matrices _____________________________________

X <- model.matrix(full_model)
y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients
G_inv <- solve(t(X) %*% X)

vec <- 1 / (1 - hatvalues(full_model))

N <- nrow(X)

# B) Calcular loocv _______________________________________

LOO <- numeric(N)

for (i in 1:N) {
  
  new_beta <- beta_hat - 
    vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  
  new_error <- (y[i] - (X[i, ] %*% new_beta))^2
  
  LOO[i] <- new_error
}
#Para estas 3 obs el modelo se remegajusto por que h = 1 entonces da NA
sum(is.na(LOO))
which(is.na(LOO))


loo_mse <- mean(LOO, na.rm = TRUE)
loo_rmse <- sqrt(loo_mse)
loo_rmse

# Comparación LOOCV vs validación
cat("RMSE validación:", rmse_m9, "\n")
cat("RMSE LOOCV:     ", loo_rmse, "\n")
cat("Diferencia:     ", abs(loo_rmse - rmse_m9), "\n")

# Tabla comparación LOOCV vs validación para LaTeX
comparacion_table <- data.frame(
  Métrica = c("RMSE Validación", "RMSE LOOCV", "Diferencia"),
  Valor = round(c(rmse_m9, loo_rmse, abs(loo_rmse - rmse_m9)), 4)
)

print(
  xtable(comparacion_table,
         caption = "Comparación LOOCV vs error de validación, modelo M9",
         label = "tab:loocv",
         digits = c(0, 0, 4)),
  include.rownames = FALSE,
  booktabs = TRUE
)

print(
  xtable(comparacion_table,
         caption = "Comparación LOOCV vs error de validación, modelo M9",
         label = "tab:loocv",
         digits = c(0, 0, 4)),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "02_output/tables/comparacion_loocv.tex"
)

# --------------------------------------------------------------
# 6) Observaciones más difíciles de predecir
# --------------------------------------------------------------

# Error LOO vectorizado
loo_error <- resid(m9_train) / (1 - hatvalues(m9_train))

# Filas que usó el modelo (sin NA) y pegar error LOO
train_used <- train[as.integer(rownames(model.matrix(m9_train))), ]
train_used$loo_error <- abs(loo_error)

# Las 10 más difíciles de predecir (excluyendo Inf por h=1)
train_used %>%
  filter(is.finite(loo_error)) %>%
  arrange(desc(loo_error)) %>%
  select(loo_error, age, female, maxEducLevel, totalHoursWorked,
         relab, logw) %>%
  head(10)

# Nota: 3 observaciones tienen leverage = 1 (h=1), lo que genera
# loo_error = Inf. El modelo las excluyen del análisis.

# Gráfico: error LOO vs ingreso (muestra que falla en las colas)
train_used %>%
  filter(is.finite(loo_error)) %>%
  ggplot(aes(x = logw, y = loo_error)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(color = "red", se = TRUE) +
  coord_cartesian(ylim = c(0, 5)) +  # <- esto
  labs(title = "Error de predicción leave-one-out según nivel de ingreso",
       x = "Log salario mensual",
       y = "Error de predicción leave-one-out (valor absoluto)") +
  theme_minimal()
ggsave(
  "02_output/figures/loo_error_vs_logw.jpg",
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------------------------------------------
# 7) Medida de influencia sobre los coeficientes
# --------------------------------------------------------------

# Matrices necesarias
X <- model.matrix(m9_train)
e <- resid(m9_train) #û_t = y_t - X_t * β̂
h <- hatvalues(m9_train) #h_t = X_t(X'X)^{-1}X_t'
XtX_inv <- solve(t(X) %*% X) #X_t' =  (X'X)^{-1}

# Escalar por observación: 
mult <- e / (1 - h) #û_t / (1 - h_t)

# beta(-i) - beta para cada observación (cada columna es una obs) β̂ - β̂_{(t)} = (X'X)^{-1} * X_t' * [û_t/(1-h_t)]
beta_diff <- XtX_inv %*% t(X * mult) #(X'X)^{-1} X_t' * escalar

# Norma euclidiana por observación, normalizada por número de parámetros
influence_beta <- sqrt(colSums(beta_diff^2)) / sqrt(ncol(X))

# Pegar al dataset
train_used$influence <- influence_beta

# Las 10 más influyentes
train_used %>%
  filter(is.finite(influence)) %>%
  arrange(desc(influence)) %>%
  select(influence, loo_error, age, female, maxEducLevel,
         totalHoursWorked, relab, logw) %>%
  head(10)

# Gráfico: error LOO vs influencia (muestra que son grupos distintos)
train_used %>%
  filter(is.finite(loo_error), is.finite(influence)) %>%
  ggplot(aes(x = influence, y = loo_error)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(color = "red", se = TRUE) +
  coord_cartesian(ylim = c(0, 5), xlim = c(0, 0.08))+
labs(title = "Relación entre error de predicción leave-one-out e influencia sobre los coeficientes",
       x = "Influencia sobre el vector de coeficientes (‖β̂₍₋ᵢ₎ − β̂‖₂ normalizada)",
       y = "Error absoluto de predicción leave-one-out",
       caption = "Nota: 3 observaciones con leverage = 1 excluidas") +
  theme_minimal()
ggsave(
  "02_output/figures/loo_vs_influence.jpg",
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------------------------------------------
# 8) Cruce de métricas: error LOO vs influencia
# --------------------------------------------------------------

# Clasificar obs usando media + 1 desviación estándar (criterio estándar para outliers)
train_used <- train_used %>%
  filter(is.finite(loo_error), is.finite(influence)) %>%
  mutate(
    alto_error      = loo_error > mean(loo_error) + sd(loo_error),
    alta_influencia = influence > mean(influence) + sd(influence)
  )

# Tabla resumen de los 4 cuadrantes
train_used %>%
  group_by(alto_error, alta_influencia) %>%
  summarise(n = n(), .groups = "drop")

# Boxplot por cuadrante
train_used %>%
  mutate(grupo = case_when(
    alto_error & alta_influencia   ~ "Alto error\nAlta influencia",
    alto_error & !alta_influencia  ~ "Alto error\nBaja influencia",
    !alto_error & alta_influencia  ~ "Bajo error\nAlta influencia",
    TRUE                           ~ "Bajo error\nBaja influencia"
  )) %>%
  ggplot(aes(x = grupo, y = loo_error, fill = grupo)) +
  geom_boxplot(alpha = 0.7) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(title = "Clasificación de observaciones según error de predicción e influencia estructural",
       subtitle = "Clasificación basada en media + 1 desviación estándar",
       x = "",
       y = "Error absoluto de predicción leave-one-out (log ingreso)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(
  "02_output/figures/cuadrantes_loo_influencia.jpg",
  width = 8,
  height = 5,
  dpi = 300
)

# Tabla LaTeX: 10 observaciones más problemáticas (alto error Y alta influencia)
tabla_problematicas <- train_used %>%
  filter(alto_error, alta_influencia) %>%
  arrange(desc(loo_error)) %>%
  select(loo_error, influence, age, female, maxEducLevel,
         totalHoursWorked, relab, logw) %>%
  head(10) %>%
  rename(
    `\\makecell{Error \\\\ LOO}`        = loo_error,
    `Influencia`                        = influence,
    `Edad`                              = age,
    `Mujer`                             = female,
    `\\makecell{Nivel \\\\ educativo}`  = maxEducLevel,
    `\\makecell{Horas \\\\ trabajadas}` = totalHoursWorked,
    `\\makecell{Tipo \\\\ empleo}`      = relab,
    `\\makecell{Log \\\\ salario}`      = logw
  )

print(
  xtable(tabla_problematicas,
         caption = "Observaciones con alto error de predicción y alta influencia",
         label  = "tab:problematicas",
         digits = c(0, 3, 6, 0, 0, 0, 0, 0, 2)),
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.colnames.function = identity
)

print(
  xtable(tabla_problematicas,
         caption = "Observaciones con alto error de predicción y alta influencia",
         label  = "tab:problematicas",
         digits = c(0, 3, 6, 0, 0, 0, 0, 0, 2)),
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.colnames.function = identity,
  file = "02_output/tables/observaciones_problematicas.tex"
)

# --------------------------------------------------------------
# 9) Análisis por grupos
# --------------------------------------------------------------

# Crear variables de grupo PRIMERO
train_used <- train_used %>%
  mutate(
    quintil_logw = ntile(logw, 5),
    grupo_edad   = cut(age, breaks = c(18, 30, 45, 60, 100),
                       labels = c("18-30", "31-45", "46-60", "60+"))
  )

# Tabla 1: por grupo de edad
tabla_edad <- train_used %>%
  group_by(grupo_edad) %>%
  summarise(
    `Error promedio`      = round(mean(loo_error, na.rm = TRUE), 3),
    `Influencia promedio` = round(mean(influence, na.rm = TRUE), 6),
    N = n()
  ) %>%
  arrange(desc(`Error promedio`))
print(
  xtable(tabla_edad,
         caption = "Error de predicción LOO e influencia por grupo de edad",
         label = "tab:edad",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE
)

print(
  xtable(tabla_edad,
         caption = "Error de predicción LOO e influencia por grupo de edad",
         label = "tab:edad",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "02_output/tables/error_por_edad.tex"
)
# Tabla 2: por quintil de ingreso
tabla_quintil <- train_used %>%
  group_by(quintil_logw) %>%
  summarise(
    `Error promedio`      = round(mean(loo_error, na.rm = TRUE), 3),
    `Influencia promedio` = round(mean(influence, na.rm = TRUE), 6),
    N = n()
  ) %>%
  rename(`Quintil ingreso` = quintil_logw) %>%
  arrange(`Quintil ingreso`)

print(
  xtable(tabla_quintil,
         caption = "Error de predicción LOO e influencia por quintil de ingreso",
         label = "tab:quintil",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE
)

print(
  xtable(tabla_quintil,
         caption = "Error de predicción LOO e influencia por quintil de ingreso",
         label = "tab:quintil",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "02_output/tables/error_por_quintil.tex"
)

# Tabla 3: por relación laboral
tabla_relab <- train_used %>%
  mutate(relab = case_when(
    relab == "1" ~ "Obrero empresa particular",
    relab == "2" ~ "Obrero gobierno",
    relab == "3" ~ "Empleado doméstico",
    relab == "4" ~ "Cuenta propia",
    relab == "5" ~ "Patrón o empleador",
    relab == "9" ~ "Otro",
    TRUE ~ as.character(relab)
  )) %>%
  group_by(relab) %>%
  summarise(
    `Error promedio`      = round(mean(loo_error, na.rm = TRUE), 3),
    `Influencia promedio` = round(mean(influence, na.rm = TRUE), 6),
    N = n()
  ) %>%
  arrange(desc(`Error promedio`))

print(
  xtable(tabla_relab,
         caption = "Error de predicción LOO e influencia por relación laboral",
         label = "tab:relab",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE
)

print(
  xtable(tabla_relab,
         caption = "Error de predicción LOO e influencia por relación laboral",
         label = "tab:relab",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "02_output/tables/error_por_relacion_laboral.tex"
)

# Tabla 4: por nivel educativo
tabla_educ <- train_used %>%
  mutate(maxEducLevel = case_when(
    maxEducLevel == "1" ~ "1: Ninguno",
    maxEducLevel == "2" ~ "2: Preescolar",
    maxEducLevel == "3" ~ "3: Primaria incompleta (1-4)",
    maxEducLevel == "4" ~ "4: Primaria completa (5)",
    maxEducLevel == "5" ~ "5: Secundaria incompleta (6-10)",
    maxEducLevel == "6" ~ "6: Secundaria completa (11)",
    maxEducLevel == "7" ~ "7: Terciaria",
    maxEducLevel == "9" ~ "9: N/A",
    TRUE ~ as.character(maxEducLevel)
  )) %>%
  group_by(maxEducLevel) %>%
  summarise(
    `Error promedio`      = round(mean(loo_error, na.rm = TRUE), 3),
    `Influencia promedio` = round(mean(influence, na.rm = TRUE), 6),
    N = n()
  ) %>%
  arrange(desc(`Error promedio`)) %>%
  rename(`Nivel educativo` = maxEducLevel)

print(
  xtable(tabla_educ,
         caption = "Error de predicción LOO e influencia por nivel educativo",
         label = "tab:educ",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE
)

print(
  xtable(tabla_educ,
         caption = "Error de predicción LOO e influencia por nivel educativo",
         label = "tab:educ",
         digits = c(0, 0, 3, 6, 0)),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "02_output/tables/error_por_nivel_educativo.tex"
)
