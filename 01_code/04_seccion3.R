#Seccion 3

# --------------------------------------------------------------
# 1) training y valid data
# --------------------------------------------------------------
set.seed(1013)

train <- base %>% filter(chunk %in% 1:7)
valid <- base %>% filter(chunk %in% 8:10)
nrow(train)
nrow(valid)

valid <- valid %>%
  filter(oficio %in% levels(train$oficio))


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


m6_train <- lm(logw ~ age + I(age^2)  + maxEducLevel * female + 
                 totalHoursWorked + relab, data = train)


m7_train <- lm(logw ~ age * female +
                 (age + I(age^2)) * maxEducLevel +
                 totalHoursWorked + relab, data = train)


m8_train <- lm(logw ~ (age) * (female + maxEducLevel) +
                 I(age^2) + I(age^3)+
                 totalHoursWorked * relab +
                 sizeFirm ,
               data = train)


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

library(xtable)

rmse_table <- data.frame(
  Modelo = c("M1: Edad cuadrática", 
             "M2: Edad + horas + tipo empleo",
             "M3: Solo género",
             "M4: Género + edad + controles",
             "M5: Edad cúbica + educación",
             "M6: Edad² + educación×género",
             "M7: Edad×género + edad×educación",
             "M8: Interacciones + cúbico",
             "M9: Mejor modelo (seleccionado)"),
  RMSE = round(rmse_values, 4),
  Seccion = c("S1","S1","S2","S2","S3","S3","S3","S3","S3")
)

print(xtable(rmse_table, 
             caption = "Validación RMSE por modelo",
             label = "tab:rmse"),
      include.rownames = FALSE,
      booktabs = TRUE)
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
sum(is.na(LOO_errors))
which(is.na(LOO_errors))


loo_mse <- mean(LOO, na.rm = TRUE)
loo_rmse <- sqrt(loo_mse)
loo_rmse

# Comparación LOOCV vs validación
cat("RMSE validación:", rmse_m9, "\n")
cat("RMSE LOOCV:     ", loo_rmse, "\n")
cat("Diferencia:     ", abs(loo_rmse - rmse_m9), "\n")

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

# --------------------------------------------------------------
# 7) Medida de influencia sobre los coeficientes
# --------------------------------------------------------------

# Matrices necesarias
X <- model.matrix(m9_train)
e <- resid(m9_train)
h <- hatvalues(m9_train)
XtX_inv <- solve(t(X) %*% X)

# Escalar por observación: e_i / (1 - h_i)
mult <- e / (1 - h)

# beta(-i) - beta para cada observación (cada columna es una obs)
beta_diff <- XtX_inv %*% t(X * mult)

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

# --------------------------------------------------------------
# 8) Cruce de métricas: error LOO vs influencia
# --------------------------------------------------------------

# Clasificar obs en cuadrantes
train_used <- train_used %>%
  filter(is.finite(loo_error), is.finite(influence)) %>%
  mutate(
    alto_error     = loo_error  > median(loo_error),
    alta_influencia = influence > median(influence)
  )

# Tabla resumen de los 4 cuadrantes
train_used %>%
  group_by(alto_error, alta_influencia) %>%
  summarise(n = n(), .groups = "drop")

# Ver las obs que tienen AMBAS métricas altas (las más problemáticas)
train_used %>%
  filter(alto_error, alta_influencia) %>%
  arrange(desc(loo_error)) %>%
  select(loo_error, influence, age, female, maxEducLevel,
         totalHoursWorked, relab, logw) %>%
  head(10)

# --------------------------------------------------------------
# 9) Análisis por grupos
# --------------------------------------------------------------

# Crear quintiles de ingreso
train_used <- train_used %>%
  mutate(
    quintil_logw = ntile(logw, 5),
    grupo_edad   = cut(age, breaks = c(18, 30, 45, 60, 100),
                       labels = c("18-30", "31-45", "46-60", "60+"))
  )

# Error LOO promedio por grupo de edad
train_used %>%
  group_by(grupo_edad) %>%
  summarise(
    error_promedio   = mean(loo_error, na.rm = TRUE),
    influencia_media = mean(influence, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(error_promedio))

# Error LOO promedio por género y educación
train_used %>%
  group_by(female, maxEducLevel) %>%
  summarise(
    error_promedio = mean(loo_error, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(error_promedio))

# Error LOO promedio por quintil de ingreso
train_used %>%
  group_by(quintil_logw) %>%
  summarise(
    error_promedio = mean(loo_error, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(error_promedio))
