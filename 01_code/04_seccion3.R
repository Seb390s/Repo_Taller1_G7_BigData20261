#Seccion3 

set.seed(1013)
base <- base %>%  filter(y_total_m > 0)
#base <- base %>% mutate (logw=log(y_total_m))




train <- base %>% filter(chunk %in% 1:7)
valid <- base %>% filter(chunk %in% 8:10)
nrow(train)
nrow(valid)

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


#calcular en RMSE
rmse_m1 <- RMSE(pred = pred_m1, obs = valid$logw, na.rm = TRUE)
rmse_m2 <- RMSE(pred = pred_m2, obs = valid$logw, na.rm = TRUE)
rmse_m3 <- RMSE(pred = pred_m3, obs = valid$logw, na.rm = TRUE)
rmse_m4 <- RMSE(pred = pred_m4, obs = valid$logw, na.rm = TRUE)
#===============================================================================

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


pred_m5 <- predict(m5_train, valid)
pred_m6 <- predict(m6_train, valid)
pred_m7 <- predict(m7_train, valid)
pred_m8 <- predict(m8_train, valid)
pred_m9 <- predict(m9_train, valid)

rmse_m5 <- RMSE(pred_m5, valid$logw, na.rm = TRUE)
rmse_m6 <- RMSE(pred_m6, valid$logw, na.rm = TRUE)
rmse_m7 <- RMSE(pred_m7, valid$logw, na.rm = TRUE)
rmse_m8 <- RMSE(pred_m8, valid$logw, na.rm = TRUE)
rmse_m9 <- RMSE(pred_m9, valid$logw, na.rm = TRUE)


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


#loocv

full_model <- m9_train  # o el modelo que haya ganado

#extraer matrices 
X <- model.matrix(full_model)
y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients
G_inv <- solve(t(X) %*% X)

vec <- 1 / (1 - hatvalues(full_model))

N <- nrow(X)

#calcular loocv con formula eficiente 

LOO <- numeric(N)

for (i in 1:N) {
  
  new_beta <- beta_hat - 
    vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  
  new_error <- (y[i] - (X[i, ] %*% new_beta))^2
  
  LOO[i] <- new_error
}
 #pARA estas 3 obs el modelo se remegajusto por que h = 1 entonces da NA
sum(is.na(LOO_errors))
which(is.na(LOO_errors))


loo_mse <- mean(LOO, na.rm = TRUE)
loo_rmse <- sqrt(loo_mse)

loo_rmse


#b-i - b

X <- model.matrix(full_model)
y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients
G_inv <- solve(t(X) %*% X)

h <- hatvalues(full_model)
res <- residuals(full_model)

N <- nrow(X)

beta_influence <- numeric(N)

for (i in 1:N) {
  
  beta_minus_i <- beta_hat - 
    (1 / (1 - h[i])) * G_inv %*% as.vector(X[i, ]) * res[i]
  
  beta_influence[i] <- sqrt(sum((beta_minus_i - beta_hat)^2))
}

summary(beta_influence)
max(beta_influence)

order(beta_influence, decreasing = TRUE)[1:10]

