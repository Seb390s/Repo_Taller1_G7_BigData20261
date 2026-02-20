#Seccion3 

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
#     - modelo4 <- lm(logw ~ female + age + age2 + totalHoursWorked + maxEducLevel + oficio + estrato1 + relab + sizeFirm + regSalud, data = base)

#Entrenando los modelos 

m1_train <- lm(logw ~ age + agecua, data = train)

m2_train <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = train)

m3_train <- lm(logw ~ female, data = train)

m4_train <- lm(logw ~ female + age + agecua + totalHoursWorked + maxEducLevel + oficio + estrato1 + relab + sizeFirm + regSalud, data = train)

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


#Los 5 modelos adicionales que tuve que crear (mejor un lopp con esto )

# Modelo 5 <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = base)

# Modelo 6 <- lm(logw ~ age + agecua, data = base)

# Modelo 7 <- lm(logw ~ age + agecua, data = base)

# Modelo 8 <- lm(logw ~ age + agecua, data = base)

# Modelo 9 <- lm(logw ~ age + agecua, data = base)


#esta es la forma manual no hay forma de hacerlo con predict o algo así
rmse_m5 <- sqrt(mean((valid$logw - predict(m5_train, valid))^2))
rmse_m6 <- sqrt(mean((valid$logw - predict(m6_train, valid))^2))
rmse_m7 <- sqrt(mean((valid$logw - predict(m7_train, valid))^2))
rmse_m8 <- sqrt(mean((valid$logw - predict(m8_train, valid))^2))
rmse_m9 <- sqrt(mean((valid$logw - predict(m9_train, valid))^2))
