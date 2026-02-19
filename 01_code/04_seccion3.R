#Seccion3 




train <- base %>% filter(chunk %in% 1:7)
valid <- base %>% filter(chunk %in% 8:10)
nrow(train)
nrow(valid)

#Los modelos son:

#a) Sección 1
#     - modelo1 <- lm(logw ~ age + agecua, data = base)
#     - modelo2 <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = base)
#     - modelo3
#     - modelo4

#Entrenando los modelos 

m1_train <- lm(logw ~ age + agecua, data = train)

m2_train <- lm(logw ~ age + agecua + totalHoursWorked + relab, data = train)

#faltan los dos de gap género - income

#predecir en validación
pred_m1 <- predict(m1_train, newdata = valid)
pred_m2 <- predict(m2_train, newdata = valid)

#calcular en RMSE
rmse_m1 <- sqrt(mean((valid$logw - pred_m1)^2))
rmse_m2 <- sqrt(mean((valid$logw - pred_m2)^2))


#===============================================================================

#Los 5 modelos adicionales que tuve que crear (mejor un lopp con esto )

# Modelo 5

# Modelo 6

# Modelo 7

# Modelo 8

# Modelo 9


#esta es la forma manual no hay forma de hacerlo con predict o algo así
rmse_m5 <- sqrt(mean((valid$logw - predict(m5_train, valid))^2))
rmse_m6 <- sqrt(mean((valid$logw - predict(m6_train, valid))^2))
rmse_m7 <- sqrt(mean((valid$logw - predict(m7_train, valid))^2))
rmse_m8 <- sqrt(mean((valid$logw - predict(m8_train, valid))^2))
rmse_m9 <- sqrt(mean((valid$logw - predict(m9_train, valid))^2))
