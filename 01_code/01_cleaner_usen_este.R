#geih select aqui cogemos solo las variables que vamos a usar 

# Se crea un nuevo DF, en el cual se aplica la limpieza por edad("age") mayor a 18 años y ocupado

base <- GEIH2018 %>% filter(age>18, ocu ==1) 

#Se verifican las variables 
#AGREGAR FILTRO DE SALARIO POSITIVO (PONER LA VARIABLE SEBAS)

base <- base %>% mutate (
  logw=log(y_total_m),
  agecua = age^2
)
# Se revisan el tipo de variables (numeric, character, logical, etc.)
var_types <- tibble(
  variable = names(base),
  class = sapply(base, class)
)
cat("\nTipo de cada variable:\n")
print(var_types)
# Se verifican los porcentajes de "missing values"

# Número total de observaciones
Nobs <- nrow(base)

#  # Tabla con missing values por variable
  missing_summary <- tibble(
  variable   = names(base),
  n_missing  = sapply(base, function(x) sum(is.na(x))),
  p_missing  = sapply(base, function(x) mean(is.na(x)))
) %>%
  arrange(desc(p_missing))
cat("\nResumen de missing values por variable:\n")
print(missing_summary)

cat("\n Los datos con valor menor a 0 en el ingreso son:\n")
print(sum(base$y_total_m < 0, na.rm = TRUE))





