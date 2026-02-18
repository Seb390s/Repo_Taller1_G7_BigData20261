#Código webscrapping con la data del taller 1 BigData
#(Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)

# =============================================================================
# SECCIÓN 3: Limpieza de datos
# =============================================================================
# Importar librerías (Mover arriba al final)

install.packages(c('quantmod','ff','foreign','R.matlab'),dependency=T)
suppressPackageStartupMessages(library(tidyverse))


cat("4: Inspección de variables y missing values\n")
# Convertir a tibble (mejor impresión en consola)

db_final <- GEIH2018

db <- as_tibble(db_final)

# ----------------------------------------------------------------------------
## SECCIÓN 3.1: Dimensiones y estructura general del dataset
# ----------------------------------------------------------------------------
cat("\nDimensiones del dataset (filas, columnas):\n")
print(dim(db))

cat("\nEstructura de las variables:\n")
str(db)

# -----------------------------------------------------------------------------
# 3.2 Tipo de variable por columna
# -----------------------------------------------------------------------------

# Tipo de cada variable (numeric, character, logical, etc.)
  var_types <- tibble(
  variable = names(db),
  class = sapply(db, class)
  )
cat("\nTipo de cada variable:\n")
print(var_types)

# Resumen agregado por tipo de variable
cat("\nResumen del número de variables por tipo:\n")
var_types %>%
  count(class) %>%
  arrange(desc(n)) %>%
  print()
# -----------------------------------------------------------------------------
# 3.3 Cálculo de missing values por variable
# -----------------------------------------------------------------------------

# Número total de observaciones
Nobs <- nrow(db)

# Tabla con missing values por variable
missing_summary <- tibble(
  variable   = names(db),
  n_missing  = sapply(db, function(x) sum(is.na(x))),
  p_missing  = sapply(db, function(x) mean(is.na(x)))
) %>%
  arrange(desc(p_missing))

cat("\nResumen de missing values por variable:\n")
print(missing_summary)

# Variables con al menos un missing
cat("\nVariables con valores faltantes:\n")
missing_summary %>%
  filter(n_missing > 0) %>%
  print()

# -----------------------------------------------------------------------------
# 3.4 Variables sin missing values
# -----------------------------------------------------------------------------

cat("\nVariables sin valores faltantes:\n")
missing_summary %>%
  filter(n_missing == 0) %>%
  select(variable) %>%
  print(n=22)

# -----------------------------------------------------------------------------
# 3.5 Tabla consolidada: tipo de variable + missing values
# -----------------------------------------------------------------------------

var_overview <- var_types %>%
  left_join(missing_summary, by = c("variable"))

cat("\nResumen consolidado (tipo de variable y missing values):\n")
print(var_overview)

# Mostrar solo las variables más problemáticas
cat("\nVariables con más del 20% de missing values:\n")
var_overview %>%
  filter(p_missing > 0.20) %>%
  arrange(desc(p_missing)) %>%
  print(n=132)

# Limpieza de datos
#filtrar datos por edad mayor a 18

df_cl <- dplyr::filter(db,age >= 18)
#------------------------------------------------------------------------------
# filtrar por ingreso = 0, ya que esto se puede interpretar como inactivo
# ===================
## Datos con ingreso total 0, convertir en 1, no perturba la muestra
df_cl <- dplyr::filter(db,ingtot >= 1)
# w <-- se toma como ingresos las columnas de ingtot
w = df_cl[,"ingtot"]
w_log = log(w)
#hist(w$ingtot)
# age <-- se toma para la edad
age <- as.list(df_cl[,"age"])
# age <-- primer plot
plot(age$age,w_log$ingtot)         
