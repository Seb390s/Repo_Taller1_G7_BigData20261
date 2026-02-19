#Código webscrapping con la data del taller 1 BigData
#(Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)


# =============================================================================
# SECCIÓN 0: Descargue y cargue de paquetes
# =============================================================================

rm(list = ls())

# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "rvest",      # Para web scraping
  "httr" #Hace solicitudes http a urls
)

# Función auxiliar para instalar paquetes si no están disponibles
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Instalando paquetes faltantes:", paste(new_packages, collapse=", "), "\n")
    install.packages(new_packages)
  } else {
    cat("Todos los paquetes ya están instalados.\n")
  }
}
install_if_missing(required_packages)

# Cargar todas las librerías necesarias
lapply(required_packages, function(pkg) {
  cat("Cargando paquete:", pkg, "...\n")
  library(pkg, character.only = TRUE)
})

# =============================================================================
# SECCIÓN 1: Exploración de los elementos de la página
# =============================================================================

data_problem_set1 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/index.html"
cat("- Fuente madre:", data_problem_set1, "\n")

# Al usar html_table() no se obtiene ninguna tabla, porque las páginas "pagei.html" no contienen las tablas directamente,
# sino que las tablas se cargan de forma dinámica mediante una llamada a JavaScript después de que la página ya abrió.
#En DevTools -> Network -> Filtro -> Fetch/XHR se observa una petición (request):
#https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html
#Será entonces la estructura de las url cambiando 1 a i = 1, 2,...,10

# =============================================================================
# SECCIÓN 2: Extracción de los datos
# =============================================================================

base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"

GEIH2018 <- data.frame() #crea un df vacío 

for (i in 1:10) {
  url <- paste0(base_url, i, ".html") #Configuro las URL 
  cat("Descargando:", url, "\n")
  
  resp <- GET(url) #Hace la petición http
  stop_for_status(resp) #Verifica que el status code sea bueno 
  
  text <- content(resp, as = "text", encoding = "UTF-8") #toma el cuerpo de la respuesta y lo convierte en texto 
  h <- read_html(text) #parsea el text convirtiendolo en tipo HTML para poder buscar etiquetas 
  tabs <- html_table(h, fill = TRUE) #hace una lista con los tablas y las convierte en df
  
  if (length(tabs) == 0) {
    warning("No se encontró tabla en: ", url) #verifica que haya encontrado al menos una tabla
    next
  }
  
  tabla_i <- tabs[[1]]              #Accede a la primera tabla
  names(tabla_i) <- make.names(names(tabla_i), unique = TRUE) #transforma los nombres de las columnas para que sean válidas

  GEIH2018 <- rbind(GEIH2018, tabla_i) #Pega las filas debajo de las filas que ya se habian cargado en el df
}

GEIH2018
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
  print()

# ==============================================================================
# 5.6 filtrar datos por edad mayor a 18
# ==============================================================================

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
##============================================================================
plot(df_cl$hoursWorkUsual,df_cl$age)
hist(
  df_cl$age,
  main = "Histograma de edad",
  xlab = "Edad",
  col = "lightblue",
  border = "white"
)
##============================================================================
# Unconditional age-labor income profile
##================
df_cl<-df_cl %>% mutate(ingtot_log = log(ingtot),
                        age2 = age^2)

#===========
# Regresion
mod1<-lm(ingtot_log~age+age^2,df_cl)
#liberías
require("pacman")
p_load("tidyverse","stargazer")

stargazer(mod1,type="text")

plot(df_cl$age_norm, df_cl$ingtot_log,
     xlab = "Age",
     ylab = "Income",
     main = "Age vs Income")

abline(mod1, col = "red", lwd = 2)


                      
# ==============================================================================


