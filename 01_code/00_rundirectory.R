# ==============================================================
# Taller 1 Big Data
# (Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)
# MAIN / RUN DIRECTORY
# ==============================================================

# ==============================================================
# SECCIÓN 1: Preparando el entorno
# ==============================================================

rm(list = ls())

cat("Working directory:\n")
print(getwd())

# Crear carpetas de output si no existen
for (path in c("02_output/figures",
               "02_output/tables")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# ==============================================================
# SECCIÓN 2: Cargue de paquetes
# ==============================================================


# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "rvest",      # Para web scraping
  "httr", #Hace solicitudes http a urls
  "tidyverse",
  "magrittr",
  "dplyr"
)

# Función auxiliar para instalar paquetes si no están disponibles ----
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


# ==============================================================
# SECCIÓN 3: Ejecución de scripts
# ==============================================================

source("00_webscrapping.R")
source("01_cleaner_usen_este.R")
