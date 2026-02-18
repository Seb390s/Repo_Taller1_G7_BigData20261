# Taller 1 Big data
#(Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)
#Main 

rm(list = ls())

cat("Working directory:\n")
print(getwd())

# Crear carpetas de output si no existen
for (path in c("02_output/figures",
               "02_output/tables")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# Ejecutar scripts en orden
source("01_code/00_webscrapping.R")
source("01_code/00_datacleaner.R")
#source("01_code/01_limpiezaDatos.R") Se repite de lo que está en data cleaner? el comienzo cita df_final, pero en ese orden que pusimos los source no existe
