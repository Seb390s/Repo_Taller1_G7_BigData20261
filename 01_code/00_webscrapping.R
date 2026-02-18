#Código webscrapping con la data del taller 1 BigData
#(Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)
#

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
