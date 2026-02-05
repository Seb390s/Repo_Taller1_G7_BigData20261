#Código webscrapping con la data del taller 1 BigData
#(Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)


# Nota: Para reproducir este código debe asegurarse de fijar su ruta, lo puede hacer
# agregandola en la sección "0.2 Directorios"


# =============================================================================
# SECCIÓN 0.1: Descargue y cargue de paquetes
# =============================================================================

rm(list = ls())

# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "rvest",      # Para web scraping
  "stringr",    # Manipulación de cadenas de texto
  "dplyr",      # Manipulación de datos
  "purrr",      # Programación funcional
  "tidytext",   # Análisis de texto con enfoque tidy
  "tidyverse",  # Conjunto completo de herramientas para análisis de datos
  "chromote"
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
username <- Sys.getenv("USERNAME")

# =============================================================================
# SECCIÓN 0.2: Directorios
# =============================================================================

ruta_datos <- file.path("C:/Users", username, 
                        "OneDrive - Universidad de los Andes/UNIANDES/2.Maestría/1.Primer_semestre/BigData_MachingLearning/talleres/Taller 1")
setwd(ruta_datos)
getwd()

# =============================================================================
# SECCIÓN 1: Definición de la ruta de los datos
# =============================================================================

data_problem_set1 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/index.html"
cat("URLs de origen definidas:\n")
cat("- data problem set1:", data_problem_set1, "\n")

# =============================================================================
# SECCIÓN 2: Inspección de los elementos de la página
# =============================================================================
cat("1: Inspeccionando los elementos de la página para obtener las url de las tablas")
pagina <- read_html(data_problem_set1)

#Hicimos una inspección de los elementos de la página y notamos que:
# a) Los enlaces de cada chunck tienen la etiqueta "a" (de enlace), además,
# b) tienen el atributo href que es la URL del archivo html tal "pagei.html" con con i= 1,...,10
# c) y el texto visible asociado a cada href es "Data Chunk i" con i= 1,...,10


# 2. Extraer información de enlaces
urls <- pagina %>% html_nodes("a") %>% html_attr("href") #Me quedo con los enlaces (etiqueta a) que tengan etiqueta href
textos <- pagina %>% html_nodes("a") %>% html_text() #De esos enlaces, hago una lista con su text asociado


es_chunk <- str_detect(textos, "Data chunk") #TRUE para cada elemento de la lista textos que estan asociados a un texto "Data Chunk i"
urls_chunks <- urls[es_chunk] #Me quedo solo con la lista de urls para las que en el paso anterior fue TRUE
textos_chunks <- textos[es_chunk] #Me quedo con los textos asociados a cada una de esas URL


# Construir URLs completas
base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
chunks_urls_completas <- paste0(base_url, urls_chunks)

cat("URLs completas:\n")
for(i in seq_along(chunks_urls_completas)) {
  cat(sprintf("%2d. %s\n", i, chunks_urls_completas[i]))
}
cat("\n")

# =============================================================================
# SECCIÓN 3: Extracción de los datos
# =============================================================================
cat("2:  Extracción de los datos")

b <- ChromoteSession$new() #Abrir sesión en Chrome

#Función robusta: abre una URL, espera a que la tabla exista y tenga filas, y la extrae
extraer_tabla_js_robusta <- function(b, url, timeout_sec = 40) {
  b$Page$navigate(url)
  
  t0 <- Sys.time()
  repeat {
    ok <- b$Runtime$evaluate( #Verifica si la tabla de JS ya está cargada 
      "(() => {
        const t = document.querySelector('table');
        if (!t) return false;
        const nrows = t.querySelectorAll('tr').length;
        return nrows >= 5;  // ajusta si quieres
      })()"
    )$result$value
    
    if (isTRUE(ok)) break
    
    #Si ya esperamos más de timeout_sec segundos, paramos con error
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_sec) {
      stop(paste("Timeout: no apareció/llenó la tabla en:", url))
    }
    Sys.sleep(0.25)
  }
  #Cuando la tabla ya está lista, extraemos SOLO el HTML de la tabla
  table_html <- b$Runtime$evaluate("document.querySelector('table').outerHTML")$result$value
  pg <- read_html(table_html)
  pg %>% html_node("table") %>% html_table(fill = TRUE)   #Convertimos el table HTML en dataframe

}

#Si un chunk falla, intenta una vez más y si falla devuelve NULL
extraer_con_reintento <- function(url) {
  tryCatch(
    extraer_tabla_js_robusta(b, url),
    error = function(e1) {
      message("Fallo en: ", url, " | Reintentando 1 vez...")
      Sys.sleep(2)
      tryCatch(
        extraer_tabla_js_robusta(b, url, timeout_sec = 60),
        error = function(e2) {
          message("Fallo definitivo en: ", url)
          # devuelve NULL para que no tumbe el proceso
          NULL
        }
      )
    }
  )
}

t0 <- Sys.time() #para medir el tiempo total 
cat("Se están extrayendo los datos, se demora aproximadamente 200 segundos = 3.3 min")
lista <- map(chunks_urls_completas, extraer_con_reintento)

# Quitar los NULL (chunks fallidos)
lista_ok <- compact(lista)

df_final <- bind_rows(lista_ok) #Se unen los df en uno solo pegando por filas

message("Chunks exitosos: ", length(lista_ok), " / ", length(chunks_urls_completas))
message("Tiempo total (s): ", round(as.numeric(difftime(Sys.time(), t0, units="secs")), 1))
print(dim(df_final))

b$close() #cerrar la sesión del navegador

# =============================================================================
# SECCIÓN 4: Exportar los datos
# =============================================================================

saveRDS(df_final, "df_final.rds")
cat("3: Se ha exportado la base con todos los chuncks en un archivo rds")

