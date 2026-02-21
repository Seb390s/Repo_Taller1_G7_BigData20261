# Taller 1 – BDML: Perfil Edad–Ingreso y Brecha Salarial por Género
## Paquete reproducible de replicación (GEIH 2018 sample – Bogotá)

**Autores:** Sebastián Rodríguez, Hernan Yepez, Michael Salcedo  
**Curso:** Big Data & Machine Learning (BDML) – Universidad de los Andes – 2026  

Este repositorio contiene un flujo reproducible para: (i) descargar datos (web scraping), (ii) limpiar y preparar la base, (iii) estimar perfiles edad–ingreso, (iv) estimar brechas salariales por género con y sin controles (incluyendo descomposición FWL y bootstrap), y (v) generar outputs (tablas y figuras) listos para incluir en el informe/presentación.

---

## Instrucciones de replicación

Para reproducir **todos** los resultados desde cero, ejecute un solo comando en R:

```r
source("00_rundirectory.R")
