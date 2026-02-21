# ==============================================================
# Cleaner: 
# Selecciona las variables, relevantes, agrega filtros, arregla variables categoricas,
# y hace una tabla que relaciona el tipo de variable y los NAs

# --------------------------------------------------------------
# 1) Selección de variables necesarias
# --------------------------------------------------------------

base <- GEIH2018 %>%
  select(age, ocu, y_total_m, sex, totalHoursWorked, maxEducLevel, oficio,estrato1, relab,
          sizeFirm, regSalud, chunk, p6050, p6426)

# --------------------------------------------------------------
# 2) Filtros
# --------------------------------------------------------------

base <- base %>% filter(age>18, ocu ==1) 

# --------------------------------------------------------------
# 3) Variables creadas
# --------------------------------------------------------------

base <- base %>% mutate (
  logw=log(y_total_m),
  agecua = age^2,
  female = 1 - sex
)

# --------------------------------------------------------------
# 4) Correción categoricas 
# --------------------------------------------------------------

# VARIABLE RELAB NIVELES 6 Y 7
#En la exploración tuvimos problemas con los niveles 6 y 7 de la variable categorica Relab,
#Al revisar con más detalle estas observaciones tienen NA sistemático en la variable de ingreso
prueba <- base %>%
  select(y_total_m,  relab, female)
prueba <- prueba %>% filter(relab == 6 | relab == 7)
nrow(prueba)
sum(is.na(prueba))
#luego revisamos bien el diccionario de variables y estos dos niveles corresponden a trabajo no remunerado
#particularmente, 6 =Trabajador familiar sin remuneración, 7=Trabajador sin remuneración en empresas o negocios
#de otros hogares, por tal motivo decidimos eliminar estos dos niveles, pues no hay una variable dependiente que estimar 
#o con la cual podamos comparar una predicción.

base <- base %>%
  filter(!relab %in% c(6,7)) #Eliminandolas 

#Haciendolas categoricas
base <- base %>%
  mutate(
    maxEducLevel = as.factor(maxEducLevel),
    p6050        = as.factor(p6050),
    oficio       = as.factor(oficio),
    estrato1     = as.factor(estrato1),
    relab        = as.factor(relab),
    regSalud     = as.factor(regSalud)
  )



# --------------------------------------------------------------
# 3) Tabla con NAs y tipo de variable
# --------------------------------------------------------------

summary_table <- tibble(
  variable   = names(base),
  type       = sapply(base, function(x) class(x)[1]),
  n_missing  = sapply(base, function(x) sum(is.na(x))),
  p_missing  = sapply(base, function(x) mean(is.na(x))),
  n_unique   = sapply(base, function(x) length(unique(x)))
) %>%
  arrange(desc(p_missing))

cat("\nResumen completo de variables:\n")
print(summary_table)


