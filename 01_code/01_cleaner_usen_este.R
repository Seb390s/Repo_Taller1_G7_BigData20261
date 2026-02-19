#geih select aqui cogemos solo las variables que vamos a usar 



base <- GEIH2018 %>% filter(age>18, ocu ==1) 

#AGREGAR FILTRO DE SALARIO POSITIVO (PONER LA VARIABLE SEBAS)

base <- base %>% mutate (
  logw=log(y_total_m),
  agecua = age^2
)


#tABLA VARIABLE, TIPO DE VARIABLE, #MISSINGS Y #PORCENTAJE DE MISSING
#(Esto sale de limpiezaDatosrevisar y datacleaner revisar)

#borrar datacleaner y limpiezadatos

