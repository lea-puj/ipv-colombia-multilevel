####################################################################
## INCLUSIÓN VARIABLES DE 2DO NIVEL A PROBAR EN MODELO MULTINIVEL ##
####################################################################


# Este script es para anexar en la BD usada en anteriores entregas para el modelo multinivel,
# las variables de segundo nivel "Clasificación OCDE" e "Indice de Pobreza Multidimensional"
# a modo de reemplazo de "Zona(urbano/rural)" y "Tasa de Necesidades Básicas Insatisfechas",
# respectivamente. Adicionalmente, tambien se anexan las tasas de participación
# laboral de mujeres y hombre. Para la entrega 7 se incorporan variables de:

# Educación del esposo que habita en el hogar
# Proporción de mujeres en Q4 y Q5
# Proporción de mujeres en Q5
# Proporción de mujeres con estudios superiores


#-----------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(here)  

# Rutas del proyecto (ver README para la ubicación de cada archivo)
dir_municipal <- here("00_data", "raw", "municipal")
dir_processed <- here("00_data", "processed")

# Cargamos nuestra base de datos (salida de 01_build_individual_data.R)

BD_mod <- readRDS(file.path(dir_processed, "individual_data.rds"))

# Ahora vamos a ajustar el código DIVIPOLA para que coincida cuando peguemos las variables de interés

  BD_mod$divipola <- sprintf("%05d", BD_mod$codmpio)

# Cargamos información sobre el IPM (Índice de Pobreza Multidimensional)

IPM <- read_excel(file.path(dir_municipal, "IPM_Municipal2018.xlsx"))


# Cargamos información sobre clasificación OCDE para mpios

OCDE <- read_excel(file.path(dir_municipal, "MpiosOCDE.xlsx"))

setdiff(unique(BD_mod$divipola), unique(OCDE$divipola)) # observamos que no hay clas OCDE para "94663" (MAPIRIPANA)
table(BD_mod$divipola == "94663") # corresponde en nuestra base de datos original a 40 Observaciones


# Caregamos información sobre la tasa de participación laboral fem y masc

PART_LAB <- read_excel(file.path(dir_municipal, "part_laboral_cnpv2018.xlsx"))


# Pegamos las bases de datos

BD_final <- BD_mod %>%
  left_join(
    IPM %>%
      select(divipola, IPM_total), by = "divipola") %>%
  left_join(
    OCDE %>%
      select(divipola, Clas_OCDE), by = "divipola") %>%
  left_join(
    PART_LAB %>%
      select(divipola, prop_muj_trabajo, prop_hom_trabajo), by = "divipola"
  )

# Verificamos NA's del merge
colSums(is.na(BD_final))

# Guardamos nuestra nueva BD (la lee 03_build_municipal_aggregates.R)

saveRDS(BD_final, file.path(dir_processed, "individual_municipal_data.rds"))
