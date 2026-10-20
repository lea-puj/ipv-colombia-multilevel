
#############################################################
##   CONSTRUCCIÓN DE VARIABLES MUNICIPALES (SEGUNDO NIVEL)  ##
#############################################################

# Este script reúne la construcción de las variables municipales que antes estaba al final del
# script de estadísticas descriptivas. Produce dos bases:

#   1. municipal_data.rds: una fila por municipio, con las proporciones municipales ponderadas
#      (svyby), las variables municipales externas y la incidencia de IPV por municipio que usa
#      el análisis espacial (antes BD_incIPV_mpal.xlsx).

#   2. model_data.rds: base individual con las variables municipales anexadas. Es la base que
#      leen los descriptivos y el modelo multinivel (antes BD_modelo_Entrega7.xlsx en su
#      versión sobrescrita por el script de descriptivos).

# El código de las secciones 1 a 3 y 5 es el del script de descriptivos original; solo cambian la
# lectura y la escritura de archivos. La sección 4 es nueva.


library(dplyr)
library(survey)
library(here) 

# Rutas del proyecto
dir_processed <- here("00_data", "processed")


# Importamos nuestro dataset (salida de 02_merge_municipal_covariates.R)
data.mlvl.2 <- readRDS(file.path(dir_processed, "individual_municipal_data.rds"))



# 1. A J U S T E   D E   V A R I A B L E S   Y   E T I Q U E T A S
#-----------------------------------------------------------------------------------------------------------------------

# Guardamos los valores originales para verificar las etiquetas (ver nota más abajo)
decision_orig <- data.mlvl.2$decision
ipv_att_orig  <- data.mlvl.2$ipv_att

data.mlvl.2$w_qt <- as.factor(data.mlvl.2$w_qt)
data.mlvl.2$job <- as.factor(data.mlvl.2$job)
data.mlvl.2$ipv_att <- as.factor(data.mlvl.2$ipv_att)
data.mlvl.2$decision <- as.factor(data.mlvl.2$decision)
data.mlvl.2$urban <- as.factor(data.mlvl.2$urban)
data.mlvl.2$pdet <- as.factor(data.mlvl.2$pdet)
data.mlvl.2$eth <- as.factor(data.mlvl.2$eth)
data.mlvl.2$ipv <- as.factor(data.mlvl.2$ipv)
data.mlvl.2$viol_wtn <- as.factor(data.mlvl.2$viol_wtn)

# Antes, ajustamos algunos labels

levels(data.mlvl.2$eth) <- c("Ninguna", "Pertenece")
levels(data.mlvl.2$urban) <- c("Rural", "Urbano")
levels(data.mlvl.2$job) <- c("No trabaja", "Trabaja")
levels(data.mlvl.2$viol_wtn) <- c("No", "Sí")
levels(data.mlvl.2$ipv_att) <- c("Rechaza IPV", "Valida IPV")
levels(data.mlvl.2$decision) <- c("Es autónoma","No es autónoma")

# VERIFICACIÓN DE ETIQUETAS: as.factor() ordena los niveles como "0", "1", de modo que levels<-
# asigna la primera etiqueta al valor 0. En 01_build_individual_data.R, decision = 1 indica que
# la mujer participa en las tres decisiones, e ipv_att = 1 indica que rechaza todas las
# justificaciones de la violencia. Los cruces siguientes muestran qué etiqueta recibió cada valor.
print(table(original = decision_orig, etiqueta = data.mlvl.2$decision))
print(table(original = ipv_att_orig,  etiqueta = data.mlvl.2$ipv_att))

# Dejamos fija la categoría "Ninguna"
data.mlvl.2$cat_eth <- factor(data.mlvl.2$cat_eth)
data.mlvl.2$cat_eth <- relevel(data.mlvl.2$cat_eth, ref = "Ninguna")

# Declaramos el diseño de muestra

dhs_survey = svydesign(id = data.mlvl.2$v021,
                       data = data.mlvl.2,
                       strata = data.mlvl.2$v022,
                       weight = data.mlvl.2$weight,
                       nest = TRUE)

# "adjust" = cuando hay un único psu en v022
options(survey.lonely.psu = "adjust")

svytable(~v044, dhs_survey)  # Verificamos el total de nuestro universo (ponderado) - Coherente con reporte Min


# Miramos cómo se distribuye la muestra en los mpios

data.mlvl.2 %>%
  count(codmpio) %>%
  summary()           # Se observa una gran heterogeneidad, con mpios con muy pocas obs y otros muchas obs, recordar
                      # que la representatividad no es municipal.



# 2. P R O P O R C I O N E S   M U N I C I P A L E S   P O N D E R A D A S
#-----------------------------------------------------------------------------------------------------------------------

ipv_rate <- svyby(
  ~ipv,
  ~codmpio,
  dhs_survey,
  svymean
)

l2_job <- svyby(
  ~job,
  ~codmpio,
  dhs_survey,
  svymean
)

l2_attitude <- svyby(
  ~ipv_att,
  ~codmpio,
  dhs_survey,
  svymean
)


l2_decision <- svyby(
  ~decision,
  ~codmpio,
  dhs_survey,
  svymean
)

l2_Q45 <- svyby(
  ~lvl2_Q45,
  ~codmpio,
  dhs_survey,
  svymean
)


l2_Q5 <- svyby(
  ~lvl2_Q5,
  ~codmpio,
  dhs_survey,
  svymean
)


l2_edusup <- svyby(
  ~lvl2_edusup,
  ~codmpio,
  dhs_survey,
  svymean
)



# 3. B A S E   M U N I C I P A L
#-----------------------------------------------------------------------------------------------------------------------

#Unimos las bases de datos en una solo por municipio y agregamos variables externas contextuales


lvl2_data <- ipv_rate[c("codmpio", "ipv1")] %>%
  left_join(l2_job[c("codmpio", "jobTrabaja")]) %>%
  left_join(l2_attitude[c("codmpio", "ipv_attRechaza IPV")]) %>%
  left_join(l2_decision[c("codmpio", "decisionEs autónoma")]) %>%
  left_join(l2_Q45[c("codmpio", "lvl2_Q45")]) %>%
  left_join(l2_Q5[c("codmpio", "lvl2_Q5")]) %>%
  left_join(l2_edusup[c("codmpio", "lvl2_edusup")]) %>%
  rename(
    ipv = ipv1,
    job = jobTrabaja,
    ipv_att = 'ipv_attRechaza IPV',
    decision = 'decisionEs autónoma'
    )

lvl2_data <- lvl2_data %>%
  rename_with(~ paste0(.x, "_mpal"), -codmpio)


pdet_nbi_ocde_ipm <- data.mlvl.2 %>%
  group_by(codmpio) %>%
  summarise(pdet = first(pdet),
            tasa_nbi = first(tasa_nbi),
            clas_OCDE = first(Clas_OCDE),
            IPM = first(IPM_total),
            part_lab_m = first(prop_muj_trabajo),
            part_lab_h = first(prop_hom_trabajo),
            lvl2_Q45= first(lvl2_Q45),
            lvl2_Q5 = first(lvl2_Q5)
            )

IPV_mpio <- merge(lvl2_data, pdet_nbi_ocde_ipm, by = "codmpio")



# 4. I N C I D E N C I A   M U N I C I P A L   D E   I P V   ( A N Á L I S I S   E S P A C I A L )
#-----------------------------------------------------------------------------------------------------------------------

# Reemplaza el archivo BD_incIPV_mpal.xlsx que leía el análisis espacial. Incidencia sin ponderar:
# casos por cada 100 mujeres encuestadas en el municipio, total y por tipo de violencia.
# Los nombres de las columnas son los que usa 02_spatial_analysis/01_spatial_data.R.

inc_mpio <- data.mlvl.2 %>%
  group_by(codmpio) %>%
  summarise(
    n_mujeres     = n(),
    casos_ipv     = sum(ipv == "1", na.rm = TRUE),
    casos_ipv_f   = sum(ipvf == 1,  na.rm = TRUE),
    casos_ipv_sex = sum(ipvs == 1,  na.rm = TRUE),
    casos_ipv_psi = sum(ipvpsi == 1, na.rm = TRUE),
    casos_ipv_eco = sum(ipveco == 1, na.rm = TRUE)
  ) %>%
  mutate(
    inc_ipv     = 100 * casos_ipv     / n_mujeres,
    inc_ipv_fis = 100 * casos_ipv_f   / n_mujeres,
    inc_ipv_sex = 100 * casos_ipv_sex / n_mujeres,
    inc_ipv_psi = 100 * casos_ipv_psi / n_mujeres,
    inc_ipv_eco = 100 * casos_ipv_eco / n_mujeres
  )

IPV_mpio_completa <- IPV_mpio %>% left_join(inc_mpio, by = "codmpio")



# 5. B A S E   F I N A L   P A R A   D E S C R I P T I V O S   Y   M O D E L O
#-----------------------------------------------------------------------------------------------------------------------

# Exportamos la BD definitiva que incluye IPV_mpal y las demás variables de segundo nivel con pesos
# (se anexa IPV_mpio sin la incidencia de la sección 4, igual que en la versión original)

data.mlvl.2 <- rename(data.mlvl.2, IPM = IPM_total)
data.mlvl.2 <- rename(data.mlvl.2, clas_OCDE = Clas_OCDE)
data.mlvl.2 <- rename(data.mlvl.2, part_lab_m = prop_muj_trabajo)
data.mlvl.2 <- rename(data.mlvl.2, part_lab_h = prop_hom_trabajo)


BD_modelo <- data.mlvl.2 %>%
  left_join(IPV_mpio, by = c("codmpio", "pdet", "IPM", "tasa_nbi", "clas_OCDE", "part_lab_m", "part_lab_h"))



# 6. G U A R D A M O S   L A S   B A S E S
#-----------------------------------------------------------------------------------------------------------------------

# Igual que en 01_build_individual_data.R, replicamos la conversión que ocurría al guardar en Excel
# (sin etiquetas ni atributos, factores como texto), para que los scripts siguientes reciban los
# mismos tipos de dato que antes.

IPV_mpio_completa[] <- lapply(IPV_mpio_completa, function(x) as.vector(haven::zap_labels(x)))
BD_modelo[]         <- lapply(BD_modelo,         function(x) as.vector(haven::zap_labels(x)))

saveRDS(IPV_mpio_completa, file.path(dir_processed, "municipal_data.rds"))
saveRDS(BD_modelo,         file.path(dir_processed, "model_data.rds"))
