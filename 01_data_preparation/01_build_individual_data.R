
#############################################################
##   PROCESAMIENTO DHS COLOMBIA 2015 - BD PARA MULTINIVEL  ##
#############################################################

# Esta versión contiene los siguientes ajustes respecto a la versión inicial:

#   1. Cambio de IPV Emocional como esta en el paper de Ethiopia por IPV Psicológico como está en el reporte del MinSalud

#   2. Inclusión de IPV Económico 

#   3. Eliminación de la variable control marital

#   4. Inclusión de variables de 1er y 2do nivel encontradas en la Revisión de Literatura

#   5. Procesamiento de variables para descriptivas y modelo

#   6. Inclusión de variables sugeridas en sesiones de trabajo

# Educación del esposo que habita en el hogar
# Proporción de mujeres en Q4 y Q5
# Proporción de mujeres en Q5
# Proporción de mujeres con estudios superiores



# P R E P A R A C I Ó N  I N I C I A L 
#---------------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(haven) 
library(janitor)  
library(labelled) 
library(sf)
library(stringr)
library(here)  

# Rutas del proyecto (ver README para la ubicación de cada archivo)
dir_dhs       <- here("00_data", "raw", "dhs")
dir_shp       <- here("00_data", "raw", "shapefiles")
dir_municipal <- here("00_data", "raw", "municipal")
dir_processed <- here("00_data", "processed")

# Cargamos los microdatos del módulo de mujeres (IR) - Individuals (women) Recode
IR <- readRDS(file.path(dir_dhs, "COIR72DT", "COIR72FL.RDS")) %>% clean_names()  # clean names estandariza el formato de nombre de variables

# Cargamos los microdatos del módulo de personas (PR) - Persons Recode
PR <- read_dta(file.path(dir_dhs, "COPR72DT", "COPR72FL.DTA")) %>% clean_names()
            
# Homogeneizamos las variable llave para poder pegar las bases de datos
PR <- PR %>% rename(v001 = hv001, v002 = hv002, v003 = hvidx) # Aquí estamos usando v001 (# cluster), v002 (# hogar)
                                                              #     y v003 (# entrevistado) como llaves    
# Pegamos las bases de datos
IR_BD <- IR %>% left_join(PR, by = c("v001", "v002", "v003"))

# Segundo MERGE para enlazar información de interes de la pareja (esposo/marido) que habita el mismo hogar (limitado a mujeres
# con pareja, cuya pareja esté en el hogar y adicionalmente haya tomado la encuesta)

PR_pareja <- PR %>% 
  transmute(
    v001, 
    v002,
    v034 = v003,
    h_edu_pj = hv106,
    edu_yr_pj = hv107
  )

IR_BD <- IR_BD %>% 
  left_join(PR_pareja, by = c("v001", "v002", "v034"))


# Ajustamos los pesos muestrales
IR_BD$weight <- IR_BD$d005 / 1000000 # Pesos muestrales para el módulo de violencia

# Realizamos un filtro que nos deje solo las mujeres que alguna vez han vivido (o vivan actualmente) con su pareja
# y hayan sido seleccionadas para el módulo de violencia doméstica:


IR_BD <- IR_BD %>% filter(v502 != 0) # excluimos las mujeres que nunca han estado en unión
IR_BD <- IR_BD %>% filter(v044 == 1) # filtramos las mujeres seleccionadas para el módulo de violencia doméstica



# S E L E C C I O N A  M O S   L A S   V A R I A B L E S  R E L E V A N T E S   P A R A   E L  A N Á L I S I S 
#--------------------------------------------------------------------------------------------------------------------------------------

# Listamos las variables de PRIMER NIVEL
var_lvl1 <- c(
  "v044", # Selected for domestic violence module
  "v012", # Respondent's current age
  "v511", # Age at first cohabitation ** (variable sugerida por Linda) **
  "v212", # Age at first birth        ** (variable sugerida por Linda) **
  "v106", # Highest educational level
  "v107", # Highest year of education
  "v133", # Education in single years
  "d121", # Witnessing family violence (respondent's father ever beat her mother)
  "v743a",    # Person who usually decides on respondent's health care (women)
  "v743b",    # Person who usually decides on large household purchases (women)
  "v743d",    # Person who usually decides on visits to family or relatives (women)
  "v744a",    # beating justified if wife goes out without telling husband
  "v744b",    # [ibíd] if wife neglects the children
  "v744c",    # [ibíd] if wife argues with husband
  "v744d",    # [ibíd] if wife refuses to have sex with husband
  "v744e",    # [ibíd] if wife burns food
  "v201",      # Total children ever born
  "v202",      # Sons at home
  "v203",      # Daughters at home
  "v137",      # Number of children 5 and under in household
  "v218",      # Number of living children
  "d101a",     # husband/partner jealous if respondent talks with other men
  "d101b",     # husband/partner accuses respondent of unfaithfulness
  "d101c",     # husband/partner does not permit respondent to meet female friends
  "d101d",     # husband/partner tries to limit respondent's contact with family
  "d101e",     # husband/partner insists on knowing where respondent is
  "d101f",     # Husband/partner doesn't trust respondent with money
  "v025",      # Zona urbana/rural
  "v190",      # Quintile - Wealth index
  "v191",      # Wealth index score
  "v137",      # number of children 5 and under in household (de jure)
  "V190",      # Individual wealth index (women)
  "v191",      # Individual wealth index score (women)
  "v717" ,     # Occupation (grouped)
  "v131",      # Ethnicity (instead of Religion)
  "v501",      # Current marital status
  "v714",      # Currently working
  "h_edu_pj",  # Highest education level of husband / partner living in the household
  "edu_yr_pj"  # Highest education years of husband / partner living in the household
)

# Listamos las variables de SEGUNDO NIVEL
var_lvl2 <- c(
  "shdepto",  # Código de departamento
  "shmunip",  # Código de municipio
  "sharea" ,  # Área 
  "shsubreg", # Subregiones
  "v025"     # Zona urbano/rural
  
)

# R E N O M B R A M O S  y  R E C O D I F I C A M O S  L A S   V A R I A B L E S   D E  1 E R   N I V E L 
#------------------------------------------------------------------------------------------------------------------------------------

IR_BD <- IR_BD %>%
  mutate(
    # Variables demográficas
    age   = v012,
    edu_yr = v133,# años de educación
    eth   = v131, # etnia
    w_qt  = v190, # riqueza en quintiles
    w_sc  = v191, # riqueza en score
    job   = v714, # tiene trabajo actualmente
    h_edu = case_when( # nivel educativo más alto
      v106 == 0 ~ "No edu",  
      v106 == 1 ~ "Prim",
      v106 == 2 ~ "Sec",
      v106 == 3 ~ "Sup"
    ),
    h_edu_pj = case_when( # nivel educativo más alto de pareja en hogar
      h_edu_pj == 0 ~ "No edu",  
      h_edu_pj == 1 ~ "Prim",
      h_edu_pj == 2 ~ "Sec",
      h_edu_pj == 3 ~ "Sup"
    ),
    
    
    # Fertilidad y hogar
    ch_ever = v201, # total hijos e hijas ever born
    ch_sons = v202, # total hijos que viven en el hogar
    ch_daus = v203, # total hijas que viven en el hogar
    ch_u5   = v137, # total niños < 5años
    ch_live = v218, # total hijos e hijas vivos
    
    # Variables sugeridas por Linda 
    age_uni   = v511, # edad a la primera unión (matrimonio o cohabitación)
    age_fborn = v212, # edad al primer nacimiento
    
    
    # Violencia Familiar
    viol_wtn = factor(d121, levels = c(0, 1, 8),     # Testigo de violencia de padre a madre
                      labels = c("No", "Yes", "DK")),
    
    # Empoderamiento: toma de decisiones (1 = independiente)
    dec_hlth = if_else(v743a %in% c(1, 2), 1, 0), # decisiones en salud 
    dec_buy  = if_else(v743b %in% c(1, 2), 1, 0), # decisiones en compras grandes
    dec_mv   = if_else(v743d %in% c(1, 2), 1, 0), # decisiones en visitas a flia
    
    # Empoderamiento: actitudes frente a IPV (1 = en desacuerdo IPV)
    att_food = if_else(is.na(v744e) | v744e != 1, 1, 0), # golpear por dejar quemar la comida
    att_arg  = if_else(is.na(v744c) | v744c != 1, 1, 0), # golpear por ella discutir con él
    att_out  = if_else(is.na(v744a) | v744a != 1, 1, 0), # golpear por salir sin decirle
    att_neg  = if_else(is.na(v744b) | v744b != 1, 1, 0), # golpear por descuidar a los niños
    att_sex  = if_else(is.na(v744d) | v744d != 1, 1, 0), # golpear por rechazar tener sexo con él
    
    # Geografía
    dept  = shdepto,
    muni  = shmunip,
    reg   = shsubreg,
    area  = sharea,
    urban = if_else(v025 == 1, 1, 0),
    
    # Estatus marital
    marital_status = v501,
    marital_status_cat = case_when(
      v501 == 1 ~ "Casada",
      v501 == 2 ~ "Unión libre",
      v501 %in% c(0, 3, 4, 5) ~ "Otro"
    )
    
  )

 
# C R E A M O S  N U E V A S   V A R I A B L E S   D E   1 E R  N I V E L   A   P A R T I R   D E   L A   B D 
#-------------------------------------------------------------------------------------------------------------------------------------

# Decision-making variable
IR_BD$decision <- ifelse(IR_BD$dec_hlth == 1 &      # La mujer tiene autonomía en sus decisiones (1). Todas las variables = 1
                        IR_BD$dec_buy == 1 &
                        IR_BD$dec_mv == 1, 1, 0)

# IPV attitudes
IR_BD$ipv_att <- ifelse(IR_BD$att_food == 1 &         # La mujer está en desacuerdo con actitudes violentas (1). Todas las variables =1
                          IR_BD$att_arg == 1 &
                          IR_BD$att_out == 1 &
                          IR_BD$att_neg == 1 &
                          IR_BD$att_sex == 1, 1, 0)



# D E F I N I M O S   L A S   V A R I A B L E S  D E   2 D O   N I V E L - AGREGACIÓN DE VARIABLES INDIVIDUALES
#--------------------------------------------------------------------------------------------------------------------------------

# Proporción de mujeres involucradas en toma de decisiones del hogar por MUNICIPIO 

ir.decision <- IR_BD %>% group_by(shmunip) %>%    # agrupa la base por mpio
  summarize(den = n(),                            # reduce cada grupo (mpio) a una sola fila
            lvl2_decision = sum(decision)/den)    # crea la variable de proporción


# Proporción de mujeres con una actitud de rechazo a IPV por MUNICIPIO 

ir.attitude <- IR_BD %>% group_by(shmunip) %>%
  summarize(den = n(),
            lvl2_attitude = sum(ipv_att)/den)

# Proporción de mujeres que trabajan por MUNICIPIO 

 ir.job <- IR_BD %>% group_by(shmunip) %>%             
  summarize(den = n(),                                         
             lvl2_job = sum(job)/den)    
 
 # Proporción de mujeres con educación superior por MUNICIPIO 
 
 ir.edusup <- IR_BD %>% group_by(shmunip) %>%             
   summarize(den = n(),                                         
             lvl2_edusup = sum(h_edu == "Sup")/den)  
 

  # Proporción de mujeres con solo Q5 de riqueza por MUNICIPIO 
 
 #ir.Q5_m <- IR_BD %>% group_by(shmunip) %>%             
#   summarize(den = n(),                                         
#             lvl2_Q5 = sum(w_qt == 5)/den)  
 
 
 # Cargamos la base de datos del hogar para calcular la proporción de hogares en Q5 de riqueza por municipio
 
 HR <- readRDS(file.path(dir_dhs, "COHR72DT", "COHR72FL.RDS")) %>% clean_names()
 
 HR <- HR %>% 
   mutate(
     Q5 = ifelse(hv270 == 5, 1, 0),
     Q45 = ifelse(hv270 %in% c(4, 5), 1, 0)
   )
 
ir.Q5 <- HR %>% group_by(shmunip) %>% 
   summarize(lvl2_Q5 = mean(Q5, na.rm = TRUE),
             den = n())
   
   
 # Proporción de mujeres en Q4 y Q5 de riqueza por MUNICIPIO 
   
ir.Q45 <- HR %>% group_by(shmunip) %>%             
   summarize(lvl2_Q45 = mean(Q45, na.rm = TRUE),
             den = n())  


# Promedio del índice de riqueza por MUNICIPIO

ir.wealth <- IR_BD %>% group_by(shmunip) %>%
  summarize(lvl2_wealth = mean(w_sc))

# Unimos las variables de 2do nivel en un solo dataset por MUNICIPIO 

lvl2.ir <- ir.decision[c("shmunip", "lvl2_decision")] %>%
  left_join(ir.attitude[c("shmunip", "lvl2_attitude")]) %>%
  left_join(ir.wealth[c("shmunip", "lvl2_wealth")]) %>% 
  left_join(ir.job[c("shmunip", "lvl2_job")]) %>% 
  left_join(ir.edusup[c("shmunip", "lvl2_edusup")]) %>% 
  left_join(ir.Q45[c("shmunip", "lvl2_Q45")]) %>% 
  left_join(ir.Q5[c("shmunip", "lvl2_Q5")])



  
# A N E X A M O S   L AS   V A R I A B L ES   D E  2 D O   N I V E L   A   N U ES T R A  B D   I N D I V I D U A L 
#--------------------------------------------------------------------------------------------------------------------- 

# Unimos la data de 2do y 1er nivel por municipio en otra BD nueva
lvl12_ir <- IR_BD %>% left_join(lvl2.ir, by = "shmunip")


# E S T I M A M O S  L A S   T A S A S  D E L   I P V 
#---------------------------------------------------------------------------------------------------------------------

# Variables para estimar el IPV FÍSICO

ipv_fisico = c("d105a", # Esposo la empujó, sacudió o lanzó algo
               "d105b", # Esposo la abofeteó
               "d105c", # Esposo le golpeó con el puño o algo dañino
               "d105d", # Esposo la pateó o arrastró
               "d105e", # Esposo intentó estrangularla o quemarla
               "d105f", # Esposo la amenazó con cuchillo, arma de fuego u otra arma
               "d105g"  # Esposo la atacó con cuchillo, arma de fuego u otra arma
)


lvl12_ir <- lvl12_ir %>%
  mutate(
    pushed     = as.numeric(ifelse(!is.na(d105a), ifelse(d105a %in% c(1, 2), 1, 0), NA)),
    slapped    = as.numeric(ifelse(!is.na(d105b), ifelse(d105b %in% c(1, 2), 1, 0), NA)),
    punched    = as.numeric(ifelse(!is.na(d105c), ifelse(d105c %in% c(1, 2), 1, 0), NA)),
    kicked     = as.numeric(ifelse(!is.na(d105d), ifelse(d105d %in% c(1, 2), 1, 0), NA)),
    strangled  = as.numeric(ifelse(!is.na(d105e), ifelse(d105e %in% c(1, 2), 1, 0), NA)),
    threatened = as.numeric(ifelse(!is.na(d105f), ifelse(d105f %in% c(1, 2), 1, 0), NA)),
    attacked   = as.numeric(ifelse(!is.na(d105g), ifelse(d105g %in% c(1, 2), 1, 0), NA))
  ) %>%
  set_value_labels(
    pushed     = c(yes = 1, no = 0),
    slapped    = c(yes = 1, no = 0),
    punched    = c(yes = 1, no = 0),
    kicked     = c(yes = 1, no = 0),
    strangled  = c(yes = 1, no = 0),
    threatened = c(yes = 1, no = 0),
    attacked   = c(yes = 1, no = 0)
  )


# Variables usadas para estimar IPV SEXUAL

ipv_sexual = c("d105h", # ever been physically forced into unwanted sex by husband/partner
               "d105i", # ever been forced into other unwanted sexual acts by husband/partner, NO EXISTE NA
               "d105k" # ever been physically forced to perform sexual acts respondent didn't want t. NO EXISTE NA
)


lvl12_ir <- lvl12_ir %>%
  mutate(
    ph_unwanted     = as.numeric(ifelse(!is.na(d105h), ifelse(d105h %in% c(1, 2), 1, 0), NA)),
  ) %>%
  set_value_labels(
    ph_unwanted     = c(yes = 1, no = 0),
  )


# Variables usadas para estimar IPV PSICOLÓGICO

ipv_psi <- c("d101a",# Esposo celoso
            "d101b", # Esposo la acusa de infidelidad
            "d101c", # Esposo no le permite verse con amigas
            "d101d", # Esposo limita su tiempo con familia
            "d101e", # Esposo quiere saber todo el tiempo donde está
            "d101g", # Esposo la ignora
            "d101h", # Esposo no cuenta con ella para reuniones familiares
            "d101i", # Esposo la excluye de decisiones importantes
            "d103c", # Esposo le habla en términos desobligantes
            "d103d", # Esposo amenaza con dejarla 
            "d103e"  # Esposo amenaza con llevarse a los niños
)


lvl12_ir <- lvl12_ir %>%
  mutate(
    jealous          = as.numeric(ifelse(!is.na(d101a), ifelse(d101a %in% c(1, 2), 1, 0), NA)),
    accs_unfaithful  = as.numeric(ifelse(!is.na(d101b), ifelse(d101b %in% c(1, 2), 1, 0), NA)),
    no_friends       = as.numeric(ifelse(!is.na(d101c), ifelse(d101c %in% c(1, 2), 1, 0), NA)),
    limits_fmly      = as.numeric(ifelse(!is.na(d101d), ifelse(d101d %in% c(1, 2), 1, 0), NA)),
    cntrl_location   = as.numeric(ifelse(!is.na(d101e), ifelse(d101e %in% c(1, 2), 1, 0), NA)),
    ignores          = as.numeric(ifelse(!is.na(d101g), ifelse(d101g %in% c(1, 2), 1, 0), NA)),
    no_social        = as.numeric(ifelse(!is.na(d101h), ifelse(d101h %in% c(1, 2), 1, 0), NA)),
    excludes         = as.numeric(ifelse(!is.na(d101i), ifelse(d101i %in% c(1, 2), 1, 0), NA)),
    insulted         = as.numeric(ifelse(!is.na(d103c), ifelse(d103c %in% c(1, 2), 1, 0), NA)),
    th_leave         = as.numeric(ifelse(!is.na(d103d), ifelse(d103d %in% c(1, 2), 1, 0), NA)),
    th_children      = as.numeric(ifelse(!is.na(d103e), ifelse(d103e %in% c(1, 2), 1, 0), NA))
    
  ) %>%
  set_value_labels(
    jealous          = c(yes = 1, no = 0),
    accs_unfaithful  = c(yes = 1, no = 0),
    no_friends       = c(yes = 1, no = 0),
    limits_fmly      = c(yes = 1, no = 0),
    cntrl_location   = c(yes = 1, no = 0),
    ignores          = c(yes = 1, no = 0),
    no_social        = c(yes = 1, no = 0),
    excludes         = c(yes = 1, no = 0),
    insulted         = c(yes = 1, no = 0),
    th_leave         = c(yes = 1, no = 0),
    th_children      = c(yes = 1, no = 0)
  )



# Variables usadas para estimar IPV ECONÓMICO


ipv_eco <- c("s1205b", # Esposo amenazó con retirar apoyo económico
             "s1205c", # Esposo prohibió estudiar/trabajar
             "s1205d", # Esposo gastó el dinero del hogar
             "s1205e" # Esposo se adueño de dinero o bienes
)


lvl12_ir <- lvl12_ir %>%
  mutate(
    no_eco_sp        = as.numeric(ifelse(!is.na(s1205b), ifelse(s1205b %in% c(1, 2), 1, 0), NA)),
    no_study_work    = as.numeric(ifelse(!is.na(s1205c), ifelse(s1205c %in% c(1, 2), 1, 0), NA)),
    spent_housemoney = as.numeric(ifelse(!is.na(s1205d), ifelse(s1205d %in% c(1, 2), 1, 0), NA)),
    took_money       = as.numeric(ifelse(!is.na(s1205e), ifelse(s1205e %in% c(1, 2), 1, 0), NA))
    
  ) %>%
  set_value_labels(
    no_eco_sp          = c(yes = 1, no = 0),
    no_study_work  = c(yes = 1, no = 0),
    spent_housemoney       = c(yes = 1, no = 0),
    took_money      = c(yes = 1, no = 0)
  )


# Generamos la variable para IPV GENERAL (cualquier tipo de IPV) 

lvl12_ir <- lvl12_ir %>%
  mutate(
    ipvf = case_when(
      rowSums(across(c(pushed, slapped, punched, # suma las filas donde =1, las suma y si > 0 entonces asigna 1
                       kicked, strangled, threatened, attacked), ~ . == 1), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(c(pushed, slapped, punched, 
                       kicked, strangled, threatened, attacked), ~ !is.na(.))) == 0 ~ NA_real_, 
      TRUE ~ 0                                                                                  
    ),                                                                                          
    
    ipvs = case_when(
      rowSums(across(c(ph_unwanted), ~ . == 1), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(c(ph_unwanted), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    ipvpsi = case_when(
      rowSums(across(c(jealous, accs_unfaithful, no_friends, limits_fmly, cntrl_location, ignores,
                       no_social, excludes, insulted, th_leave, th_children), ~ . == 1), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(c(jealous, accs_unfaithful, no_friends, limits_fmly, cntrl_location, ignores,
                       no_social, excludes, insulted, th_leave, th_children), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    ipveco = case_when(
      rowSums(across(c(no_eco_sp, no_study_work, spent_housemoney, took_money), ~ . == 1), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(c(no_eco_sp, no_study_work, spent_housemoney, took_money), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    ipv = case_when(
      rowSums(across(c(ipvf, ipvs, ipvpsi, ipveco), ~ . == 1), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(c(ipvf, ipvs, ipvpsi, ipveco), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
    
  )


# V A R I A B L E S  P A R A  M O D E L O   M U L T I N I V E L
#------------------------------------------------------------------------------------------------------------------------------------

# Anexamos los codigos DIVIPOLA a nuesta BD

mun_shape <- st_read(dsn = file.path(dir_shp, "DANE_geodata"),
                     "MGN_ANM_MPIOS") %>% select(
                       DPTO_CCDGO,
                       MPIO_CCDGO,
                       MPIO_CNMBR,
                       MPIO_CDPMP,
                       AREA, LATITUD,
                       LONGITUD,
                       Shape_Area,
                       geometry
                     ) %>% rename(cod_mun = MPIO_CDPMP)


mun_shape$cod_mun = as.numeric(mun_shape$cod_mun)

lvl12_ir <- lvl12_ir %>% mutate(  
  mun3d = sprintf("%03d", muni), 
  mun = paste0(dept, mun3d),    
  cod_mun = as.numeric(mun)       
)

multilevel_dataset = merge(lvl12_ir, mun_shape[c("cod_mun", "MPIO_CNMBR")], 
                           by = "cod_mun", all.x = T)

# Ad-hoc: añadir ocupación y etnia en categorías
multilevel_dataset$occup = multilevel_dataset$v717
multilevel_dataset$cat_eth = multilevel_dataset$v131


# Select final variables for multilevel dataset
multilevel_dataset <- multilevel_dataset[c("v001", "v002", "v003", "v021", "v022", "v044", "cat_eth",
                                           "age_uni", "weight",
                                           "marital_status_cat", "age", "h_edu",
                                           "edu_yr", "viol_wtn", "eth", 
                                           "occup", "ch_sons", "ch_daus", 
                                           "w_qt", "w_sc", "dept",
                                           "muni", "MPIO_CNMBR", "area", "reg", "urban", 
                                           "decision", "ipv_att",
                                           "lvl2_decision", "lvl2_attitude", 
                                           "lvl2_wealth", "job", "lvl2_job", "ipvf", 
                                           "ipvs", "ipvpsi", "ipveco", "ipv",
                                           "lvl2_edusup", "lvl2_Q45", "lvl2_Q5", "h_edu_pj")]


# Procesamos variables

# Supuesto: si "don't know", entonces "No" (Variable de testigo de violencia)
multilevel_dataset$viol_wtn[multilevel_dataset$viol_wtn == "DK"] = "No"

# Recodificación de la variable etnia - dejamos si se reconoce con algun grupo etnico = 1, de otra forma = 0
multilevel_dataset$eth[multilevel_dataset$eth == 6] = 0
multilevel_dataset$eth[multilevel_dataset$eth != 0] = 1

# Ajustamos los labels de la categoría de etnia

multilevel_dataset$cat_eth <- to_factor(
  labelled(
    multilevel_dataset$cat_eth,
    labels = val_labels(multilevel_dataset$cat_eth)
  )
)

levels(multilevel_dataset$cat_eth) <- c(
  "Indígena",
  "Rom",
  "Raizal",
  "Palenq",
  "Afro",
  "Ninguna"
)


# Categorizar las edades
multilevel_dataset = multilevel_dataset %>%
  mutate(age_cat = case_when(
    age < 18 ~ "<18",
    age >= 18 & age < 26 ~ "[18, 26)",
    age >= 26 ~ ">26"
  ),
  sons_daus = ch_sons + ch_daus,  # Sumamos el total de hijos en el hogar
  age_q = case_when(
    age < 18 ~ "13-17",
    age >= 18 & age <= 22 ~ "18-22",
    age >= 23 & age <= 27 ~ "23-27",
    age >= 28 & age <= 32 ~ "28-32",
    age >= 33 & age <= 37 ~ "33-37",
    age >= 38 & age <= 42 ~ "38-42",
    age >= 43 ~ "43-49")
  ) 


# Crear la variable de municipio - Esto lo hacemos para poder pegar luego con nbi
multilevel_dataset$depto = multilevel_dataset$dept*1000
multilevel_dataset$codmpio = multilevel_dataset$depto + multilevel_dataset$muni


# Cargamos la base de datos de NBI 

nbi = readxl::read_excel(file.path(dir_municipal, "NBI_MUN_CNPV2018.xlsx"))
nbi <- nbi %>%  
  mutate(
    tasa_nbi = as.numeric(str_replace(tasa_nbi, ",", ".")),
    codmpio = cod
  ) %>% select(codmpio, tasa_nbi)

# Unimos la info de NBI a nuestra BD

multilevel_dataset <- multilevel_dataset %>% left_join(nbi, by = "codmpio")


# Ahora agregamos información de MPIOS PDET

pdet = readxl::read_excel(file.path(dir_municipal, "MPIOS_PDET.xlsx"))

pdet <- pdet %>% 
  select(codmpio, pdet)

multilevel_dataset <- multilevel_dataset %>% left_join(pdet, by = "codmpio")

multilevel_dataset$pdet[is.na(multilevel_dataset$pdet)] <- 0



# Guardamos nuestra base de datos

# Antes se guardaba en Excel y los scripts siguientes leían ese archivo. Al exportar a Excel se
# perdían las etiquetas de haven y los factores pasaban a texto. Para que los scripts siguientes
# reciban exactamente los mismos tipos de dato que antes, replicamos esa conversión antes de
# guardar en RDS: se eliminan etiquetas y atributos, y los factores quedan como texto.

multilevel_dataset[] <- lapply(multilevel_dataset, function(x) as.vector(zap_labels(x)))

saveRDS(multilevel_dataset, file.path(dir_processed, "individual_data.rds"))

