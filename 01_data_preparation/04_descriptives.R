
#############################################
##  ESTADÍSTICAS DESCRIPTIVAS               ##
#############################################

# Este script reúne el script de estadísticas descriptivas para el modelo y el de tablas para el
# documento de métodos y resultados. La construcción de variables municipales que estaba al final
# del primero pasó a 03_build_municipal_aggregates.R.

# Secciones:
#   1. Variables de nivel 1: tablas por condición de IPV y pruebas exploratorias por variable
#   2. Pruebas de normalidad
#   3. Pruebas de diferencias entre grupos (IPV vs. no IPV)
#   4. Variables de nivel 2: descriptivas y exploración
#   5. Correlaciones de Spearman y matriz de correlaciones

# Las tablas se exportan en CSV y las figuras en PNG a output/01_data_preparation/.


library(tidyverse)
library(table1)
library(survey)
library(rcompanion)
library(gtsummary)
library(tseries)
library(gt)
library(patchwork)
library(DescTools)
library(ggpubr)
library(corrplot)
library(ggcorrplot)
library(here)

# Rutas del proyecto
dir_processed <- here("00_data", "processed")
dir_out       <- here("output", "01_data_preparation")


# Importamos nuestros datasets (salidas de 03_build_municipal_aggregates.R)
data.mlvl.2 <- readRDS(file.path(dir_processed, "model_data.rds"))
BD_IPV_mpal <- readRDS(file.path(dir_processed, "municipal_data.rds"))
IPV_mpio    <- BD_IPV_mpal

data.mlvl.2$w_qt <- as.factor(data.mlvl.2$w_qt)
data.mlvl.2$job <- as.factor(data.mlvl.2$job)
data.mlvl.2$ipv_att <- as.factor(data.mlvl.2$ipv_att)
data.mlvl.2$decision <- as.factor(data.mlvl.2$decision)
data.mlvl.2$urban <- as.factor(data.mlvl.2$urban)
data.mlvl.2$pdet <- as.factor(data.mlvl.2$pdet)
data.mlvl.2$eth <- as.factor(data.mlvl.2$eth)
data.mlvl.2$ipv <- as.factor(data.mlvl.2$ipv)
data.mlvl.2$viol_wtn <- as.factor(data.mlvl.2$viol_wtn)

# Dejamos fija la categoría "Ninguna"
data.mlvl.2$cat_eth <- as.factor(data.mlvl.2$cat_eth)
data.mlvl.2$cat_eth <- relevel(
  data.mlvl.2$cat_eth,
  ref = "Ninguna"
)

# Declaramos el diseño de muestra

dhs_survey = svydesign(id = data.mlvl.2$v021,
                       data = data.mlvl.2,
                       strata = data.mlvl.2$v022,
                       weight = data.mlvl.2$weight,
                       nest = TRUE)

# "adjust" = cuando hay un único psu en v022
options(survey.lonely.psu = "adjust")



#######################################################################################################

# 1.  V A R I A B L E S   D E   N I V E L   1


#   DEMOGRÁFICAS ----------------


tab_demo_cat <- tbl_svysummary(
  dhs_survey,
  by = ipv,
  include = c(marital_status_cat,
              age_q,
              #eth,
              cat_eth,
              urban,
              sons_daus,
              age),
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ " {n} ({p}%)"
    ),
  percent = "column",
  digits = list(
    all_categorical() ~ c(0,1),
    all_continuous() ~ 1
    ),
  type = all_dichotomous() ~ "categorical",
  label = list(
    marital_status_cat ~ "Estado civil",
    age_q ~ "Edad (categorías)",
    #eth ~ "Pertenencia étnica",
    cat_eth ~ "Etnicidad",
    urban ~ "Área",
    sons_daus ~ "Número de hijos",
    age ~ "Edad (años)"
  )
  ) %>%
  add_overall()  %>%
  modify_header(
    label ~ "**Variable**",
    stat_1 ~ "**No IPV**",
    stat_2 ~ "**IPV**",
    stat_0 ~ "**Total**<br>N = {style_number(N, digits = 0)}"
  ) %>%
  modify_table_body(
    ~ dplyr::relocate(.x, stat_0, .after = last_col())) %>%
  modify_footnote(
    all_stat_cols() ~ "**n (%); Mediana (Q1-Q3).** Porcentajes calculados por columna."

  )

tab_demo_cat

# Columnas del CSV: label = variable; stat_1 = No IPV; stat_2 = IPV; stat_0 = Total
write_csv(as_tibble(tab_demo_cat, col_labels = FALSE), file.path(dir_out, "01_descriptivas_sociodemograficas.csv"))

# Estado civil
svychisq(~marital_status_cat + ipv, dhs_survey)
marital_status_tab <- table(data.mlvl.2$marital_status_cat, data.mlvl.2$ipv)
cramerV(marital_status_tab)
mod_ms <- glm(ipv ~ marital_status_cat, family = binomial, data = data.mlvl.2)
summary(mod_ms)

# Edad categorizada
svychisq(~age_cat + ipv, dhs_survey)
age_cat_tab <- table(data.mlvl.2$age_cat, data.mlvl.2$ipv)
cramerV(age_cat_tab)
mod_age <- glm(ipv ~ age_cat, family = binomial, data = data.mlvl.2)
summary(mod_age)

# Edad
svyttest(age ~ ipv, dhs_survey)
wilcox.test(age ~ ipv, data = data.mlvl.2)


# Categoría Etnica

svychisq(~cat_eth + ipv, dhs_survey)
cat_eth_tab <- table(data.mlvl.2$cat_eth, data.mlvl.2$ipv)
cramerV(cat_eth_tab)
mod_cat_eth <- glm(ipv ~ cat_eth, family = binomial, data = data.mlvl.2)
summary(mod_cat_eth)


# Vivienda urbana
svychisq(~urban + ipv, dhs_survey)
urban_tab <- table(data.mlvl.2$urban, data.mlvl.2$ipv)
cramerV(urban_tab)
mod_urban <- glm(ipv ~ urban, family = binomial, data = data.mlvl.2)
summary(mod_urban)


# Hijos en el hogar
svyttest(sons_daus ~ ipv, design = dhs_survey)
wilcox.test(sons_daus ~ ipv, data = data.mlvl.2) # Para variables no normales (evalua si las distribuciones son similares en ambos grupos)
boxplot(sons_daus ~ ipv, data = data.mlvl.2)
mod_sonsdaus <- glm(ipv ~ sons_daus, family = binomial, data = data.mlvl.2)
summary(mod_sonsdaus)

ggplot(data.mlvl.2, aes(x = sons_daus, weight = weight, fill = factor(ipv))) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
  labs(
    title = "Distribución de hijos en el hogar según IPV",
    x = "Número de hijos",
    fill = "IPV"
  )

ggplot(data.mlvl.2, aes(x = sons_daus, weight = weight)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~ipv) +
  labs(title = "Distribución de hijos en el hoga por IPV")



#   ECONÓMICAS ---------------


tab_eco <- tbl_svysummary(
  dhs_survey,
  by = ipv,
  include = c(w_qt, h_edu, job, h_edu_pj),
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ " {n} ({p}%)"
  ),
  percent = "column",
  digits = list(
    all_categorical() ~ c(0,1),
    all_continuous() ~ 1
  ),
  type = all_dichotomous() ~ "categorical",
  label = list(
    w_qt ~ "Quintiles de Riqueza",
    h_edu ~ "Nivel Max Educación",
    job ~ "Situación Laboral",
    h_edu_pj ~ "Nivel Max Educación Pareja"
  )
) %>%
  add_overall()  %>%
  modify_table_body(
    ~ dplyr::relocate(.x, stat_0, .after = last_col())) %>%
  modify_header(
    label ~ "**Variable**",
    stat_1 ~ "**No IPV**",
    stat_2 ~ "**IPV**",
    stat_0 ~ "**Total**<br>N = {style_number(N, digits = 0)}"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "**n (%).** Porcentajes calculados por columna."
  )

tab_eco

write_csv(as_tibble(tab_eco, col_labels = FALSE), file.path(dir_out, "01_descriptivas_economicas.csv"))

# Quintiles de riqueza
svychisq(~w_qt + ipv, dhs_survey)
w_qt_tab <- table(data.mlvl.2$w_qt, data.mlvl.2$ipv)
cramerV(w_qt_tab)
mod_w_qt <- glm(ipv ~ w_qt, family = binomial, data = data.mlvl.2)
summary(mod_w_qt)

# Nivel max de educación
svychisq(~h_edu + ipv, dhs_survey)
h_edu_tab <- table(data.mlvl.2$h_edu, data.mlvl.2$ipv)
cramerV(h_edu_tab)
mod_h_edu <- glm(ipv ~ h_edu, family = binomial, data = data.mlvl.2)
summary(mod_h_edu)

# Nivel max de educación PAREJA

svychisq(~h_edu_pj + ipv, dhs_survey)
h_edu_pj_tab <- table(data.mlvl.2$h_edu_pj, data.mlvl.2$ipv)
cramerV(h_edu_pj_tab)
mod_h_edu <- glm(ipv ~ h_edu, family = binomial, data = data.mlvl.2)
summary(mod_h_edu)

# Trabaja actualmente
svychisq(~job + ipv, dhs_survey)
job_tab <- table(data.mlvl.2$job, data.mlvl.2$ipv)
cramerV(job_tab)
mod_job <- glm(ipv ~ job, family = binomial, data = data.mlvl.2)
summary(mod_job)


#   NORMAS -----------------------


tab_nor <- tbl_svysummary(
  dhs_survey,
  by = ipv,
  include = c(viol_wtn, ipv_att, decision, age_uni),
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ " {n} ({p}%)"
  ),
  percent = "column",
  digits = list(
    all_categorical() ~ c(0,1),
    all_continuous() ~ 1
  ),
  type = all_dichotomous() ~ "categorical",
  label = list(
    viol_wtn ~ "Presenció IPV de padre a madre en la infancia",
    ipv_att ~ "Actitud frente al IPV",
    decision ~ "Autonomía en toma de decisiones",
    age_uni ~ "Edad a la primera unión"
  )
) %>%
  add_overall()  %>%
  modify_table_body(
    ~ dplyr::relocate(.x, stat_0, .after = last_col())) %>%
  modify_header(
    label ~ "**Variable**",
    stat_1 ~ "**No IPV**",
    stat_2 ~ "**IPV**",
    stat_0 ~ "**Total**<br>N = {style_number(N, digits = 0)}"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "**n (%); Mediana (Q1-Q3).** Porcentajes calculados por columna."

  )

tab_nor

write_csv(as_tibble(tab_nor, col_labels = FALSE), file.path(dir_out, "01_descriptivas_normas.csv"))

# Testigo de violencia
svychisq(~viol_wtn + ipv, dhs_survey)
viol_wtn_tab <- table(data.mlvl.2$viol_wtn, data.mlvl.2$ipv)
cramerV(viol_wtn_tab)
mod_viol_wtn <- glm(ipv ~ viol_wtn, family = binomial, data = data.mlvl.2)
summary(mod_viol_wtn)

# Actitudes hacia IPV
svychisq(~ipv_att + ipv, dhs_survey)
ipv_att_tab <- table(data.mlvl.2$ipv_att, data.mlvl.2$ipv)
cramerV(ipv_att_tab)
mod_ipv_att <- glm(ipv ~ ipv_att, family = binomial, data = data.mlvl.2)
summary(mod_ipv_att)

# Edad a la que cohabitó por primera vez
svyttest(age_uni ~ ipv, design = dhs_survey)
wilcox.test(age_uni ~ ipv, data = data.mlvl.2)
boxplot(age_uni ~ ipv, data = data.mlvl.2)
mod_age_uni <- glm(ipv ~ age_uni, family = binomial, data = data.mlvl.2)
summary(mod_age_uni)

ggplot(data.mlvl.2, aes(x = age_uni, weight = weight, fill = factor(ipv))) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
  labs(
    title = "Distribución de edad a la primera unión según IPV",
    x = "Edad",
    fill = "IPV"
  )

ggplot(data.mlvl.2, aes(x = age_uni, weight = weight)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  facet_wrap(~ipv) +
  labs(title = "Distribución de edad a la primera unión por IPV")


# Autonomía en la toma de decisiones
svychisq(~decision + ipv, dhs_survey)
decision_tab <- table(data.mlvl.2$decision, data.mlvl.2$ipv)
cramerV(decision_tab)
mod_decision <- glm(ipv ~ decision, family = binomial, data = data.mlvl.2)
summary(mod_decision)



#######################################################################################################

# 2.  P R U E B A S   D E   N O R M A L I D A D


var_cuanti<- c("age", "sons_daus", "age_uni", "IPM" ,
                "ipv_att_mpal", "decision_mpal",
               "part_lab_m", "lvl2_Q45_mpal",
               "lvl2_edusup_mpal")

tab_norm <- lapply(var_cuanti, function(x){   # Cuadro de resultados del test

  test <- jarque.bera.test(data.mlvl.2[[x]])

  data.frame(
    Variable = x,
    `Estadístico JB` = round(as.numeric(test$statistic), 2),
    `Valor P` = format.pval(test$p.value, digits = 3, eps = .001)
  )

}) %>%
  bind_rows()

tab_norm$Variable <- c(   # Cambiamos labels
  "Edad (años)",
  "Número de hijos",
  "Edad a la primera cohabitación",
  "Indice de Pobreza Multidimensional (IPM)",
  "Actitudes IPV mpal",
  "Autonomía mpal",
  "Participación laboral (Mujeres)",
  "Prop. hogares más ricos (Q4 y Q5)",
  "Prop. mujeres con educación superior"
)

write_csv(tab_norm, file.path(dir_out, "02_normalidad_jarque_bera.csv"))

tabla_gt <- tab_norm %>%   # Tabla para visualizar

  gt() %>%

  tab_header(
    title = md("**Pruebas de normalidad para variables continuas**")
  ) %>%

  cols_label(
    Variable = "Variable"
  ) %>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%

  cols_align(
    columns = everything()
  ) %>%

  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(15),
    data_row.padding = px(4)
  )

tabla_gt



# Análisis gráfico - Histogramas


var_labels <- c(   # Definimos labels
  "Edad (años)",
  "Número de hijos",
  "Edad a la primera cohabitación",
  "Indice de Pobreza Multidimensional (IPM)",
  "Actitudes IPV mpal",
  "Autonomía mpal",
  "Participación laboral (Mujeres)",
  "Prop. hogares más ricos (Q4 y Q5)",
  "Prop. mujeres con educación superior"
)

plots_hist <- lapply(seq_along(var_cuanti), function(i){ # Graficamos histogramas para cada variable

  ggplot(data.mlvl.2, aes(x = .data[[var_cuanti[i]]])) +

    geom_histogram(
      bins = 30,
      color = "white",
      fill = "#3182bd"
    ) +

    labs(
      title = var_labels[i],
      x = NULL,
      y = NULL
    ) +

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5
      ),
      axis.text = element_text(size = 8)
    )

})

p_hist <- wrap_plots(plots_hist, ncol = 2) # Mostramos el grid de histogramas
p_hist
ggsave(file.path(dir_out, "02_histogramas.png"), p_hist, width = 9, height = 12, dpi = 300, bg = "white")



# Análisis gráfico - QQPlots

plots_qq <- lapply(seq_along(var_cuanti), function(i){

  ggplot(data.mlvl.2, aes(sample = .data[[var_cuanti[i]]])) +

    stat_qq(size = 1, alpha = 0.5) +

    stat_qq_line(color = "red") +

    labs(
      title = var_labels[i],
      x = "C. Teo",
      y = "C. Obs"
    ) +

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5
      ),
      axis.text = element_text(size = 8)
    )

})

p_qq <- wrap_plots(plots_qq, ncol = 2)
p_qq
ggsave(file.path(dir_out, "02_qqplots.png"), p_qq, width = 9, height = 12, dpi = 300, bg = "white")



#######################################################################################################

# 3.  P R U E B A S   D E   I N D E P E N D E N C I A   E N T R E   G R U P O S   ( I P V   V S   N O   I P V )


# Variables cuantitativas

tab_cuanti <- lapply(var_cuanti, function(x){

  formula <- as.formula(paste(x, "~ ipv"))

  test_t <- svyttest(formula, design = dhs_survey)

  test_w <- wilcox.test(formula, data = data.mlvl.2)   # antes data = dhs_survey, que genera error

  data.frame(
    Variable = x,

    t_svy = round(unname(test_t$statistic), 2),

    p_t = format.pval(
      test_t$p.value,
      digits = 3,
      eps = .001
    ),

    W_MW = round(unname(test_w$statistic), 2),

    p_MW = format.pval(
      test_w$p.value,
      digits = 3,
      eps = .001
    )
  )
}) %>%
  bind_rows()

tab_cuanti$Variable <- c(
  "Edad (años)",
  "Número de hijos",
  "Edad a la primera cohabitación",
  "Indice de Pobreza Multidimensional (IPM)",
  "Actitudes IPV mpal",
  "Autonomía mpal",
  "Participación laboral (Mujeres)",
  "Prop. hogares más ricos (Q4 y Q5)",
  "Prop. mujeres con educación superior"
)

write_csv(tab_cuanti, file.path(dir_out, "03_pruebas_cuantitativas.csv"))

tabla_gt_cuanti <- tab_cuanti %>%

  gt() %>%

  tab_header(
    title = md("**Pruebas de diferencias entre grupos de IPV para variables continuas**")
  ) %>%

  cols_label(
    t_svy = "Estadístico t (diseño muestral)",
    p_t = "p-value",
    W_MW = "Estadístico U (Mann–Whitney)",
    p_MW = "p-value"
  ) %>%

  cols_align(
    align = "center",
    columns = everything()
  ) %>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%

  tab_options(
    table.font.size = px(13),
    data_row.padding = px(4)
  )

tabla_gt_cuanti


# Variables categóricas

var_cat <- c(
  "marital_status_cat",
  "cat_eth",
  "urban",
  "w_qt",
  "h_edu",
  "job",
  "viol_wtn",
  "ipv_att",
  "decision"
)

cramers_v <- function(chisq, n, k) {
  sqrt(chisq / (n * (k - 1)))
}

resultados_cat <- lapply(var_cat, function(x){

  formula <- as.formula(paste("~", x, "+ ipv"))

  test <- svychisq(formula, design = dhs_survey)

  tabla <- table(data.mlvl.2[[x]], data.mlvl.2$ipv)

  k <- min(dim(tabla))

  v <- cramers_v(
    chisq = as.numeric(test$statistic),
    n = sum(tabla),
    k = k
  )

  data.frame(
    Variable = x,
    Chi2 = round(as.numeric(test$statistic), 2),
    `Valor P` = format.pval(test$p.value, digits = 3, eps = .001),
    `V Cramer` = round(v, 3)
  )

}) %>%
  bind_rows()

resultados_cat$Variable <- c(  # Ajustamos labels
  "Estado civil",
  "Etnicidad",
  "Area (Urbana/Rural)",
  "Quintiles Riqueza",
  "Nivel educativo",
  "Trabaja",
  "Testigo de violencia",
  "Actitud frente al IPV",
  "Autonomía en toma de decisiones"
)

write_csv(resultados_cat, file.path(dir_out, "03_pruebas_categoricas.csv"))

tabla_gt_cat <- resultados_cat %>%

  gt() %>%

  tab_header(
    title = md("**Asociación entre IPV y variables categóricas**")
  ) %>%

  cols_label(
    Chi2 = md("χ²")
  ) %>%

  cols_align(
    align = "center",
    columns = everything()
  ) %>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

tabla_gt_cat



#######################################################################################################

# 4.  V A R I A B L E S   D E   N I V E L   2


# Descriptivas generales (tabla para el documento) ...................................................


tab_descripctivas_ag <- BD_IPV_mpal %>%

  select(
    ipv_mpal,
    IPM,
    ipv_att_mpal,
    decision_mpal,
    pdet,
    part_lab_m,
    lvl2_Q45,
    lvl2_edusup_mpal,

  ) %>%

  mutate(
    pdet = factor(pdet,
                  levels = c(0,1),
                  labels = c("No", "Sí"))
  ) %>%


  tbl_summary(

    statistic = list(                                       # Estadísticas para continuas
      all_continuous() ~ "{median} ({p25}–{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),

    digits = all_continuous() ~ 2,

    label = list(
      ipv_mpal ~ "Proporción municipal de IPV",
      IPM ~ "Índice Pobreza Multidimensional (IPM)",
      ipv_att_mpal ~ "Proporción actitud municipal hacia el IPV",
      decision_mpal ~ "Proporción autonomía municipal en toma de decisiones",
      pdet ~ "Municipio PDET",
      part_lab_m ~ "Participación laboral (mujeres)",
      lvl2_Q45 ~ "Prop. hogares más ricos (Q4 y Q5)",
      lvl2_edusup_mpal ~ "Prop. mujeres con educación superior"
    ),

    missing = "no"

  ) %>%

  modify_header(label ~ "**Variable Contextual (Segundo Nivel**")


tab_descripctivas_ag

write_csv(as_tibble(tab_descripctivas_ag, col_labels = FALSE), file.path(dir_out, "04_descriptivas_municipales.csv"))


# Exploración de variables de 2do nivel ...............................................................

# DEMOGRÁFICAS / TERRITORIALES

tab_pdet <- table1(~ ipv_mpal | pdet  , data = IPV_mpio)

# ECONÓMICAS

cor(IPV_mpio$tasa_nbi, IPV_mpio$ipv_mpal)
cor(IPV_mpio$IPM, IPV_mpio$ipv_mpal)
cor(IPV_mpio$job_mpal, IPV_mpio$ipv_mpal)
cor(IPV_mpio$part_lab_m, IPV_mpio$ipv_mpal)
cor(IPV_mpio$part_lab_h, IPV_mpio$ipv_mpal)
cor(IPV_mpio$lvl2_Q45_mpal, IPV_mpio$ipv_mpal)
cor(IPV_mpio$lvl2_Q5_mpal, IPV_mpio$ipv_mpal)
cor(IPV_mpio$lvl2_edusup_mpal, IPV_mpio$ipv_mpal)


hist(IPV_mpio$tasa_nbi)
hist(IPV_mpio$job_mpal)
hist(IPV_mpio$IPM)
hist(IPV_mpio$part_lab_m)
hist(IPV_mpio$part_lab_h)
hist(IPV_mpio$lvl2_Q45_mpal)
hist(IPV_mpio$lvl2_Q5_mpal)
hist(IPV_mpio$lvl2_edusup_mpal)


# CULTURALES / NORMAS SOCIALES

cor(IPV_mpio$ipv_att_mpal, IPV_mpio$ipv_mpal)
cor(IPV_mpio$decision_mpal, IPV_mpio$ipv_mpal)


hist(IPV_mpio$ipv_mpal)
hist(IPV_mpio$ipv_att_mpal)
hist(IPV_mpio$decision_mpal)


tab_lvl2 <- tbl_summary(
  IPV_mpio,
  include = c(ipv_mpal, IPM, job_mpal, ipv_att_mpal, decision_mpal, part_lab_m,
              part_lab_h, lvl2_Q5_mpal, lvl2_Q45_mpal, lvl2_edusup_mpal),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})",
  digits = all_continuous() ~ 2,
  label = list(
    ipv_mpal ~ "IPV (municipal)",
   # tasa_nbi ~ "Tasa NBI",
    IPM ~ "IPM (Indice de Pobreza Multid.)",
    job_mpal ~ "Empleo femenino (municipal)",
    ipv_att_mpal ~ "Actitud frente al IPV (municipal)",
    decision_mpal ~ "Autonomía en toma de decisiones (municipal)",
    part_lab_m ~ "Participación laboral (Mujeres)",
    part_lab_h ~ "Participación laboral (Hombres)",
    lvl2_Q5_mpal ~ "Prop hogares más ricos (Q5)",
    lvl2_Q45_mpal ~ "Prop hogares más ricos (Q4 y Q5)",
    lvl2_edusup_mpal ~ "Prop. mujeres con educación superior (municipal)"
  )) %>%
    modify_header(
      label ~ "**Variable**",
      stat_0 ~ "**Total**<br>N = {style_number(N, digits = 0)}"
    )

tab_lvl2

# Miramos correlación entre variables cuanti de 2do nivel para indagar sobre posibles problemas de multicolinealidad

cor(IPV_mpio[, c( "IPM", "job_mpal" , "ipv_att_mpal", "decision_mpal", "part_lab_m", "part_lab_h",
                  "lvl2_Q45_mpal", "lvl2_Q5_mpal", "lvl2_edusup_mpal")])



#######################################################################################################

# 5.  C O R R E L A C I O N E S


# Correlación de Spearman con IPV municipal ...........................................................

vars_tab_cor <- c(
  "ipv_att_mpal",
  "decision_mpal",
  "IPM",
  "part_lab_m",
 "lvl2_Q45_mpal",
 "lvl2_edusup_mpal"
)

tab_cor <- map_df(vars_tab_cor, function(v) {

  cor_test <- cor.test(
    BD_IPV_mpal[[v]],
    BD_IPV_mpal$ipv_mpal,
    method = "spearman",
    exact = FALSE
  )

  tibble(
    Variable = v,
    Rho_Spearman = round(cor_test$estimate, 2),
    p_value = signif(cor_test$p.value, 2)
  )

}) %>%
  mutate(
    Variable = dplyr::recode(
      Variable,
      IPM = "IPM (Índice de Pobreza Multidimensional)",
      ipv_att_mpal = "Actitud frente al IPV (municipal)",
      decision_mpal = "Autonomía en la toma de decisiones (municipal)",
      part_lab_m = "Participación laboral (Mujeres)",
      lvl2_Q45_mpal = "Prop. hogares más ricos (Q4 y Q5)",
      lvl2_edusup_mpal = "Prop. mujeres con educación superior"

    )
  )

write_csv(tab_cor, file.path(dir_out, "05_spearman_ipv_municipal.csv"))


# ahora la pasamos a un formato más estilizado

tab_cor_p <-  tab_cor %>%

   gt() %>%

  tab_header(
    title = md("**Correlación de Spearman con IPV municipal**")
  ) %>%

  cols_label(
    Variable = md("**Variable contextual**"),
    Rho_Spearman = md("**ρ de Spearman**"),
    p_value = md("**Valor p**")
  ) %>%

  fmt_number(
    columns = Rho_Spearman,
    decimals = 2
  ) %>%

  fmt_number(
    columns = p_value,
    decimals = 3
  )

tab_cor_p

# Análisis gráfico

var_cor_ag <- c(
  "decision_mpal",
  "IPM",
  "part_lab_m",
  "ipv_att_mpal",
  "lvl2_Q45",
  "lvl2_edusup_mpal"

)

labels_contexto <- c(
  ipv_mpal = "IPV (municipal)",
  IPM = "IPM (índice Pobreza Multidimensional)",
  ipv_att_mpal = "Actitud frente al IPV (municipal)",
  decision_mpal = "Autonomía en la toma de decisiones (municipal)",
  part_lab_m = "Participación laboral (Mujeres)",
  lvl2_Q45 = "Prop. hogares más ricos (Q4 y Q5)",
  lvl2_edusup_mpal = "Prop. mujeres con educación superior"
)

# Crear lista de gráficos
scatters_ag <- lapply(var_cor_ag, function(v) {

  # Calcular Spearman
  cor_test <- cor.test(
    BD_IPV_mpal[[v]],
    BD_IPV_mpal$ipv_mpal,
    method = "spearman"
  )

  rho <- round(cor_test$estimate, 2)
  pvalue <- signif(cor_test$p.value, 2)

  ggplot(BD_IPV_mpal, aes_string(x = v, y = "ipv_mpal")) +

    geom_point(alpha = 0.7) +

    geom_smooth(method = "lm", se = TRUE) +

    labs(
      x = labels_contexto[v],
      y = "Proporción IPV"
    ) +

    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste0(
        "ρ = ", rho,
        "\np = ", pvalue
      ),
      hjust = 1.1,
      vjust = 1.5,
      size = 4
    ) +

    theme_bw()
})

# Organizar en grid
p_scatters <- wrap_plots(scatters_ag, nrow = 5)
p_scatters
ggsave(file.path(dir_out, "05_dispersion_ipv_municipal.png"), p_scatters, width = 9, height = 14, dpi = 300, bg = "white")



# Matriz de correlaciones .....................................................................................


vars_cor <- data.mlvl.2 %>%   ## Seleccionamos las variables cuantitativas del modelo
  select(
    age,
    sons_daus,
    age_uni,
   IPM,
    lvl2_attitude,
    lvl2_decision,
   part_lab_m,
   lvl2_Q45_mpal,
   lvl2_edusup_mpal
  )


labels_vars <- c(       # Labels
  age = "Edad",
  sons_daus = "Número de hijos",
  age_uni = "Edad de la unión",
  IPM = "IPM",
  lvl2_attitude = "Actitud frente al IPV",
  lvl2_decision = "Autonomía en decisiones",
  part_lab_m = "Participación laboral (Mujeres)",
 lvl2_Q45_mpal = "Prop. hogares más ricos (Q4 y Q5)",
 lvl2_edusup_mpal = "Prop. mujeres con educación superior"
)

names(vars_cor) <- labels_vars[names(vars_cor)]

cor_matrix <- cor(  # creamos la matriz de Spearman
  vars_cor,
  method = "spearman",
  use = "complete.obs"
)

write_csv(rownames_to_column(as.data.frame(cor_matrix), "Variable"), file.path(dir_out, "05_matriz_correlaciones.csv"))


corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.9,
  tl.col = "black",
  diag = FALSE
)

p_corr <- ggcorrplot(
  cor_matrix,
  type = "upper",
  lab = TRUE,
  lab_size = 3,
  colors = c("#6D9EF4", "white", "#E46"),
  title = "",
  ggtheme = theme_light() +

    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),

      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()

))

p_corr
ggsave(file.path(dir_out, "05_matriz_correlaciones.png"), p_corr, width = 9, height = 8, dpi = 300, bg = "white")
