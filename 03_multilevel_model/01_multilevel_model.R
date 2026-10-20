
#########################################
##  MODELO MULTINIVEL IPV - Entrega 7  ##
#########################################

# Esta entrega tendrá las siguientes novedades frente a la entrega anterior:

#  1. Inclusión de la variable IPM (Indice de Pobreza Multidimensional)
#  2. Inclusión de la variable Clasificación OCDE para los municipios
#  3. Nuevas especificaciones para interacciones entre niveles
#  4. Prueba de inclusión de nuevas variables:

# Educación del esposo que habita en el hogar
# Proporción de mujeres en Q4 y Q5
# Proporción de mujeres en Q5
# Proporción de mujeres con estudios superiores

# Input:  data/processed/model_data.rds (01_data_preparation/03)
# Output: output/03_multilevel_model/ (CSV y figuras)


library(lme4)
library(car)
library(tidyverse)
library(dplyr)
library(broom.mixed)
library(gt)
library(stringr)
library(ggeffects)
library(performance)
library(plotly)
library(patchwork)
library(gtsummary)
library(tidyr)
library(here)  # rutas relativas a la raíz del proyecto (reemplaza setwd)

# Rutas del proyecto
dir_processed <- here("00_data", "processed")
dir_out       <- here("output", "03_multilevel_model")

# Importamos nuestro dataset
data.mlvl <- readRDS(file.path(dir_processed, "model_data.rds"))
data.mlvl$ipv <- as.factor(data.mlvl$ipv)


var_labels <- list(

  marital_status_cat = "Estado civil",
  age_c = "Edad (Centrada)",
  cat_eth = "Pertenencia étnica",
  sons_daus_c = "Número de hijos/as en el hogar",
  w_qt = "Índice de riqueza (Quintiles)",
  h_edu = "Nivel educativo máximo",
  job = "Trabaja actualmente",
  viol_wtn = "Presenció IPV en Infancia",
  age_uni_c = "Edad para primera cohabitación con pareja",
  decision = "Autonomía en la toma de decisiones",
  urban = "Área de residencia",

  IPM_c = "Índice de Pobreza Multidimensional (IPM)",
  lvl2_job_c = "Empleo femenino municipal",
  lvl2_decision_c = "Autonomía femenina municipal"

)


#_______________________________________________________________________________________________________________________-----


# CALCULAMOS ICC para IPV GENERAL:

M0_ipv <- glmer(ipv ~ 1 + (1 | codmpio),  # El ~ 1 permite que haya un intercepto fijo y el 1 | permite el aleatorio
                data = data.mlvl,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10)


summary(M0_ipv)

ICC_ipv <- M0_ipv@theta[1]^2/ (M0_ipv@theta[1]^2 + (pi^2/3)) # M0@theta[1]^2 es la varianza entre clases al cuadraro
ICC_ipv

# INTERPRETACIÓN: El 5,2% de la probabilidad de que una mujer experimente algún tipo de IPV
# está explicado por las diferencias entre municipios. De forma complementaria, el 94.8% está explicado
# por diferencias al interior de las clases.

write_csv(
  tibble(var_municipal = M0_ipv@theta[1]^2, ICC = ICC_ipv),
  file.path(dir_out, "01_modelo_vacio_icc.csv")
)


#________________________________________________________________________________________________________________________


# AUSTES DE VARIABLES



# Se plantean ajustes a las variables de nivel 1 para correr el modelo:

# Grand Mean Centering GMC <-  Todas las variables cuantitativas

data.mlvl<- data.mlvl %>%
  mutate(age_c = age - mean(age, na.rm = TRUE)) %>%
  mutate(sons_daus_c = sons_daus - mean(sons_daus, na.rm = TRUE)) %>%
  mutate(age_uni_c = age_uni - mean(age_uni, na.rm = TRUE)) %>%
  mutate(tasa_nbi_c = tasa_nbi - mean(tasa_nbi, na.rm = TRUE)) %>%
  mutate(lvl2_job_c = job_mpal - mean(job_mpal, na.rm = TRUE)) %>%
  mutate(lvl2_attitude_c = ipv_att_mpal - mean(ipv_att_mpal, na.rm = TRUE)) %>%
  mutate(lvl2_decision_c = decision_mpal - mean(decision_mpal, na.rm = TRUE)) %>%
  mutate(IPM_c = IPM - mean(IPM, na.rm = TRUE)) %>%
  mutate(part_lab_m_c = part_lab_m - mean(part_lab_m, na.rm = TRUE)) %>%
  mutate(part_lab_h_c = part_lab_h - mean(part_lab_h, na.rm = TRUE)) %>%
  mutate(lvl2_Q45_c = lvl2_Q45_mpal - mean(lvl2_Q45_mpal, na.rm = TRUE)) %>%
  mutate(lvl2_Q5_c = lvl2_Q5_mpal - mean(lvl2_Q5_mpal, na.rm = TRUE)) %>%
  mutate(lvl2_edusup_c = lvl2_edusup_mpal - mean(lvl2_edusup_mpal, na.rm = TRUE))




# Modelo M_E6 <- Interacción: (w_q) x (part_lab_m_c) ------------------------------------------------------------------------------


# Definimos previamente categorías base de algunas variables:

data.mlvl$ipv <- as.factor(data.mlvl$ipv)
data.mlvl$h_edu <- as.factor(data.mlvl$h_edu)
data.mlvl$h_edu <- relevel(data.mlvl$h_edu, ref = "No edu")
data.mlvl$cat_eth <- as.factor(data.mlvl$cat_eth)
data.mlvl$cat_eth <- relevel(data.mlvl$cat_eth, ref = "Ninguna")
data.mlvl$clas_OCDE <- as.factor(data.mlvl$clas_OCDE)
data.mlvl$clas_OCDE <- relevel(data.mlvl$clas_OCDE, ref = "urbano")
data.mlvl$w_qt <- as.factor(data.mlvl$w_qt)
data.mlvl$urban <- as.factor(data.mlvl$urban)
data.mlvl$codmpio <- as.factor(data.mlvl$codmpio)
data.mlvl$decision <- as.factor(data.mlvl$decision)
data.mlvl$decision <- relevel(data.mlvl$decision, ref = "No es autónoma")


M_E6 <- glmer(
  ipv ~

    # Nivel 1: Variables individuales

    marital_status_cat +        # Estado civil categorizado
    age_c +                     # Edad centrada
    cat_eth +                   # Etnicidad (categorías)
    sons_daus_c +               # Número de hijos e hijas
    urban +                     # Vivienda urbana

    w_qt +                      # Índice de riqueza (nivel individual)
    h_edu +                     # Nivel máx educación
  # h_edu_pj +
    job +                       # Tiene trabajo actualmente

    viol_wtn +                  # Ha presenciado violencia doméstica
    age_uni_c +                 # Edad a la primera unión centrada
    decision +                  # Autonomía en toma de decisiones


    # Nivel 2: Variables contextuales

    lvl2_decision_c +           # Autonomía en decisiones a nivel municipal
    part_lab_m_c +              # Participación laboral (mujeres)
    lvl2_Q45_c +
    lvl2_edusup_c +


    # Interacción de Nivel 1

    age_uni_c:urban +

    # Interacciones cruzadas

    w_qt:part_lab_m_c +


    # Efectos aleatorios por municipio

    + (1 + decision | codmpio),

  data = data.mlvl,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  nAGQ = 0
)

summary(M_E6)

# Exportamos los componentes de varianza del modelo final
write_csv(as.data.frame(VarCorr(M_E6)), file.path(dir_out, "01_modelo_final_varianzas.csv"))

mod_vif <-glm(ipv ~

                # Nivel 1: Variables individuales

                marital_status_cat +        # Estado civil categorizado
                age_c +                     # Edad centrada
                cat_eth +                   # Etnicidad (categorías)
                sons_daus_c +               # Número de hijos e hijas
                urban +                     # Vivienda urbana

                w_qt +                      # Índice de riqueza (nivel individual)
                h_edu +                     # Nivel máx educación
                job +                       # Tiene trabajo actualmente

                viol_wtn +                  # Ha presenciado violencia doméstica
                age_uni_c +                 # Edad a la primera unión centrada
                decision +                  # Autonomía en toma de decisiones


                # Nivel 2: Variables contextuales

                lvl2_decision_c +           # Autonomía en decisiones a nivel municipal
                part_lab_m_c +              # Participación laboral (mujeres)
                lvl2_Q45_c +
                lvl2_edusup_c +


                # Interacción de Nivel 1

                age_uni_c:urban +

                # Interacciones cruzadas

                w_qt:part_lab_m_c,

              family = binomial, data = data.mlvl)

vif(mod_vif)

# Exportamos el GVIF calculado (GVIF, Df y GVIF^(1/(2*Df)), que es el GVIF ajustado)
write_csv(
  rownames_to_column(as.data.frame(vif(mod_vif)), "Termino"),
  file.path(dir_out, "01_gvif.csv")
)

library(performance)
check_collinearity(mod_vif)


library(tibble)
library(gt)

tabla_vif <- tribble(
  ~Nivel, ~Variable, ~`Adjusted GVIF`,
  "Individual", "Estado civil", 1.09,
  "Individual", "Edad", 1.21,
  "Individual", "Grupo étnico", 1.22,
  "Individual", "Número de hijos", 1.08,
  "Individual", "Residencia urbana", 1.58,
  "Individual", "Educación del compañero", 1.34,
  "Individual", "Empleo de la mujer", 1.06,
  "Individual", "Violencia presenciada en la infancia", 1.01,
  "Individual", "Edad a la primera unión", 2.00,
  "Individual", "Autonomía en la toma de decisiones", 1.04,
  "Individual", "Quintil de riqueza", 3.94,
  "Municipal", "Autonomía femenina municipal", 1.14,
  "Municipal", "Proporción hogares Q4-Q5", 1.75,
  "Municipal", "Proporción mujeres con educación superior", 1.61,
  "Municipal", "Participación laboral femenina", 2.83,
  "Interacción", "Edad a la primera unión × Residencia urbana", 1.94,
  "Interacción", "Quintil de riqueza × Participación laboral femenina", 4.53
)


tabla_vif %>%
  gt(groupname_col = "Nivel") %>%
  fmt_number(columns = `Adjusted GVIF`,
             decimals = 2) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_column_labels()
  ) %>%
  cols_label(
    Variable = "Predictor",
    `Adjusted GVIF` = "Adjusted GVIF"
  ) %>%
  tab_header(
    title = md("**Evaluación de la multicolinealidad**")
  ) %>%
  tab_source_note(
    source_note = md(
      "*Nota.* Se reporta el factor de inflación de la varianza generalizado ajustado (Adjusted GVIF). Valores inferiores a 5 indican ausencia de problemas importantes de multicolinealidad."
    )
  )%>%
  cols_align(
    align = "center",
    columns = `Adjusted GVIF`
  )

# Graficamos las interacciones -----------


# Edad a la primera unión x Zona (urbana vs rural)

mean_age_uni <- mean(data.mlvl$age_uni)
breaks_age_uni <- seq(10, 45, by = 5)

pred_ageuni_urban <- ggpredict(M_E6, terms = c("age_uni_c[all]", "urban"), bias_correction = TRUE)

write_csv(as.data.frame(pred_ageuni_urban), file.path(dir_out, "01_pred_edad_union_x_zona.csv"))

cruce <- as.data.frame(pred_ageuni_urban) %>% # hallamos el punto de corte de las lineas urb vs rural
  select(x, predicted, group) %>%
  pivot_wider(
    names_from = group,
    values_from = predicted
  )

cruce <- cruce %>%
  mutate(diff = Urbano - Rural)

i <- which(diff(cruce$diff > 0) != 0)

i

x1 <- cruce$x[i]
x2 <- cruce$x[i + 1]

d1 <- cruce$diff[i]
d2 <- cruce$diff[i + 1]

x_cruce <- x1 - d1 * (x2 - x1) / (d2 - d1)

x_cruce

edad_cruce <- x_cruce + mean_age_uni


p0 <- plot(pred_ageuni_urban, show_ci = FALSE) + # Interaccion de primer nivel

        scale_x_continuous(
          breaks = breaks_age_uni - mean_age_uni,
          labels = breaks_age_uni) +

  coord_cartesian(ylim = c(0.35, 0.55)) +
  labs( title = NULL,
        x = "Edad a la primera cohabitación",
        y = "Probabilidad del IPV",
        colour = "Zona de Residencia") +

       geom_vline(
         xintercept = x_cruce,
          linetype = "dashed",
            colour = "black",
         linewidth = 0.6 ) +

      annotate(
        "text",
            x = x_cruce,
            y = 0.51,    # ajusta la altura
        label = paste0("≈ ", round(edad_cruce, 0), " años"),
        angle = 90,
        vjust = -0.4,
        hjust = 0,
       colour = "grey30",
         size = 3.5) +

        theme(
          plot.caption = element_text(
          hjust = 0,           # izquierda
          colour = "grey40",   # color
          size = 10,
          face = "italic")
           )

p0
ggplotly(p0)



# Hacemos gráfico de la brecha para la primera interacción

pred_df <- as.data.frame(pred_ageuni_urban)

gap_df <- pred_df %>%
  select(x, group, predicted) %>%
  pivot_wider(
    names_from = group,
    values_from = predicted
  ) %>%
  mutate(
    gap_pp = 100 * (`Urbano` - `Rural`)  # Urbano - Rural (Puntos porcentuales)
  )

themep0 <- p0$theme

p0_gap <- ggplot(gap_df, aes(x = x + mean_age_uni, y = gap_pp)) +

  geom_line() +

  geom_hline(yintercept = 0, linetype = "dashed") +

  geom_vline(
    xintercept = edad_cruce,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.7
  ) +

  annotate(
    "text",
    x = 25.5,
    y = -4,
    label = paste0("≈ ", round(edad_cruce, 0), " años"),
    angle = 90,
    hjust = -0.2,
    size = 3.5,
    colour = "grey30"
  ) +

  scale_x_continuous(
    breaks = seq(10, 45, 5)) +

  scale_y_continuous(
    breaks = seq(-8, 3, 2)
  ) +
  labs(
    x = "Edad a la primera cohabitación",
    y = "Brecha Urbano-Rural de Prob IPV (pp)",
    caption = paste0(
              "Nota: La media para la edad a la primera cohabitación es de ",
              round(mean_age_uni, 1),
              " años." )) +
  themep0

p0_gap
p0 / p0_gap

ggsave(file.path(dir_out, "01_interaccion_edad_union_x_zona.png"), p0 / p0_gap,
       width = 8, height = 9, dpi = 300, bg = "white")


# Gráfico quintil de riqueza x empleo femenino

mean_part_lab_m <- mean(data.mlvl$part_lab_m, na.rm = TRUE)
breaks_part_lab_m<-  seq(0, 1, by = 0.1)

pred_riq_part_m <- ggpredict(M_E6, terms = c("part_lab_m_c[all]", "w_qt"), bias_correction = TRUE )

write_csv(as.data.frame(pred_riq_part_m), file.path(dir_out, "01_pred_part_laboral_x_quintil.csv"))

p2b <- plot(pred_riq_part_m, show_ci = FALSE ) +
  scale_x_continuous(
    breaks = breaks_part_lab_m - mean_part_lab_m,
    labels = breaks_part_lab_m) +
  coord_cartesian(ylim = c(0.44, 0.51)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.7
  ) +

  annotate(
    "text",
    x = 0.01,
    y = 0.445,
    label = paste0("Media part. laboral femenina ≈ ", round(mean_part_lab_m * 100, 0), "%"),
    angle = 90,
    hjust = -0.1,
    size = 3.5,
    colour = "grey30"
  ) +

  labs( title = NULL,
        x = "Participación Laboral (Mujeres)",
        y = "Probabilidad del IPV",
        colour = "Quintil de Riqueza",
        caption = paste0(
          "Nota. La tasa media de participación laboral femenina es de ",
          round(mean_part_lab_m * 100, 1), "%.")) +

  theme(
    plot.caption = element_text(
      hjust = 0,
      colour = "grey40",
      size = 10,
      face = "italic"
    )
  )


p2b
ggplotly(p2b)

ggsave(file.path(dir_out, "01_interaccion_part_laboral_x_quintil.png"), p2b,
       width = 8, height = 6, dpi = 300, bg = "white")



# Generamos la tabla de resultados del modelo M_E6 ----
library(broom.mixed)
library(dplyr)

tabla_M_E6 <- tidy(
  M_E6,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = FALSE
) %>%

  # Eliminar intercepto
  filter(term != "(Intercept)") %>%

  # Crear OR manualmente
  mutate(
    OR = exp(estimate),
    conf.low.or = exp(conf.low),
    conf.high.or = exp(conf.high)
  ) %>%

  # Renombrar variables según nombre REAL del término
  mutate(
    Variable = case_when(

      # Estado civil (Ref.: Casada)

      term == "marital_status_catOtro" ~
        "Estado civil (Ref.: Casada): Separada/Divorciada/Viuda",

      term == "marital_status_catUnión libre" ~
        "Estado civil (Ref.: Casada): Unión libre",


      # Pertenencia étnica (Ref.: Ninguna)

      term == "cat_ethAfro" ~
        "Pertenencia étnica (Ref.: Ninguna): Afro",

      term == "cat_ethIndígena" ~
        "Pertenencia étnica (Ref.: Ninguna): Indígena",

      term == "cat_ethPalenq" ~
        "Pertenencia étnica (Ref.: Ninguna): Palenquera",

      term == "cat_ethRaizal" ~
        "Pertenencia étnica (Ref.: Ninguna): Raizal",

      term == "cat_ethRom" ~
        "Pertenencia étnica (Ref.: Ninguna): Rom",


      # Variables continuas

      term == "age_c" ~ "Edad - Centrada",
      term == "sons_daus_c" ~ "Número de hijos/as - Centrada",
      term == "urbanUrbano" ~ "Zona de residencia (Ref.: Rural): Urbana",


      # Quintiles de riqueza (Ref.: Q1)

      term == "w_qt2" ~
        "Nivel de riqueza (Ref.: Q1): Q2",

      term == "w_qt3" ~
        "Nivel de riqueza (Ref.: Q1): Q3",

      term == "w_qt4" ~
        "Nivel de riqueza (Ref.: Q1): Q4",

      term == "w_qt5" ~
        "Nivel de riqueza (Ref.: Q1): Q5",


      # Educación (Ref.: Sin educación)

      term == "h_eduPrim" ~
        "Nivel educativo (Ref.: Sin educación): Primaria",

      term == "h_eduSec" ~
        "Nivel educativo (Ref.: Sin educación): Secundaria",

      term == "h_eduSup" ~
        "Nivel educativo (Ref.: Sin educación): Superior",


      # Situación laboral (Ref.: No trabaja)

      term == "jobTrabaja" ~
        "Situación laboral (Ref.: No trabaja): Trabaja",


      # Normas sociales

      term == "viol_wtnSí" ~
        "Presenció IPV en la infancia (Ref.: No): Sí",

      term == "age_uni_c" ~
        "Edad a la primera unión - Centrada",

      term == "decisionEs autónoma" ~
        "Autonomía en la toma de decisiones (Ref.: No autónoma): Es autónoma",


      term == "lvl2_decision_c" ~
        "Autonomía municipal en la toma de decisiones - Centrada",

      term == "part_lab_m_c" ~
        "Participación laboral (Mujeres) - Centrada",

      term == "lvl2_Q45_c" ~
        "Proporción hogares más ricos (Q4 y Q5) - Centrada",

      term == "lvl2_edusup_c" ~
        "Proporción de mujeres con educación superior - Centrada",


      # Interacción individual

      term == "urbanUrbano:age_uni_c" ~
        "Edad a la primera unión (centrada) × Zona de residencia (urbana)",


      # Interacciones cruzadas

      term == "w_qt2:part_lab_m_c" ~
        "Q2 (Ref.: Q1) × Participación laboral (Mujeres)",

      term == "w_qt3:part_lab_m_c" ~
        "Q3 (Ref.: Q1) × Participación laboral (Mujeres)",

      term == "w_qt4:part_lab_m_c" ~
        "Q4 (Ref.: Q1) × Participación laboral (Mujeres)",

      term == "w_qt5:part_lab_m_c" ~
        "Q5 (Ref.: Q1) × Participación laboral (Mujeres)",

      # Si algo no coincide, deja el nombre original
      TRUE ~ term
    )
  ) %>%

  # Agrupar visualmente
  mutate(
    Nivel = case_when(

      term %in% c(
        "marital_status_catOtro",
        "marital_status_catUnión libre",

        "cat_ethAfro",
        "cat_ethIndígena",
        "cat_ethPalenq",
        "cat_ethRaizal",
        "cat_ethRom",

        "age_c",
        "sons_daus_c",
        "urbanUrbano"

      ) ~ "Nivel individual: Sociodemográficas",

      term %in% c(

        "w_qt2",
        "w_qt3",
        "w_qt4",
        "w_qt5",

        "h_eduPrim",
        "h_eduSec",
        "h_eduSup",

        "jobTrabaja"

      ) ~ "Nivel individual: Económicas",

      term %in% c(

        "viol_wtnSí",
        "age_uni_c",
        "decisionEs autónoma"

      ) ~ "Nivel individual: Normas Sociales",

      term %in% c(
        "lvl2_decision_c",
        "part_lab_m_c",
        "lvl2_Q45_c",
        "lvl2_edusup_c"

      ) ~ "Nivel contextual",

      term %in% c(
        "urbanUrbano:age_uni_c"

      ) ~ "Interacción 1er Nivel",


      TRUE ~ "Interacción Nivel Cruzado"
    )
  ) %>%

  # Formato final
  mutate(

    sig = p.value < 0.1,

    beta = round(estimate, 3),

    OR = round(OR, 2),

    IC95 = paste0(
      round(conf.low.or, 2),
      " – ",
      round(conf.high.or, 2)
    ),

    p.value = case_when(
      p.value < .001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  ) %>%

  select(
    Nivel,
    Variable,
    beta,
    OR,
    IC95,
    p.value,
    sig
  )

write_csv(tabla_M_E6, file.path(dir_out, "01_modelo_final_coeficientes.csv"))



# Ajustamos a gt para visualizar

tabla_M_E6 %>%

  gt(groupname_col = "Nivel") %>%

  tab_header(
    title = md("**Modelo multinivel de IPV**"),
    subtitle = md("Odds Ratios (OR) e intervalos de confianza al 95%")
  ) %>%

  cols_label(
    Variable = md("**Variable**"),
    beta = md("**β**"),
    OR = md("**OR**"),
    IC95 = md("**IC 95%**"),
    p.value = md("**p-valor**")
  ) %>%

  cols_align(
    align = "center",
    columns = c(beta, OR, IC95, p.value)
  ) %>%

  cols_align(
    align = "left",
    columns = Variable
  ) %>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = sig,
      columns = c(beta, OR,p.value))
  ) %>%

  cols_hide(columns = sig) %>%

  opt_row_striping() %>%

  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13)
  )



# Tabla de variables


tabla_vars <- tibble(
  Nivel = c(rep("Nivel 1: Individual",11),
            rep("Nivel 2: Municipal",4),
            rep("Términos de interacción",2)),
  Simbolo = c(
    "X₁ᵢⱼ","X₂ᵢⱼ","X₃ᵢⱼ","X₄ᵢⱼ","X₅ᵢⱼ",
    "X₆ᵢⱼ","X₇ᵢⱼ","X₈ᵢⱼ","X₉ᵢⱼ","X₁₀ᵢⱼ","X₁₁ᵢⱼ",
    "Z₁ⱼ","Z₂ⱼ","Z₃ⱼ","Z₄ⱼ",
    "X₁₀ᵢⱼ × X₅ᵢⱼ",
    "X₆ᵢⱼ × Z₂ⱼ"
  ),
  Variable = c(
    "Estado civil",
    "Edad (centrada)",
    "Grupo étnico",
    "Número de hijos/as (centrado)",
    "Área de residencia urbana",
    "Quintil de riqueza",
    "Educación de la pareja",
    "Participación laboral de la mujer",
    "Exposición a violencia durante la infancia",
    "Edad a la primera unión (centrada)",
    "Capacidad de decisión de la mujer",
    "Proporción municipal de mujeres con capacidad de decisión",
    "Participación laboral femenina municipal",
    "Proporción de hogares en los quintiles 4 y 5 de riqueza",
    "Proporción de población con educación superior",
    "Interacción nivel 1:Edad a la primera unión × área de residencia urbana",
    "Interacción nivel cruzado: Quintil de riqueza × participación laboral femenina"
  )
)

tabla_vars |>
  gt(groupname_col = "Nivel") |>
  cols_label(
    Simbolo = md("**Símbolo**"),
    Variable = md("**Variable**")
  ) |>
  tab_header(
    title = md("")
  ) |>
  cols_align(
    align = "center",
    columns = Simbolo
  ) |>
  cols_width(
    Simbolo ~ px(120),
    Variable ~ px(500)
  ) |>
  tab_style(
    style = cell_text(style = "normal"),
    locations = cells_body(columns = Simbolo)
  ) |>
  tab_source_note(
    md("*Nota.* Las variables indicadas como centradas fueron centradas respecto a la media global.")
  ) |>
  opt_table_font(
    font = list(
      google_font("Garamond"),
      default_fonts()
    )
  ) |>
  tab_options(
    table.font.size = px(15),
    heading.title.font.size = px(20),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    table.border.top.width = px(1.5),
    table.border.bottom.width = px(1.5),
    data_row.padding = px(6)
  )
