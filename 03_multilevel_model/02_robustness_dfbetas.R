###############################################################
##  ROBUSTEZ: MUNICIPIOS INFLUYENTES EN EL MODELO MULTINIVEL  ##
###############################################################

# Sigue el procedimiento de Nieuwenhuis, te Grotenhuis y Pelzer (2012), "influence.ME: Tools for
# detecting influential data in mixed effects models", The R Journal 4(2): 38-47, y de Van der Meer,
# te Grotenhuis y Pelzer (2010), American Sociological Review 75: 173-178.

# Procedimiento: se excluye cada municipio con todas las mujeres que pertenecen a él, se reestima el
# modelo final M_E6 y se compara cada estimación con la del modelo completo. Se repite para los 294
# municipios. A partir de esas estimaciones se calculan:

#   1. DFBETAS: cambio estandarizado de cada parámetro.
#        DFBETAS_ij = (gamma_i - gamma_i(-j)) / se(gamma_i(-j))
#        Umbral: 2 / sqrt(n), con n = número de municipios.

#   2. Distancia de Cook: cambio conjunto de un grupo de parámetros.
#        C_j = (gamma - gamma(-j))' Sigma(-j)^-1 (gamma - gamma(-j)) / r
#        Umbral: 4 / n. Se calcula para tres grupos de parámetros: los municipales, los de la
#        interacción entre niveles y todos los efectos fijos (sin intercepto).

#   3. Cambio porcentual: (gamma_i - gamma_i(-j)) / gamma_i * 100. Sin umbral.

#   4. Prueba de cambio de significancia (sigtest): si excluir un municipio cambia la
#      significancia de algún parámetro con un valor crítico de 1,96.

#   5. Reestimación del modelo sin los municipios influyentes sobre los parámetros municipales.

# No se evalúa la influencia de cada mujer (obs = TRUE): implicaría 24.890 reestimaciones, y el
# paper advierte que en encuestas grandes no es factible ni es el foco del análisis.

# TIEMPO DE CÓMPUTO: las 294 reestimaciones pueden tardar más de una hora. El resultado se guarda en
# data/processed/estex_M_E6_codmpio.rds y se reutiliza en las corridas siguientes. Para recalcularlo
# (por ejemplo, si cambia el modelo), poner recalcular <- TRUE.

# Input:  data/processed/M_E6.rds y data/processed/model_data_estimation.rds (01_multilevel_model.R)
# Output: output/03_multilevel_model/ (CSV y figuras)


library(lme4)
library(influence.ME)
library(tidyverse)
library(here)  

# Rutas del proyecto
dir_processed <- here("00_data", "processed")
dir_out       <- here("output", "03_multilevel_model")

recalcular <- FALSE



# 1. C A R G A   D E L   M O D E L O   Y   L O S   D A T O S
#-----------------------------------------------------------------------------------------------------------------------

# Si el script se corre en la misma sesión de 01_multilevel_model.R, se usan M_E6 y data.mlvl que ya
# están en memoria. Si no, se cargan desde data/processed.

# influence.ME busca la base de datos por el nombre con que se estimó el modelo, por eso debe
# llamarse data.mlvl y estar en el entorno global

if (!exists("M_E6") || !exists("data.mlvl")) {
  M_E6      <- readRDS(file.path(dir_processed, "M_E6.rds"))
  data.mlvl <- readRDS(file.path(dir_processed, "model_data_estimation.rds"))
}

stopifnot(as.character(M_E6@call)[3] == "data.mlvl")

summary(M_E6)

# Nombre y número de mujeres de cada municipio, para identificar los casos influyentes
info_mpio <- data.mlvl %>%
  group_by(codmpio) %>%
  summarise(municipio = first(MPIO_CNMBR),
            n_mujeres = n()) %>%
  mutate(codmpio = as.character(codmpio))



# 2. P A R Á M E T R O S   Y   U M B R A L E S
#-----------------------------------------------------------------------------------------------------------------------

nombres_fe <- names(fixef(M_E6))

# Todos los efectos fijos sin intercepto
idx_todos <- which(nombres_fe != "(Intercept)")

# Variables municipales (nivel 2)
idx_mpal <- which(nombres_fe %in% c("lvl2_decision_c", "part_lab_m_c", "lvl2_Q45_c", "lvl2_edusup_c"))

# Interacción entre niveles: quintil de riqueza x participación laboral femenina municipal
idx_cruzada <- grep("part_lab_m_c:|:part_lab_m_c", nombres_fe)

nombres_fe[idx_mpal]
nombres_fe[idx_cruzada]

n_mpio    <- nlevels(M_E6@flist[["codmpio"]])
corte_dfb <- 2 / sqrt(n_mpio)
corte_cook <- 4 / n_mpio

cat("Municipios:", n_mpio, "\n")
cat("Umbral DFBETAS (2/sqrt(n)):", round(corte_dfb, 4), "\n")
cat("Umbral distancia de Cook (4/n):", round(corte_cook, 4), "\n")



# 3. R E E S T I M A C I Ó N   E X C L U Y E N D O   C A D A   M U N I C I P I O
#-----------------------------------------------------------------------------------------------------------------------

archivo_estex <- file.path(dir_processed, "estex_M_E6_codmpio.rds")

if (recalcular || !file.exists(archivo_estex)) {
  
  t0 <- Sys.time()
  estex_mpio <- influence.ME::influence(M_E6, group = "codmpio", count = TRUE)  # imprime la cuenta regresiva
  print(Sys.time() - t0)
  
  saveRDS(estex_mpio, archivo_estex)
  
} else {
  
  estex_mpio <- readRDS(archivo_estex)
  
}



# 4. D F B E T A S
#-----------------------------------------------------------------------------------------------------------------------

dfb <- dfbetas(estex_mpio, parameters = idx_todos)

# Matriz completa: una fila por municipio, una columna por parámetro
dfb_df <- as.data.frame(dfb) %>%
  rownames_to_column("codmpio") %>%
  left_join(info_mpio, by = "codmpio") %>%
  relocate(municipio, n_mujeres, .after = codmpio)

write_csv(dfb_df, file.path(dir_out, "02_dfbetas.csv"))

# Resumen por parámetro: cuántos municipios superan el umbral y cuál tiene el mayor valor absoluto
resumen_dfb <- tibble(
  parametro       = colnames(dfb),
  n_superan_corte = colSums(abs(dfb) > corte_dfb),
  max_abs_dfbetas = apply(abs(dfb), 2, max),
  codmpio_max     = rownames(dfb)[apply(abs(dfb), 2, which.max)]
) %>%
  left_join(info_mpio, by = c("codmpio_max" = "codmpio")) %>%
  mutate(corte = corte_dfb)

resumen_dfb

write_csv(resumen_dfb, file.path(dir_out, "02_dfbetas_resumen.csv"))



# 5. D I S T A N C I A   D E   C O O K
#-----------------------------------------------------------------------------------------------------------------------

cook_mpal    <- cooks.distance(estex_mpio, parameters = idx_mpal)
cook_cruzada <- cooks.distance(estex_mpio, parameters = idx_cruzada)
cook_todos   <- cooks.distance(estex_mpio, parameters = idx_todos)

tabla_cook <- tibble(
  codmpio      = rownames(cook_mpal),
  cook_mpal    = cook_mpal[, 1],
  cook_cruzada = cook_cruzada[, 1],
  cook_todos   = cook_todos[, 1]
) %>%
  left_join(info_mpio, by = "codmpio") %>%
  relocate(municipio, n_mujeres, .after = codmpio) %>%
  mutate(
    supera_mpal    = cook_mpal    > corte_cook,
    supera_cruzada = cook_cruzada > corte_cook,
    supera_todos   = cook_todos   > corte_cook
  ) %>%
  arrange(desc(cook_mpal))

tabla_cook %>% filter(supera_mpal | supera_cruzada | supera_todos)

write_csv(tabla_cook, file.path(dir_out, "02_cook.csv"))



# 6. C A M B I O   P O R C E N T U A L
#-----------------------------------------------------------------------------------------------------------------------

# Se calcula con la fórmula del paper (dividiendo por la estimación del modelo completo). La función
# pchange() del paquete divide por la estimación sin el municipio, por eso no se usa aquí.

est_original <- estex_mpio$or.fixed[1, idx_todos]
est_alterada <- estex_mpio$alt.fixed[, idx_todos, drop = FALSE]

pcambio <- t(apply(est_alterada, 1, function(x) (est_original - x) / est_original * 100))

pcambio_df <- as.data.frame(pcambio) %>%
  rownames_to_column("codmpio") %>%
  left_join(info_mpio, by = "codmpio") %>%
  relocate(municipio, n_mujeres, .after = codmpio)

write_csv(pcambio_df, file.path(dir_out, "02_cambio_porcentual.csv"))

resumen_pcambio <- tibble(
  parametro       = colnames(pcambio),
  max_abs_pcambio = apply(abs(pcambio), 2, max),
  codmpio_max     = rownames(pcambio)[apply(abs(pcambio), 2, which.max)]
) %>%
  left_join(info_mpio, by = c("codmpio_max" = "codmpio"))

resumen_pcambio

write_csv(resumen_pcambio, file.path(dir_out, "02_cambio_porcentual_resumen.csv"))



# 7. C A M B I O S   D E   S I G N I F I C A N C I A
#-----------------------------------------------------------------------------------------------------------------------

# sigtest() compara el estadístico z con un valor crítico en una sola dirección. Como en el paper, se
# usa +1,96 para los parámetros con estimación original positiva y -1,96 para los negativos.

sig_pos <- sigtest(estex_mpio, test =  1.96)
sig_neg <- sigtest(estex_mpio, test = -1.96)

z_original <- estex_mpio$or.test

tabla_sig <- map_dfr(nombres_fe[idx_todos], function(p) {
  
  lista <- if (z_original[p] > 0) sig_pos else sig_neg
  
  tibble(
    parametro     = p,
    z_original    = z_original[p],
    codmpio       = rownames(lista[[p]]),
    z_sin_mpio    = lista[[p]]$Altered.Teststat,
    sig_sin_mpio  = lista[[p]]$Altered.Sig,
    cambia_sig    = lista[[p]]$Changed.Sig
  )
}) %>%
  left_join(info_mpio, by = "codmpio")

# Solo los casos en que excluir un municipio cambia la significancia
cambios_sig <- tabla_sig %>% filter(cambia_sig)

cambios_sig
cambios_sig %>% count(parametro, name = "n_mpios_cambian_sig")

write_csv(tabla_sig,   file.path(dir_out, "02_sigtest_completo.csv"))
write_csv(cambios_sig, file.path(dir_out, "02_sigtest_cambios.csv"))



# 8. M O D E L O   S I N   L O S   M U N I C I P I O S   I N F L U Y E N T E S
#-----------------------------------------------------------------------------------------------------------------------

# Se reestima el modelo (a) sin el municipio con mayor distancia de Cook sobre los parámetros
# municipales y (b) sin todos los municipios que superan el umbral en esa medida.

influyentes_mpal <- tabla_cook %>%
  filter(supera_mpal) %>%
  arrange(desc(cook_mpal)) %>%
  pull(codmpio)

influyentes_mpal

extraer_coef <- function(fit, nombre) {
  cs      <- coef(summary(fit))
  n_obs   <- nobs(fit)
  n_grupo <- nlevels(fit@flist[["codmpio"]])
  tibble(
    modelo    = nombre,
    parametro = rownames(cs),
    beta      = cs[, 1],
    ee        = cs[, 2],
    z         = cs[, 3],
    p_valor   = cs[, 4],
    OR        = exp(cs[, 1]),
    n_mujeres = n_obs,
    n_mpios   = n_grupo
  )
}

comparacion <- extraer_coef(M_E6, "Modelo completo")

if (length(influyentes_mpal) > 0) {
  
  M_sin_top1 <- exclude.influence(M_E6, grouping = "codmpio", level = influyentes_mpal[1])
  M_sin_infl <- exclude.influence(M_E6, grouping = "codmpio", level = influyentes_mpal)
  
  comparacion <- bind_rows(
    comparacion,
    extraer_coef(M_sin_top1, paste0("Sin el municipio más influyente (", influyentes_mpal[1], ")")),
    extraer_coef(M_sin_infl, paste0("Sin los ", length(influyentes_mpal), " municipios influyentes"))
  )
}

comparacion %>%
  filter(parametro %in% nombres_fe[c(idx_mpal, idx_cruzada)]) %>%
  select(modelo, parametro, beta, ee, p_valor)

write_csv(comparacion, file.path(dir_out, "02_modelo_sin_influyentes.csv"))



# 9. F I G U R A S
#-----------------------------------------------------------------------------------------------------------------------

# Distancia de Cook sobre los parámetros municipales: los 30 municipios con mayor valor
p_cook <- tabla_cook %>%
  slice_max(cook_mpal, n = 30) %>%
  mutate(etiqueta = fct_reorder(paste0(municipio, " (", codmpio, ")"), cook_mpal)) %>%
  ggplot(aes(x = cook_mpal, y = etiqueta, colour = supera_mpal, shape = supera_mpal)) +
  geom_point(size = 2.2) +
  geom_vline(xintercept = corte_cook, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("FALSE" = "#2C7BB6", "TRUE" = "#D7191C"), guide = "none") +
  scale_shape_manual(values = c("FALSE" = 19, "TRUE" = 17), guide = "none") +
  labs(x = "Distancia de Cook (variables municipales)", y = NULL,
       caption = paste0("Línea punteada: umbral 4/n = ", round(corte_cook, 4), ". Se muestran los 30 municipios con mayor valor.")) +
  theme_bw(base_family = "serif") +
  theme(plot.caption = element_text(hjust = 0, colour = "grey40"))

p_cook
ggsave(file.path(dir_out, "02_cook_variables_municipales.png"), p_cook,
       width = 8, height = 8, dpi = 300, bg = "white")


# DFBETAS de las variables municipales y de la interacción entre niveles
p_dfb <- as.data.frame(dfb[, nombres_fe[c(idx_mpal, idx_cruzada)], drop = FALSE]) %>%
  rownames_to_column("codmpio") %>%
  pivot_longer(-codmpio, names_to = "parametro", values_to = "dfbetas") %>%
  group_by(parametro) %>%
  mutate(orden = rank(dfbetas, ties.method = "first"),
         supera = abs(dfbetas) > corte_dfb) %>%
  ungroup() %>%
  ggplot(aes(x = dfbetas, y = orden, colour = supera, shape = supera)) +
  geom_point(size = 1.2) +
  geom_vline(xintercept = c(-corte_dfb, corte_dfb), linetype = "dashed", colour = "grey40") +
  facet_wrap(~ parametro, ncol = 2) +
  scale_colour_manual(values = c("FALSE" = "#2C7BB6", "TRUE" = "#D7191C"), guide = "none") +
  scale_shape_manual(values = c("FALSE" = 19, "TRUE" = 17), guide = "none") +
  labs(x = "DFBETAS", y = "Municipios (ordenados)",
       caption = paste0("Líneas punteadas: umbral 2/sqrt(n) = ±", round(corte_dfb, 4), ".")) +
  theme_bw(base_family = "serif") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey40"))

p_dfb
ggsave(file.path(dir_out, "02_dfbetas_municipales_y_cruzada.png"), p_dfb,
       width = 9, height = 10, dpi = 300, bg = "white")
