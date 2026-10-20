# =============================================================
# ANÁLISIS ESPACIAL DEL IPV EN COLOMBIA
# SCRIPT 02: I DE MORAN GLOBAL
# =============================================================
# Descripción:
#   Construye la matriz de pesos espaciales por k vecinos más
#   cercanos (KNN) a partir de los centroides de los 294
#   municipios con datos. KNN es la especificación más apropiada
#   cuando se trabaja con una submuestra irregular del territorio
#   nacional, ya que garantiza exactamente k vecinos por unidad
#   sin islas artificiales, independientemente de la dispersión.
#
#   Selección de k:
#     Se evalúan k ∈ {3, 4, 5, 6} y se retiene el valor que
#     maximiza el I de Moran global. El resto se reportan como
#     análisis de robustez.
#
#   Estandarización: por fila (W), simétrica (B) como robustez.
#
#   Produce:
#     - Tabla comparativa de I de Moran para k=3..6
#     - Mapa de centroides y conexiones (k óptimo)
#     - I de Moran global: MC + analítico (k óptimo)
#     - Correlograma por órdenes KNN (k=1..8)
#     - Diagrama de dispersión de Moran con cuadrantes
#
# Input:  data/processed/sf_ipv.rds, data/processed/shape_latam.rds (Script 01)
# Output: output/02_spatial_analysis/ (figuras y CSV)
#         data/processed/moran_objects.rds (para Script 03)
# Dependencias: sf, spdep, tidyverse, patchwork, ggspatial, here
# =============================================================

library(sf)
library(spdep)
library(tidyverse)
library(patchwork)
library(ggspatial)
library(here)  

# -------------------------------------------------------------
# 1. DIRECTORIOS Y CARGA
# -------------------------------------------------------------

dir_processed <- here("00_data", "processed")
dir_shapes    <- here("00_data", "raw", "shapefiles")
dir_output    <- here("output", "02_spatial_analysis")

sf_ipv      <- readRDS(file.path(dir_processed, "sf_ipv.rds"))
shape_latam <- readRDS(file.path(dir_processed, "shape_latam.rds"))

y     <- sf_ipv$inc_ipv
y_std <- scale(y)[, 1]

# -------------------------------------------------------------
# 2. CENTROIDES (EPSG:3116, metros)
# -------------------------------------------------------------

centroides <- sf_ipv %>%
  st_centroid() %>%
  mutate(
    lon_c = st_coordinates(.)[, 1],
    lat_c = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(MPIO_CDPMP, MPIO_CNMBR, lon_c, lat_c)

coords_mat <- centroides %>% select(lon_c, lat_c) %>% as.matrix()

# -------------------------------------------------------------
# 3. SELECCIÓN DE k POR CRITERIO DE I DE MORAN MÁXIMO
# -------------------------------------------------------------

evaluar_knn <- function(k) {
  nb  <- knearneigh(coords_mat, k = k) %>% knn2nb()
  lw  <- nb2listw(nb, style = "W", zero.policy = FALSE)
  mt  <- moran.test(y, lw, zero.policy = FALSE)
  tibble(
    k           = k,
    I_moran     = round(mt$estimate["Moran I statistic"], 4),
    E_I         = round(mt$estimate["Expectation"], 4),
    Var_I       = round(mt$estimate["Variance"], 6),
    z_score     = round(mt$statistic, 4),
    p_valor     = round(mt$p.value, 6),
    n_sin_islas = sum(card(nb) == 0)
  )
}

tabla_k <- bind_rows(map(3:6, evaluar_knn))

cat("\n--- SELECCIÓN DE k (I de Moran por especificación KNN) ---\n")
print(tabla_k)

# k óptimo: maximiza I de Moran
k_opt <- tabla_k$k[which.max(tabla_k$I_moran)]
cat(sprintf("\nk seleccionado: %d  (I = %.4f)\n",
            k_opt, max(tabla_k$I_moran)))

write_csv(tabla_k, file.path(dir_output, "02_seleccion_k.csv"))

# -------------------------------------------------------------
# 4. MATRIZ DE PESOS FINAL (k óptimo, estandarización W)
# -------------------------------------------------------------

nb_knn  <- knearneigh(coords_mat, k = k_opt) %>% knn2nb()
lw_knn  <- nb2listw(nb_knn, style = "W", zero.policy = FALSE)

# Matriz densa para rezago vectorial en LISA
w_mat <- listw2mat(lw_knn)

cat(sprintf("\nMatriz KNN (k=%d):\n", k_opt))
cat("  Municipios:", length(nb_knn), "\n")
cat("  Promedio vecinos:", round(mean(card(nb_knn)), 2), "\n")
cat("  Municipios sin vecinos:", sum(card(nb_knn) == 0), "\n")

# -------------------------------------------------------------
# 5. MAPA DE CENTROIDES Y CONEXIONES
# -------------------------------------------------------------

conexiones <- map_dfr(seq_along(nb_knn), function(i) {
  vecinos_i <- nb_knn[[i]]
  tibble(
    x    = centroides$lon_c[i],
    y    = centroides$lat_c[i],
    xend = centroides$lon_c[vecinos_i],
    yend = centroides$lat_c[vecinos_i]
  )
})

shape_fondo <- st_read(
  file.path(dir_shapes, "DANE_geodata", "MGN_ANM_MPIOS.shp"),
  quiet = TRUE
) %>% st_transform(3116) %>% filter(DPTO_CCDGO != 88)

p_pesos <- ggplot() +
  geom_sf(data = shape_latam, fill = "#E8F4E8", color = "black", linewidth = 0.15) +
  geom_sf(data = shape_fondo, fill = "grey96",  color = "grey80", linewidth = 0.08) +
  geom_sf(data = sf_ipv,      fill = "grey88",  color = "grey60", linewidth = 0.15) +
  geom_segment(
    data  = conexiones,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#C0392B", alpha = 0.35, linewidth = 0.2
  ) +
  geom_point(
    data  = centroides,
    aes(x = lon_c, y = lat_c),
    color = "#2C3E6B", size = 1.2, alpha = 0.85
  ) +
  coord_sf(
    xlim = st_bbox(shape_fondo)[c("xmin", "xmax")],
    ylim = st_bbox(shape_fondo)[c("ymin", "ymax")]
  ) +
  annotation_north_arrow(
    location = "tl",
    style    = north_arrow_fancy_orienteering(
      fill = c("grey30", "white"), line_col = "grey30",
      text_col = "grey30", text_size = 7
    )
  ) +
  annotation_scale(location = "br", text_family = "serif") +
  labs(
    title    = sprintf("Matriz de pesos espaciales — KNN (k = %d)", k_opt),
    subtitle = sprintf(
      "n = %d municipios  |  cada punto conectado a sus %d vecinos más cercanos",
      nrow(centroides), k_opt
    ),
    caption  = "Segmentos: pares con w_ij > 0. Puntos: centroides municipales (EPSG:3116).",
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 8, color = "grey55", hjust = 0),
    panel.background = element_rect(fill = "#dce9f5", color = NA),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank()
  )

ggsave(
  filename = file.path(dir_output, "02_mapa_matriz_pesos.png"),
  plot     = p_pesos,
  width    = 10, height = 11, dpi = 300, bg = "white"
)
cat("Mapa de pesos guardado.\n")

# -------------------------------------------------------------
# 6. I DE MORAN GLOBAL — PERMUTACIÓN DE MONTE CARLO
# -------------------------------------------------------------

set.seed(2025)
moran_perm <- moran.mc(
  x     = y,
  listw = lw_knn,
  nsim  = 9999,
  zero.policy = FALSE
)

moran_anal <- moran.test(y, lw_knn, zero.policy = FALSE)

cat(sprintf("\n--- I DE MORAN GLOBAL (inc_ipv, KNN k=%d) ---\n", k_opt))
cat(sprintf("  I observado : %.4f\n", moran_perm$statistic))
cat(sprintf("  E[I]        : %.4f\n", moran_anal$estimate["Expectation"]))
cat(sprintf("  Var[I]      : %.6f\n", moran_anal$estimate["Variance"]))
cat(sprintf("  z-score     : %.4f\n", moran_anal$statistic))
cat(sprintf("  p-valor MC  : %.4f  (nsim = 9999)\n", moran_perm$p.value))
cat(sprintf("  p-valor     : %.4f  (test analítico)\n", moran_anal$p.value))

moran_tabla <- tibble(
  variable    = "inc_ipv",
  n_mun       = nrow(sf_ipv),
  k           = k_opt,
  I_obs       = round(moran_perm$statistic, 4),
  E_I         = round(moran_anal$estimate["Expectation"], 4),
  Var_I       = round(moran_anal$estimate["Variance"], 6),
  z_score     = round(moran_anal$statistic, 4),
  p_MC        = round(moran_perm$p.value, 4),
  p_analitico = round(moran_anal$p.value, 4),
  nsim        = 9999,
  pesos       = sprintf("KNN k=%d (W)", k_opt)
)

write_csv(moran_tabla, file.path(dir_output, "02_moran_global_resultado.csv"))

# -------------------------------------------------------------
# 7. CORRELOGRAMA POR ÓRDENES KNN (k=1..8)
# -------------------------------------------------------------

calcular_moran_knn_orden <- function(h) {
  nb_h <- knearneigh(coords_mat, k = h) %>% knn2nb()
  lw_h <- nb2listw(nb_h, style = "W", zero.policy = FALSE)
  mt   <- moran.test(y, lw_h, zero.policy = FALSE)
  tibble(
    orden    = h,
    I        = round(mt$estimate["Moran I statistic"], 4),
    expected = round(mt$estimate["Expectation"], 4),
    p_val    = round(mt$p.value, 4),
    sig      = mt$p.value < 0.05
  )
}

corr_df <- bind_rows(map(1:8, calcular_moran_knn_orden)) %>%
  mutate(sig_label = if_else(sig, "*", ""))

cat("\n--- CORRELOGRAMA KNN (órdenes 1 a 8) ---\n")
print(corr_df)

p_corr <- ggplot(corr_df, aes(x = orden, y = I)) +
  geom_hline(yintercept = corr_df$expected[1],
             linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  geom_segment(aes(xend = orden, yend = corr_df$expected[1]),
               color = "grey70", linewidth = 0.4) +
  geom_point(aes(color = sig), size = 3.5) +
  geom_line(color = "grey50", linewidth = 0.4, linetype = "dotted") +
  geom_text(aes(label = sig_label), vjust = -0.9, size = 4.5,
            color = "#C0392B") +
  geom_point(data = filter(corr_df, orden == k_opt),
             aes(x = orden, y = I),
             shape = 21, size = 5, color = "#C0392B",
             fill = NA, stroke = 1.2) +
  scale_color_manual(
    values = c("TRUE" = "#2C3E6B", "FALSE" = "grey60"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name   = ""
  ) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title    = "Correlograma espacial — I de Moran por orden KNN",
    subtitle = "Variable: incidencia de IPV total (por 100 mujeres encuestadas)",
    x        = "Número de vecinos más cercanos (k)",
    y        = "I de Moran"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title            = element_text(face = "bold", size = 12),
    plot.caption          = element_text(size = 8, color = "grey55", hjust = 0),
    legend.position       = "bottom",
    legend.background     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

# -------------------------------------------------------------
# 8. DIAGRAMA DE DISPERSIÓN DE MORAN
# -------------------------------------------------------------

wy_std <- as.vector(w_mat %*% y_std)

moran_scatter_df <- tibble(
  MPIO_CDPMP = sf_ipv$MPIO_CDPMP,
  municipio  = sf_ipv$MPIO_CNMBR,
  inc_ipv    = y,
  z          = y_std,
  wz         = wy_std,
  cuadrante  = case_when(
    z >= 0 & wz >= 0 ~ "Alto-Alto (HH)",
    z <  0 & wz <  0 ~ "Bajo-Bajo (LL)",
    z >= 0 & wz <  0 ~ "Alto-Bajo (HL)",
    z <  0 & wz >= 0 ~ "Bajo-Alto (LH)"
  )
)

x_lim <- max(abs(y_std)) * 0.80
y_lim <- max(abs(wy_std)) * 0.80

p_scatter <- ggplot(moran_scatter_df, aes(x = z, y = wz)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_point(aes(fill = cuadrante),
             shape = 21, size = 2, stroke = 0.3,
             color = "grey30", alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE,
              color  = "#C0392B",
              fill   = scales::alpha("#C0392B", 0.12),
              linewidth = 0.9) +
  annotate("text", x =  x_lim, y =  y_lim, label = "HH",
           size = 3.5, color = "grey45", fontface = "bold", family = "serif") +
  annotate("text", x = -x_lim, y =  y_lim, label = "LH",
           size = 3.5, color = "grey45", fontface = "bold", family = "serif") +
  annotate("text", x =  x_lim, y = -y_lim, label = "HL",
           size = 3.5, color = "grey45", fontface = "bold", family = "serif") +
  annotate("text", x = -x_lim, y = -y_lim, label = "LL",
           size = 3.5, color = "grey45", fontface = "bold", family = "serif") +
  scale_fill_manual(
    values = c(
      "Alto-Alto (HH)" = "#C0392B",
      "Bajo-Bajo (LL)" = "#2C3E6B",
      "Alto-Bajo (HL)" = "#E67E22",
      "Bajo-Alto (LH)" = "#27AE60"
    ),
    name = "Cuadrante"
  ) +
  labs(
    title    = "Diagrama de dispersión de Moran",
    subtitle = sprintf(
      "Variable: incidencia de IPV total (estandarizada) — KNN k=%d (W)", k_opt
    ),
    x = "z  (incidencia estandarizada)",
    y = "Wz  (rezago espacial estandarizado)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title            = element_text(face = "bold", size = 12),
    plot.caption          = element_text(size = 8, color = "grey55", hjust = 0),
    legend.position       = "bottom",
    legend.background     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

# -------------------------------------------------------------
# 9. GUARDAR FIGURAS POR SEPARADO
# -------------------------------------------------------------

ggsave(
  filename = file.path(dir_output, "02_corr_knn.png"),
  plot     = p_corr,
  width    = 10, height = 8, dpi = 300, bg = "white"
)

ggsave(
  filename = file.path(dir_output, "02_moran_scatter_plot.png"),
  plot     = p_scatter,
  width    = 10, height = 8, dpi = 300, bg = "white"
)

# -------------------------------------------------------------
# 10. EXPORTAR DATOS DEL SCATTER
# -------------------------------------------------------------

write_csv(
  moran_scatter_df %>% mutate(across(c(z, wz), ~ round(.x, 4))),
  file.path(dir_output, "02_moran_scatter_data.csv")
)

# -------------------------------------------------------------
# 11. GUARDAR OBJETOS PARA SCRIPT 03 (LISA)
# -------------------------------------------------------------

saveRDS(
  list(
    nb_knn           = nb_knn,
    lw_knn           = lw_knn,
    w_mat            = w_mat,
    k_opt            = k_opt,
    y_std            = y_std,
    wy_std           = wy_std,
    moran_scatter_df = moran_scatter_df
  ),
  file = file.path(dir_processed, "moran_objects.rds")
)

cat("\nResumen de outputs generados:\n")
cat("  output/02_spatial_analysis/02_seleccion_k.csv\n")
cat("  output/02_spatial_analysis/02_mapa_matriz_pesos.png\n")
cat("  output/02_spatial_analysis/02_moran_global_resultado.csv\n")
cat("  output/02_spatial_analysis/02_moran_scatter_data.csv\n")
cat("  output/02_spatial_analysis/02_corr_knn.png\n")
cat("  output/02_spatial_analysis/02_moran_scatter_plot.png\n")
cat("  data/processed/moran_objects.rds\n")
