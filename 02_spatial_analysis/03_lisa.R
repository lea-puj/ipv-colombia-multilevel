# =============================================================
# ANÁLISIS ESPACIAL DEL IPV EN COLOMBIA
# SCRIPT 03: LISA — I DE MORAN LOCAL
# =============================================================
# Descripción:
#   Calcula el I de Moran local (LISA) para inc_ipv mediante
#   permutación condicional (nsim = 9999).
#
#   Produce cinco figuras independientes:
#     (a) 03_lisa_completo.png      — todos los municipios sin filtro de sig.
#     (b) 03_lisa_panel_alpha.png   — panel 1×3: α=0.01, α=0.05, α=0.10
#     (c) 03_lisa_pvalores.png      — mapa de p-valores por categoría
#     (d) 03_lisa_scatter.png       — scatter LISA con cuadrantes
#   Y una tabla:
#     (e) 03_lisa_municipios_sig.csv — municipios significativos (α=0.05)
#
# Input:
#   data/processed/sf_ipv.rds         (Script 01)
#   data/processed/moran_objects.rds  (Script 02)
# Output:
#   output/02_spatial_analysis/ (figuras y CSV)
#   data/processed/sf_lisa.rds
# Dependencias: sf, spdep, tidyverse, ggspatial, patchwork, ggrepel, here
# =============================================================

library(sf)
library(spdep)
library(tidyverse)
library(ggspatial)
library(patchwork)
library(ggrepel)
library(here)  

# -------------------------------------------------------------
# 1. DIRECTORIOS Y CARGA
# -------------------------------------------------------------

dir_processed <- here("00_data", "processed")
dir_shapes    <- here("00_data", "raw", "shapefiles")
dir_output    <- here("output", "02_spatial_analysis")

sf_ipv <- readRDS(file.path(dir_processed, "sf_ipv.rds"))
aux    <- readRDS(file.path(dir_processed, "moran_objects.rds"))

lw_knn <- aux$lw_knn
w_mat  <- aux$w_mat
k_opt  <- aux$k_opt
y_std  <- aux$y_std

y <- sf_ipv$inc_ipv

# Shape completo como fondo
shape_fondo <- st_read(
  file.path(dir_shapes, "DANE_geodata", "MGN_ANM_MPIOS.shp"),
  quiet = TRUE
) %>% st_transform(3116)

# -------------------------------------------------------------
# 2. I DE MORAN LOCAL (LISA)
# -------------------------------------------------------------

set.seed(062026)
lisa_result <- localmoran(
  x           = y,
  listw       = lw_knn,
  zero.policy = FALSE,
  alternative = "two.sided"
)

# -------------------------------------------------------------
# 3. CONSTRUCCIÓN DEL DATA FRAME BASE
# -------------------------------------------------------------

wy_std <- as.vector(w_mat %*% y_std)

# Cuadrante sin filtro de significancia (para mapa completo)
asignar_cuadrante <- function(z, wz) {
  case_when(
    z >= 0 & wz >= 0 ~ "Alto-Alto (HH)",
    z <  0 & wz <  0 ~ "Bajo-Bajo (LL)",
    z >= 0 & wz <  0 ~ "Alto-Bajo (HL)",
    z <  0 & wz >= 0 ~ "Bajo-Alto (LH)"
  )
}

niveles_cuadrante <- c(
  "Alto-Alto (HH)", "Bajo-Bajo (LL)",
  "Alto-Bajo (HL)", "Bajo-Alto (LH)",
  "No significativo"
)

lisa_df <- tibble(
  MPIO_CDPMP    = sf_ipv$MPIO_CDPMP,
  municipio     = sf_ipv$MPIO_CNMBR,
  inc_ipv       = y,
  z             = y_std,
  wz            = wy_std,
  Ii            = lisa_result[, "Ii"],
  E_Ii          = lisa_result[, "E.Ii"],
  Var_Ii        = lisa_result[, "Var.Ii"],
  Z_Ii          = lisa_result[, "Z.Ii"],
  p_val         = lisa_result[, "Pr(z != E(Ii))"]
) %>%
  mutate(
    cuadrante_raw = factor(asignar_cuadrante(z, wz),
                           levels = niveles_cuadrante),
    # Cuadrantes filtrados a α=0.05 (referencia para tabla)
    sig_05    = p_val < 0.05,
    cuadrante = factor(
      if_else(sig_05, as.character(cuadrante_raw), "No significativo"),
      levels = niveles_cuadrante
    ),
    p_cat = cut(
      p_val,
      breaks         = c(0, 0.01, 0.05, 0.10, 1),
      labels         = c("p < 0.01", "0.01 ≤ p < 0.05",
                         "0.05 ≤ p < 0.10", "p ≥ 0.10"),
      right          = FALSE,
      include.lowest = TRUE
    )
  )

# Diagnóstico por nivel de significancia
for (a in c(0.01, 0.05, 0.10)) {
  n_sig <- sum(lisa_df$p_val < a)
  cat(sprintf("Municipios significativos (α=%.2f): %d de %d\n",
              a, n_sig, nrow(lisa_df)))
}

# Unión al objeto sf
sf_lisa <- sf_ipv %>%
  left_join(
    lisa_df %>% select(MPIO_CDPMP, Ii, E_Ii, Var_Ii, Z_Ii,
                       p_val, sig_05, cuadrante, cuadrante_raw,
                       p_cat, z, wz),
    by = "MPIO_CDPMP"
  )

# -------------------------------------------------------------
# 4. PALETAS Y TEMA BASE
# -------------------------------------------------------------

pal_lisa <- c(
  "Alto-Alto (HH)"   = "#D7191C",  # rojo vivo
  "Bajo-Bajo (LL)"   = "#2C7BB6",  # azul medio
  "Alto-Bajo (HL)"   = "#FDAE61",  # naranja pálido
  "Bajo-Alto (LH)"   = "#ABD9E9",  # celeste
  "No significativo" = "#D9D9D9"
)

pal_pval <- c(
  "p < 0.01"         = "#1A1A2E",
  "0.01 ≤ p < 0.05"  = "#6B2D6B",
  "0.05 ≤ p < 0.10"  = "#C97B84",
  "p ≥ 0.10"         = "grey88"
)

tema_mapa <- theme_bw(base_family = "serif") +
  theme(
    plot.title            = element_text(face = "bold", size = 12),
    plot.subtitle         = element_text(size = 9, color = "grey40"),
    axis.text             = element_blank(),
    axis.ticks            = element_blank(),
    panel.grid            = element_blank(),
    panel.background      = element_rect(fill = "#dce9f5", color = NA),
    legend.position       = "bottom",
    legend.background     = element_rect(color = "black", fill = NA,
                                         linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    legend.title          = element_text(size = 9, face = "bold"),
    legend.text           = element_text(size = 8)
  )

# Función auxiliar: capa base del mapa
capa_fondo <- function() {
  list(
    geom_sf(data = shape_fondo,
            fill = "grey96", color = "grey80", linewidth = 0.08),
    annotation_north_arrow(
      location = "tl",
      style    = north_arrow_fancy_orienteering(
        fill = c("grey30", "white"), line_col = "grey30",
        text_col = "grey30", text_size = 7
      )
    ),
    annotation_scale(location = "br", text_family = "serif")
  )
}

# -------------------------------------------------------------
# 5. FIGURA A: MAPA COMPLETO (sin filtro de significancia)
# -------------------------------------------------------------

p_completo <- ggplot() +
  capa_fondo() +
  geom_sf(data = sf_lisa,
          aes(fill = cuadrante_raw),
          color = "grey30", linewidth = 0.12) +
  scale_fill_manual(
    values = pal_lisa,
    name   = "Cuadrante LISA",
    drop   = FALSE
  ) +
  labs(
    title    = "Clústeres LISA — todos los municipios",
    subtitle = "Sin filtro de significancia estadística",
    x = NULL, y = NULL
  ) +
  tema_mapa

ggsave(
  filename = file.path(dir_output, "03_lisa_completo.png"),
  plot     = p_completo,
  width    = 9, height = 11, dpi = 300, bg = "white"
)
cat("Mapa LISA completo guardado.\n")

# -------------------------------------------------------------
# 6. FIGURA B: PANEL 1×3 POR NIVEL DE SIGNIFICANCIA
# -------------------------------------------------------------

mapa_alpha <- function(alpha_val, mostrar_norte = FALSE,
                       mostrar_escala = FALSE) {

  sf_plot <- sf_lisa %>%
    mutate(
      cuad_alpha = factor(
        if_else(p_val < alpha_val,
                as.character(cuadrante_raw),
                "No significativo"),
        levels = niveles_cuadrante
      )
    )

  n_sig <- sum(sf_plot$p_val < alpha_val, na.rm = TRUE)

  ggplot() +
    geom_sf(data = shape_fondo,
            fill = "grey96", color = "grey80", linewidth = 0.08) +
    geom_sf(data = sf_plot,
            aes(fill = cuad_alpha),
            color = "grey30", linewidth = 0.12) +
    scale_fill_manual(
      values = pal_lisa,
      name   = "Cuadrante LISA",
      drop   = FALSE
    ) +
    { if (mostrar_norte)
      annotation_north_arrow(
        location = "tl",
        style    = north_arrow_fancy_orienteering(
          fill = c("grey30", "white"), line_col = "grey30",
          text_col = "grey30", text_size = 6
        )
      )
    } +
    { if (mostrar_escala)
      annotation_scale(location = "br", text_family = "serif",
                       text_cex = 0.6)
    } +
    labs(
      title    = sprintf("α = %.2f", alpha_val),
      subtitle = sprintf("n sig. = %d", n_sig),
      x = NULL, y = NULL
    ) +
    tema_mapa +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
          plot.subtitle = element_text(size = 8, hjust = 0.5,
                                       color = "grey40"))
}

p_alpha_01 <- mapa_alpha(0.01, mostrar_norte = TRUE)
p_alpha_05 <- mapa_alpha(0.05)
p_alpha_10 <- mapa_alpha(0.10, mostrar_escala = TRUE)

p_panel_alpha <- (p_alpha_01 | p_alpha_05 | p_alpha_10) +
  plot_layout(guides = "collect") &
  theme(
    legend.position       = "bottom",
    legend.background     = element_rect(color = "black", fill = NA,
                                         linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

ggsave(
  filename = file.path(dir_output, "03_lisa_panel_alpha.png"),
  plot     = p_panel_alpha,
  width    = 18, height = 11, dpi = 300, bg = "white"
)
cat("Panel α guardado.\n")

# -------------------------------------------------------------
# 7. FIGURA C: MAPA DE P-VALORES
# -------------------------------------------------------------

p_pval <- ggplot() +
  capa_fondo() +
  geom_sf(data = sf_lisa,
          aes(fill = p_cat),
          color = "grey35", linewidth = 0.12) +
  scale_fill_manual(
    values   = pal_pval,
    name     = "p-valor (permutación)",
    drop     = FALSE,
    na.value = "grey88"
  ) +
  labs(
    title    = "Significancia estadística — LISA",
    subtitle = "Variable: incidencia de IPV total (por 100 mujeres encuestadas)",
    x = NULL, y = NULL
  ) +
  tema_mapa

ggsave(
  filename = file.path(dir_output, "03_lisa_pvalores.png"),
  plot     = p_pval,
  width    = 9, height = 11, dpi = 300, bg = "white"
)
cat("Mapa de p-valores guardado.\n")

# -------------------------------------------------------------
# 8. FIGURA D: SCATTER LISA
# -------------------------------------------------------------

x_lim <- max(abs(lisa_df$z))  * 0.80
y_lim <- max(abs(lisa_df$wz)) * 0.80

etiquetas <- lisa_df %>%
  filter(sig_05,
         cuadrante %in% c("Alto-Alto (HH)", "Bajo-Bajo (LL)")) %>%
  arrange(desc(abs(Ii))) %>%
  slice_head(n = 12)

p_scatter_lisa <- ggplot(lisa_df, aes(x = z, y = wz)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_point(data = filter(lisa_df, !sig_05),
             shape = 21, size = 1.8, stroke = 0.2,
             fill = "grey82", color = "grey60", alpha = 0.6) +
  geom_point(data = filter(lisa_df, sig_05),
             aes(fill = cuadrante),
             shape = 21, size = 2.5, stroke = 0.35,
             color = "grey20", alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE,
              color     = "#C0392B",
              fill      = scales::alpha("#C0392B", 0.10),
              linewidth = 0.85) +
  geom_text_repel(
    data          = etiquetas,
    aes(label     = municipio),
    size          = 2.6,
    family        = "serif",
    color         = "grey20",
    box.padding   = 0.3,
    max.overlaps  = 15,
    segment.color = "grey60",
    segment.size  = 0.3
  ) +
  annotate("text", x =  x_lim, y =  y_lim, label = "HH",
           size = 4, color = "#C0392B", fontface = "bold", family = "serif") +
  annotate("text", x = -x_lim, y =  y_lim, label = "LH",
           size = 4, color = "#27AE60", fontface = "bold", family = "serif") +
  annotate("text", x =  x_lim, y = -y_lim, label = "HL",
           size = 4, color = "#E67E22", fontface = "bold", family = "serif") +
  annotate("text", x = -x_lim, y = -y_lim, label = "LL",
           size = 4, color = "#2C3E6B", fontface = "bold", family = "serif") +
  scale_fill_manual(
    values = pal_lisa[1:4],
    name   = "Cuadrante LISA"
  ) +
  labs(
    title    = "Diagrama de dispersión LISA",
    subtitle = "Variable: incidencia de IPV total (estandarizada)",
    x        = "z  (incidencia estandarizada)",
    y        = "Wz  (rezago espacial estandarizado)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title            = element_text(face = "bold", size = 12),
    plot.subtitle         = element_text(size = 9, color = "grey40"),
    panel.grid            = element_line(color = "grey92"),
    legend.position       = "bottom",
    legend.background     = element_rect(color = "black", fill = NA,
                                         linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    legend.title          = element_text(size = 9, face = "bold"),
    legend.text           = element_text(size = 8)
  )

ggsave(
  filename = file.path(dir_output, "03_lisa_scatter.png"),
  plot     = p_scatter_lisa,
  width    = 9, height = 8, dpi = 300, bg = "white"
)
cat("Scatter LISA guardado.\n")

# -------------------------------------------------------------
# 9. TABLA DE MUNICIPIOS SIGNIFICATIVOS (α = 0.05)
# -------------------------------------------------------------

tabla_sig <- lisa_df %>%
  filter(sig_05) %>%
  select(municipio, MPIO_CDPMP, inc_ipv,
         Ii, Z_Ii, p_val, cuadrante) %>%
  mutate(
    across(c(inc_ipv, Ii, Z_Ii), ~ round(.x, 3)),
    p_val = round(p_val, 4)
  ) %>%
  arrange(cuadrante, desc(abs(Ii)))

write_csv(tabla_sig, file.path(dir_output, "03_lisa_municipios_sig.csv"))

cat(sprintf("\nMunicipios significativos (α=0.05): %d de %d\n",
            nrow(tabla_sig), nrow(sf_ipv)))
print(tabla_sig, n = Inf)

# -------------------------------------------------------------
# 10. GUARDAR OBJETO sf_lisa
# -------------------------------------------------------------

saveRDS(sf_lisa, file = file.path(dir_processed, "sf_lisa.rds"))

cat("\nResumen de outputs generados:\n")
cat("  output/02_spatial_analysis/03_lisa_completo.png\n")
cat("  output/02_spatial_analysis/03_lisa_panel_alpha.png\n")
cat("  output/02_spatial_analysis/03_lisa_pvalores.png\n")
cat("  output/02_spatial_analysis/03_lisa_scatter.png\n")
cat("  output/02_spatial_analysis/03_lisa_municipios_sig.csv\n")
cat("  data/processed/sf_lisa.rds\n")
