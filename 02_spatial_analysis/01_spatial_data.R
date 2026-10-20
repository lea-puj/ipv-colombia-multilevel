# =============================================================
# ANÁLISIS ESPACIAL DEL IPV EN COLOMBIA
# SCRIPT 01: CARGA, LIMPIEZA Y PREPARACIÓN DE DATOS
# =============================================================
# Descripción:
#   Une el dataset de incidencia de IPV (294 municipios) con el
#   shapefile DANE de municipios colombianos (1122 unidades).
#   Produce un objeto sf filtrado, reproyectado y listo para
#   los análisis de autocorrelación espacial (Scripts 02 y 03).
# Input:  data/processed/municipal_data.rds (01_data_preparation/03)
# Output: data/processed/sf_ipv.rds, data/processed/shape_latam.rds
#         output/02_spatial_analysis/01_cobertura_municipios.png
# Dependencias: sf, tidyverse, here
# =============================================================

library(sf)
library(tidyverse)
library(here)  

# -------------------------------------------------------------
# 1. DIRECTORIOS
# -------------------------------------------------------------

dir_processed <- here("00_data", "processed")
dir_shapes    <- here("00_data", "raw", "shapefiles")
dir_output    <- here("output", "02_spatial_analysis")

# Crear carpeta de output si no existe
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# -------------------------------------------------------------
# 2. CARGA DE DATOS
# -------------------------------------------------------------

# Dataset de IPV (antes BD_incIPV_mpal.xlsx; ahora lo genera
# 01_data_preparation/03_build_municipal_aggregates.R)
dataset_ipv <- readRDS(file.path(dir_processed, "municipal_data.rds"))

# Shapefile municipal DANE (MAGNA-SIRGAS / EPSG:4686)
shape_mun <- st_read(file.path(dir_shapes, "DANE_geodata", "MGN_ANM_MPIOS.shp"))

# Eliminar San Andrés y Providencia:
shape_mun <- shape_mun %>% filter(DPTO_CCDGO != 88)

# Cargar mapa de Latam
shape_latam <- st_read(file.path(dir_shapes, "World", "World.shp"))
shape_latam <- shape_latam %>% filter(region_wb == "Latin America & Caribbean")

# -------------------------------------------------------------
# 3. INSPECCIÓN INICIAL
# -------------------------------------------------------------

# Verificar la clave de unión en el shapefile
# El campo DIVIPOLA de 5 dígitos en este shapefile DANE es MPIO_CDPMP
glimpse(shape_mun %>% st_drop_geometry() %>% select(DPTO_CCDGO, MPIO_CCDGO, MPIO_CDPMP, MPIO_CNMBR))

# Verificar clase y rango del codmpio en el dataset
cat("Rango codmpio:", range(dataset_ipv$codmpio), "\n")
cat("Municipios en dataset:", nrow(dataset_ipv), "\n")
cat("Municipios en shapefile:", nrow(shape_mun), "\n")

# -------------------------------------------------------------
# 4. PREPARACIÓN DE LA CLAVE DE UNIÓN
# -------------------------------------------------------------

# MPIO_CDPMP en el shapefile DANE ya es un string de 5 dígitos (p.ej. "18001").
# codmpio en el dataset es numérico; se convierte a string con ceros a la izquierda.

dataset_ipv <- dataset_ipv %>%
  mutate(MPIO_CDPMP = str_pad(as.character(codmpio), width = 5, pad = "0"))

# shape_mun ya trae MPIO_CDPMP como character; se verifica que no tenga NAs
stopifnot(all(!is.na(shape_mun$MPIO_CDPMP)))

# Diagnóstico: ¿cuántos municipios del dataset están en el shape?
match_check <- dataset_ipv$MPIO_CDPMP %in% shape_mun$MPIO_CDPMP
cat("Municipios del dataset encontrados en el shapefile:",
    sum(match_check), "de", nrow(dataset_ipv), "\n")

if (any(!match_check)) {
  cat("Códigos no encontrados:\n")
  print(dataset_ipv$MPIO_CDPMP[!match_check])
}

# -------------------------------------------------------------
# 5. UNIÓN ESPACIAL
# -------------------------------------------------------------

# Filtrar shape a los 294 municipios con datos y unir atributos
# Nota: MPIO_CNMBR ya viene del shapefile; se excluye del dataset para evitar
# columnas duplicadas (.x / .y) en el join.
sf_ipv <- shape_mun %>%
  filter(MPIO_CDPMP %in% dataset_ipv$MPIO_CDPMP) %>%
  left_join(
    dataset_ipv %>% select(
      MPIO_CDPMP,
      n_mujeres, inc_ipv, casos_ipv,
      inc_ipv_fis, casos_ipv_f,
      inc_ipv_sex, casos_ipv_sex,
      inc_ipv_psi, casos_ipv_psi,
      inc_ipv_eco, casos_ipv_eco
    ),
    by = "MPIO_CDPMP"
  )

cat("Filas en sf_ipv:", nrow(sf_ipv), "\n")
cat("CRS original:", st_crs(sf_ipv)$epsg, "\n")

# -------------------------------------------------------------
# 6. REPROYECCIÓN A MAGNA-SIRGAS / COLOMBIA ZONA ORIGEN ÚNICO
#    EPSG:3116 — métrico, adecuado para cálculo de vecindades
# -------------------------------------------------------------

sf_ipv      <- st_transform(sf_ipv,      crs = 3116)
shape_fondo <- st_transform(shape_mun,   crs = 3116)
shape_latam <- st_transform(shape_latam, crs = 3116)

cat("CRS reproyectado:", st_crs(sf_ipv)$epsg, "\n")

# -------------------------------------------------------------
# 7. VALIDACIÓN DE GEOMETRÍAS
# -------------------------------------------------------------

invalid <- !st_is_valid(sf_ipv)
if (any(invalid)) {
  cat("Geometrías inválidas detectadas:", sum(invalid), "— aplicando st_make_valid()\n")
  sf_ipv <- st_make_valid(sf_ipv)
} else {
  cat("Todas las geometrías son válidas.\n")
}

# -------------------------------------------------------------
# 8. MAPA DE DIAGNÓSTICO: cobertura espacial
# -------------------------------------------------------------

p_cobertura <- ggplot() +
  geom_sf(data = shape_latam, fill = "#E8F4E8", color = "black", linewidth = 0.15) +
  geom_sf(data = shape_fondo, fill = "grey92",  color = "grey75", linewidth = 0.1) +
  geom_sf(data = sf_ipv, aes(fill = inc_ipv),   color = "grey40", linewidth = 0.15) +
  scale_fill_distiller(
    palette   = "YlOrRd",
    direction = 1,
    name      = "Incidencia IPV\n(por 100 mujeres)",
    na.value  = "grey85"
  ) +
  coord_sf(
    xlim = st_bbox(shape_fondo)[c("xmin", "xmax")],
    ylim = st_bbox(shape_fondo)[c("ymin", "ymax")]
  ) +
  labs(
    title    = "Municipios con información de IPV — Colombia",
    subtitle = paste0("n = ", nrow(sf_ipv), " municipios"),
    caption  = "Fuente: elaboración propia. Shapefile: DANE MGN."
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 10),
    legend.position = "right"
  )

ggsave(
  filename = file.path(dir_output, "01_cobertura_municipios.png"),
  plot     = p_cobertura,
  width    = 10, height = 9, dpi = 300
)

# -------------------------------------------------------------
# 9. GUARDAR OBJETOS sf PARA SCRIPTS POSTERIORES
# -------------------------------------------------------------

saveRDS(sf_ipv,      file = file.path(dir_processed, "sf_ipv.rds"))
saveRDS(shape_latam, file = file.path(dir_processed, "shape_latam.rds"))

cat("Objeto sf_ipv guardado en:",      file.path(dir_processed, "sf_ipv.rds"), "\n")
cat("Objeto shape_latam guardado en:", file.path(dir_processed, "shape_latam.rds"), "\n")

# Resumen final
cat("\n--- RESUMEN OBJETO sf_ipv ---\n")
print(sf_ipv %>% st_drop_geometry() %>%
        summarise(
          n_mun     = n(),
          inc_media = round(mean(inc_ipv, na.rm = TRUE), 2),
          inc_min   = round(min(inc_ipv,  na.rm = TRUE), 2),
          inc_max   = round(max(inc_ipv,  na.rm = TRUE), 2),
          n_mujeres = sum(n_mujeres, na.rm = TRUE)
        ))

