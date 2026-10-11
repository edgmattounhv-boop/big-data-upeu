library(tidyverse)

# Cargar los datos generados por datos.R
source("datos.R")

# ============================================================
# 1. ESTRUCTURA GENERAL DEL DATASET
# ============================================================

dim(datos)

names(datos)

glimpse(datos)

# ============================================================
# 2. VERIFICAR VALORES FALTANTES
# ============================================================

colSums(is.na(datos))

# ============================================================
# 3. ESTADÍSTICOS PRINCIPALES
# ============================================================

resumen_general <- datos %>%
  summarise(
    registros = n(),
    vehiculos_min = min(vehiculos),
    vehiculos_max = max(vehiculos),
    vehiculos_promedio = mean(vehiculos),
    velocidad_min = min(velocidad_kmh),
    velocidad_max = max(velocidad_kmh),
    velocidad_promedio = mean(velocidad_kmh)
  )

print(resumen_general)

# ============================================================
# 4. RESUMEN POR PUNTO
# ============================================================

resumen_punto <- datos %>%
  group_by(punto) %>%
  summarise(
    registros = n(),
    vehiculos_promedio = mean(vehiculos),
    velocidad_promedio = mean(velocidad_kmh),
    .groups = "drop"
  )

print(resumen_punto)

# ============================================================
# 5. DISTRIBUCIÓN DE CONGESTIÓN
# ============================================================

table(datos$congestion)

prop.table(
  table(datos$congestion)
) * 100

# ============================================================
# 6. CORRELACIÓN ENTRE VEHÍCULOS Y VELOCIDAD
# ============================================================

cor(
  datos$vehiculos,
  datos$velocidad_kmh
)

# ============================================================
# 7. RESUMEN POR HORA
# ============================================================

resumen_hora <- datos %>%
  mutate(
    hora_entera = floor(hora)
  ) %>%
  group_by(hora_entera) %>%
  summarise(
    vehiculos_promedio = mean(vehiculos),
    velocidad_promedio = mean(velocidad_kmh),
    .groups = "drop"
  )

print(resumen_hora)