library(geojson)
library(httr2)
library(rgplates)
library(icosa)
library(chronosphere)
library(sf)

# Script para usar con el modulo Online de GPlates Web Service
# Default model: MERDITH2021 - present-day, partitioning polygons
# Naming convention: no model name = default model

###########################################
## PASO 1: Definir parámetros principales
###########################################

# Edad de reconstrucción en millones de años
edad <- 250  # Ejemplo para el Triásico Superior

# Modelo tectónico a utilizar (ver data(gws) para alternativas)
modelo <- "MERDITH2021"

# Proyección cartográfica (Mollweide - ESRI:54009)
proj <- "ESRI:54009"

###########################################
## PASO 2: Reconstrucciones paleogeográficas
###########################################

# 2.1 Reconstruir límites de placas
paleoplates <- reconstruct("static_polygons",
                           age = edad,
                           verbose = TRUE,
                           model = modelo)

# 2.2 Reconstruir líneas de costa modernas (referencia)
modern_coast <- reconstruct("coastlines",
                            age = edad,  
                            model = modelo)

###########################################
## PASO 3: Sistema de coordenadas y proyección
###########################################

# Transformar todos los elementos a Mollweide
paleoplates_proj <- st_transform(paleoplates, crs = proj)
modern_coast_proj <- st_transform(modern_coast, crs = proj)

###########################################
## PASO 4: Carga y procesamiento de datos fósiles
###########################################

# 4.1 Cargar  actuales desde CSV
# fosiles <- read.csv(
#   file = 'fosiles.csv',
#   header = TRUE,
#   sep = ",",
#   dec = "."
# )

# 4.2 Reconstruir coordenadas paleo
paleo_coords <- reconstruct(fosiles[, c("long", "lat")],
                            age = edad,
                            verbose = TRUE,
                            model = modelo)

# 4.3 Combinar datos originales con coordenadas reconstruidas
resultados <- cbind(fosiles, paleo_coords)

# 4.4 Crear objeto espacial proyectado
fosiles_sf <- st_as_sf(
  resultados[!is.na(resultados$paleolat), ],  # Filtrar NA
  coords = c("paleolong", "paleolat"),
  crs = 4326  # WGS84 (CRS nativo de GPlates)
) %>% 
  st_transform(crs = proj)  # Transformar a Mollweide

###########################################
## PASO 5: Visualización integrada
###########################################

# Configurar área de plot
par(mar = c(0, 0, 0, 0), bg = "white")

# 5.1 Capa base: Límites de placas
plot(st_geometry(paleoplates_proj),
     col = "gray",
     border = NA,
     main = paste("Reconstrucción", edad, "Ma"))

# 5.2 Capa intermedia: Costa moderna (referencia)
plot(st_geometry(modern_coast_proj),
     col = "gray70",
     border = NA,
     add = TRUE)

# # 5.3 Capa superior: Localizaciones fósiles
# plot(st_geometry(fosiles_sf),
#      pch = 17,        # Símbolo triángulo
#      col = "black",   # Color del símbolo
#      cex = 1.2,       # Tamaño del símbolo
#      lwd = 0.5,       # Grosor del borde
#      add = TRUE)



# 5.3 Capa superior: Localizaciones fósiles (diferenciadas por grupo)
# Filtrar por grupos
grupo_A <- fosiles_sf[fosiles_sf$grupo == "A", ]
grupo_B <- fosiles_sf[fosiles_sf$grupo == "B", ]

# Plotear cada grupo con su símbolo
plot(st_geometry(grupo_A),
     pch = 17,        # Triángulo para grupo A
     col = "black",
     cex = 1.2,
     lwd = 0.5,
     add = TRUE)

plot(st_geometry(grupo_B),
     pch = 16,        # Círculo para grupo B
     col = "black",
     cex = 1.2,
     lwd = 0.5,
     add = TRUE)

# 5.4 Leyenda
legend(
  "bottomleft",
  legend = c("Grupo A", "Grupo B"),
  pch = c(17, 16),
  col = "black",
  pt.cex = 1.2,
  title = "Grupos taxonómicos",
  bg = "white"
)

