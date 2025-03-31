library(geojson)
library(httr2)
library(rgplates)
library(sf)
library(dplyr)

# Función para agregar jitter a un objeto sf
add_jitter_to_sf <- function(sf_object, jitter_amount = 0.1) {
  # Extraer las coordenadas del objeto sf
  coords <- st_coordinates(sf_object)
  
  # Aplicar jitter a las coordenadas
  coords_jittered <- coords
  coords_jittered[, 1] <- jitter(coords[, 1], amount = jitter_amount) # Jitter en longitud
  coords_jittered[, 2] <- jitter(coords[, 2], amount = jitter_amount) # Jitter en latitud
  
  # Crear un nuevo objeto sf con las coordenadas modificadas
  sf_object_jittered <- st_as_sf(
    data.frame(sf_object, coords_jittered),
    coords = c("X", "Y"),
    crs = st_crs(sf_object)
  )
  
  return(sf_object_jittered)
}

# Función para generar un mapa paleogeográfico con jitter
plot_paleomap_jitter <- function(df, age, model = "MERDITH2021", proj = "ESRI:54009", jitter_amount = 0.1) {
  # Validar que las columnas 'long' y 'lat' existan en el dataset
  if (!all(c("long", "lat") %in% colnames(df))) {
    stop("El dataset debe contener las columnas 'long' y 'lat'.")
  }

  # Reconstruir límites de placas
  paleoplates <- reconstruct("static_polygons", age = age, verbose = TRUE, model = model)

  # Reconstruir líneas de costa modernas
  modern_coast <- reconstruct("coastlines", age = age, model = model)

  # Transformar a la proyección especificada
  paleoplates_proj <- st_transform(paleoplates, crs = proj)
  modern_coast_proj <- st_transform(modern_coast, crs = proj)

  # Reconstruir coordenadas paleogeográficas de los fósiles
  paleo_coords <- reconstruct(df[, c("long", "lat")], age = age, verbose = TRUE, model = model)

  # Combinar datos originales con coordenadas reconstruidas
  resultados <- cbind(df, paleo_coords)

  # Crear objeto espacial proyectado
  fosiles_sf <- st_as_sf(
    resultados[!is.na(resultados$paleolat), ], # Filtrar NA
    coords = c("paleolong", "paleolat"),
    crs = 4326 # WGS84 (CRS nativo de GPlates)
  ) %>%
    st_transform(crs = proj) # Transformar a la proyección especificada

  # Aplicar jitter a las coordenadas
  fosiles_sf_jittered <- add_jitter_to_sf(fosiles_sf, jitter_amount = jitter_amount)

  # Configurar área de plot
  par(mar = c(0, 0, 0, 0), bg = "white")

  # Plotear límites de placas
  plot(st_geometry(paleoplates_proj),
    col = "gray",
    border = NA,
    main = paste("Reconstrucción", age, "Ma")
  )

  # Plotear líneas de costa modernas
  plot(st_geometry(modern_coast_proj),
    col = "gray70",
    border = NA,
    add = TRUE
  )

  # Plotear localizaciones fósiles con jitter
  plot(st_geometry(fosiles_sf_jittered),
    pch = 17, # Símbolo triángulo
    col = "black", # Color del símbolo
    cex = 1.2, # Tamaño del símbolo
    lwd = 0.5, # Grosor del borde
    add = TRUE
  )

  # Mensaje de éxito
  message("Mapa generado exitosamente para la edad ", age, " Ma con jitter aplicado.")
}