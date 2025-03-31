library(geojson)
library(httr2)
library(rgplates)
library(sf)
library(dplyr)
library(scales) # Para escalar valores

# Función para generar un mapa paleogeográfico con tamaño de puntos proporcional al número de especies
plot_paleomap_richness <- function(df, age, model = "MERDITH2021", proj = "ESRI:54009") {
  # Validar que las columnas 'long', 'lat' y 'Formation' existan en el dataset
  if (!all(c("long", "lat", "Formation") %in% colnames(df))) {
    stop("El dataset debe contener las columnas 'long', 'lat' y 'Formation'.")
  }

  # Calcular el número de especies por formación
  species_count <- df %>%
    group_by(Formation) %>%
    summarise(species_count = n(), .groups = "drop")

  # Unir el conteo de especies al dataset original
  df <- df %>%
    left_join(species_count, by = "Formation")

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

  # Escalar el tamaño de los puntos en función del número de especies
  max_cex <- 3 # Tamaño máximo del punto
  min_cex <- 0.5 # Tamaño mínimo del punto
  scaled_cex <- rescale(fosiles_sf$species_count, to = c(min_cex, max_cex))

  # Plotear localizaciones fósiles con tamaño proporcional al número de especies
  plot(st_geometry(fosiles_sf),
    pch = 17, # Símbolo triángulo
    col = "black", # Color del símbolo
    cex = scaled_cex, # Tamaño del símbolo proporcional al número de especies
    lwd = 0.5, # Grosor del borde
    add = TRUE
  )

  # Mensaje de éxito
  message("Mapa generado exitosamente para la edad ", age, " Ma.")
}

# Ejemplo de uso:
# plot_paleomap_richness(df = my_dataset, age = 100)