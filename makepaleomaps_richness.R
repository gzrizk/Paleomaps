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

  # Configurar área de ploteo con márgenes ajustados
  par(mar = c(3, 3, 3, 3), bg = "white") # Márgenes más amplios para evitar cortes

  # Ajustar manualmente los límites para cubrir toda la elipse
  xlim <- c(-20000000, 20000000) # Límites X ajustados para Mollweide
  ylim <- c(-10000000, 10000000) # Límites Y ajustados para Mollweide

  # Dibujar un fondo blanco para completar la elipse
  plot(1, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "", asp = 1, axes = FALSE)
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "white", border = NA)

  # Plotear límites de placas
  plot(st_geometry(paleoplates_proj),
    col = "gray",
    border = NA,
    add = TRUE
  )

  # Plotear líneas de costa modernas
  plot(st_geometry(modern_coast_proj),
    col = "gray70",
    border = NA,
    add = TRUE
  )

  # Plotear grilla de líneas de latitud y longitud
  plot(st_geometry(graticule),
    col = "gray80",
    lty = "dotted",
    add = TRUE
  )

  # Plotear localizaciones fósiles
  plot(st_geometry(fosiles_sf),
    pch = 17, # Símbolo triángulo
    col = "black", # Color del símbolo
    cex = 1.2, # Tamaño del símbolo
    lwd = 0.5, # Grosor del borde
    add = TRUE
  )

  # Mensaje de éxito
  message("Mapa generado exitosamente para la edad ", age, " Ma.")
}

