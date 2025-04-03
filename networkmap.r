# Cargar librerías necesarias
library(geojson)
library(httr2)
library(rgplates)
library(sf)
library(dplyr)
library(igraph)  # Para manejar grafos y conexiones

# Leer el dataset de fósiles
fossils <- read.csv("data/split/fossils_up.csv")

# Función para generar un único mapa paleogeográfico con conexiones entre formaciones
plot_single_paleomap_with_connections <- function(df, age, model = "MERDITH2021", proj = "ESRI:54009", output_file = "paleomap_with_connections.pdf") {
  # Validar que las columnas 'long', 'lat' y 'validName' existan en el dataset
  if (!all(c("long", "lat", "validName") %in% colnames(df))) {
    stop("El dataset debe contener las columnas 'long', 'lat' y 'validName'.")
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

  # Crear grilla global en WGS84 y proyectarla
  graticule_wgs84 <- st_graticule(
    lon = seq(-180, 180, by = 30),
    lat = seq(-90, 90, by = 30)
  ) %>% 
    st_transform(crs = proj)

  # Límites estándar de Mollweide
  xlim <- c(-18086197, 18086197)
  ylim <- c(-9023548, 9023548)

  # Crear grafo de conexiones entre formaciones
  connections <- df %>%
    select(formation, validName) %>%
    distinct() %>%
    group_by(validName) %>%
    summarise(formations = list(formation)) %>%
    ungroup()

  edges <- do.call(rbind, lapply(connections$formations, function(f) {
    if (length(f) > 1) {
      combn(f, 2, simplify = FALSE)
    } else {
      NULL
    }
  }))

  edges <- do.call(rbind, edges)
  colnames(edges) <- c("from", "to")
  edges <- as.data.frame(edges)

  # Configurar dispositivo gráfico para exportación a PDF
  pdf(output_file, width = 7.09, height = 4.72)  # Tamaño en pulgadas (180 mm x 120 mm)

  # Configurar área de ploteo
  par(mar = c(0.1, 0.1, 2, 0.1))  # Margen superior para el título
  plot.new()
  plot.window(xlim = xlim, ylim = ylim, asp = 1)
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "white", border = NA)

  # Plotear capas
  plot(st_geometry(paleoplates_proj), col = "gray", border = NA, add = TRUE)
  plot(st_geometry(modern_coast_proj), col = "gray70", border = NA, add = TRUE)
  plot(st_geometry(graticule_wgs84), col = "gray80", lty = 3, add = TRUE)
  plot(st_geometry(fosiles_sf), pch = 17, col = "black", cex = 1.2, add = TRUE)

  # Dibujar conexiones entre formaciones
  for (i in 1:nrow(edges)) {
    from <- edges$from[i]
    to <- edges$to[i]

    # Obtener coordenadas de las formaciones
    from_coords <- fosiles_sf[fosiles_sf$formation == from, ]
    to_coords <- fosiles_sf[fosiles_sf$formation == to, ]

    if (nrow(from_coords) > 0 && nrow(to_coords) > 0) {
      lines(
        x = c(st_coordinates(from_coords)[1, 1], st_coordinates(to_coords)[1, 1]),
        y = c(st_coordinates(from_coords)[1, 2], st_coordinates(to_coords)[1, 2]),
        col = "black", lty = 1
      )
    }
  }

  # Añadir título
  title(sprintf("Mapa paleogeográfico con conexiones - Edad: %d Ma", age), line = 0.5, cex.main = 0.9)

  # Cerrar dispositivo gráfico
  dev.off()

  # Mensaje de confirmación
  message("Mapa guardado como: ", output_file)
}

# Probar la función con el dataset de fósiles y una edad específica
plot_single_paleomap_with_connections(fossils, age = 300)
