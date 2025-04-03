library(geojson)
library(httr2)
library(rgplates)
library(sf)
library(dplyr)

#Leer atentamente, cada paso esta especificado que hace! 


# Creamos la función para cargar todos los archivos CSV de una carpeta y asignarlos al entorno global
load_all_datasets <- function(folder_path) {
  # Obtener la lista de archivos CSV en la carpeta
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

  # Verificar si hay archivos en la carpeta
  if (length(files) == 0) {
    stop("No se encontraron archivos CSV en la carpeta especificada.")
  }

  # Leer cada archivo y asignarlo al entorno global
  for (file in files) {
    # Leer el archivo CSV
    df <- read.csv(file)

    # Obtener el nombre base del archivo (sin extensión)
    dataset_name <- tools::file_path_sans_ext(basename(file))

    # Asignar el dataframe al entorno global con el nombre del archivo
    assign(dataset_name, df, envir = .GlobalEnv)

    # Mostrar un resumen compacto del dataset usando glimpse
    message("Cargando dataset: ", dataset_name)
    glimpse(df)
  }

  message("Todos los datasets han sido cargados en el entorno global.")
}

# Cargar todos los datasets de la carpeta 'split'
load_all_datasets("data/split")


# Función para generar un mapa paleogeográfico
plot_paleomap <- function(df, age, model = "MERDITH2021", proj = "ESRI:54009") {
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

  # Crear grilla de líneas de latitud y longitud
  graticule <- st_graticule(
    x = st_bbox(paleoplates_proj), # Usar los límites del mapa proyectado
    crs = proj, # Proyección del mapa
    lon = seq(-180, 180, by = 30), # Longitudes cada 30 grados
    lat = seq(-90, 90, by = 30)    # Latitudes cada 30 grados
  )

  # Configurar área de ploteo con márgenes ajustados
  par(mar = c(3, 3, 3, 3), bg = "white") # Márgenes más amplios para evitar cortes
  
  # Ajustar manualmente los límites para cubrir toda la elipse
  xlim <- c(-20000000, 20000000) # Límites X ajustados para Mollweide
  ylim <- c(-10000000, 10000000) # Límites Y ajustados para Mollweide
  
  # Dibujar un fondo blanco para completar la elipse
  plot(1, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "", asp = 1, axes = FALSE)
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "white", border = NA)

  # Obtener límites del mapa proyectado
  bbox <- st_bbox(paleoplates_proj)
  xlim <- c(bbox["xmin"] * 1.1, bbox["xmax"] * 1.1) # Expandir límites en un 10%
  ylim <- c(bbox["ymin"] * 1.1, bbox["ymax"] * 1.1) # Expandir límites en un 10%

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

