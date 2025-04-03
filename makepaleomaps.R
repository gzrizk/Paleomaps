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
plot_paleomap <- function(df, age, model = "MERDITH2021", proj = "ESRI:54009", new_plot = TRUE, title = "") {
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

  # Crear grilla global en WGS84 y proyectarla
  graticule_wgs84 <- st_graticule(
    lon = seq(-180, 180, by = 30),
    lat = seq(-90, 90, by = 30)
  ) %>% 
    st_transform(crs = proj)

  # Límites estándar de Mollweide
  xlim <- c(-18086197, 18086197)
  ylim <- c(-9023548, 9023548)

  # Configurar área de ploteo solo si es un nuevo gráfico
  if (new_plot) {
    par(mar = c(0.1, 0.1, 2, 0.1))  # Margen superior para el título
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = 1)
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "white", border = NA)
  }

  # Plotear capas
  plot(st_geometry(paleoplates_proj), col = "gray", border = NA, add = TRUE)
  plot(st_geometry(modern_coast_proj), col = "gray70", border = NA, add = TRUE)
  plot(st_geometry(graticule_wgs84), col = "gray80", lty = 3, add = TRUE)
  plot(st_geometry(fosiles_sf), pch = 17, col = "black", cex = 1.2, add = TRUE)

  # Añadir título
  if (new_plot) {
    title(paste(title, age, "Ma"), line = 0.5, cex.main = 0.9)
  }
}

# Función para generar dos mapas apilados
plot_two_maps <- function(df1, age1, df2, age2) {
  # Configurar dispositivo gráfico para exportación
  tiff("mapas_apilados.tiff", 
       width = 180, height = 240, units = "mm", res = 600, 
       compression = "lzw", bg = "white")

  # Configurar layout vertical (2 filas, 1 columna)
  layout(matrix(c(1, 2), nrow = 2, byrow = TRUE))
  
  # Primer mapa (arriba)
  plot_paleomap(df1, age1, title = "A. Edad", new_plot = TRUE)
  
  # Segundo mapa (abajo)
  plot_paleomap(df2, age2, title = "B. Edad", new_plot = TRUE)

  # Cerrar dispositivo gráfico
  dev.off()
}

