library(geojson)
library(httr2)
library(rgplates)
library(sf)
library(dplyr)

# Función para cargar todos los archivos CSV de una carpeta
load_all_datasets <- function(folder_path) {
  # Obtener la lista de archivos CSV en la carpeta
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Verificar si hay archivos en la carpeta
  if (length(files) == 0) {
    stop("No se encontraron archivos CSV en la carpeta especificada.")
  }
  
  # Leer cada archivo y almacenarlo en una lista
  datasets <- lapply(files, function(file) {
    message("Cargando archivo: ", file)
    df <- read.csv(file)
    df$file_name <- basename(file)  # Agregar el nombre del archivo como columna
    return(df)
  })
  
  # Asignar nombres a la lista basados en los nombres de los archivos
  names(datasets) <- basename(files)
  
  # Mostrar un resumen compacto de cada dataset usando glimpse
  message("Resumen de los datasets cargados:")
  lapply(datasets, glimpse)
  
  # Retornar la lista de datasets
  return(datasets)
}

# Cargar todos los datasets de la carpeta 'split'
datasets <- load_all_datasets("data/split")


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
    resultados[!is.na(resultados$paleolat), ],  # Filtrar NA
    coords = c("paleolong", "paleolat"),
    crs = 4326  # WGS84 (CRS nativo de GPlates)
  ) %>% 
    st_transform(crs = proj)  # Transformar a la proyección especificada
  
  # Configurar área de plot
  par(mar = c(0, 0, 0, 0), bg = "white")
  
  # Plotear límites de placas
  plot(st_geometry(paleoplates_proj),
       col = "gray",
       border = NA,
       main = paste("Reconstrucción", age, "Ma"))
  
  # Plotear líneas de costa modernas
  plot(st_geometry(modern_coast_proj),
       col = "gray70",
       border = NA,
       add = TRUE)
  
  # Plotear localizaciones fósiles
  plot(st_geometry(fosiles_sf),
       pch = 17,        # Símbolo triángulo
       col = "black",   # Color del símbolo
       cex = 1.2,       # Tamaño del símbolo
       lwd = 0.5,       # Grosor del borde
       add = TRUE)
  
  # Mensaje de éxito
  message("Mapa generado exitosamente para la edad ", age, " Ma.")
}



