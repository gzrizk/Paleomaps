library(rgplates)

# Función para calcular paleocoordenadas
calculate_paleocoordinates <- function(df, age, model = "MERDITH2021") {
  # Validar que las columnas 'long' y 'lat' existan en el dataset
  if (!all(c("long", "lat") %in% colnames(df))) {
    stop("El dataset debe contener las columnas 'long' y 'lat'.")
  }

  # Reconstruir coordenadas paleogeográficas
  paleo_coords <- reconstruct(df[, c("long", "lat")], age = age, verbose = TRUE, model = model)

  # Combinar datos originales con coordenadas reconstruidas
  resultados <- cbind(df, paleo_coords)

  # Retornar el dataframe con las paleocoordenadas
  return(resultados)
}

# Ejemplo de uso
# Supongamos que tienes un dataset llamado 'fossils_up' cargado en el entorno global
# y deseas calcular las paleocoordenadas para una edad de 300 Ma:
# fossils_up_paleo <- calculate_paleocoordinates(fossils_up, age = 300)

# Ver los resultados
# head(fossils_up_paleo)
