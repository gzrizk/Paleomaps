library(rgplates)
library(dplyr)
library(deeptime)
library(ggplot2)
### Version en construccion. 

# Vector para convertir AgeGroup a edades numéricas (Ma)
age_group_to_ma <- c(
  "Cambrian" = 541,
  "Ordovician" = 485,
  "Silurian" = 444,
  "Devonian" = 419,
  "Carboniferous" = 359,
  "Permian" = 299,
  "Triassic" = 252,
  "Jurassic" = 201,
  "Cretaceous" = 145,
  "Paleogene" = 66,
  "Neogene" = 23,
  "Quaternary" = 2.58
)

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

# Leer el dataset principal (fossils)
fossils <- read.csv("fossils.csv")

# Convertir AgeGroup a edades numéricas (Ma)
fossils <- fossils %>%
  mutate(Age = age_group_to_ma[AgeGroup])

# Inicializar un dataframe vacío para almacenar los resultados
fossils_with_paleocoords <- data.frame()

# Recorrer cada grupo de edad único y calcular paleocoordenadas
unique_ages <- unique(fossils$Age)
for (age in unique_ages) {
  # Filtrar los registros correspondientes a la edad actual
  fossils_subset <- fossils %>% filter(Age == age)
  
  # Calcular paleocoordenadas para este subconjunto
  paleo_coords <- calculate_paleocoordinates(fossils_subset, age = age)
  
  # Ensamblar los resultados en el dataframe final
  fossils_with_paleocoords <- bind_rows(fossils_with_paleocoords, paleo_coords)
}

# Guardar el resultado en un archivo CSV
write.csv(fossils_with_paleocoords, "fossils_with_paleocoords.csv", row.names = FALSE)

# Ver los primeros registros del dataframe resultante
print(head(fossils_with_paleocoords))

# Leer el dataset con paleolatitud y edad
paleolat_vs_age <- read.csv("paleolat_vs_age.csv")

# Crear el scatterplot
ggplot(paleolat_vs_age, aes(x = Age, y = paleolat)) +
  geom_point(color = "blue") + # Puntos en azul
  labs(
    x = "Age (Ma)",
    y = "Paleolatitude (º)",
    title = "Paleolatitude vs Age"
  ) +
  scale_x_reverse() + # Invertir el eje X (edad decreciente hacia la derecha)
  theme_minimal()







