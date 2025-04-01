library(rgplates)
library(dplyr)
library(deeptime)
library(ggplot2)

### Version en construccion. 

# Vector para convertir stages a edades numéricas (Ma)
stage_to_age <- c(

  # Mesozoico - Cretácico
  "Maastrichtian" = 66.0,     # 72.1–66 Ma
  "Campanian" = 83.6,         # 83.6–72.1 Ma
  "Santonian" = 86.3,         # 86.3–83.6 Ma
  "Coniacian" = 89.8,         # 89.8–86.3 Ma
  "Turonian" = 93.9,          # 93.9–89.8 Ma
  "Cenomanian" = 100.5,       # 100.5–93.9 Ma
  "Albian" = 113.0,           # 113–100.5 Ma
  "Aptian" = 125.0,           # 125–113 Ma
  "Barremian" = 129.4,        # 129.4–125 Ma
  "Hauterivian" = 132.9,      # 132.9–129.4 Ma
  "Valanginian" = 139.8,      # 139.8–132.9 Ma
  "Berriasian" = 145.0        # 145–139.8 Ma


  # Mesozoico - Jurásico
  "Tithonian" = 152.1,        # 152.1–145 Ma
  "Kimmeridgian" = 157.3,     # 157.3–152.1 Ma
  "Oxfordian" = 163.5,        # 163.5–157.3 Ma
  "Callovian" = 166.1,        # 166.1–163.5 Ma
  "Bathonian" = 168.3,        # 168.3–166.1 Ma
  "Bajocian" = 170.3,         # 170.3–168.3 Ma
  "Aalenian" = 174.1,         # 174.1–170.3 Ma
  "Toarcian" = 182.7,         # 182.7–174.1 Ma
  "Pliensbachian" = 190.8,    # 190.8–182.7 Ma
  "Sinemurian" = 199.3,       # 199.3–190.8 Ma
  "Hettangian" = 201.3,       # 201.3–199.3 Ma

  # Mesozoico - Triásico
  "Rhaetian" = 208.5,         # 208.5–201.3 Ma
  "Norian" = 227.0,           # 227–208.5 Ma
  "Carnian" = 237.0,          # 237–227 Ma
  "Ladinian" = 242.0,         # 242–237 Ma
  "Anisian" = 247.2,          # 247.2–242 Ma
  "Olenekian" = 251.2,        # 251.2–247.2 Ma
  "Induan" = 252.17,          # 252.17–251.2 Ma


  # Paleozoico 
  "Changhsingian" = 252.17,   # 254.14–250.2 Ma
  "Wuchiapingian" = 256.97,   # 259.51–254.14 Ma
  "Capitanian" = 262.04,      # 265.1–259.51 Ma
  "Wordian" = 266.95,         # 268.8–265.1 Ma
  "Roadian" = 271.38,         # 272.95–268.8 Ma
  "Kungurian" = 279.98,       # 283.5–272.95 Ma
  "Artinskian" = 286.8,       # 290.1–283.5 Ma
  "Sakmarian" = 292.55,       # 295–290.1 Ma
  "Asselian" = 297.0,         # 298.9–295 Ma

  #Paleozoico - Carbonífero
  "Gzhelian" = 301.3,         # 303.7–298.9 Ma
  "Kasimovian" = 305.35,      # 307–303.7 Ma
  "Moscovian" = 311.1,        # 315.2–307 Ma
  "Serpukhovian" = 323.05,    # 330.9–315.2 Ma
  "Visean" = 338.8,           # 346.7–330.9 Ma
  "Tournaisian" = 352.8,      # 358.9–346.7 Ma

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







