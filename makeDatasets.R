library(tidyr)
library(dplyr)
library(readxl)
library(snakecase)  # Para convertir nombres a snake_case
library(purrr)

# Leer los archivos Excel desde el directorio actual
taxon_data <- read_excel("data/taxon_data_cyathocarpus.xlsx")
formation_data <- read_excel("data/formation_data_cyathocarpus.xlsx")

# Unir ambos datasets
fossils <- taxon_data %>% 
  left_join(formation_data, by = c("Formation"))

# Ver resultado
print(fossils)

# Función para crear datasets separados por AgeGroup
split_by_age_group <- function(fossils) {
  # Obtener los grupos de edad únicos
  age_groups <- unique(fossils$AgeGroup)
  
  # Crear un directorio para los datasets separados si no existe
  dir.create("data/split", showWarnings = FALSE)
  
  # Iterar sobre cada grupo de edad
  walk(age_groups, function(age) {
    subset_data <- fossils %>% 
      filter(AgeGroup == age)
    
    # Crear nombre del archivo en snake_case
    file_name <- paste0("fossils_", to_snake_case(age), ".csv")
    file_path <- file.path("data/split", file_name)
    
    # Guardar el subset como CSV
    write.csv(subset_data, file_path, row.names = FALSE)
    
    # Mensaje de confirmación
    paste("Dataset creado para:", age, "en:", file_path)
  })
}

# Aplicar la función para crear los datasets separados
split_by_age_group(fossils)

# Guardar el archivo principal
write.csv(fossils, "fossils.csv", row.names = FALSE)


----

library(tidyr)
library(dplyr)
library(readxl)
library(snakecase)  # Para convertir nombres a snake_case
library(purrr)

# Leer los archivos Excel desde el directorio actual
taxon_data <- read_excel("data/taxon_data_cyathocarpus.xlsx")
formation_data <- read_excel("data/formation_data_cyathocarpus.xlsx")

# Unir ambos datasets
fossils <- taxon_data %>% 
  left_join(formation_data, by = c("Formation"))

# Ver resultado
print(fossils)

# Función para crear datasets separados por AgeGroup
split_by_age_group <- function(fossils) {
  # Obtener los grupos de edad únicos
  age_groups <- unique(fossils$AgeGroup)
  
  # Crear un directorio para los datasets separados si no existe
  dir.create("data/split", showWarnings = FALSE)
  
  # Crear una lista para almacenar los dataframes
  split_data <- map(age_groups, function(age) {
    subset_data <- fossils %>% 
      filter(AgeGroup == age)
    
    # Crear nombre del archivo en snake_case
    file_name <- paste0("fossils_", to_snake_case(age), ".csv")
    file_path <- file.path("data/split", file_name)
    
    # Guardar el subset como CSV
    write.csv(subset_data, file_path, row.names = FALSE)
    
    # Mostrar el dataframe antes de guardar
    print(paste("Dataset para:", age))
    print(subset_data)
    
    return(subset_data)
  })
  
  return(split_data)
}

# Aplicar la función para crear los datasets separados y mostrarlos
split_data <- split_by_age_group(fossils)

# Ver los dataframes en la lista split_data
for (df in split_data) {
  print(df)
}

# Guardar el archivo principal
write.csv(fossils, "fossils.csv", row.names = FALSE)
