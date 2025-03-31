library(tidyr)
library(dplyr)
library(readxl)
library(snakecase)  # Para convertir nombres a snake_case

# Leer los archivos Excel desde el directorio actual
taxon_data <- read_excel("data/taxon_data.xlsx")
formation_data <- read_excel("data/formation_data.xlsx")

# Unir ambos datasets
fossils <- taxon_data %>% 
  left_join(formation_data, by = c("Formation"))

# Ver resultado
print(fossils)

# Función para crear datasets separados por AgeGroup
split_by_age_group <- function(data) {
  # Obtener los grupos de edad únicos
  age_groups <- unique(data$AgeGroup)
  
  # Crear un directorio para los datasets separados si no existe
  dir.create("data/split", showWarning = FALSE)
  
  # Iterar sobre cada grupo de edad
  walk(age_groups, function(age) {
    subset_data <- data %>% 
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


