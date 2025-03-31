library(tidyr)
library(dplyr)
library(readxl)

# Leer los archivos Excel desde el directorio actual
taxon_data <- read_excel("data/taxon_data.xlsx")

formation_data <- read_excel("data/formation_data.xlsx")

# Unir ambos datasets
fossils <- taxon_data %>% 
  left_join(formation_data, by = c("Formation"))

# Ver resultado
print(fossils)

# Guardar el archivo resultado
write.csv(fossils, "fossils.csv", row.names = FALSE)

