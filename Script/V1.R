library(pacman)
p_load("readxl", "tidyverse", "ggthemes", "tidysynth", "TTR", "zoo", "lubridate", "reshape2")

# Importación de datos
datos_incidencia <- read_excel("C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Datos/IDEFC_NM_abr24.xls")
poblacion <- read_excel("C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Datos/poblacion.xlsx")

# Filtrar los datos
datos_incidencia <- datos_incidencia %>% filter(Año > 2016)

# Nombres entidades
datos_incidencia$Entidad[datos_incidencia$Entidad == "Coahuila de Zaragoza"] <- "Coahuila"
datos_incidencia$Entidad[datos_incidencia$Entidad == "Michoacán de Ocampo"] <- "Michoacán"
datos_incidencia$Entidad[datos_incidencia$Entidad == "Veracruz de Ignacio de la Llave"] <- "Veracruz"
poblacion$Entidad[poblacion$Entidad == "Coahuila de Zaragoza"] <- "Coahuila"
poblacion$Entidad[poblacion$Entidad == "Estado de México"] <- "México"
poblacion$Entidad[poblacion$Entidad == "Michoacán de Ocampo"] <- "Michoacán"
poblacion$Entidad[poblacion$Entidad == "Veracruz de Ignacio de la Llave"] <- "Veracruz"

# Arreglo base delitos y fechas
datos_long <- datos_incidencia %>%
  gather(mes, valor, -c(Año, Clave_Ent, Entidad, `Bien jurídico afectado`, `Tipo de delito`, `Subtipo de delito`, Modalidad))

datos_long$mes <- recode(datos_long$mes, "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5, "Junio" = 6, 
                         "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12)

datos_long$Fecha <- as.yearmon(paste(datos_long$Año, datos_long$mes), "%Y %m")
datos_long$Fecha <- my(format(datos_long$Fecha, "%m-%Y"))

# Agregación y pivotación
datos_agrupados <- datos_long %>%
  group_by(Fecha, Entidad, Modalidad) %>%
  summarise(Total = sum(valor, na.rm = TRUE)) %>%
  dcast(Fecha + Entidad ~ Modalidad)

datos_agrupados[,62] <- rowSums(datos_agrupados[,3:61], na.rm = TRUE)
datos_agrupados <- datos_agrupados[,c(1, 2, 62, 3:61)]
names(datos_agrupados)[names(datos_agrupados) == 'V62'] <- 'TotalDelitos'
names(datos_agrupados)[names(datos_agrupados) == 'Con arma de fuego'] <- 'armas'

datos_agrupados <- datos_agrupados %>% filter(Fecha < as.Date("2024-01-01"))

# Unir datos de población
datos_agrupados <- full_join(datos_agrupados, poblacion, by = "Entidad")

# Calcular la tasa por 100,000 habitantes
datos_agrupados[, 3:63] <- datos_agrupados[, 3:63] * 100000 / datos_agrupados$Poblacion2020

# Convertir Entidad a factor
datos_agrupados <- datos_agrupados %>% mutate(Entidad = as.factor(Entidad))

# Marcar Ciudad de México como tratada
datos_agrupados <- datos_agrupados %>%
  mutate(is_treated = ifelse(Entidad == "Ciudad de México", 1, 0))

# Filtrar datos necesarios para el modelo
datos_agrupados <- datos_agrupados %>% filter(!Entidad %in% c("Coahuila", "Durango", "Guanajuato", "Jalisco", "México", "Morelos", "Nayarit", "Querétaro", "Tamulipas", "Tlaxcala", "Zacatecas"))

# Crear el control sintético
library(tidysynth)

synth_model <- datos_agrupados %>%
  synthetic_control(outcome = armas, 
                    unit = Entidad, 
                    time = Fecha, 
                    i_unit = "Ciudad de México", 
                    i_time = as.Date("2019-01-01")) %>%
  generate_predictor(time_window = as.Date("2017-01-01") : as.Date("2018-12-01"), 
                     predictors = list(
                       across(starts_with("Tasa_Suavizada_"), mean, na.rm = TRUE))) %>%
  generate_weights(optimization_window = as.Date("2017-01-01") : as.Date("2018-12-01")) %>%
  generate_control()

# Verificar el modelo
print(synth_model)
