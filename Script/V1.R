library(pacman)
p_load("readxl", "tidyverse", "ggthemes", "tidysynth", "Synth", "TTR", "zoo", 
       "lubridate", "reshape2", "httr", "curl", "reshape2", "forecast", "gtable", "grid")

############################ DATOS #############################

# DELITOS

url <- "https://raw.githubusercontent.com/marcomna/Control-Sintetico/main/Datos/IDEFC_NM_abr24.csv?token=GHSAT0AAAAAACTP3PHWGNFTFMGHHNUWCOUGZTJYRXA"

# Nombre temporal del archivo
temp_file <- tempfile(fileext = ".csv")

# Descargar el archivo usando download.file
download.file(url, destfile = temp_file, method = "curl")

# Leer el archivo CSV desde el archivo temporal
delitos <- read_csv(temp_file, locale = locale(encoding = "UTF-8"))


# POBLACIÓN

# URL del archivo CSV en GitHub
url1 <- "https://raw.githubusercontent.com/marcomna/Control-Sintetico/main/Datos/poblacion.csv?token=GHSAT0AAAAAACTP3PHXM6WPVRCQ67ENJK5UZTJYR7Q"

# Nombre temporal del archivo
temp_file1 <- tempfile(fileext = ".csv")

# Descargar el archivo usando download.file
download.file(url1, destfile = temp_file1, method = "curl")

# Leer el archivo CSV desde el archivo temporal
poblacion <- read_csv(temp_file1, locale = locale(encoding = "UTF-8"))

########################## DATA WRANGLING ##########################

# Nos quedamos con 01/2016-12/2023

delitos <- delitos %>% 
  filter(Año > 2016 & Año < 2024)

# Homologar nombres de entidades
delitos$Entidad <- recode(delitos$Entidad,
                          "Coahuila de Zaragoza" = "Coahuila",
                          "Michoacán de Ocampo" = "Michoacán",
                          "Veracruz de Ignacio de la Llave" = "Veracruz")

poblacion$Entidad <- recode(poblacion$Entidad,
                            "Coahuila de Zaragoza" = "Coahuila",
                            "Estado de México" = "México",
                            "Michoacán de Ocampo" = "Michoacán",
                            "Veracruz de Ignacio de la Llave" = "Veracruz")

# Arreglo base delitos y fechas
delitos <- delitos %>%
  gather(mes, valor, -c(Año, Clave_Ent, Entidad, `Bien jurídico afectado`, 
                        `Tipo de delito`, `Subtipo de delito`, Modalidad)) %>%
  mutate(mes = recode(mes, 
                      "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4,
                      "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8,
                      "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12),
         Fecha = as.yearmon(paste(Año, mes), "%Y %m") %>%
           format("%m-%Y") %>%
           my())

####################### PREP CONTROL SINTÉTICO ########################

# Arreglo para CS

delitosCS <- delitos %>% 
  group_by(Fecha, Entidad, Modalidad) %>% 
  summarise(Total = sum(valor, na.rm = TRUE))

delitosCS <- dcast(delitosCS, Fecha + Entidad ~ Modalidad)

delitosCS[,62] <- rowSums(delitosCS[,c(3:61)])
delitosCS <- delitosCS[,c(1,2,62, 3:61)]
names(delitosCS)[names(delitosCS) == 'V62'] <- 'TotalDelitos'
names(delitosCS)[names(delitosCS) == 'Con arma de fuego'] <- 'armas'

delitosCS <- full_join(x = delitosCS, y = poblacion, by = "Entidad")

delitosCS[,3:63] <- delitosCS[,3:63]*100000/delitosCS$Poblacion2020

delitosCS <- delitosCS %>% 
  mutate(Fecha = as.Date(Fecha))

delitosCS <- delitosCS %>% 
  mutate(Entidad = as.factor(Entidad))

####################### SUAVIZACIÓN LOESS ########################

# Función para aplicar suavizado LOESS a una columna
apply_loess_smoothing <- function(column, span = 0.3) {
  if (all(is.na(column))) {
    return(rep(NA, length(column)))
  } else {
    return(predict(loess(column ~ seq_along(column), span = span, na.action = na.exclude), se = FALSE))
  }
}

# Aplicar suavizado LOESS a las columnas de la 3 a la 62
delitosCS_smooth <- delitosCS %>%
  arrange(Fecha) %>%
  group_by(Entidad) %>%
  mutate(across(3:62, ~ apply_loess_smoothing(.)))

####################### SUAVIZACIÓN MOVING AVERAGE ########################

# Función para aplicar medias móviles a una columna
#apply_moving_average <- function(column, n = 3) {
#  if (sum(!is.na(column)) >= n) {
#    smoothed <- rollmean(column, k = n, fill = NA, align = "right")
#    # Reemplazar los NAs iniciales con los valores originales
#   na_indices <- which(is.na(smoothed))
#    smoothed[na_indices] <- column[na_indices]
#    return(smoothed)
#  } else {
#    return(column)
#  }
#}

# Aplicar medias móviles a las columnas de la 3 a la 62
#delitosCS_smooth_ma <- delitosCS %>%
#  arrange(Fecha) %>%
#  group_by(Entidad) %>%
#  mutate(across(3:62, ~ apply_moving_average(.)))

# delitosCS_smooth <- delitosCS_smooth_ma


################# CONTROL SINTÉTICO ##############

CSdelitos <- delitosCS_smooth %>%
  filter(!Entidad %in% c("San Luis Potosí", "Campeche", "Sonora", "Puebla")) %>%
  synthetic_control(outcome = armas, # outcome
                    unit = Entidad, # unit index in the panel data
                    time = Fecha, # time index in the panel data
                    i_unit = "Ciudad de México", # unit where the intervention occurred
                    i_time = as.Date("2019-01-01"), # time period when the intervention occurred
                    generate_placebos = TRUE) %>%
  # Predictores
  generate_predictor(time_window = c(as.Date("2017-01-01"), as.Date("2018-12-01")),
                     rapto = mean(`Con violencia`, na.rm = TRUE),
                     allanamiento = mean(`Robo de motocicleta Con violencia`, na.rm = TRUE),
                     # violacion = mean(`Violación simple`, na.rm = TRUE),
                     narcomenudeo = mean(Narcomenudeo, na.rm = TRUE),
                     coches = mean(`Robo de coche de 4 ruedas Con violencia`, na.rm = TRUE)) %>%
  generate_predictor(time_window = as.Date("2017-01-01"),
                     delitos0417 = armas) %>%
  generate_predictor(time_window = as.Date("2017-04-01"),
                     delitos0717 = armas) %>%
  generate_predictor(time_window = as.Date("2017-07-01"),
                     delitos0917 = armas) %>%
  generate_predictor(time_window = as.Date("2017-10-01"),
                     delitos1017 = armas) %>%
  generate_predictor(time_window = as.Date("2018-01-01"),
                     delitos0418 = armas) %>%
  generate_predictor(time_window = as.Date("2018-04-01"),
                     delitos0718 = armas) %>%
  generate_predictor(time_window = as.Date("2018-07-01"),
                     delitos0918 = armas) %>%
  generate_predictor(time_window = as.Date("2018-10-01"),
                     delitos1018 = armas) %>%
  
  # Promedios anuales
  generate_predictor(time_window = c(as.Date("2017-01-01"), as.Date("2017-12-01")),
                     promedio_anual_2017 = mean(armas, na.rm = TRUE)) %>%
  generate_predictor(time_window = c(as.Date("2018-01-01"), as.Date("2018-12-01")),
                     promedio_anual_2018 = mean(armas, na.rm = TRUE)) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = c(as.Date("2017-01-01"), as.Date("2018-12-01")),
                   margin_ipop = 0.02, sigf_ipop = 7, bound_ipop = 5) %>%
  # Generate the synthetic control
  generate_control()

######################### ANÁLISIS #########################


# Gráfica principal

principalCSdelitos <- unnest(
  CSdelitos , cols = c('.synthetic_control')) %>% 
  filter(.id == "Ciudad de México" & .type == "treated") %>% 
  select(time_unit, real_y, synth_y) %>% 
  mutate(diff = real_y-synth_y)

principalCSdelitos %>%
  ggplot() +
  geom_line(aes(time_unit, real_y), size = 1, color = "#363537") +
  geom_line(aes(time_unit, synth_y), size = 1, color = "#EF2D56") +
  theme_clean() +
  labs(title="Delitos con armas de fuego por cada 100 000 habitantes, \n CDMX observada vs. CDMX sintética.\nSuavizado por promedios móviles",
       x = "Fecha", y = "Incidencia", caption = "\n Fuente: Elaboración propia con datos del SESNSP y Sedena. \n \n Nota: La línea punteada vertical indica el inicio del programa 'Sí al desarme, sí a la paz'.") +
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2023-12-01")),
               breaks = seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "3 months"), 
               labels = scales::date_format("%m/%Y")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 65, hjust = 1), 
        plot.caption = element_text(hjust = 0)) +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", size = 1.1) +
  annotate("text", x = as.Date("2020-05-01"), y = 1.9, label = "Observada", size = 5, fontface = "bold", color = "#363537") +
  annotate("text", x = as.Date("2020-05-01"), y = 2.9, label = "Sintética", size = 5, fontface = "bold", color = "#EF2D56") +
  scale_y_continuous(limits = c(0.5, 3.7))


# Ratio MSPE

MSPEdelitos <- CSdelitos %>% grab_significance()

MSPEdelitos <- MSPEdelitos %>% 
  mutate(RMSPEpre = sqrt(pre_mspe), RMSPEpost = sqrt(post_mspe))


MSPEdelitos$unit_name <- factor(MSPEdelitos$unit_name, levels = unique(MSPEdelitos$unit_name)[order(MSPEdelitos$mspe_ratio, decreasing = F)])

MSPEdelitos %>% 
  ggplot(aes(x=mspe_ratio, y=unit_name)) +
  geom_bar(stat='identity', fill = "#ED7D3A") +
  theme_clean() +
  labs(title="Ratio de MSPE pre y postratamiento \n de CDMX y 27 unidades de control.",
       x = "Ratio", y = "Entidad federativa", )+
  theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold"), axis.title=element_text(size=12,face="bold"),
        axis.text=element_text(size=10), plot.caption = element_text(hjust = 0))


# p-value

MSPEdelitos %>%
  mutate(unit_name = recode(unit_name,
                            "Baja California" = "BC",
                            "Quintana Roo" = "QRoo",
                            "Baja California Sur" = "BCS",
                            "Aguascalientes" = "Ags",
                            "Nuevo León" = "NL",
                            "San Luis Potosí" = "SLP",
                            "Ciudad de México" = "CDMX")) %>% 
  ggplot() +
  geom_point(aes(unit_name, fishers_exact_pvalue), size = 6, color = "#8CD867", fill = "black") +
  theme_clean() +
  labs(title="Niveles de significancia de un efecto de tratamiento sobre la \n variable resultado para todas las entidades federativas.",
       x = "Entidad", y = "p-value")+
  theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold"), axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 65, hjust =1), plot.caption = element_text(hjust = 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))
