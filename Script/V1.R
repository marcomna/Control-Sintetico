library(pacman)
p_load("readxl", "tidyverse", "ggthemes", "tidysynth", "Synth", "TTR", "zoo", 
       "lubridate", "reshape2", "httr", "curl", "reshape2", "forecast", "gtable",
       "grid", "sf")
library(foreign)

############################ DATOS #############################

# DELITOS

url <- "https://raw.githubusercontent.com/marcomna/Control-Sintetico/main/Datos/IDEFC_NM_abr24.csv"

# Nombre temporal del archivo
temp_file <- tempfile(fileext = ".csv")

# Descargar el archivo usando download.file
download.file(url, destfile = temp_file, method = "curl")

# Leer el archivo CSV desde el archivo temporal
delitos <- read_csv(temp_file, locale = locale(encoding = "UTF-8"))


# POBLACIÓN

# URL del archivo CSV en GitHub
url1 <- "https://raw.githubusercontent.com/marcomna/Control-Sintetico/main/Datos/poblacion.csv"

# Nombre temporal del archivo
temp_file1 <- tempfile(fileext = ".csv")

# Descargar el archivo usando download.file
download.file(url1, destfile = temp_file1, method = "curl")

# Leer el archivo CSV desde el archivo temporal
poblacion <- read_csv(temp_file1, locale = locale(encoding = "UTF-8"))

########################## DATA WRANGLING ##########################

# Nos quedamos con 01/2016-12/2023

delitos <- delitos %>% 
  filter(Año > 2016 & Año < 2023)

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

############## DEFUNCIONES ########################

# Define las rutas de los archivos
file_paths <- list(
  "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2022/DEFUN22.dbf",
  "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2021/defun21.dbf",
  "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2020/defun20.dbf",
  "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2019/DEFUN19.dbf",
  "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2018/DEFUN18.dbf",
  "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2017/DEFUN17.dbf"
)

# Función para leer, procesar y seleccionar columnas de cada archivo DBF
process_dbf <- function(file_path) {
  read.dbf(file_path, as.is = TRUE) %>% 
    mutate(across(where(is.character), ~ iconv(., from = "latin1", to = "UTF-8"))) %>% 
    select(ENT_REGIS, CAUSA_DEF, MES_REGIS, ANIO_REGIS)
}

# Leer y procesar todos los archivos DBF
defunciones_list <- lapply(file_paths, process_dbf)

# Combinar las bases de datos
defunciones_combined <- bind_rows(defunciones_list)

# Restablecer el índice y borrar lista
rownames(defunciones_combined) <- NULL
rm(defunciones_list)

# Pegar categorías
file_path_cat <- "C:/Users/L03533611/OneDrive - El Colegio de México A.C/Documentos/COLMEX/Paper/Github/Control-Sintetico/Datos/Defunciones/2022/CATMINDE.dbf"
categorias <- read.dbf(file_path_cat, as.is = TRUE)
categorias <- categorias %>% 
  mutate(across(where(is.character), ~ iconv(., from = "latin1", to = "UTF-8")))
defunciones_combined <- defunciones_combined %>% 
  left_join(categorias, by = c("CAUSA_DEF" = "CVE"))

# Recodificar entidades
# Diccionario de recodificación
entidades <- c(
  "01" = "Aguascalientes",
  "02" = "Baja California",
  "03" = "Baja California Sur",
  "04" = "Campeche",
  "05" = "Coahuila",
  "06" = "Colima",
  "07" = "Chiapas",
  "08" = "Chihuahua",
  "09" = "Ciudad de México",
  "10" = "Durango",
  "11" = "Guanajuato",
  "12" = "Guerrero",
  "13" = "Hidalgo",
  "14" = "Jalisco",
  "15" = "México",
  "16" = "Michoacán",
  "17" = "Morelos",
  "18" = "Nayarit",
  "19" = "Nuevo León",
  "20" = "Oaxaca",
  "21" = "Puebla",
  "22" = "Querétaro",
  "23" = "Quintana Roo",
  "24" = "San Luis Potosí",
  "25" = "Sinaloa",
  "26" = "Sonora",
  "27" = "Tabasco",
  "28" = "Tamaulipas",
  "29" = "Tlaxcala",
  "30" = "Veracruz",
  "31" = "Yucatán",
  "32" = "Zacatecas"
)

# Recodificar la columna ENT_REGIS
defunciones_combined <- defunciones_combined %>%
  mutate(ENT_REGIS = recode(ENT_REGIS, !!!entidades))

# Solo eventos no especificados
defunciones_noespec <- defunciones_combined %>%
  filter(str_detect(DESCRIP, "Evento no especificado"))


# Cuenta por entidad, mes, año y causa
defunciones_noespec <- defunciones_noespec %>%
  group_by(ENT_REGIS, ANIO_REGIS, MES_REGIS) %>%
  summarise(Cuenta = n(), .groups = 'drop')

# Formato fecha
# Crear una nueva columna FECHA_REGIS en formato yyyy-mm-dd
defunciones_noespec <- defunciones_noespec %>%
  mutate(FECHA_REGIS = as.Date(paste(ANIO_REGIS, MES_REGIS, "01", sep = "-"), format = "%Y-%m-%d"))


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

# Asegurarse de que ENT_REGIS y Entidad sean del mismo tipo (factor o character)
delitosCS <- delitosCS %>%
  mutate(Entidad = as.character(Entidad))

defunciones_noespec <- defunciones_noespec %>%
  mutate(ENT_REGIS = as.character(ENT_REGIS))

# Realizar el join y sumar las columnas correspondientes
delitosCS <- delitosCS %>%
  left_join(defunciones_noespec %>% select(FECHA_REGIS, ENT_REGIS, Cuenta), 
            by = c("Fecha" = "FECHA_REGIS", "Entidad" = "ENT_REGIS")) %>%
  mutate(armas_Cuenta = armas + coalesce(Cuenta, 0)) %>%
  select(-Cuenta)  # Eliminar la columna Cuenta si ya no es necesaria

# Mover la columna "armas" a la posición 4 y "armas_Cuenta" a la posición 5
delitosCS <- delitosCS %>%
  select(1:3, armas, armas_Cuenta, 4:10, 12:64)

delitosCS[,3:64] <- delitosCS[,3:64]*100000/delitosCS$Poblacion2020

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
  mutate(across(3:63, ~ apply_loess_smoothing(.)))

####################### SUAVIZACIÓN MOVING AVERAGE ########################

# Función para aplicar medias móviles a una columna
apply_moving_average <- function(column, n = 3) {
  if (sum(!is.na(column)) >= n) {
    smoothed <- rollmean(column, k = n, fill = NA, align = "right")
    # Reemplazar los NAs iniciales con los valores originales
   na_indices <- which(is.na(smoothed))
    smoothed[na_indices] <- column[na_indices]
    return(smoothed)
  } else {
    return(column)
  }
}

# Aplicar medias móviles a las columnas de la 3 a la 62
delitosCS_smooth_ma <- delitosCS %>%
  arrange(Fecha) %>%
  group_by(Entidad) %>%
  mutate(across(3:62, ~ apply_moving_average(.)))

 delitosCS_smooth <- delitosCS_smooth_ma

################## FILTRO VARIABILIDAD ######################}

# Definir el periodo antes del tratamiento
pre_treatment_period <- c(as.Date("2017-01-01"), as.Date("2018-12-01"))

# Calcular la variabilidad de cada unidad antes del tratamiento
variabilidad <- delitosCS_smooth %>%
  filter(Fecha >= pre_treatment_period[1] & Fecha <= pre_treatment_period[2]) %>%
  group_by(Entidad) %>%
  summarise(variabilidad = sd(armas_Cuenta, na.rm = TRUE))

# Definir un umbral de variabilidad (por ejemplo, el cuartil 75)
umbral <- quantile(variabilidad$variabilidad, 0.75)

# Filtrar las unidades con baja variabilidad
unidades_estables <- variabilidad %>%
  filter(variabilidad < umbral) %>%
  pull(Entidad)

# Filtrar el dataframe original para incluir solo unidades estables
delitosCS_filtrado <- delitosCS_smooth %>%
  filter(Entidad %in% unidades_estables | Entidad == "Ciudad de México") # Asegurarse de incluir la unidad tratada

################# CONTROL SINTÉTICO ##############

CSdelitos <- delitosCS_filtrado %>%
  #filter(!Entidad %in% c("Zacatecas", "Michoacán")) %>%
  synthetic_control(outcome = armas_Cuenta, # outcome
                    unit = Entidad, # unit index in the panel data
                    time = Fecha, # time index in the panel data
                    i_unit = "Ciudad de México", # unit where the intervention occurred
                    i_time = as.Date("2019-01-01"), # time period when the intervention occurred
                    generate_placebos = TRUE) %>%
  # Predictores
  generate_predictor(time_window = c(as.Date("2017-01-01"), as.Date("2018-12-01")),
                     # rapto = mean(`Con violencia`, na.rm = TRUE),
                      moto = mean(`Robo de motocicleta Con violencia`, na.rm = TRUE),
                     # violacion = mean(`Violación simple`, na.rm = TRUE),
                     narcomenudeo = mean(Narcomenudeo, na.rm = TRUE),
                     coches = mean(`Robo de coche de 4 ruedas Con violencia`, na.rm = TRUE)) %>%
  generate_predictor(time_window = as.Date("2017-01-01"),
                     delitos0417 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2017-04-01"),
                     delitos0717 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2017-07-01"),
                     delitos0917 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2017-10-01"),
                     delitos1017 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2018-01-01"),
                     delitos0418 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2018-04-01"),
                     delitos0718 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2018-07-01"),
                     delitos0918 = armas_Cuenta) %>%
  generate_predictor(time_window = as.Date("2018-10-01"),
                     delitos1018 = armas_Cuenta) %>%
  
  # Promedios anuales
  generate_predictor(time_window = c(as.Date("2017-01-01"), as.Date("2017-12-01")),
                     promedio_anual_2017 = mean(armas_Cuenta, na.rm = TRUE)) %>%
  generate_predictor(time_window = c(as.Date("2018-01-01"), as.Date("2018-12-01")),
                     promedio_anual_2018 = mean(armas_Cuenta, na.rm = TRUE)) %>%
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
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2022-12-01")),
               breaks = seq(as.Date("2017-01-01"), as.Date("2022-12-01"), by = "3 months"), 
               labels = scales::date_format("%m/%Y")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 65, hjust = 1), 
        plot.caption = element_text(hjust = 0)) +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", size = 1.1) +
  annotate("text", x = as.Date("2020-05-01"), y = 1.9, label = "Observada", size = 5, fontface = "bold", color = "#363537") +
  annotate("text", x = as.Date("2020-05-01"), y = 3.7, label = "Sintética", size = 5, fontface = "bold", color = "#EF2D56") +
  scale_y_continuous(limits = c(0.5, 5.7))


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


# placebos

CSplacebos <- unnest(
  CSdelitos , cols = c('.synthetic_control')) %>% 
  filter(.type == "treated") %>% 
  select(.id, time_unit, real_y, synth_y) %>% 
  rename(Entidad = .id, Fecha = time_unit, `Sintetica`= synth_y, Real = real_y) %>% 
  mutate(diff = Real - Sintetica)


CSplacebos %>%
  filter(Entidad != "Ciudad de México") %>% 
  ggplot(aes(Fecha, diff, group = Entidad,color = Entidad)) +
  geom_line(size = 1, color = "grey50", alpha = 0.2) +
  theme_clean() +
  labs(title="Figura 12. Delitos con armas de fuego por cada 100 000 habitantes, \n diferencias para CDMX y 21 placebos estatales.",
       x = "Fecha", y = "Diferencia", caption = "\n Fuente: Elaboración propia con datos del SESNSP y Sedena. \n \n Nota: Las líneas grises representan la diferencia entre la unidad sintética y observada para las 21 entidades \n federativas del grupo de donantes. La línea punteada vertical indica el inicio del programa 'Sí al desarme, sí \n a la paz'.")+
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%Y") +
  theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold"), axis.title=element_text(size=12,face="bold"),
        axis.text.x = element_text(angle = 65, hjust =1), plot.caption = element_text(hjust = 0))+
  geom_line(data = filter(CSplacebos, Entidad == "Ciudad de México"), size = 1, color = "#8ac926") +
  annotate("text", x=as.Date("2021-01-01"), y=-3, label= "CDMX", size = 5, fontface = "bold", color = "#8ac926") +
  geom_vline(xintercept=as.Date("2019-01-01"), linetype="dashed", size = 1.1)+
  geom_hline(yintercept=0, linetype="dashed", size = 1.1)



