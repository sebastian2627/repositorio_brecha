# Repositorio ENEMDU

# Preliminares --------------------------------------------------------------------------------------------

# Cargar librerias
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")

# Cargando datos ------------------------------------------------------------------------------------------

ENEMDU_2007 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2007_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2008 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2008_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2009 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2009_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2010 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2010_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2011 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2011_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2012 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2012_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2013 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2013_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2014 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2014_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2015 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2015_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2016 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2016_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2017 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2017_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
ENEMDU_2018 <- read_delim("data_enemdu/ENEMDU_PERSONAS_2018_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Definir una función para seleccionar las variables deseadas de las ENEMDU

vars <- function(data) {
  return(data[, c("p02", "p01", "p03", "p06", "p24", "p45", "ingrl", "nnivins", "periodo", "rama1")])
}

# Crear una lista de las bases de datos

bases <- list(ENEMDU_2007, ENEMDU_2008, ENEMDU_2009, ENEMDU_2010, ENEMDU_2011,
                       ENEMDU_2012, ENEMDU_2013, ENEMDU_2014, ENEMDU_2015, ENEMDU_2016,
                       ENEMDU_2017, ENEMDU_2018)

# Aplicar la función a cada base de datos usando lapply

bases_selec <- lapply(bases, vars)

# Unir los dataframes en la lista con bind_rows

ENEMDU_TOT <- bind_rows(bases_selec)

# Base para el analisis ------------------------------------------------------------------------------------------------

df_bases <- ENEMDU_TOT %>%
  select(   'sexo' = 'p02',
            'persona' = 'p01',
            'edad' = 'p03',
            'estado_civil' = 'p06',
            'horas_trabajadas' = 'p24',
            'experiencia_laboral' = 'p45',
            'ingreso_laboral'='ingrl',
            'nivel_instruccion'='nnivins',
            'ano' = 'periodo',
            'ciiu' = 'rama1') %>%
  filter(edad >= 23, edad <= 65, 
         horas_trabajadas > 0, horas_trabajadas < 999,
         experiencia_laboral > 0, experiencia_laboral < 99,
         ingreso_laboral > 0, ingreso_laboral < 999999,
         estado_civil %in% c(1,6)) %>%
  mutate(sexo = factor(sexo, levels = c(1:2), labels = c("Hombre", "Mujer")),
         estado_civil = factor(estado_civil, levels = c(1,6), labels = c("Casado", "Soltero")),
         nivel_instruccion = factor(nivel_instruccion, levels = c(1:5), labels = c("Ninguno",
                                                                                   "Centro de alfabetizacion",
                                                                                   "Básica ",
                                                                                   "Media",
                                                                                   "Superior")),
         ciiu4 = as.factor(ciiu))

# Analisis horas trabajadas ------------------------------------------------------------------------------------------------

df_horas_g <- 
  df_bases %>%
  filter(horas_trabajadas <= 60) %>%
  group_by(grupos_edad, sexo) %>%
  summarise(horas_promedio = mean(horas_trabajadas, na.rm = TRUE),
            ing_med = median(ingreso_laboral, na.rm = TRUE),
            salario_hora = ing_med/horas_promedio)

# Base de las medianas para hombres y mujeres ------------------------------------------------------------------------------------------------

df_median_en <- 
  df_bases %>%
  filter(!nivel_instruccion %in% c("Ninguno",
                                   "Centro de alfabetizacion")) %>%
  mutate(fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d")) %>%
  group_by(fecha_1, sexo) %>%
  summarise(sueldo_mediano = median(ingreso_laboral, na.rm = TRUE))

# Analisis horas trabajadas ------------------------------------------------------------------------------------------------

df_horas <- 
  df_bases %>%
  filter(horas_trabajadas <= 60, !nivel_instruccion %in% c("Ninguno",
                                                           "Centro de alfabetizacion")) %>%
  mutate(fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d")) %>%
  group_by(fecha_1, sexo) %>%
  summarise(horas_promedio = mean(horas_trabajadas, na.rm = TRUE),
            ing_med = median(ingreso_laboral, na.rm = TRUE),
            salario_hora = ing_med/horas_promedio)

# Analisis educacion ------------------------------------------------------------------------------------------------

df_educacion <- 
  df_bases %>%
  filter(!nivel_instruccion %in% c('Ninguno', 'Centro de alfabetizacion')) %>%
  mutate(fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d")) %>%
  group_by(fecha_1,sexo, nivel_instruccion) %>%
  summarize(persona = n()) %>%
  mutate(porcentaje_persona = persona/sum(persona))

# base de la mediana por estado civil ------------------------------------------------------------------------------------------------

df_median_enen <- 
  df_bases %>%
  filter(!nivel_instruccion %in% c("Ninguno",
                                   "Centro de alfabetizacion")) %>%
  mutate(fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d")) %>%
  group_by(fecha_1, sexo, estado_civil) %>%
  summarise(sueldo_mediano = median(ingreso_laboral, na.rm = TRUE))

# base de la mediana por estado civil ------------------------------------------------------------------------------------------------

df_ciiu <- 
  df_bases %>%
  filter(!nivel_instruccion %in% c("Ninguno",
                                   "Centro de alfabetizacion")) %>%
  mutate(ciiu4 = fct_collapse(ciiu4,
                                    "Agropecuaria y pesca" = "1",
                                    "Industria minero-energética" = c("2","4"),
                                    "Industrias manufactureras" = "3",
                                    "Servicios publicos/defensa/saneamiento" = c("5","15"),
                                    "Sector inmobiliario y construcción" = c("6","12"),
                                    "Comercio; reparación de vehículos motorizados" = "7",
                                    "Transporte y almacenamiento" = "8",
                                    "Hospitalidad y de servicio de comidas" = "9",
                                    "Información y comunicación" = "10",
                                    "Actividades financieras y de seguros" = "11",
                                    "Servicios profesionales y técnicos" = "13",
                                    "Servicios administrativos y otros" =c("14","19"),
                                    "Enseñanza" = "16",
                                    "Salud y asistencia" = "17",
                                    "Artes, entretenimiento y recreación" = "18",
                                    "Organizaciones internacionales" = "21",
                                    "Otro" = "22"),
         fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d")) %>%
  group_by(fecha_1,ciiu4, sexo) %>% 
  summarize(empleo = n()) %>%
  mutate(porcentaje_empleo = empleo/sum(empleo))

df_ciiu4 <- 
  df_bases %>%
  filter(!nivel_instruccion %in% c("Ninguno",
                                   "Centro de alfabetizacion")) %>%
  mutate(ciiu4 = fct_collapse(ciiu4,
                              "Agropecuaria y pesca" = "1",
                              "Industria minero-energética" = c("2","4"),
                              "Industrias manufactureras" = "3",
                              "Servicios publicos/defensa/saneamiento" = c("5","15"),
                              "Sector inmobiliario y construcción" = c("6","12"),
                              "Comercio; reparación de vehículos motorizados" = "7",
                              "Transporte y almacenamiento" = "8",
                              "Hospitalidad y de servicio de comidas" = "9",
                              "Información y comunicación" = "10",
                              "Actividades financieras y de seguros" = "11",
                              "Servicios profesionales y técnicos" = "13",
                              "Servicios administrativos y otros" =c("14","19"),
                              "Enseñanza" = "16",
                              "Salud y asistencia" = "17",
                              "Artes, entretenimiento y recreación" = "18",
                              "Organizaciones internacionales" = "21",
                              "Otro" = "22"),
         fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d")) %>%
  group_by(fecha_1,ciiu4) %>% 
  summarise(sueldo_mediano = median(ingreso_laboral, na.rm = TRUE))



# theme -----

theme_ress <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"),
        text =  element_text(color = 'black', size = 12))

# captions -----


# visualizacion de la mediana ------------------------------------------------------------------------------------------------

graf_sueldo <- ggplot(df_median_en, aes(fecha_1, sueldo_mediano, color = sexo)) +
  geom_line() +
  geom_point(color = 'black') +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "Mediana del sueldo para hombres y mujeres Ecuador 2008-2018") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 12))

# Visualizacion salario por estado civil ------------------------------------------------------------------------------------------------

graf_est <- ggplot(df_median_enen, aes(fecha_1, sueldo_mediano, color = sexo)) +
  geom_line() +
  geom_point(color = 'black') +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  facet_grid(estado_civil ~ .) +
  labs(x = "Fecha", y = "Ingreso Mediano", color = "Sexo") +
  labs(x = "",
       y = "",
       title = "Brecha del salario mediano entre hombres y mujeres por estado civil 2008 -2018") +
  theme_ress

# visualizacion horas ------------------------------------------------------------------------------------------------

graf_horas <- ggplot(df_horas, aes(fecha_1, salario_hora, color = sexo)) +
  geom_line() +
  geom_point(color = 'black') +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "Mediana del sueldo para hombres y mujeres Ecuador dividido por el numero de horas promedio trabajadas 2008-2018") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 12))

# Visualizacion educacion ------------------------------------------------------------------------------------------------

graf_eduacion <- ggplot(df_educacion, aes(x = fecha_1, y = porcentaje_persona, color = nivel_instruccion)) +
  geom_line() +
  geom_point(color = 'black') +
  scale_color_manual(values = c("#FFAC8E","#647A8F", "#7bd9f2")) +
  facet_wrap(~sexo) +
  labs(x = "",
       y = "",
       title = "Evolucion porcentual del nivel de instrucción de hombres y mujeres en el mercado laboral 2008-2018",
       color = "Nivel de instruccion") +
  theme_ress

# Visualizacion salario por estado civil ------------------------------------------------------------------------------------------------

graf_ciiu <- ggplot(df_ciiu %>% filter(ciiu4 %in% c("Enseñanza",
                                                    "Industria minero-energética",
                                                          "Servicios publicos/defensa/saneamiento",
                                                          "Actividades financieras y de seguros",
                                                          "Salud y asistencia")), 
                    aes(fecha_1, porcentaje_empleo, fill = ciiu4)) +
  geom_bar(stat = "identity",
           position = "fill") +
  facet_wrap(~sexo) +
  geom_text(aes(label = scales::percent(porcentaje_empleo,accuracy = 0.1)), color = "black",
            size = 2.4,
            position = position_fill(),
            hjust = 0.5,
            vjust = 0.9) +
  scale_fill_manual(values = c("#FFAC8E", '#2E5994', '#09A4CC','#F44D54','#52307c')) +
  labs(x = "",
       y = "",
       title = "% de trabajos de hombres y mujeres segun la actividad productiva",
       fill = "Actividad productiva") +
  theme_ress









