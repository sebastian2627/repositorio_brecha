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
  return(data[, c("p02", "p01", "p03", "p06", "p24", "p45", "ingrl", "nnivins", "periodo")])
}

# Crear una lista de las bases de datos

bases <- list(ENEMDU_2007, ENEMDU_2008, ENEMDU_2009, ENEMDU_2010, ENEMDU_2011,
                       ENEMDU_2012, ENEMDU_2013, ENEMDU_2014, ENEMDU_2015, ENEMDU_2016,
                       ENEMDU_2017, ENEMDU_2018)

# Aplicar la función a cada base de datos usando lapply

bases_selec <- lapply(bases, vars)

# Unir los dataframes en la lista con bind_rows

ENEMDU_TOT <- bind_rows(bases_selec)

df_bases <- ENEMDU_TOT %>%
  select(   'sexo' = 'p02',
            'persona' = 'p01',
            'edad' = 'p03',
            'estado_civil' = 'p06',
            'horas_trabajadas' = 'p24',
            'experiencia_laboral' = 'p45',
            'ingreso_laboral'='ingrl',
            'nivel_instruccion'='nnivins') %>%
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
         logsal=ifelse(ingreso_laboral>=1,log(ingreso_laboral),NA),
         grupos_edad = case_when(
           edad <= 22 ~ 'menos de 23',
           edad %>% between(23,26) ~ '23-26',
           edad %>% between(27,30) ~ '27-30',
           edad %>% between(31,34) ~ '31-34',
           edad %>% between(35,39) ~ '35-39',
           edad %>% between(40,43) ~ '40-43',
           edad %>% between(44,47) ~ '44-47',
           edad %>% between(48,51) ~ '48-51',
           edad %>% between(52,55) ~ '52-55',
           edad %>% between(56,59) ~ '56-59',
           edad %>% between(60,64) ~ '60-64',
           edad >= 65 ~ 'mas de 64'))



