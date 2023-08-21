# Repositorio REES 2

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

# Cargando datos ------------------------------------------------------------------------------------------

source("code/ress_descarga.R")

# Análisis ------------------------------------------------------------------------------------------------

df_raw <- ress_raw %>%
  select(ano, mes, provincia, edad, sueldo , dias, genero, ciiu4_1, tamano_empleo) %>% 
  filter(provincia %in% seq(1:24), sueldo>0) %>%
  mutate(Sexo = factor(genero, levels = c(1:2), labels = c("Hombres", "Mujeres")),
         ciiu4_1_fct = as.factor(ciiu4_1),
         prov_fct = factor(provincia, levels = c(1:24), 
                           labels = c("Azuay","Bolívar","Cañar","Carchi","Cotopaxi","Chimborazo",
                                      "El Oro","Esmeraldas","Guayas","Imbabura","Loja","Los Ríos",
                                      "Manabí","Morona Santiago","Napo","Pastaza","Pichincha",
                                      "Tungurahua","Z.Chinchipe","Galápagos","Sucumbíos","Orellana",
                                      "Santo Domingo de los Tsáchilas","Santa Elena")),
         tamano_empleo = as.factor(tamano_empleo))

# Base de las medianas para hombres y mujeres ------------------------------------------------------------------------------------------------

df_median <- 
  df_raw %>%
  mutate(fecha_1= paste("01", paste(mes,ano, sep = '-')) %>% dmy()) %>%
  group_by(fecha_1, Sexo) %>%
  summarise(sueldo_mediano = median(sueldo, na.rm = TRUE))

# Base de el promedio para hombres y mujeres ------------------------------------------------------------------------------------------------

df_mean <- 
  df_raw %>%
  mutate(fecha_1= paste("01", paste(mes,ano, sep = '-')) %>% dmy()) %>%
  group_by(fecha_1, Sexo) %>%
  summarise(sueldo_promedio = mean(sueldo, na.rm = TRUE))

# Base del % para hombres y mujeres en los tipos de empresas ------------------------------------------------------------------------------------------------

df_empresa <- 
  df_raw %>%
  filter(ano == 2023, mes == 3) %>% 
  mutate(empresas_fct = fct_collapse(tamano_empleo,
                                     "Microempresa" = "1",
                                     "Pequeña" = "2",
                                     "Mediana" = c("3","4"),
                                     "Grande" = "5")) %>%
  group_by(Sexo, empresas_fct) %>%
  summarize(empleo = n()) %>%
  mutate(porcentaje_empleo = empleo/sum(empleo)*100)

# Base del % para hombres y mujeres por ciiu ------------------------------------------------------------------------------------------------

df_ciiu <- 
  df_raw %>% 
  filter(ano == 2023, mes == 3) %>% 
  mutate(ciiu4_1_fct = fct_collapse(ciiu4_1_fct,
                                    "Agropecuaria y pesca" = "A",
                                    "Industria minero-energética" = c("B","D"),
                                    "Industrias manufactureras" = "C",
                                    "Servicios publicos/defensa/saneamiento" = c("E","O"),
                                    "Sector inmobiliario y construcción" = c("F","L"),
                                    "Comercio; reparación de vehículos motorizados" = "G",
                                    "Transporte y almacenamiento" = "H",
                                    "Hospitalidad y de servicio de comidas" = "I",
                                    "Información y comunicación" = "J",
                                    "Actividades financieras y de seguros" = "K",
                                    "Servicios profesionales y técnicos" = "M",
                                    "Servicios administrativos y otros" =c("N","S"),
                                    "Enseñanza" = "P",
                                    "Salud y asistencia" = "Q",
                                    "Artes, entretenimiento y recreación" = "R",
                                    "Organizaciones internacionales" = "U",
                                    "Otro" = "Z0_Nocla_CIIU")) %>%
  group_by(ciiu4_1_fct, Sexo) %>% 
  summarize(empleo = n()) %>%
  mutate(porcentaje_empleo = empleo/sum(empleo)*100)

# Base para la edad de hombres y mujeres con respecto del salario ------------------------------------------------------------------------------------------------

regresion_edad <- lm

