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
if(!require(tidyr)) install.packages("scales", repos = "http://cran.us.r-project.org")


# Cargando datos ------------------------------------------------------------------------------------------

source("code/ress_descarga.R")

# Análisis ------------------------------------------------------------------------------------------------

df_raw <- ress_raw %>%
  select(ano, mes, provincia, edad, sueldo , est_civil_rc, genero, ciiu4_1, tamano_empleo) %>% 
  filter(provincia %in% seq(1:24), sueldo>0, est_civil_rc %in% c(1,2)) %>%
  mutate(Sexo = factor(genero, levels = c(1:2), labels = c("Hombres", "Mujeres")),
         ciiu4_1_fct = as.factor(ciiu4_1),
         prov_fct = factor(provincia, levels = c(1:24), 
                           labels = c("Azuay","Bolívar","Cañar","Carchi","Cotopaxi","Chimborazo",
                                      "El Oro","Esmeraldas","Guayas","Imbabura","Loja","Los Ríos",
                                      "Manabí","Morona Santiago","Napo","Pastaza","Pichincha",
                                      "Tungurahua","Z.Chinchipe","Galápagos","Sucumbíos","Orellana",
                                      "Santo Domingo de los Tsáchilas","Santa Elena")),
         tamano_empleo = as.factor(tamano_empleo),
         est_civil_rc = as.factor(est_civil_rc),
         logsal=ifelse(sueldo>=1,log(sueldo),NA),
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

# Base de las medianas para hombres y mujeres ------------------------------------------------------------------------------------------------

df_median <- 
  df_raw %>%
  mutate(fecha_1= paste("01", paste(mes,ano, sep = '-')) %>% dmy()) %>%
  group_by(fecha_1, Sexo) %>%
  summarise(sueldo_mediano = median(sueldo, na.rm = TRUE))

# Base del % para hombres y mujeres en los tipos de empresas ------------------------------------------------------------------------------------------------

df_empresa <- 
  df_raw %>%
  filter(ano == 2023, mes == 3) %>% 
  mutate(empresas_fct = fct_collapse(tamano_empleo,
                                     "Microempresa" = "1",
                                     "Pequeña" = "2",
                                     "Mediana" = c("3","4"),
                                     "Grande" = "5")) %>%
  group_by(empresas_fct, Sexo) %>%
  summarize(empleo = n()) %>%
  mutate(porcentaje_empleo = empleo/sum(empleo))

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
  mutate(porcentaje_empleo = empleo/sum(empleo))

# Base para la edad de hombres y mujeres con respecto del salario ------------------------------------------------------------------------------------------------

df_edad <- 
  df_raw %>%
  filter(edad <= 75) %>%
  mutate(grupos_edad = as.factor(grupos_edad)) %>%
  group_by(grupos_edad, Sexo) %>%
  summarize(sueldo_promedio = mean(sueldo, na.rm = TRUE),
            sueldo_mediano = median(sueldo, na.rm = TRUE))

# Regresion ------------------------------------------------------------------------------------------------

regresion_cv <- 

# theme -----

theme_ress <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"),
        text =  element_text(color = 'black', size = 12))

# visualizacion de la mediana ------------------------------------------------------------------------------------------------

graf_sueldo <- ggplot(df_median, aes(fecha_1, sueldo_mediano, color = Sexo)) +
  geom_line() +
  geom_point(color = 'black') +
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%b-%y') +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "Mediana del sueldo para hombres y mujeres Ecuador 2022-2023") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 12))

# visualizacion del % para hombres y mujeres en los tipos de empresas ------------------------------------------------------------------------------------------------

df_perc <- ggplot(df_empresa, 
           aes(reorder(empresas_fct,porcentaje_empleo),porcentaje_empleo, fill = Sexo)) +
  geom_col(width = 0.8,
           position = "dodge",
           color = "black") +
  geom_text(aes(label = scales::percent(porcentaje_empleo,accuracy = 0.1)), color = "black", 
            size = 2.4,
            position = position_dodge(0.9),
            hjust = 0.4,
            vjust = -0.3) +
  scale_fill_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "% de trabajos de hombres y mujeres segun el tipo de empresa") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 12))

# visualizacion del % para hombres y mujeres en los tipos de ciiu ------------------------------------------------------------------------------------------------

graf_ciiu <- ggplot(df_ciiu %>% filter(ciiu4_1_fct %in% c("Organizaciones internacionales",
                                                          "Industria minero-energética",
                                                          "Servicios publicos/defensa/saneamiento",
                                                          "Actividades financieras y de seguros",
                                                          "Salud y asistencia")), 
                  aes(ciiu4_1_fct,porcentaje_empleo, fill = Sexo)) +
  geom_col(width = 0.8,
           position = "stack",
           color = "black") +
  coord_flip() +
  geom_text(aes(label = scales::percent(porcentaje_empleo,accuracy = 0.1)), color = "black", 
            size = 2.4,
            position = position_stack(0.5),
            hjust = 0.1,
            vjust = -0.1) +
  scale_fill_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "% de trabajos de hombres y mujeres segun la actividad productiva") +
  theme_ress

# visualizacion de la edad ------------------------------------------------------------------------------------------------

graf_edad <- ggplot(df_edad %>% filter(grupos_edad != "menos de 23"),
                    aes(grupos_edad, sueldo_promedio, color = Sexo)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "Evolucion del sueldo promedio para hombres y mujeres Ecuador por grupos de edad") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 12))

# visualizacion de la edad ------------------------------------------------------------------------------------------------

graf_edad_med <- ggplot(df_edad %>% filter(grupos_edad != "menos de 23"),
                    aes(grupos_edad, sueldo_mediano, color = Sexo)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "Evolucion de la mediana del salario para hombres y mujeres Ecuador por grupos de edad") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 12))
