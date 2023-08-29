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

enemdu_persona <- read_delim("data_enemdu/enemdu_persona_2023_II_trimestre.csv", 
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Base para la regresion y horas trabajadas ------------------------------------------------------------------------------------------------

df_enemdu <- enemdu_persona %>%
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
  
# Analisis horas trabajadas ------------------------------------------------------------------------------------------------

df_horas <- 
  df_enemdu %>%
  filter(horas_trabajadas <= 60) %>%
  group_by(grupos_edad, sexo) %>%
  summarise(horas_promedio = mean(horas_trabajadas, na.rm = TRUE),
            ing_med = median(ingreso_laboral, na.rm = TRUE),
            salario_hora = ing_med/horas_promedio)

# Analisis regresion ------------------------------------------------------------------------------------------------

regresion_sal <- lm(formula = logsal ~ sexo*estado_civil + experiencia_laboral + nivel_instruccion,
                    data = df_enemdu)

# Analisis educacion ------------------------------------------------------------------------------------------------

df_educ <- 
  df_enemdu %>%
  filter(!nivel_instruccion %in% c('Ninguno', 'Centro de alfabetizacion')) %>%
  group_by(sexo, nivel_instruccion) %>%
  summarize(persona = n()) %>%
  mutate(porcentaje_persona = persona/sum(persona))


# Analisis edad ------------------------------------------------------------------------------------------------

df_edades <- 
  df_enemdu %>%
  mutate(grupos_edad = as.factor(grupos_edad)) %>%
  group_by(grupos_edad, sexo) %>%
  summarize(horas_promedio = mean(horas_trabajadas, na.rm = TRUE))

# Visualizacion educacion ------------------------------------------------------------------------------------------------

graf_educacion <- ggplot(df_educ, aes(nivel_instruccion, porcentaje_persona, fill = sexo)) +
  geom_col(width = 0.8,
           color = "black") +
  geom_text(aes(label = scales::percent(porcentaje_persona,accuracy = 0.1)), color = "black", 
            size = 2.4,
            position = position_dodge(0.5),
            hjust = 0.4,
            vjust = -0.3) +
  scale_fill_manual(values = c("#647A8F","#FFAC8E")) +
  facet_grid(cols = vars(sexo)) +
  labs(x = "",
       y = "",
       title = "% del nivel de instrucción entre hombres y mujeres en el mercado laboral") +
  theme_ress +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Visualizacion horas trabajadas y sueldo por hora ------------------------------------------------------------------------------------------------

df_brecha <- ggplot(df_horas %>% filter(grupos_edad != "menos de 23"),
                    aes(grupos_edad, ing_med, color = sexo)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#FFAC8E","#647A8F")) +
  labs(x = "",
       y = "",
       title = "Mediana del salario para hombres y mujeres Ecuador por grupos de edad") +
  theme_ress +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 12))
  
df_hora <- 
  df_enemdu %>%
  filter(horas_trabajadas <= 60) %>%
  group_by(sexo) %>%
  summarise(horas_promedio = mean(horas_trabajadas, na.rm = TRUE),
            ing_prom = mean(ingreso_laboral, na.rm = TRUE),
            ing_med = median(ingreso_laboral, na.rm = TRUE),
            salario_hora = ing_med/horas_promedio)
