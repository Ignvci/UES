rm(list=ls())

# Librerias ---------------------------------------------------------------

library(kableExtra)
library(tidyverse)
library(tidyr)
library(writexl)
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(janitor)
library(hrbrthemes)
library(tidytext)
library(wordcloud2)
library(wordcloud)
library(syuzhet)
library(patchwork)
library(pdftools)
library(tm)
library(textdata)
library(stringr)
library(ggthemes)

# Importacion de Resultados -----------------------------------------------

Estudiantes <- read_xlsx("InsumosProcesados/EvaluacionEstudiantes2022.xlsx")
AutoevaluacionH <- read_xlsx("InsumosProcesados/Autoevaluacionh.xlsx")
AutoevaluacionPC <- read_xlsx("InsumosProcesados/AutoevaluacionPC.xlsx")
JefeDirectorH <- read_xlsx("InsumosProcesados/JefeDirectorH.xlsx")
JefeDirectorPC <- read_xlsx("InsumosProcesados/JefeDirectorPC.xlsx")

Estudiantes <- Estudiantes %>% 
  mutate(
    instrumento = "Estudiantes"
  )

AutoevaluacionH <- AutoevaluacionH %>% 
  mutate(
    instrumento = "Autoevaluacion"
  )

AutoevaluacionPC <- AutoevaluacionPC %>% 
  mutate(
    instrumento = "Autoevaluacion"
  )

JefeDirectorH <- JefeDirectorH %>% 
  mutate(
    instrumento = "JefeDirector"
  )

JefeDirectorPC <- JefeDirectorPC %>% 
  mutate(
    instrumento = "JefeDirector"
  )

Estudiantes %>% names()

Estudiantes <- Estudiantes %>% 
  group_by(
      proceso,
      semestre,
      tipo_carrera,
      unidad_academica,
      sede,
      codigo_carrera,
      nombre_carrera,
      rut_docente,
      nombre_docente,
      apaterno_docente,
      amaterno_docente,
      instrumento,
  ) %>% 
  summarize(
    porcentaje_finalizado = round(mean(porcentaje_finalizado),2),
    estudiantes_1M = round(mean(estudiantes_1M),2),
    estudiantes_2M = round(mean(estudiantes_2M),2),
    estudiantes_3M = round(mean(estudiantes_3M),2),
    estudiantes_4M = round(mean(estudiantes_4M),2),
    estudiantes_5M = round(mean(estudiantes_5M),2),
    estudiantes_6M = round(mean(estudiantes_6M),2),
    estudiantes_7M = round(mean(estudiantes_7M),2),
    estudiantes_8M = round(mean(estudiantes_8M),2),
    estudiantes_9M = round(mean(estudiantes_9M),2),
    estudiantes_10M = round(mean(estudiantes_10M),2),
    estudiantes_11M = round(mean(estudiantes_11M),2),
    estudiantes_12M = round(mean(estudiantes_12M),2),
    estudiantes_13M = round(mean(estudiantes_13M),2),
    estudiantes_1G = round(mean(estudiantes_1G),2),
    estudiantes_2G = round(mean(estudiantes_2G),2)
  )

Integridad <- bind_rows(
  Estudiantes,
  AutoevaluacionH,
  AutoevaluacionPC,
  JefeDirectorH,
  JefeDirectorPC
)


# Integridad --------------------------------------------------------------

Integridad <- Integridad %>% 
  group_by(
    proceso,
    semestre,
    tipo_carrera,
    unidad_academica,
    sede,
    codigo_carrera,
    nombre_carrera,
    rut_docente,
    nombre_docente,
    apaterno_docente,
    amaterno_docente
  ) %>% 
  count(instrumento) %>% 
  pivot_wider(
    names_from = instrumento,
    values_from = n
  ) %>%
  ungroup() %>% 
  clean_names() %>% 
  mutate(
    integridad = case_when(
      is.na(autoevaluacion) & is.na(jefe_director) & is.na(estudiantes)~"NA",
      is.na(autoevaluacion) & !is.na(jefe_director) & is.na(estudiantes)~"SD",
      is.na(autoevaluacion) & !is.na(jefe_director) & !is.na(estudiantes)~"ED",
      !is.na(autoevaluacion) & !is.na(jefe_director) & is.na(estudiantes)~"AD",
      !is.na(autoevaluacion) & !is.na(estudiantes) & is.na(jefe_director)~"AE ",
      !is.na(autoevaluacion) & is.na(jefe_director) & is.na(estudiantes)~"SA",
      is.na(autoevaluacion) & is.na(jefe_director) & !is.na(estudiantes)~"SE",
      !is.na(autoevaluacion) & !is.na(jefe_director) & !is.na(estudiantes)~"TI"
    )) %>% 
  select(c(1:11),
         c(15))

autoevaluacion <- bind_rows(
  AutoevaluacionH,
  AutoevaluacionPC
)

JefeDirector <- bind_rows(
  JefeDirectorH,
  JefeDirectorPC
)




EvaluacionDocente <- Integridad %>% 
  left_join(
    Estudiantes,
    by = c(
      "proceso",
      "semestre",
      "tipo_carrera",
      "unidad_academica",
      "sede",
      "codigo_carrera",
      "nombre_carrera",
      "rut_docente",
      "nombre_docente",
      "apaterno_docente",
      "amaterno_docente"
    )) %>% 
  left_join(
    autoevaluacion,
    by = c("proceso",
                         "semestre",
                         "tipo_carrera",
                         "unidad_academica",
                         "sede",
                         "codigo_carrera",
                         "nombre_carrera",
                         "rut_docente",
                         "nombre_docente",
                         "apaterno_docente",
                         "amaterno_docente")
  ) %>% 
  left_join(
    JefeDirector,
    by = c ("proceso",
      "semestre",
      "tipo_carrera",
      "unidad_academica",
      "sede",
      "codigo_carrera",
      "nombre_carrera",
      "rut_docente",
      "nombre_docente",
      "apaterno_docente",
      "amaterno_docente"
    )
  ) %>% 
  rename(
    finalizado_estudiantes = porcentaje_finalizado.x,
    finalizado_autoevaluacion = porcentaje_finalizado.y,
    finalizado_jefedirector = porcentaje_finalizado
  ) %>% 
  mutate(
    contrato = case_when(
      contrato.x == "Honorario" & contrato.y =="Honorario" ~ "Honorario",
      contrato.x == "Planta" & contrato.y == "Planta"~"Planta",
      contrato.x == "Contrata" & contrato.y == "Contrata" ~ "Contrata",
      is.na(contrato.x) & contrato.y == "Honorario" ~ "Honorario",
      is.na(contrato.x) & contrato.y == "Contrata" ~ "Contrata",
      is.na(contrato.x) & contrato.y == "Planta" ~ "Planta",
      contrato.x == "Honorario" & is.na(contrato.y) ~ "Honorario",
      contrato.x == "Planta" & is.na(contrato.y)~ "Planta",
      contrato.x == "Contrata" & is.na(contrato.y)~"Contrata",
      contrato.y == "Contrata" & contrato.x == "Planta"~"Contrata",
      contrato.y == "Contrata" & contrato.x == "Honorario"~"Contrata",
      contrato.y == "Honorario" & contrato.x == "Planta"~"Honorario",
      contrato.y == "Honorario" & contrato.x == "Contrata"~"Honorario",
      contrato.y == "Planta" & contrato.x == "Contrata"~"Planta",
      contrato.y == "Planta" & contrato.x == "Honorario"~"Planta",
      T~"Honorario"
    )) %>%
  select(
    c(1:12),
    c(68),
    c(14),
    c(15:29),
    c(31:47),
    c(49:52),
    c(54:63),
    c(65:67)
  ) %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
  clean_names() %>% 
  group_by(
    proceso,
    semestre,
    tipo_carrera,
    unidad_academica,
    sede,
    codigo_carrera,
    nombre_carrera,
    rut_docente,
    nombre_docente,
    apaterno_docente,
    amaterno_docente,
    integridad,
    contrato
  ) %>% 
  mutate(
    competencia_mediador_estudiantes = round(sum(
      estudiantes_1m,
      estudiantes_2m,
      estudiantes_3m,
      estudiantes_4m,
      estudiantes_5m,
      estudiantes_6m,
      estudiantes_7m,
      estudiantes_8m,
      estudiantes_9m,
      estudiantes_10m,
      estudiantes_11m,
      estudiantes_12m,
      estudiantes_13m
    )/13,2),
    competencia_mediador_autoevaluacion = round(sum(
      autoevaluacion_1m,
      autoevaluacion_2m,
      autoevaluacion_3m,
      autoevaluacion_4m,
      autoevaluacion_5m,
      autoevaluacion_6m,
      autoevaluacion_7m,
      autoevaluacion_8m,
      autoevaluacion_9m,
      autoevaluacion_10m,
      autoevaluacion_11m,
      autoevaluacion_12m,
      autoevaluacion_13m,
      autoevaluacion_14m
    )/14,2),
    competencia_mediador_jefedirector = round(sum(
      jefe_director_1m,
      jefe_director_2m,
      jefe_director_3m,
      jefe_director_4m,
      jefe_director_5m,
      jefe_director_6m
    )/6,2),
    competencia_gestion_estudiantes = round(sum(
      estudiantes_1g,
      estudiantes_2g
    )/2,2),
    competencia_gestion_autoevaluacion = round(sum(
      autoevaluacion_1g,
      autoevaluacion_2g
    )/2,2),
    competencia_gestion_jefedirector = round(sum(
      jefe_director_1g,
      jefe_director_2g
    )/2,2),
    competencia_indagador_autoevaluacion = round(sum(
      autoevaluacion_1i,
      autoevaluacion_2i,
      autoevaluacion_3i,
      autoevaluacion_4i
    )/4,2),
    competencia_indagador_jefedirector = case_when(
      contrato != "Honorario" ~ round(sum(
        jefe_director_1i,
        jefe_director_2i,
        jefe_director_3i,
        jefe_director_4i
      )/4,2),
      contrato == "Honorario" ~ round(sum(jefe_director_2i))
    ),
    competencia_mediador = sum(
      competencia_mediador_autoevaluacion,
      competencia_mediador_estudiantes,
      competencia_mediador_jefedirector
    ),
    competencia_indagador = sum(competencia_indagador_autoevaluacion,
                                    competencia_indagador_jefedirector),
    competencia_gestion = sum(
      competencia_gestion_autoevaluacion,
      competencia_gestion_jefedirector,
      competencia_gestion_estudiantes
    )
  ) %>% 
  mutate(
    clasificacion_mediador = case_when(
      competencia_mediador == 9 | competencia_mediador > 8 ~ "Sobresaliente",
      competencia_mediador == 8 | competencia_mediador > 6 ~ "Satisfactorio",
      competencia_mediador == 6 | competencia_mediador > 4 ~ "Suficiente",
      competencia_mediador == 4 | competencia_mediador > 2 ~ "Deficiente",
      competencia_mediador == 2 | competencia_mediador >= 0 ~ "Insuficiente"
    ),
    clasificacion_gestion = case_when(
      competencia_gestion == 9 | competencia_gestion > 8 ~ "Sobresaliente",
      competencia_gestion == 8 | competencia_gestion > 6 ~ "Satisfactorio",
      competencia_gestion == 6 | competencia_gestion > 4 ~ "Suficiente",
      competencia_gestion == 4 | competencia_gestion > 2 ~ "Deficiente",
      competencia_gestion == 2 | competencia_gestion >= 0 ~ "Insuficiente"
    ),
    clasificacion_indagador = case_when(
      contrato == "Honorario" & competencia_indagador >= 3  ~ "Sobresaliente",
      contrato == "Honorario" & competencia_indagador >= 2  ~ "Satisfactorio",
      contrato == "Honorario" & competencia_indagador >= 1  ~ "Suficiente",
      contrato == "Honorario" & competencia_indagador >= 0 ~ "Deficiente",
      contrato != "Honorario" & competencia_indagador >= 4.8 ~ "Satisfactorio",
      contrato != "Honorario" & competencia_indagador >= 3.6 ~ "Satisfactorio",
      contrato != "Honorario" & competencia_indagador >= 2.4 ~ "Suficiente",
      contrato != "Honorario" & competencia_indagador >= 1.2  ~ "Deficiente",
      contrato != "Honorario" & competencia_indagador >= 0 ~ "Insuficiente"
    ),
    clasificacion_cumplimiento = case_when(
      integridad == "TI" & finalizado_autoevaluacion >0 & finalizado_jefedirector >0 & finalizado_estudiantes > 0 ~ "100%",
      integridad == "TI" & finalizado_autoevaluacion >0 & finalizado_jefedirector >0 & finalizado_estudiantes == 0 ~ "66,66%",
      integridad == "TI" & finalizado_autoevaluacion >0 & finalizado_jefedirector >0 & finalizado_estudiantes == 0 ~ "33,33%",
      integridad == "TI" & finalizado_autoevaluacion ==0 & finalizado_jefedirector ==0 & finalizado_estudiantes == 0 ~ "0%",
      integridad == "TI" & finalizado_autoevaluacion ==0 & finalizado_jefedirector >0 & finalizado_estudiantes > 0 ~ "66,66%",
      integridad == "TI" & finalizado_autoevaluacion ==0 & finalizado_jefedirector >0 & finalizado_estudiantes == 0 ~ "33,33%",
      integridad == "TI" & finalizado_autoevaluacion ==0 & finalizado_jefedirector ==0 & finalizado_estudiantes > 0 ~ "33,33%"
    )
  )
EvaluacionDocente <- EvaluacionDocente %>% 
  filter(
    rut_docente != "1390648-3"
  )

EvaluacionDocente <- EvaluacionDocente %>% 
  ungroup() %>% 
  mutate(
    tipo_carrera = str_replace(tipo_carrera,
                               "Pregrado Trabajador","Continuidad de Estudios"
    )
  ) %>% 
  mutate(
    compromiso = 
      case_when(
        integridad == "TI" & finalizado_estudiantes>0 & finalizado_autoevaluacion>0 & finalizado_jefedirector>0 ~ "TC",
        integridad == "TI" & finalizado_estudiantes>0 & finalizado_autoevaluacion>0 & finalizado_jefedirector==0 ~ "FDJ",
        integridad == "TI" & finalizado_estudiantes>0 & finalizado_autoevaluacion==0 & finalizado_jefedirector==0 ~ "FADJ",
        integridad == "TI" & finalizado_estudiantes>0 & finalizado_autoevaluacion==0 & finalizado_jefedirector>0 ~ "FA",
        integridad == "TI" & finalizado_estudiantes==0 & finalizado_autoevaluacion>0 & finalizado_jefedirector>0 ~ "FE",
        integridad == "TI" & finalizado_estudiantes==0 & finalizado_autoevaluacion==0 & finalizado_jefedirector>0 ~ "FAE",
        integridad == "TI" & finalizado_estudiantes==0 & finalizado_autoevaluacion>0 & finalizado_jefedirector==0 ~ "FEDJ",
        integridad == "TI" & finalizado_estudiantes==0 & finalizado_autoevaluacion==0 & finalizado_jefedirector==0 ~ "FT",
        T ~ "ND"
      )
  )

write_xlsx(EvaluacionDocente,"InsumosProcesados/EvaluacionDocente22.xlsx")
