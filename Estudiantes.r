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

# Funcion -----------------------------------------------------------------

clean_bd <- function(x){
  x <- x[-1:-5,]
  names(x) <- x[1,]
  x <- x[-1,]
  return(x)
}

# Estudiantes PrimerSemestre2022-------------------------------------------------------------

CE <- read_xlsx("Bases de Datos/Estudiantes/CE_1erSemestre.xlsx")
CE2 <- read_xlsx("Bases de Datos/Estudiantes/CE_2dorSemestre.xlsx")
PTU <- read_xlsx("Bases de Datos/Estudiantes/PTU_1erSemestre.xlsx")
PTU2 <- read_xlsx("Bases de Datos/Estudiantes/PTU_2doSemestre.xlsx")
TNS <- read_xlsx("Bases de Datos/Estudiantes/TNS_1erSemestre.xlsx")
TNS2 <- read_xlsx("Bases de Datos/Estudiantes/TNS_2doSemestre.xlsx")

## Limpieza de datos

CE <- CE %>% 
  clean_bd() %>% 
  clean_names()

CE2 <- CE2 %>% 
  clean_bd() %>% 
  clean_names()

PTU <- PTU %>%
  clean_bd() %>% 
  clean_names()

PTU2 <- PTU2 %>% 
  clean_bd() %>% 
  clean_names()

TNS <- TNS %>% 
  clean_bd() %>% 
  clean_names()

TNS2 <- TNS2 %>% 
  clean_bd() %>% 
  clean_names()


## Union de Bases de datos

Estudiantes <- bind_rows(CE,
          CE2,
          TNS,
          TNS2,
          PTU,
          PTU2)

## Eliminacion de Tablas Individuales
rm(CE,
   CE2,
   TNS,
   TNS2,
   PTU,
   PTU2)


# Calculo de Cobertura ----------------------------------------------------

CoberturasEstudiantes <- Estudiantes %>% 
  group_by(proceso,
           semestre,
           tipo_carrera,
           unidad_academica,
           sede,
           codigo_carrera,
           nombre_carrera,
           codigo_actividad,
           nombre_actividad,
           tipo_asig,
           paralelo,
           rut_docente,
           nombre_docente,
           apaterno_docente,
           amaterno_docente) %>% 
  count(estado) %>% 
  pivot_wider(names_from = estado,
              values_from = n) %>%
  clean_names() %>% 
  replace_na(list(no_iniciada = 0,
                  pendiente = 0,
                  finalizado = 0)) %>% 
  mutate(total = sum(no_iniciada,
                     pendiente,
                     finalizado)) %>% 
  mutate(porcentaje_finalizado = round(finalizado/total,4)) %>%
  select(
    -no_iniciada,
    -finalizado,
    -pendiente,
    -total
  )

ResultadosEstudiantes <- Estudiantes %>% 
  filter(estado=="Finalizado") %>% 
  mutate_at(c(22:36),
            funs(recode(.,
                        "Siempre"=3,
                        "Generalmente"=2,
                        "Pocas veces"=1,
                        "Nunca"=0))) %>% 
  rename(
    "Estudiantes_1" = "respuesta_1_1",
    "Estudiantes_2" = "respuesta_1_2",
    "Estudiantes_3" = "respuesta_1_3",
    "Estudiantes_4" = "respuesta_1_4",
    "Estudiantes_5" = "respuesta_1_5",
    "Estudiantes_6" = "respuesta_1_6",
    "Estudiantes_7" = "respuesta_1_7",
    "Estudiantes_8" = "respuesta_1_8",
    "Estudiantes_9" = "respuesta_1_9",
    "Estudiantes_10" = "respuesta_1_10",
    "Estudiantes_11" = "respuesta_1_11",
    "Estudiantes_12" = "respuesta_1_12",
    "Estudiantes_13" = "respuesta_1_13",
    "Estudiantes_14" = "respuesta_2_1",
    "Estudiantes_15" = "respuesta_2_2"
  ) %>% 
  group_by(proceso,
           semestre,
           tipo_carrera,
           unidad_academica,
           sede,
           codigo_carrera,
           nombre_carrera,
           codigo_actividad,
           nombre_actividad,
           tipo_asig,
           paralelo,
           rut_docente,
           nombre_docente,
           apaterno_docente,
           amaterno_docente) %>% 
  summarize(
    estudiantes_1M = round(mean(Estudiantes_1),2),
    estudiantes_2M = round(mean(Estudiantes_2),2),
    estudiantes_3M = round(mean(Estudiantes_3),2),
    estudiantes_4M = round(mean(Estudiantes_4),2),
    estudiantes_5M = round(mean(Estudiantes_5),2),
    estudiantes_6M = round(mean(Estudiantes_6),2),
    estudiantes_7M = round(mean(Estudiantes_7),2),
    estudiantes_8M = round(mean(Estudiantes_8),2),
    estudiantes_9M = round(mean(Estudiantes_9),2),
    estudiantes_10M = round(mean(Estudiantes_10),2),
    estudiantes_11M = round(mean(Estudiantes_11),2),
    estudiantes_12M = round(mean(Estudiantes_12),2),
    estudiantes_13M = round(mean(Estudiantes_13),2),
    estudiantes_1G = round(mean(Estudiantes_14),2),
    estudiantes_2G = round(mean(Estudiantes_15),2)
  )

Estudiantes <- CoberturasEstudiantes %>% 
  left_join(ResultadosEstudiantes,
            by = c("proceso",
                   "semestre",
                   "tipo_carrera",
                   "unidad_academica",
                   "sede",
                   "codigo_carrera",
                   "nombre_carrera",
                   "codigo_actividad",
                   "nombre_actividad",
                   "tipo_asig",
                   "paralelo",
                   "rut_docente",
                   "nombre_docente",
                   "apaterno_docente",
                   "amaterno_docente"))

Estudiantes <- Estudiantes %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0))

rm(CoberturasEstudiantes,
   ResultadosEstudiantes)

write_xlsx(Estudiantes,"InsumosProcesados/EvaluacionEstudiantes2022.xlsx")

