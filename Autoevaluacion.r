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


# AutoevaluacionHonorarios ------------------------------------------------

CE_H <- read_xlsx("Bases de Datos/Autoevaluación/CE_HONORARIOS_1erSemestre.xlsx")
CE2_H <- read_xlsx("Bases de Datos/Autoevaluación/CE_Honorarios_2doSemestre.xlsx")
CE_PC <- read_xlsx("Bases de Datos/Autoevaluación/CE_PlantaContrata_1erSemestre.xlsx")
CE2_PC <- read_xlsx("Bases de Datos/Autoevaluación/CE_PlantaContrata_2doSemestre.xlsx")
PTU_H <- read_xlsx("Bases de Datos/Autoevaluación/PTU_HONORARIOS_1erSemestre.xlsx")
PTU2_H <- read_xlsx("Bases de Datos/Autoevaluación/PTU_HONORARIOS_2doSemestre.xlsx")
PTU_PC <- read_xlsx("Bases de Datos/Autoevaluación/PTU_PLANTACONTRATA_1erSemestre.xlsx")
PTU2_PC <- read_xlsx("Bases de Datos/Autoevaluación/PTU_PLANTACONTRATA_2doSemestre.xlsx")
TNS_H <- read_xlsx("Bases de Datos/Autoevaluación/TNS_Honorarios_1erSemestre.xlsx")
TNS2_H <- read_xlsx("Bases de Datos/Autoevaluación/TNS_HONORARIOS_2doSemestre.xlsx")
TNS_PC <- read_xlsx("Bases de Datos/Autoevaluación/TNS_PLANTACONTRATA_1erSemestre.xlsx")
TNS2_PC <- read_xlsx("Bases de Datos/Autoevaluación/TNS_PLANTACONTRATA_2doSemestre.xlsx")


CE_H <- CE_H %>% 
  clean_bd() %>% 
  clean_names()

CE2_H <- CE2_H %>% 
  clean_bd() %>% 
  clean_names()

CE_PC <- CE_PC %>% 
  clean_bd() %>% 
  clean_names()

CE2_PC <- CE2_PC %>% 
  clean_bd() %>% 
  clean_names()

PTU_H <- PTU_H %>% 
  clean_bd() %>% 
  clean_names()

PTU2_H <- PTU2_H %>% 
  clean_bd() %>% 
  clean_names()

PTU_PC <- PTU_PC %>% 
  clean_bd() %>% 
  clean_names()

PTU2_PC <- PTU2_PC %>% 
  clean_bd() %>% 
  clean_names()

TNS_H <- TNS_H %>% 
  clean_bd() %>% 
  clean_names()

TNS2_H <- TNS2_H %>% 
  clean_bd() %>% 
  clean_names()

TNS_PC <- TNS_PC %>% 
  clean_bd() %>% 
  clean_names()

TNS2_PC <- TNS2_PC %>% 
  clean_bd() %>% 
  clean_names()

AutoevaluacionH <- bind_rows(CE_H,
          CE2_H,
          PTU_H,
          PTU2_H,
          TNS_H,
          TNS2_H)

AutoevaluacionPC <- bind_rows(CE_PC,
          CE2_PC,
          PTU_PC,
          PTU2_PC,
          TNS_PC,
          TNS2_PC)

rm(CE_H,
   CE2_H,
   PTU_H,
   PTU2_H,
   TNS_H,
   TNS2_H,
   CE_PC,
   CE2_PC,
   PTU_PC,
   PTU2_PC,
   TNS_PC,
   TNS2_PC)

CoberturasAutoevaluacionPC <- AutoevaluacionPC %>% 
  rename(
    rut_docente = rut_completo,
    nombre_docente = nombres,
    apaterno_docente = apaterno,
    amaterno_docente = amaterno,
    unidad_academica = facultad
  ) %>% 
  group_by(proceso,
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
           contrato) %>% 
  count(estado) %>% 
  pivot_wider(
    names_from = estado,
    values_from = n
  ) %>% 
  clean_names() %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
  mutate(total = sum(no_iniciada,
                     finalizado,
                     pendiente)) %>% 
  mutate(porcentaje_finalizado = round(finalizado/total,4)) %>%
  select(
    -no_iniciada,
    -finalizado,
    -pendiente,
    -total
  )

ResultadosAutoevaluacionPC <- AutoevaluacionPC %>% 
  rename(
    rut_docente = rut_completo,
    nombre_docente = nombres,
    apaterno_docente = apaterno,
    amaterno_docente = amaterno,
    unidad_academica = facultad
  ) %>%
  filter(
    estado == "Finalizado"
  ) %>% mutate_at(c(24:43),
                  funs(recode(.,
                              "Siempre"=3,
                              "Generalmente"=2,
                              "Pocas veces"=1,
                              "Nunca"=0))) %>% 
  rename(
    "Autoevaluacion_1M" = "respuesta_1_1",
    "Autoevaluacion_2M" = "respuesta_1_2",
    "Autoevaluacion_3M" = "respuesta_1_3",
    "Autoevaluacion_4M" = "respuesta_1_4",
    "Autoevaluacion_5M" = "respuesta_1_5",
    "Autoevaluacion_6M" = "respuesta_1_6",
    "Autoevaluacion_7M" = "respuesta_1_7",
    "Autoevaluacion_8M" = "respuesta_1_8",
    "Autoevaluacion_9M" = "respuesta_1_9",
    "Autoevaluacion_10M" = "respuesta_1_10",
    "Autoevaluacion_11M" = "respuesta_1_11",
    "Autoevaluacion_12M" = "respuesta_1_12",
    "Autoevaluacion_13M" = "respuesta_1_13",
    "Autoevaluacion_14M" = "respuesta_1_14",
    "Autoevaluacion_1I" = "respuesta_2_1",
    "Autoevaluacion_2I" = "respuesta_2_2",
    "Autoevaluacion_3I" = "respuesta_2_3",
    "Autoevaluacion_4I" = "respuesta_2_4",
    "Autoevaluacion_1G" = "respuesta_3_1",
    "Autoevaluacion_2G" = "respuesta_3_2"
  ) %>% 
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
    contrato
  ) %>% 
  summarize(
    Autoevaluacion_1M = round(mean(Autoevaluacion_1M),2),
    Autoevaluacion_2M = round(mean(Autoevaluacion_2M),2),
    Autoevaluacion_3M = round(mean(Autoevaluacion_3M),2),
    Autoevaluacion_4M = round(mean(Autoevaluacion_4M),2),
    Autoevaluacion_5M = round(mean(Autoevaluacion_5M),2),
    Autoevaluacion_6M = round(mean(Autoevaluacion_6M),2),
    Autoevaluacion_7M = round(mean(Autoevaluacion_7M),2),
    Autoevaluacion_8M = round(mean(Autoevaluacion_8M),2),
    Autoevaluacion_9M = round(mean(Autoevaluacion_9M),2),
    Autoevaluacion_10M = round(mean(Autoevaluacion_10M),2),
    Autoevaluacion_11M = round(mean(Autoevaluacion_11M),2),
    Autoevaluacion_12M = round(mean(Autoevaluacion_12M),2),
    Autoevaluacion_13M = round(mean(Autoevaluacion_13M),2),
    Autoevaluacion_14M = round(mean(Autoevaluacion_14M),2),
    Autoevaluacion_1I = round(mean(Autoevaluacion_1I),2),
    Autoevaluacion_2I = round(mean(Autoevaluacion_2I),2),
    Autoevaluacion_3I = round(mean(Autoevaluacion_3I),2),
    Autoevaluacion_4I = round(mean(Autoevaluacion_4I),2),
    Autoevaluacion_2G = round(mean(Autoevaluacion_1G),2),
    Autoevaluacion_2G = round(mean(Autoevaluacion_2G),2))


AutoevaluacionPC <- CoberturasAutoevaluacionPC %>%
  left_join(
    ResultadosAutoevaluacionPC,
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
      "amaterno_docente",
      "contrato"
    )
  )

AutoevaluacionPC <- AutoevaluacionPC %>%
  mutate_if(is.numeric,~replace(.,is.na(.),0))

rm(CoberturasAutoevaluacionPC,
   ResultadosAutoevaluacionPC)

##

CoberturaAutoevaluacionH <- AutoevaluacionH %>% 
  rename(
    rut_docente = rut_completo,
    nombre_docente = nombres,
    apaterno_docente = apaterno,
    amaterno_docente = amaterno,
    unidad_academica = facultad
  ) %>% 
  group_by(proceso,
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
           contrato) %>% 
  count(estado) %>% 
  pivot_wider(
    names_from = estado,
    values_from = n
  ) %>% 
  clean_names() %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
  mutate(total = sum(no_iniciada,
                     finalizado,
                     pendiente)) %>% 
  mutate(porcentaje_finalizado = round(finalizado/total,4)) %>%
  select(
    -no_iniciada,
    -finalizado,
    -pendiente,
    -total
  )


ResultadosAutoevaluacionh <- AutoevaluacionH %>% 
  rename(
    rut_docente = rut_completo,
    nombre_docente = nombres,
    apaterno_docente = apaterno,
    amaterno_docente = amaterno,
    unidad_academica = facultad
  ) %>%
  filter(
    estado == "Finalizado"
  ) %>% mutate_at(c(24:39),
                  funs(recode(.,
                              "Siempre"=3,
                              "Generalmente"=2,
                              "Pocas veces"=1,
                              "Nunca"=0))) %>% 
  rename(
    "Autoevaluacion_1M" = "respuesta_1_1",
    "Autoevaluacion_2M" = "respuesta_1_2",
    "Autoevaluacion_3M" = "respuesta_1_3",
    "Autoevaluacion_4M" = "respuesta_1_4",
    "Autoevaluacion_5M" = "respuesta_1_5",
    "Autoevaluacion_6M" = "respuesta_1_6",
    "Autoevaluacion_7M" = "respuesta_1_7",
    "Autoevaluacion_8M" = "respuesta_1_8",
    "Autoevaluacion_9M" = "respuesta_1_9",
    "Autoevaluacion_10M" = "respuesta_1_10",
    "Autoevaluacion_11M" = "respuesta_1_11",
    "Autoevaluacion_12M" = "respuesta_1_12",
    "Autoevaluacion_13M" = "respuesta_1_13",
    "Autoevaluacion_14M" = "respuesta_1_14",
    "Autoevaluacion_1G" = "respuesta_2_1",
    "Autoevaluacion_2G" = "respuesta_2_2"
  ) %>% 
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
    contrato
  ) %>% 
  summarize(
    Autoevaluacion_1M = round(mean(Autoevaluacion_1M),2),
    Autoevaluacion_2M = round(mean(Autoevaluacion_2M),2),
    Autoevaluacion_3M = round(mean(Autoevaluacion_3M),2),
    Autoevaluacion_4M = round(mean(Autoevaluacion_4M),2),
    Autoevaluacion_5M = round(mean(Autoevaluacion_5M),2),
    Autoevaluacion_6M = round(mean(Autoevaluacion_6M),2),
    Autoevaluacion_7M = round(mean(Autoevaluacion_7M),2),
    Autoevaluacion_8M = round(mean(Autoevaluacion_8M),2),
    Autoevaluacion_9M = round(mean(Autoevaluacion_9M),2),
    Autoevaluacion_10M = round(mean(Autoevaluacion_10M),2),
    Autoevaluacion_11M = round(mean(Autoevaluacion_11M),2),
    Autoevaluacion_12M = round(mean(Autoevaluacion_12M),2),
    Autoevaluacion_13M = round(mean(Autoevaluacion_13M),2),
    Autoevaluacion_14M = round(mean(Autoevaluacion_14M),2),
    Autoevaluacion_1G = round(mean(Autoevaluacion_1G),2),
    Autoevaluacion_2G = round(mean(Autoevaluacion_2G),2))


AutoevaluacionH <- CoberturaAutoevaluacionH %>%
  left_join(
    ResultadosAutoevaluacionh,
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
      "amaterno_docente",
      "contrato"
    )
  )

AutoevaluacionH <- AutoevaluacionH %>%
  mutate_if(is.numeric,~replace(.,is.na(.),0))

rm(CoberturaAutoevaluacionH,
   ResultadosAutoevaluacionh)

write_xlsx(AutoevaluacionH,
      "InsumosProcesados/Autoevaluacionh.xlsx")

write_xlsx(AutoevaluacionPC,
           "InsumosProcesados/AutoevaluacionPC.xlsx")
