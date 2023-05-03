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





# Recopilacion ------------------------------------------------------------

CE_H <- read_xlsx("Bases de Datos/JefeDirector/CE_HONORARIOS_1ERSEMESTRE.xlsx")
CE2_H <- read_xlsx("Bases de Datos/JefeDirector/CE_HONORARIOS_2doSemestre.xlsx")
CE_PC <- read_xlsx("Bases de Datos/JefeDirector/CE_PLANTACONTRATA_1ERSEMESTRE.xlsx")
CE2_PC <- read_xlsx("Bases de Datos/JefeDirector/CE_PLANTACONTRATA_2doSemestre.xlsx")
PTU_H <- read_xlsx("Bases de Datos/JefeDirector/PTU_HONORARIOS_1ERSEMESTRE.xlsx")
PTU2_H <- read_xlsx("Bases de Datos/JefeDirector/PTU_Honorarios_2doSemestre.xlsx")
PTU_PC <- read_xlsx("Bases de Datos/JefeDirector/PTU_PlantaContrata_1erSemestre.xlsx")
PTU2_PC <- read_xlsx("Bases de Datos/JefeDirector/PTU_PlantaContrata_2doSemestre.xlsx")
TNS_H <- read_xlsx("Bases de Datos/JefeDirector/TNS_HONORARIOS_1ERSEMESTRE.xlsx")
TNS2_H <- read_xlsx("Bases de Datos/JefeDirector/TNS_HONORARIOS_2doSemestre.xlsx")
TNS_PC <- read_xlsx("Bases de Datos/JefeDirector/TNS_PLANTACONTRATA_1ERSEMESTRE.xlsx")
TNS2_PC <- read_xlsx("Bases de Datos/JefeDirector/TNS_PLANTACONTRATA_2doSemestre.xlsx")

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



JefeDirectorH <- bind_rows(CE_H,
                             CE2_H,
                             PTU_H,
                             PTU2_H,
                             TNS_H,
                             TNS2_H)


JefeDirectorPC <- bind_rows(CE_PC,
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

JefeDirectorPC
# JefeDirectorH -----------------------------------------------------------



CoberturasJefeDirectorH <- JefeDirectorH %>% 
  rename(nombre_docente = nombres_docente) %>% 
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

JefeDirectorH %>% 
  view()

ResultadosJefeDirectorH <- JefeDirectorH %>% 
  rename(nombre_docente = nombres_docente) %>% 
  filter(estado == "Finalizado") %>% 
  mutate_at(c(26:34),
                funs(recode(.,
                            "Siempre"=3,
                            "Generalmente"=2,
                            "Pocas veces"=1,
                            "Nunca"=0))) %>% 
  rename(
    JefeDirector_1M = respuesta_1_1,
    JefeDirector_2M = respuesta_1_2,
    JefeDirector_3M = respuesta_1_3,
    JefeDirector_4M = respuesta_1_4,
    JefeDirector_5M = respuesta_1_5,
    JefeDirector_6M = respuesta_1_6,
    JefeDirector_2I = respuesta_2_1,
    JefeDirector_1G = respuesta_3_1,
    JefeDirector_2G = respuesta_3_2
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
    JefeDirector_1M = round(mean(JefeDirector_1M),2),
    JefeDirector_2M = round(mean(JefeDirector_2M),2),
    JefeDirector_3M = round(mean(JefeDirector_3M),2),
    JefeDirector_4M = round(mean(JefeDirector_4M),2),
    JefeDirector_5M = round(mean(JefeDirector_5M),2),
    JefeDirector_6M = round(mean(JefeDirector_6M),2),
    JefeDirector_2I = round(mean(JefeDirector_2I),2),
    JefeDirector_1G = round(mean(JefeDirector_1G),2),
    JefeDirector_2G = round(mean(JefeDirector_2G),2)
  )


JefeDirectorH <- CoberturasJefeDirectorH %>% 
  left_join(ResultadosJefeDirectorH,
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
            ))

JefeDirectorH <- JefeDirectorH %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0))

rm(CoberturasJefeDirectorH,
   ResultadosJefeDirectorH)



# JefeDirector Planta y Contrata ------------------------------------------

CoberturasJefeDirectorPC <- JefeDirectorPC %>% 
  rename(nombre_docente = nombres_docente) %>% 
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



ResultadosJefeDirectorPC <- JefeDirectorPC %>% 
  rename(nombre_docente = nombres_docente) %>% 
  filter(estado == "Finalizado") %>% 
  mutate_at(c(26:37),
            funs(recode(.,
                        "Siempre"=3,
                        "Generalmente"=2,
                        "Pocas veces"=1,
                        "Nunca"=0))) %>% 
  rename(
    JefeDirector_1M = respuesta_1_1,
    JefeDirector_2M = respuesta_1_2,
    JefeDirector_3M = respuesta_1_3,
    JefeDirector_4M = respuesta_1_4,
    JefeDirector_5M = respuesta_1_5,
    JefeDirector_6M = respuesta_1_6,
    JefeDirector_1I = respuesta_2_1,
    JefeDirector_2I = respuesta_2_2,
    JefeDirector_3I = respuesta_2_3,
    JefeDirector_4I = respuesta_2_4,
    JefeDirector_1G = respuesta_3_1,
    JefeDirector_2G = respuesta_3_2
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
    JefeDirector_1M = round(mean(JefeDirector_1M),2),
    JefeDirector_2M = round(mean(JefeDirector_2M),2),
    JefeDirector_3M = round(mean(JefeDirector_3M),2),
    JefeDirector_4M = round(mean(JefeDirector_4M),2),
    JefeDirector_5M = round(mean(JefeDirector_5M),2),
    JefeDirector_6M = round(mean(JefeDirector_6M),2),
    JefeDirector_1I = round(mean(JefeDirector_1I),2),
    JefeDirector_2I = round(mean(JefeDirector_2I),2),
    JefeDirector_3I = round(mean(JefeDirector_3I),2),
    JefeDirector_4I = round(mean(JefeDirector_4I),2),
    JefeDirector_1G = round(mean(JefeDirector_1G),2),
    JefeDirector_2G = round(mean(JefeDirector_2G),2)
  )


JefeDirectorPC <- CoberturasJefeDirectorPC %>%
  left_join(
    ResultadosJefeDirectorPC,
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

JefeDirectorPC <- JefeDirectorPC %>%
  mutate_if(is.numeric,~replace(.,is.na(.),0))


rm(CoberturasJefeDirectorPC,
   ResultadosJefeDirectorPC)

write_xlsx(JefeDirectorPC,"InsumosProcesados/JefeDirectorPC.xlsx")
write_xlsx(JefeDirectorH,"InsumosProcesados/JefeDirectorH.xlsx")
