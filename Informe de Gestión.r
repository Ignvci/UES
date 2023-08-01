rm(list=ls())

# Librerias ---------------------------------------------------------------

library(kableExtra);library(tidyverse);library(tidyr);library(writexl);library(readr);library(readxl);library(ggplot2)
library(dplyr);library(janitor);library(hrbrthemes);library(tidytext);library(wordcloud2);library(wordcloud)
library(syuzhet);library(patchwork);library(pdftools);library(tm);library(textdata);library(stringr);library(ggthemes)
library(viridis)


# Importación de Datos --------------------------------------------------------------------------------------------

estudiantes2022 <- file.path("D:\\6.-Universidad Arturo Prat - Python\\Evaluación Docente 2022\\Resultados\\EvaluaciónDocenteEstudiantes2022.xlsx")
data2022 <- file.path("D:\\6.-Universidad Arturo Prat - Python\\Evaluación Docente 2022\\Resultados\\evaluaciondocente2022.xlsx")
estudiantes2023 <- file.path("D:\\6.-Universidad Arturo Prat - Python\\Evaluación Docente 2022\\Resultados\\Estudiantes2023.xlsx")


df2023 <- readxl::read_xlsx(estudiantes2023)
df2022 <- readxl::read_xlsx(estudiantes2022)
df <- readxl::read_xlsx(data2022)


# Union y Arreglos --------------------------------------------------------------------------------------------------------

df_ <- bind_rows(df2023,df2022)

df_ <- df_ %>% select(
  -clasificacion_mediador,
  -clasificacion_gestion,
  -clasificacion_actividad
)

df_ <-  df_ %>% mutate(
  clasificacion_actividad =case_when(
    str_detect(codigo_actividad,"^FH")~"Licenciatura de FCH",
    str_detect(codigo_actividad, "^DD|^DR")~ "Electivos de Formación General",
    str_detect(codigo_actividad, "^FIA")~"Plan común FIA",
    str_detect(codigo_actividad, "^FR1|^FR3")~"Plan común FAREN",
    str_detect(codigo_actividad, "^PII")~"Programa Institiucional de Inglés",
    TRUE ~ "NORMAL"
  )
)

df_ <- df_ %>% dplyr::mutate(
  clasificacion_mediador = dplyr::case_when(
                          mediador>= 2.5 | mediador <=3 ~ "Sobresaliente",
                          mediador>= 1.5 | mediador <=2.49 ~ "Satisfactorio",
                          mediador>= 0.5 | mediador <=1.49 ~ "Insuficiente",
                          mediador>=0 | mediador <= 0.49 ~ "Deficiente",
                          finalizado == 0 ~ "No Evaluado"
  )
) %>% dplyr::mutate(
  clasificacion_gestion = dplyr::case_when(
                            gestion>= 2.5 | gestion <=3 ~ "Sobresaliente",
                            gestion>= 1.5 | gestion <=2.49 ~ "Satisfactorio",
                            gestion>= 0.5 | gestion <=1.49 ~ "Insuficiente",
                            gestion>=0 | gestion <= 0.49 ~ "Deficiente",
                            finalizado == 0 ~ "No Evaluado"
  )
)

df_$tipo_carrera <-  gsub("Pregrado Trabajador","CE",df_$tipo_carrera)
df_$tipo_carrera <-  gsub("Pregrado PSU","PSU/PTU/PAES",df_$tipo_carrera)
df_$tipo_carrera <-  gsub("Técnicas","TNS",df_$tipo_carrera)

df$tipo_carrera <-  gsub("Pregrado Trabajador","CE",df$tipo_carrera)
df$tipo_carrera <-  gsub("Pregrado PSU","PSU/PTU/PAES",df$tipo_carrera)
df$tipo_carrera <-  gsub("Técnicas","TNS",df$tipo_carrera)
# Graficos --------------------------------------------------------------------------------------------------------

# Tablas ----------------------------------------------------------------------------------------------------------
df_$no_participa <- df_$no_participa %>% as.double()


estudiantes_resumen <- df_ %>%
  dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
    finalizado = sum(finalizado),
    pendiente = sum(pendiente),
    no_participa = ifelse(
      is.na(sum(no_participa)),
      0,
      sum(no_participa)),
    no_iniciada = sum(no_iniciada),
    total = ifelse(
      is.na(sum(finalizado,no_iniciada,pendiente,no_participa)),
      sum(finalizado,no_iniciada,pendiente),
      sum(finalizado,no_iniciada,pendiente,no_participa)
    ),
    percent_finalizado = round(finalizado/total*100,1),
    percent_pendiente = round(pendiente/total*100,1),
    percent_noparticipa = round(no_participa/total*100,1),
    percent_no_iniciada = round(no_iniciada/total*100,1),
    percent_total = round(sum(percent_finalizado,
                              percent_pendiente,
                              percent_noparticipa,
                              percent_no_iniciada),0)
  )

# Grafico: Cobetura de Estudiantes --------------------------------------------------------------------------------

Participacion_e <- estudiantes_resumen %>% dplyr::filter(semestre==1) %>% 
  tidyr::unite(col = proceso,
               c(proceso,semestre),
               sep="-") %>% 
  ggplot2::ggplot(aes(x=fct_reorder(tipo_carrera,percent_finalizado),
                      y=percent_finalizado,fill=tipo_carrera))+
  geom_col()+facet_grid(~proceso)+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", face = "italic"),
    strip.text = element_text(face = 'italic',color = 'white'),
    strip.background = element_rect(fill='#1C2833')
  )+ ylim(0,25)+ scale_fill_manual(values = 
                                     c("#1C2833","#2471A3","#DC7633"))+
  geom_text(aes(label=paste(percent_finalizado,"%")),nudge_y = 0.7)+
  labs(
    fill = "Modalidad",
    title = "Participación de Estudiantes",
    subtitle = "2022-2023",
    y = "Porcentaje(%)",
    x = "Modalidad",
    caption = "Fuente: Datos de Reportes Genéricos | DACID-UES"
  )


# Tabla: Resumen Cobertura Estudiantes ----------------------------------------------------------------------------

resumen_estudiantes <- df_ %>%
  dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
    finalizado = sum(finalizado),
    pendiente = sum(pendiente),
    no_participa = ifelse(
      is.na(sum(no_participa)),
      0,
      sum(no_participa)),
    no_iniciada = sum(no_iniciada),
    total = ifelse(
      is.na(sum(finalizado,no_iniciada,pendiente,no_participa)),
      sum(finalizado,no_iniciada,pendiente),
      sum(finalizado,no_iniciada,pendiente,no_participa)
    ),
    percent_finalizado = round(finalizado/total*100,1),
    percent_pendiente = round(pendiente/total*100,1),
    percent_noparticipa = round(no_participa/total*100,1),
    percent_no_iniciada = round(no_iniciada/total*100,1),
    percent_total = round(sum(percent_finalizado,
                              percent_pendiente,
                              percent_noparticipa,
                              percent_no_iniciada),0)
  ) %>% tidyr::unite(col = proceso,
                     c(proceso,
                       semestre),
                     sep = "-") %>% 
  select(proceso,
         tipo_carrera,
         finalizado,
         total,
         percent_finalizado) %>% filter(proceso=="2022-1"|
                                          proceso=="2023-1") %>%
  dplyr::ungroup() %>% 
  kbl(caption="Resumen: Estudiantes",
      col.names = c("Proceso",
                    "Modalidad",
                    "E.F",
                    "E.G",
                    "%"),
      align = c("l","l","r","r","r"),
      booktabs = T,
      format = "pipe")



# Tabla: Resumen Variación Cobertura Estudiantes ------------------------------------------------------------------

r2022 <- estudiantes_resumen %>% dplyr::filter(proceso==2022,semestre==1)
r2023 <- estudiantes_resumen %>% dplyr::filter(proceso==2023,semestre==1)

resumenvar_estudiantes <- r2022 %>% left_join(r2023,
                                              by=c("semestre","tipo_carrera"),
                                              suffix = c("_2022","_2023")) %>% 
  mutate(
    var_ab = finalizado_2023 - finalizado_2022,
    var_re = round(percent_finalizado_2023 - percent_finalizado_2022,1)
  ) %>% 
  ungroup() %>% select(
    tipo_carrera,
    var_ab,
    var_re,
    -semestre
  ) %>% kbl(
    caption = "Variación Estudiantes",
    col.names = c("Modalidad","V.A","V.R"),
    booktabs = T,
    format = "pipe"
  )

# Gráfico: Coberturas Autoevaluación --------------------------------------------------------------------------------------------------

Participacion_a <- df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  autoevaluacion = sum(finalizado_autoevaluacion),
  autoevaluacion_t = sum(total_autoevaluacion)
) %>% dplyr::mutate( porcentaje = round(autoevaluacion/autoevaluacion_t*100,1)
) %>% unite(col=proceso,
            c(proceso,semestre),
            sep = "-") %>% ggplot(aes(x=tipo_carrera,y=porcentaje,fill=tipo_carrera))+geom_col()+facet_grid(~proceso)+
  ylim(0,100)+geom_text(aes(label=paste(porcentaje,"%")),nudge_y = 2)+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", face = "italic")
  )+ scale_fill_manual(values = c("#1C2833","#2471A3","#DC7633"))+
  labs(
    fill = "Modalidad",
    title = "Participación de Autoevaluación",
    subtitle = "2022",
    y = "Porcentaje(%)",
    x = "Modalidad",
    caption = "Fuente: Datos de Reportes Genéricos | DACID-UES"
  )


# Tabla: Resumen Cobertura Autoevaluación -------------------------------------------------------------------------

resumen_autoevaluacion <- df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  autoevaluacion = sum(finalizado_autoevaluacion),
  autoevaluacion_t = sum(total_autoevaluacion)
) %>% dplyr::mutate( porcentaje = round(autoevaluacion/autoevaluacion_t*100,1)) %>% 
  unite(col=proceso,
        c(proceso,semestre),
        sep = "-") %>% kbl(
          caption = "Resumen: Autoevaluación",
          col.names = c("Proceso","Modalidad","E.F","E.G","%"),
          booktabs = T,
          format = "pipe"
        )


# Tabla: Resumen Variación de Cobertura Autoevaluación ------------------------------------------------------------


a_1 = df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  autoevaluacion = sum(finalizado_autoevaluacion),
  autoevaluacion_t = sum(total_autoevaluacion)
) %>% dplyr::mutate( porcentaje = round(autoevaluacion/autoevaluacion_t*100,1)) %>% filter(semestre==1)

a_2 = df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  autoevaluacion = sum(finalizado_autoevaluacion),
  autoevaluacion_t = sum(total_autoevaluacion)
) %>% dplyr::mutate( porcentaje = round(autoevaluacion/autoevaluacion_t*100,1)) %>% filter(semestre==2)

resumenvar_autoevaluacion <- a_1 %>% left_join(a_2, by=c("proceso","tipo_carrera"),
                                               suffix=c("_1","_2")) %>% 
  mutate(
    var_ab = autoevaluacion_2 - autoevaluacion_1,
    var_re = round(porcentaje_2 - porcentaje_1,1)
  ) %>% ungroup() %>% 
  select(tipo_carrera,
         var_ab,
         var_re) %>% kbl(
           caption = "Variación Autoevaluación",
           col.names = c("Modalidad","V.A","V.R"),
           format = "pipe",
           booktabs = T
         )




# Gráfico: Cobertura Direccioness/Jefaturas -----------------------------------------------------------------------

Participacion_d <- df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  director = sum(finalizado_director),
  director_t = sum(total_director)
) %>% dplyr::mutate( porcentaje = round(director/director_t*100,1)) %>% unite(col=proceso,
                                                                              c(proceso,semestre),
                                                                              sep = "-") %>% ggplot(aes(x=tipo_carrera,y=porcentaje,fill=tipo_carrera))+geom_col()+facet_grid(~proceso)+
  ylim(0,110)+geom_text(aes(label=paste(porcentaje,"%")),nudge_y = 2)+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", face = "italic")
  )+ scale_fill_manual(values = c("#1C2833","#2471A3","#DC7633"))+
  labs(
    fill = "Modalidad",
    title = "Participación de Direcciones/Jefaturas",
    subtitle = "2022",
    y = "Porcentaje(%)",
    x = "Modalidad",
    caption = "Fuente: Datos de Reportes Genéricos | DACID-UES"
  )

# Tabla: Resumen Cobertura Direcciones/Jefaturas ------------------------------------------------------------------

resumen_director <- df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  director = sum(finalizado_director),
  director_t = sum(total_director)
) %>% dplyr::mutate( porcentaje = round(director/director_t*100,1)) %>% 
  unite(col=proceso,
        c(proceso,semestre),
        sep = "-") %>% kbl(
          caption = "Resumen: Direcciones/Jefaturas",
          col.names = c("Proceso","Modalidad","E.F","E.G","%"),
          booktabs = T,
          format = "pipe"
        )

# Tabla: Resumen Variación de Cobertura Direcciones/Jefaturas de Carreras ------------------------------------------------------

d1 <- df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  director = sum(finalizado_director),
  director_t = sum(total_director)
) %>% dplyr::mutate( porcentaje = round(director/director_t*100,1)) %>% 
  filter(semestre==1)

d2 <- df %>% dplyr::group_by(proceso,semestre,tipo_carrera) %>% summarise(
  director = sum(finalizado_director),
  director_t = sum(total_director)
) %>% dplyr::mutate( porcentaje = round(director/director_t*100,1)) %>% 
  filter(semestre==2)

d1 %>% left_join(d2,
                 by = c("proceso","tipo_carrera"),
                 suffix = c("_1","_2")) %>% 
  mutate(
    var_ab = director_2 - director_1,
    var_re = round(porcentaje_2 - porcentaje_1,1)
  ) %>% ungroup() %>% 
  select(
    tipo_carrera,
    var_ab,
    var_re)


resumenvar_director <- d1 %>% left_join(d2,
                 by = c("proceso","tipo_carrera"),
                 suffix = c("_1","_2")) %>% 
  mutate(
    var_ab = director_2 - director_1,
    var_re = round(porcentaje_2 - porcentaje_1,1)
  ) %>% ungroup() %>% 
  select(
    tipo_carrera,
    var_ab,
    var_re) %>% kbl(
      caption = "Variación Direccion/Jefatura",
      col.names = c("Modalidad","V.A","V.R"),
      format = "pipe",
      booktabs = T
    )


# Elementos de Coberturas -----------------------------------------------------------------------------------------


proceso_2023_1 <- df_ %>% tidyr::unite(col=proceso,
                     c(proceso,
                       semestre),
                     sep = "-") %>% select(proceso) %>% 
  unique() %>% filter(proceso=="2023-1") %>% as.String()

proceso_2022_2 <- df_ %>% tidyr::unite(col=proceso,
                     c(proceso,
                       semestre),
                     sep = "-") %>% select(proceso) %>% 
  unique() %>% filter(proceso=="2022-2") %>% as.String()

proceso_2022_1 <- df_ %>% tidyr::unite(col=proceso,
                     c(proceso,
                       semestre),
                     sep = "-") %>% select(proceso) %>% 
  unique() %>% filter(proceso=="2022-1") %>% as.String()

face <- "Facultad de Ciencias Empresariales"
fia <- "Facultad de Ingeniería y Arquitectura"
cft <- "Formación Técnica"
faren <- "Facultad de Recursos Naturales Renovables"
fcs <- "Facultad de la Ciencias de la Salud"
fch <- "Facultad de Ciencias Humanas"
fcjp <- "Facultad de Ciencias Jurídicas y Políticas"



# Grafico: Consentmiento por Modalidad-----------------------------------------------------------------------------------------------------


Consentimiento_Modalidad <- estudiantes_resumen %>% dplyr::filter(semestre==1,proceso==2023) %>% 
  dplyr::select(
                proceso,
                semestre,
                tipo_carrera,
                no_participa,
                total,
                percent_noparticipa
  ) %>% tidyr::unite(
                    col = proceso,
                    c(proceso,semestre),
                    sep ="-"
  ) %>% ggplot2::ggplot(aes(x=fct_reorder(tipo_carrera,no_participa),
                            y= no_participa,
                            fill= tipo_carrera))+
  geom_col()+facet_grid(~proceso)+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", face = "italic"),
    strip.text = element_text(face = 'italic',color = 'white'),
    strip.background = element_rect(fill='#1C2833')
  )+ scale_fill_manual(values = 
                                     c("#1C2833","#2471A3","#DC7633"))+
  geom_text(aes(label=no_participa),nudge_y = 10)+
  labs(
    fill = "Modalidad",
    title = "Consentimiento de Estudiantes",
    subtitle = "Primer Semestre 2023",
    y = "Recuento",
    x = "Modalidad",
    caption = "Fuente: Datos de Reportes Genéricos | DACID-UES"
  )


# Tabla: Consentimiento por Modalidad ------------------------------------------------------------------------------

TablaConsentimiento <- estudiantes_resumen %>% dplyr::filter(semestre==1,proceso==2023) %>% 
  dplyr::select(
    proceso,
    semestre,
    tipo_carrera,
    no_participa,
    total,
    percent_noparticipa
  ) %>% tidyr::unite(
    col = proceso,
    c(proceso,semestre),
    sep ="-"
  ) %>% kbl(
    caption = "Resumen: Consentimiento Estudiantes 2023",
    col.names = c("Proceso","Modalidad","C.N","Total","%"),
    format = "pipe",
    align = c("l","l","r","r","r"),
    booktabs = T)


# Gráfico: Consentimiento por Unidad Académica ---------------------------------------------------------------------


Consentimiento_UA <-  df_ %>% dplyr::filter(semestre==1,
                      proceso==2023) %>% 
  dplyr::group_by(proceso,
                  semestre,
                  unidad_academica) %>% 
  dplyr::summarise(no_participa = sum(no_participa),
            total = sum(no_participa,finalizado,pendiente,no_iniciada),
            percent_noparticipa = round(no_participa/total*100,1)) %>% 
  dplyr::ungroup() %>% filter(no_participa>0) %>% unite(col = proceso,
                                                        c(proceso,semestre),
                                                        sep = "-") %>% 
  ggplot2::ggplot(aes(x=fct_reorder(unidad_academica,no_participa),
                      y=no_participa,
                      fill=unidad_academica))+geom_col()+
  geom_text(aes(label=no_participa),nudge_y = 5)+
  scale_fill_manual(values = c("#1C2833", 
                               "#2471A3", 
                               "#5D4037", 
                               "#556B2F", 
                               "#800000", 
                               "#006400", 
                               "#36454F"))+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", face = "italic")
  ) +
  labs(
    fill = "Modalidad",
    title = "Consentimiento de Estudiantes por Unidad Académica",
    subtitle = "Primer Semestre 2023",
    y = "Recuento",
    x = "Unidad Académica",
    caption = "Fuente: Datos de Reportes Genéricos | DACID-UES"
  )
  


# Tabla: Consentimiento por Unidad Académica ----------------------------------------------------------------------

resumen_consentimientoua <- df_ %>% dplyr::filter(semestre==1,
                      proceso==2023) %>% 
  dplyr::group_by(proceso,
                  semestre,
                  unidad_academica) %>% 
  dplyr::summarise(no_participa = sum(no_participa),
                   total = sum(no_participa,finalizado,pendiente,no_iniciada),
                   percent_noparticipa = round(no_participa/total*100,1)) %>% 
  dplyr::ungroup() %>% filter(no_participa>0) %>% unite(col = proceso,
                                                        c(proceso,
                                                          semestre),
                                                        sep = "-") %>% 
  dplyr::arrange(desc(no_participa))%>% 
  kbl(caption="Resumen: Consentimiento",
      col.names = c("Proceso",
                    "U.A",
                    "C.N",
                    "Total",
                    "%"),
      align = c("l","l","r","r","r"),
      format = "pipe",
      booktabs = T)


# Gráfico Consentimiento por Carrera ------------------------------------------------------------------------------

Consentimiento_Carrera <- df_ %>% dplyr::filter(semestre==1,
                      proceso==2023) %>% group_by(proceso,
                                                  semestre,
                                                  codigo_carrera,
                                                  nombre_carrera) %>% 
  summarise(no_participa=sum(no_participa),
            total = sum(no_participa,
                     finalizado,
                     no_iniciada,
                     pendiente),
            percent_noparticipa = round(no_participa/total*100,1)) %>% 
  dplyr::arrange(desc(no_participa)) %>% head(10) %>% 
  tidyr::unite(proceso,
               c(proceso,
                 semestre),
               sep = "-") %>% ggplot2::ggplot(aes(x=
                                                    fct_reorder(
                                                      as.factor(codigo_carrera),
                                                      no_participa),
                                                  y=no_participa,
                                                  fill=as.factor(codigo_carrera)))+
  geom_col()+scale_fill_manual(values = c("#DAF7A6", 
                                          "#FFC300", 
                                          "#FF5733", 
                                          "#E74C3C", 
                                          "#C70039",
                                          "#900C3F", 
                                          "#581845", 
                                          "#232296", 
                                          "#41A5CD", 
                                          "#1CB275"))+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", face = "italic"),
  )+
  geom_text(aes(label=no_participa),nudge_y = 2)+
  labs(
    fill = "Carrera",
    title = "Consentimiento de Estudiantes por Carrera",
    subtitle = "10 primeras carreras",
    y = "Recuento",
    x = "Carrera",
    caption = "Fuente: Datos de Reportes Genéricos | DACID-UES"
  )


# Tabla: Consentimiento por Carrera -------------------------------------------------------------------------------

resumen_consentimiento_carrera <- df_ %>% dplyr::filter(semestre==1,
                      proceso==2023) %>% group_by(proceso,
                                                  semestre,
                                                  codigo_carrera,
                                                  nombre_carrera) %>% 
  summarise(no_participa=sum(no_participa),
            total = sum(no_participa,
                        finalizado,
                        no_iniciada,
                        pendiente),
            percent_noparticipa = round(no_participa/total*100,1)) %>% 
  dplyr::arrange(desc(no_participa)) %>% head(10) %>% 
  tidyr::unite(col=proceso,
               c(proceso,
                 semestre),
               sep = "-") %>% tidyr::unite(col=carrera,
                                           c(codigo_carrera,
                                             nombre_carrera),
                                           sep = " ") %>% 
  dplyr::select(-total) %>% 
  kbl(caption = "Resumen: Consentimiento por Carrera (10 primeras)",
      col.names = c("Proceso",
                    "Carrera",
                    "C.N",
                    "%"),
      align = c("l","l","r","r"),
      format = "pipe",
      booktabs = T)


# Elementos de Consentimiento -------------------------------------------------------------------------------------

