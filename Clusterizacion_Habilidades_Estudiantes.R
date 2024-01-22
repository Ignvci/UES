Generación de perfiles por habilidad UWU 

data_perfiles_carrera <- data |> 
  mutate(
  clasificacion_docente_estudiantes = case_when(
    h_negativas == 0 ~ "Perfil Docente 0",
    h_negativas == 1 ~ "Perfil Docente 1",
    h_negativas == 2 ~ "Perfil Docente 2",
    h_negativas == 3 ~ "Perfil Docente 3",
    h_negativas == 4 ~ "Perfil Docente 4",
    h_negativas == 5 ~ "Perfil Docente 5",
    h_negativas == 6 & ef==0 ~ "Perfil Docente 6.1",
    h_negativas == 6 ~ "Perfil Docente 6",
  )
) |> 
  dplyr::group_by(codigo_carrera,
                  nombre_carrera) |> 
  count(clasificacion_docente_estudiantes) |> 
  tidyr::pivot_wider(names_from = clasificacion_docente_estudiantes,
                     values_from = n) |> 
  dplyr::mutate_if(is.integer, ~ ifelse(is.na(.),0,.))

data_perfiles_carrera <- data_perfiles_carrera |> 
  janitor::clean_names() |> 
  mutate(
    total = perfil_docente_0 + perfil_docente_1 + perfil_docente_3 + perfil_docente_5 + perfil_docente_6 + perfil_docente_6_1 + perfil_docente_2 + perfil_docente_4
  ) |> 
  mutate(
    perfil_docente_0 = round(perfil_docente_0/total*100,1),
    perfil_docente_1 = round(perfil_docente_1/total*100,1),
    perfil_docente_2 = round(perfil_docente_2/total*100,1),
    perfil_docente_3 = round(perfil_docente_3/total*100,1),
    perfil_docente_4 = round(perfil_docente_4/total*100,1),
    perfil_docente_5 = round(perfil_docente_5/total*100,1),
    perfil_docente_6 = round(perfil_docente_6/total*100,1),
    perfil_docente_6_1 = round(perfil_docente_6_1/total*100,1),
  ) |> ungroup()


model_4 <- kmeans(data_perfiles_carrera$perfil_docente_0, centers = 4)
cluster_4 <- model_4$cluster

data_perfiles_carrera <- mutate(data_perfiles_carrera, cluster_pd_0 = cluster_4)

model_4 <- kmeans(data_perfiles_carrera$perfil_docente_1, centers = 4)
cluster_4 <- model_4$cluster

data_perfiles_carrera <- mutate(data_perfiles_carrera, cluster_pd_1 = cluster_4)

data_perfiles_carrera |> 
  dplyr::group_by(cluster_pd_1) |> 
  reframe(
    mean(perfil_docente_1),
    min(perfil_docente_1),
    max(perfil_docente_1)
  )

model_4 <- kmeans(data_perfiles_carrera$perfil_docente_6_1, centers = 4)
cluster_4 <- model_4$cluster
data_perfiles_carrera <- mutate(data_perfiles_carrera, cluster_pd_6_1 = cluster_4)

data_perfiles_carrera |> View()
