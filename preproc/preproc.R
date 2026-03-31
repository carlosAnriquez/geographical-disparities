
# cargar paquetes

pacman::p_load(dplyr,readxl,ggplot2,tidyverse,tibble, scales, purrr, here, RColorBrewer, semPlot, forcats, 
               poLCA, scales, gt, lavaan, sessioninfo, sjPlot, broom, webshot2, htmltools, openxlsx, summarytools, stringr)

# cargar funciones auxiliares

source("aux_func.R")

# cargar bb.dd

df <- read_excel("input/maestra.xlsx", na = "#N/D")


df <- df %>%
  rename(retorno_pais = `RETORNO AL PAÍS`)

# guardar bb.dd

saveRDS(df, file = "output/df.rds")

## preprocesamiento

df_proc <-  as.data.frame( df %>% dplyr::select(FOLIO, ETNIA,TIPO_BECA,ANO_INICIO_BECA,
                                                DISCIPLINA_DOCTORADO, 
                                                FECHA_TERMINO_DOCTORADO,INICIO_BECA, FIN_BECA,
                                                retorno_pais, PAIS_POSGRADO,CHILEDOCTORADO,
                                                COMUNA_ESTABLECIMIENTO,SEXO_REGISTRADO,
                                                GSE_ESTABLECIMIENTO, REGION_ESTABLECIMIENTO,
                                                PREGRADO_ELITE,ANIOS_ACREDITACION_PREGRADO,
                                                ACREDITACION_PREGRADO,INTERSECCION, UNIVERSIDAD_PROGRAMA,
                                                QS.OVERALL.ESTIMATED, SHANGHAI.OVERALL.ESTIMATED,
                                                AUTHOR_ID, ID_UNIVERSIDAD, SCOPUS_COAUTHOR_COUNT, 
                                                SCOPUS_FIRST_AUTHOR_PUBLICATIONS,SCOPUS_COUNT_ANTES,
                                                SCOPUS_COUNT_DESPUES, SCOPUS_COUNT_DURANTE, 
                                                SJR_AVG_JCPD, SJR_N_Q1, SJR_N_Q2,
                                                SJR_N_Q3, SJR_N_Q4, SJR_AVG_Q))

# modificar variables, crear variables auxiliares


df_proc <- df_proc %>%
  mutate(
    ES_CHILE = if_else(PAIS_POSGRADO == "CHILE", "Chile", "Extranjero"),
    PREGRADO_ELITE = factor(as.character(PREGRADO_ELITE),
                            levels = c("0", "1", "extranjero"),
                            labels = c("No", "Si", "extranjero")),
    ETNIA = factor(ETNIA, levels = c("NO", "SI", "sin_informacion"),
                   labels = c("No", "Si", "Sin información")),
    ES_REGION = case_when(
      REGION_ESTABLECIMIENTO == "sin_informacion"      ~ "Sin información",
      REGION_ESTABLECIMIENTO == "estudio extranjero"   ~ "Estudio extranjero",
      REGION_ESTABLECIMIENTO == "Región Metropolitana" ~ "Santiago",
      TRUE                                             ~ "Región"),
    ES_REGION2 = case_when(
      REGION_ESTABLECIMIENTO %in% c("Región Metropolitana", "Región de Valparaíso",
                                    "Región del Biobío") ~ "Santiago, Valparaíso o Concepción",
      REGION_ESTABLECIMIENTO == "sin_informacion"        ~ "Sin información",
      REGION_ESTABLECIMIENTO == "estudio extranjero"     ~ "Estudio extranjero",
      TRUE                                               ~ "Otra región"),
    SEXO_REGISTRADO = factor(SEXO_REGISTRADO, levels = c("FEMENINO", "MASCULINO"),
                             labels = c("Femenino", "Masculino")),
    GSE_ESTABLECIMIENTO = factor(GSE_ESTABLECIMIENTO,
                                 levels = c("Alto", "Bajo", "estudio extranjero", "Medio",
                                            "Medio alto", "sin_informacion"),
                                 labels = c("Alto", "Bajo", "Estudios extranjeros", "Medio",
                                            "Medio alto", "Sin información")))

df_proc$SCOPUS_COAUTHOR_COUNT[is.na(df_proc$SCOPUS_COAUTHOR_COUNT)] <- 0
df_proc$SCOPUS_FIRST_AUTHOR_PUBLICATIONS[is.na(df_proc$SCOPUS_FIRST_AUTHOR_PUBLICATIONS)] <- 0
df_proc$SJR_AVG_JCPD[is.na(df_proc$SJR_AVG_JCPD)] <- 0
df_proc$SJR_N_Q1[is.na(df_proc$SJR_N_Q1)] <- 0
df_proc$SJR_N_Q2[is.na(df_proc$SJR_N_Q2)] <- 0
df_proc$SJR_N_Q3[is.na(df_proc$SJR_N_Q3)] <- 0
df_proc$SJR_N_Q4[is.na(df_proc$SJR_N_Q4)] <- 0
df_proc$SJR_AVG_Q[is.na(df_proc$SJR_AVG_Q)] <- 0


df_proc$DISCIPLINA_DOCTORADO <- dplyr::case_when(
  df_proc$DISCIPLINA_DOCTORADO %in% c("Humanidades", "HUMANIDADES") ~ "Humanidades",
  df_proc$DISCIPLINA_DOCTORADO %in% c("Ciencias Sociales", "CIENCIAS SOCIALES") ~ "Ciencias Sociales",
  df_proc$DISCIPLINA_DOCTORADO == "CIENCIAS AGRICOLAS" ~ "Ciencias Agricolas",
  df_proc$DISCIPLINA_DOCTORADO == "CIENCIAS MEDICAS Y DE SALUD" ~ "Ciencias Médicas",
  df_proc$DISCIPLINA_DOCTORADO %in% c("Ingeniería y Tecnología", "INGENIERÍA Y TECNOLOGÍA") ~ "Ingeniería",
  df_proc$DISCIPLINA_DOCTORADO %in% c("Ciencias Naturales", "CIENCIAS NATURALES") ~ "Ciencias Naturales",
  TRUE ~ df_proc$DISCIPLINA_DOCTORADO)

freq(df_proc$DISCIPLINA_DOCTORADO)

# cambiar NA a 0 en shanghai

df_proc$SHANGHAI.OVERALL.ESTIMATED[is.na(df_proc$SHANGHAI.OVERALL.ESTIMATED)] <- 0

# crear nueva variable en puntaje z y promediar

df_proc <- df_proc %>% 
  mutate(
    Z_QS = scale(QS.OVERALL.ESTIMATED)[,1],
    Z_SHANGHAI = scale(SHANGHAI.OVERALL.ESTIMATED)[,1]) %>%
  mutate(Average = rowMeans(
    cbind(Z_QS, Z_SHANGHAI), na.rm = TRUE))


# nueva variable para regiones (3)

# región del pregrado

nombres_regiones_3 <- c(
  "estudio extranjero" = "Estudio extranjero",
  "RegiÃ³n de Ãuble" = "Otra región",
  "RegiÃ³n de Antofagasta" = "Otra región",
  "RegiÃ³n de Arica y Parinacota" = "Otra región", 
  "RegiÃ³n de Atacama" = "Otra región",
  "RegiÃ³n de AysÃ©n" = "Otra región",
  "RegiÃ³n de Coquimbo" = "Otra región",
  "RegiÃ³n de La AraucanÃ­a" = "Otra región",
  "RegiÃ³n de Los Lagos" = "Otra región",
  "RegiÃ³n de Los RÃ­os" = "Otra región",
  "RegiÃ³n de Magallanes y AntÃ¡rtica Chilena" = "Otra región",
  "RegiÃ³n de TarapacÃ¡" = "Otra región", 
  "RegiÃ³n de ValparaÃ­so" = "Valparaiso o Concepción",
  "RegiÃ³n del BiobÃ­o" = "Valparaiso o Concepción",
  "RegiÃ³n del Libertador General Bernardo O'Higgins" = "Otra región",
  "RegiÃ³n del Maule" = "Otra región",
  "RegiÃ³n Metropolitana" = "Región Metropolitana",
  "sin_informacion" = "sin_informacion")

df_proc$REGIONES_3 <- nombres_regiones_3[df_proc$REGION_ESTABLECIMIENTO]

saveRDS(df_proc, file = "output/df_proc.rds")

#  filtrado
df_filtrado <- df_proc %>%
  filter(!GSE_ESTABLECIMIENTO %in% c("Sin información", "Estudios extranjeros")) %>%
  filter(!ES_REGION %in% c("Sin información", "Estudio extranjero")) %>%
  filter(!ACREDITACION_PREGRADO %in% c("extranjero"), !is.na(ACREDITACION_PREGRADO)) %>%
  filter(!REGION_ESTABLECIMIENTO %in% c("sin_informacion", "estudio extranjero")) %>%
  filter(!ANIOS_ACREDITACION_PREGRADO %in% c("Sin información")) %>%
  filter(!is.na(PREGRADO_ELITE)) %>%
  filter(!REGIONES_3 %in% c("sin_informacion", "Estudio extranjero")) %>%
  dplyr::select(FOLIO, ETNIA, TIPO_BECA, ANO_INICIO_BECA,
                DISCIPLINA_DOCTORADO, Average,
                FECHA_TERMINO_DOCTORADO, INICIO_BECA, FIN_BECA,
                retorno_pais, PAIS_POSGRADO, CHILEDOCTORADO,
                COMUNA_ESTABLECIMIENTO, SEXO_REGISTRADO, REGIONES_3,
                GSE_ESTABLECIMIENTO, REGION_ESTABLECIMIENTO,
                PREGRADO_ELITE, ANIOS_ACREDITACION_PREGRADO,
                ACREDITACION_PREGRADO, INTERSECCION, UNIVERSIDAD_PROGRAMA,
                QS.OVERALL.ESTIMATED, SHANGHAI.OVERALL.ESTIMATED,
                AUTHOR_ID, ID_UNIVERSIDAD, SCOPUS_COAUTHOR_COUNT,
                SCOPUS_FIRST_AUTHOR_PUBLICATIONS, SCOPUS_COUNT_ANTES,
                SCOPUS_COUNT_DESPUES, SCOPUS_COUNT_DURANTE,
                SJR_AVG_JCPD, SJR_N_Q1, SJR_N_Q2,
                SJR_N_Q3, SJR_N_Q4, SJR_AVG_Q, Z_SHANGHAI, Z_QS)

nrow(df_filtrado)

# Transformar CHILEDOCTORADO a factor
df_filtrado <- df_filtrado %>%
  mutate(CHILEDOCTORADO = factor(
    CHILEDOCTORADO,
    levels = c(0, 1),
    labels = c("Extranjero", "Chile")))

df_filtrado$GSE_ESTABLECIMIENTO <- factor(df_filtrado$GSE_ESTABLECIMIENTO,
                                          levels = c("Alto", "Medio alto", "Medio", "Bajo"))

df_filtrado$REGIONES_3 <- factor(df_filtrado$REGIONES_3,
                                 levels = c("Región Metropolitana", "Valparaiso o Concepción", "Otra región"))

df_filtrado$ACREDITACION_PREGRADO <- factor(df_filtrado$ACREDITACION_PREGRADO,
                                            levels = c("Investigacion", "Excelencia", "Acreditacion 5", 
                                                       "Acreditacion 4",
                                                       "Baja acreditacion o no acreditada"))

df_filtrado$SEXO_REGISTRADO<- factor(df_filtrado$SEXO_REGISTRADO,
                                     levels = c("Masculino", "Femenino"))

df_filtrado <- df_filtrado %>%
  group_by(DISCIPLINA_DOCTORADO) %>%
  mutate(z_scopus_despues = (SCOPUS_COUNT_DESPUES - mean(SCOPUS_COUNT_DESPUES, na.rm = TRUE)) / 
        sd(SCOPUS_COUNT_DESPUES, na.rm = TRUE)) %>% ungroup()

df_filtrado <- df_filtrado %>%
  mutate(tipuban = factor(
    ifelse(SCOPUS_COUNT_ANTES > 0, "Tiene publicaciones", "Sin publicaciones"),
    levels = c("Sin publicaciones", "Tiene publicaciones")))

saveRDS(df_filtrado, file = "output/df_filtrado.rds")

## cargar bb.dd 

postulantes <- read_excel("input/postulantes.xlsx")

postulantes_summary <- dfSummary(postulantes, graphs = FALSE, graph.col   = FALSE,  valid.col = FALSE,
                                 varnumbers  = FALSE, style = "multiline")

stview(postulantes_summary, file = "output/resumen_postulantes.html")

## 

postulantes_filtrado <- postulantes %>% dplyr::select("ESTADO PROYECTO", "ÁREA OECD", "GÉNERO",
                                                      "Female", "REGIÓN PERSONA", "ethnnicity", 
                                                      "Dependencia Ed. Secundaria", 
                                                      "Institución Título o Lic.", "Título o Lic.",
                                                      "Puntaje Postulación", "isSelected")

postulantes_filtrado <- postulantes_filtrado %>%
  rename(
    ESTADO_PROYECTO = `ESTADO PROYECTO`,
    GENERO = `GÉNERO`,
    AREA_OCDE = `ÁREA OECD`,
    REGION_PERSONA = `REGIÓN PERSONA`,
    DEPENDENCIA = `Dependencia Ed. Secundaria`,
    UNIVERSIDAD_PREGRADO = `Institución Título o Lic.`,
    TITULO = `Título o Lic.`,
    ETNIA = `ethnnicity`, 
    PUNTAJE_POSTULACION = `Puntaje Postulación`,
    SELECCIONADO = `isSelected`) %>%
  mutate(
    ESTADO_PROYECTO = as.factor(ESTADO_PROYECTO),
    AREA_OCDE = as.factor(AREA_OCDE),
    REGION_PERSONA = as.factor(REGION_PERSONA),
    DEPENDENCIA = as.factor(DEPENDENCIA),
    GENERO = as.factor(GENERO),
    UNIVERSIDAD_PREGRADO = as.factor(UNIVERSIDAD_PREGRADO),
    TITULO = as.factor(TITULO),
    ETNIA = as.factor(ETNIA), 
    PUNTAJE_POSTULACION = as.numeric(PUNTAJE_POSTULACION))


postulantes_filtrado_summary <- dfSummary(postulantes_filtrado, graphs = FALSE, graph.col   = FALSE,  valid.col = FALSE,
                                          varnumbers  = FALSE, style = "multiline")

stview(postulantes_filtrado_summary, file = "output/resumen_postulantes_filtrado.html")

### transformar variables

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    GENERO = case_when(
      GENERO %in% c("Femenino", "FEMENINO") ~ "Femenino",
      GENERO %in% c("Masculino", "MASCULINO") ~ "Masculino",
      TRUE ~ GENERO),
    GENERO = as.factor(GENERO),
    DEPENDENCIA = case_when(
      DEPENDENCIA == "S/I" ~ NA_character_,
      DEPENDENCIA == "ESTUDIO EN EL EXTRANJERO" ~ "Estudio extranjero",
      DEPENDENCIA == "PARTICULAR SUBVENCIONADO" ~ "Particular Subvencionado",
      TRUE ~ DEPENDENCIA),
    DEPENDENCIA = as.factor(DEPENDENCIA), 
    REGION_PERSONA = case_when(
      REGION_PERSONA %in% c("01. TARAPACÁ", "Región de Tarapacá") ~ "Tarapacá",
      REGION_PERSONA %in% c("02. ANTOFAGASTA", "Región de Antofagasta") ~ "Antofagasta",
      REGION_PERSONA %in% c("03. ATACAMA", "Región de Atacama") ~ "Atacama",
      REGION_PERSONA %in% c("04. COQUIMBO", "Región de Coquimbo") ~ "Coquimbo",
      REGION_PERSONA %in% c("05. VALPARAÍSO", "Región de Valparaíso") ~ "Valparaíso",
      REGION_PERSONA %in% c("06. LIBERTADOR GENERAL BERNARDO O'HIGGINS", 
                            "Región del Libertador General Bernardo OHiggins") ~ "O'Higgins",
      REGION_PERSONA %in% c("07. MAULE", "Región del Maule") ~ "Maule",
      REGION_PERSONA %in% c("08. BIOBÍO", "Región del Bío-Bío") ~ "Biobío",
      REGION_PERSONA %in% c("09. LA ARAUCANÍA", "Región de la Araucanía") ~ "Araucanía",
      REGION_PERSONA %in% c("10. LOS LAGOS", "Región de los Lagos") ~ "Los Lagos",
      REGION_PERSONA %in% c("11. AYSÉN DEL GENERAL CARLOS IBÁÑEZ DEL CAMPO", 
                            "Región Aysén del General Carlos Ibáñez del Campo") ~ "Aysén",
      REGION_PERSONA %in% c("12. MAGALLANES Y ANTÁRTICA CHILENA", 
                            "Región de Magallanes y la Antártica Chilena") ~ "Magallanes",
      REGION_PERSONA %in% c("13. METROPOLITANA", "Región Metropolitana") ~ "Metropolitana",
      REGION_PERSONA %in% c("14. LOS RÍOS", "Región de Los Ríos") ~ "Los Ríos",
      REGION_PERSONA %in% c("15. ARICA Y PARINACOTA", "Región de Arica y Parinacota") ~ "Arica",
      REGION_PERSONA == "EXTRANJERO" ~ "Extranjero",
      REGION_PERSONA %in% c("S/I", "SIN INFORMACIÓN") ~ NA_character_,
      TRUE ~ REGION_PERSONA),
    REGION_PERSONA = as.factor(REGION_PERSONA)) %>%
  filter(DEPENDENCIA != "Estudio extranjero" | is.na(DEPENDENCIA))  # LÍNEA NUEVA

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    REGIONES_3 = case_when(
      REGION_PERSONA == "Metropolitana" ~ "Metropolitana",
      REGION_PERSONA %in% c("Valparaíso", "Biobío") ~ "Valparaíso y Biobío",
      REGION_PERSONA == "Sin información" ~ "Sin información",
      !is.na(REGION_PERSONA) ~ "Otra región",
      TRUE ~ "Sin información"),
    REGIONES_3 = factor(REGIONES_3, levels = c("Metropolitana", "Valparaíso y Biobío", 
                                               "Otra región")),
    DEPENDENCIA = factor(DEPENDENCIA, levels = c("Municipal", "Particular Pagado", "Particular Subvencionado")))

postulantes_filtrado <- postulantes_filtrado %>% na.omit(postulantes_filtrado)

mapeo <- read_excel("input/UNIVERSIDADES_COMPLETO.xlsx") %>%
  mutate(universidad_pregrado = str_squish(universidad_pregrado)) %>%
  group_by(universidad_pregrado) %>%
  summarise(
    UNIVERSIDAD_PREGRADO_NORM = first(UNIVERSIDAD_PREGRADO_NORM[!is.na(UNIVERSIDAD_PREGRADO_NORM)]),
    ACREDITACION_PREGRADO = first(ACREDITACION_PREGRADO[!is.na(ACREDITACION_PREGRADO)]),
    .groups = "drop")

# Aplicar el mapeo

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(UNIVERSIDAD_PREGRADO = str_squish(UNIVERSIDAD_PREGRADO)) %>%
  dplyr::select(-any_of(c("UNIVERSIDAD_PREGRADO_NORM", "ACREDITACION_PREGRADO"))) %>%
  left_join(mapeo, by = c("UNIVERSIDAD_PREGRADO" = "universidad_pregrado"))

# Convertir a factor ordenado

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    ACREDITACION_PREGRADO = factor(
      ACREDITACION_PREGRADO,
      levels = c("S/I", "Baja acreditacion o no acreditada", 
                 "Acreditacion 4", "Acreditacion 5", 
                 "Excelencia", "extranjero")))

# Verificación

cat(sprintf("Registros: %d | Con acreditación: %d (%.1f%%) | Sin acreditación: %d (%.1f%%)\n",
            nrow(postulantes_filtrado),
            sum(!is.na(postulantes_filtrado$ACREDITACION_PREGRADO)),
            100 * mean(!is.na(postulantes_filtrado$ACREDITACION_PREGRADO)),
            sum(is.na(postulantes_filtrado$ACREDITACION_PREGRADO)),
            100 * mean(is.na(postulantes_filtrado$ACREDITACION_PREGRADO))))

cat("\nDistribución de ACREDITACION_PREGRADO:\n")
print(table(postulantes_filtrado$ACREDITACION_PREGRADO, useNA = "ifany"))

# crear dependiencia recod 

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    DEPENDENCIA_recod = case_when(
      DEPENDENCIA %in% c("Municipal", "Particular Subvencionado") ~ "Municipal y Subvencionado",
      DEPENDENCIA == "Particular Pagado" ~ "Particular Pagado",
      TRUE ~ NA_character_),
    DEPENDENCIA_recod = factor(DEPENDENCIA_recod, 
                               levels = c("Municipal y Subvencionado", "Particular Pagado")))

postulantes_filtrado <- postulantes_filtrado %>%
  filter(!UNIVERSIDAD_PREGRADO_NORM %in% c("S/I", "Universidad Extranjera"))


postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    PREGRADO_ELITE = factor(
      ifelse(ACREDITACION_PREGRADO == "Excelencia", "Si", "No"),
      levels = c("No", "Si")),
    UC_UCH = factor(
      ifelse(UNIVERSIDAD_PREGRADO_NORM %in% c("UCHILE", "PUC"), "Si", "No"),
      levels = c("No", "Si")))

# Verificación rápida

table(postulantes_filtrado$PREGRADO_ELITE)
table(postulantes_filtrado$UC_UCH)

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    ETNIA = factor(
      ifelse(ETNIA == 1, "Si", "No"),
      levels = c("No", "Si")))

# transformar is selected

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
    SELECCIONADO = factor(
      ifelse(SELECCIONADO == 1, "Seleccionado", "No seleccionado"),
      levels = c("No seleccionado","Seleccionado")))

# Verificación rápida
table(postulantes_filtrado$ETNIA, useNA = "ifany")

postulantes_filtrado <- postulantes_filtrado %>%
  mutate(
        GENERO = fct_relevel(GENERO, "Masculino", "Femenino"))

# Verificación

table(postulantes_filtrado$GENERO)

saveRDS(postulantes_filtrado, file = "output/postulantes_filtrado.rds")
