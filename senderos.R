
pacman::p_load(dplyr,ggplot2,tidyverse,tibble, summarytools, scales, purrr, scales, gt, sessioninfo, sjPlot,
               stringr, piecewiseSEM, nnet, lm.beta, modelsummary, lavaan, ordinal)

df_filtrado <- read_rds("output/df_filtrado.rds")

df_senderos <- df_filtrado %>% dplyr::select(FOLIO, 
                                             DISCIPLINA_DOCTORADO, 
                                             PAIS_POSGRADO, CHILEDOCTORADO,
                                             SEXO_REGISTRADO,REGIONES_3,
                                             GSE_ESTABLECIMIENTO, DISCIPLINA_DOCTORADO,
                                             PREGRADO_ELITE,ANIOS_ACREDITACION_PREGRADO,
                                             ACREDITACION_PREGRADO, SCOPUS_COUNT_ANTES,
                                             SCOPUS_COUNT_DESPUES, SCOPUS_COUNT_DURANTE,
                                             Average, tipuban, z_scopus_despues,
                                             SJR_AVG_JCPD, SJR_N_Q1, SJR_N_Q2,
                                             SJR_N_Q3, SJR_N_Q4, SJR_AVG_Q,
                                             SCOPUS_COAUTHOR_COUNT, SCOPUS_FIRST_AUTHOR_PUBLICATIONS)



saveRDS(df_senderos, "output/df_senderos.rds")


df_senderos <- df_senderos %>%
  mutate(
    SJR_AVG_Q = replace_na(SJR_AVG_Q, 0)
  ) %>%
  group_by(DISCIPLINA_DOCTORADO) %>%
  mutate(
    SJR_AVG_Q_z = scale(SJR_AVG_Q)[,1]
  ) %>%
  ungroup()


df_senderos %>%
  group_by(DISCIPLINA_DOCTORADO) %>%
  summarise(
    n_total = n(),
    n_cero = sum(SJR_AVG_Q == 0),
    media_z = mean(SJR_AVG_Q_z),
    sd_z = sd(SJR_AVG_Q_z),
    min_z = min(SJR_AVG_Q_z),
    max_z = max(SJR_AVG_Q_z))


df_senderos <- df_senderos %>%
  mutate(
    ACREDITACION_PREGRADO = factor(ACREDITACION_PREGRADO, 
                                   levels = c("Baja acreditacion o no acreditada",
                                              "Acreditacion 4",
                                              "Acreditacion 5",
                                              "Excelencia",
                                              "Investigacion"),
                                   ordered = FALSE),
  
    GSE_ESTABLECIMIENTO = factor(GSE_ESTABLECIMIENTO,
                                 levels = c("Bajo",
                                            "Medio",
                                            "Medio alto",
                                            "Alto"),
                                 ordered = FALSE))

df_senderos <- df_senderos %>%
  mutate(GSE_recod = factor(
    case_when(
      GSE_ESTABLECIMIENTO == "Alto"                    ~ "Alto",
      GSE_ESTABLECIMIENTO %in% c("Medio alto", "Medio", "Bajo") ~ "Medio y bajo"),
    levels = c("Alto", "Medio y bajo")))

df_senderos <- df_senderos %>% 
  mutate(ANIOS_ACREDITACION_recod = as.numeric(case_when(
    ANIOS_ACREDITACION_PREGRADO == "no acreditada"         ~ "0",
    ANIOS_ACREDITACION_PREGRADO == "instituto profesional" ~ "1",
    .default = ANIOS_ACREDITACION_PREGRADO
  )))

df_senderos_ciencias_agricolas  <- df_senderos %>% filter(DISCIPLINA_DOCTORADO == "Ciencias Agricolas")
df_senderos_ciencias_medicas    <- df_senderos %>% filter(DISCIPLINA_DOCTORADO == "Ciencias Médicas")
df_senderos_ciencias_naturales  <- df_senderos %>% filter(DISCIPLINA_DOCTORADO == "Ciencias Naturales")
df_senderos_ciencias_sociales   <- df_senderos %>% filter(DISCIPLINA_DOCTORADO == "Ciencias Sociales")
df_senderos_humanidades         <- df_senderos %>% filter(DISCIPLINA_DOCTORADO == "Humanidades")
df_senderos_ingenieria          <- df_senderos %>% filter(DISCIPLINA_DOCTORADO == "Ingeniería")


m1 <- lm(ACREDITACION_PREGRADO ~ GSE_recod +REGIONES_3+SEXO_REGISTRADO,
         data=df_senderos_ingenieria)

m2 <- glm(tipuban ~ ACREDITACION_PREGRADO + GSE_recod + REGIONES_3 + SEXO_REGISTRADO,
          data = df_senderos_ingenieria, family=binomial)

m3 <- lm(Average ~ ACREDITACION_PREGRADO + GSE_recod + REGIONES_3 +
           SEXO_REGISTRADO + tipuban + CHILEDOCTORADO ,data=df_senderos_ingenieria)

m4 <- lm(SCOPUS_COUNT_DESPUES ~ ACREDITACION_PREGRADO + GSE_recod + REGIONES_3 +
           SEXO_REGISTRADO + tipuban + CHILEDOCTORADO + Average  ,data=df_senderos_ingenieria)


modelos <- list(
  "Acreditación" = m1,
  "Publicaciones antes" = m2,
  "Prestigio doctorado" = m3,
  "Productividad científica" = m4)

modelsummary(modelos,
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic"))


modelsummary(modelos,
             output = "output/tabla_senderos_ingeniería.html",
             stars = TRUE,
             title = "análsis de senderos productividad científica ingeniería")




