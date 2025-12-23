
pacman::p_load(dplyr,ggplot2,tidyverse,tibble, scales, purrr, scales, gt, sessioninfo, sjPlot,
               stringr)

df_filtrado <- read_rds("input/df_filtrado.rds")

df_senderos <- df_filtrado %>% dplyr::select(FOLIO, 
                                             DISCIPLINA_DOCTORADO, 
                                             PAIS_POSGRADO, CHILEDOCTORADO,
                                             SEXO_REGISTRADO,REGIONES_3,
                                             GSE_ESTABLECIMIENTO, 
                                             PREGRADO_ELITE,ANIOS_ACREDITACION_PREGRADO,
                                             ACREDITACION_PREGRADO, SCOPUS_COUNT_ANTES,
                                             SCOPUS_COUNT_DESPUES, SCOPUS_COUNT_DURANTE,
                                             Average, tipuban, z_scopus_despues,
                                             SJR_AVG_JCPD, SJR_N_Q1, SJR_N_Q2,
                                             SJR_N_Q3, SJR_N_Q4, SJR_AVG_Q)



saveRDS(df_senderos, "output/df_senderos.rds")





