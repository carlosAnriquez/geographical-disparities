
pacman::p_load(dplyr,ggplot2,tidyverse,tibble, scales, purrr, scales, gt, sessioninfo, sjPlot,
               stringr)

df_filtrado <- read_rds("input/df_filtrado.rds")

df_senderos <- df_filtrado %>% dplyr::select(FOLIO, DISCIPLINA_DOCTORADO, ES_CHILE, SEXO_REGISTRADO, Average, 
                                             GSE_ESTABLECIMIENTO, REGIONES_3, SCOPUS_COUNT, SCOPUS_COUNT_DESPUES_x, 
                                             SCOPUS_COUNT_ANTES_x)

df_senderos <- df_senderos %>%
  group_by(DISCIPLINA_DOCTORADO) %>%
  mutate(
    z_scopus_despues = (SCOPUS_COUNT_DESPUES_x - mean(SCOPUS_COUNT_DESPUES_x, na.rm = TRUE)) / 
      sd(SCOPUS_COUNT_DESPUES_x, na.rm = TRUE)) %>% ungroup()


saveRDS(df_senderos, "output/df_senderos.rds")





