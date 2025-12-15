
pacman::p_load(dplyr,ggplot2,tidyverse,tibble, scales, purrr, scales, gt, sessioninfo, sjPlot,
               stringr)

df_filtrado <- read_rds("input/df_filtrado.rds")

df_senderos <- df_filtrado %>% dplyr::select(FOLIO, DISCIPLINA_DOCTORADO, ES_CHILE, SEXO_REGISTRADO, Average, 
                                             GSE_ESTABLECIMIENTO, REGIONES_3, SCOPUS_COUNT, SCOPUS_COUNT_ANTES_x)

df_senderos <- df_senderos %>%
  group_by(DISCIPLINA_DOCTORADO) %>%
  mutate(
    z_scopus = (SCOPUS_COUNT - mean(SCOPUS_COUNT, na.rm = TRUE)) / 
      sd(SCOPUS_COUNT, na.rm = TRUE)) %>% ungroup()



