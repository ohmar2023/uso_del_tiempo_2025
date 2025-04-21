
# Diseño de la Encuesta
d1 <- base %>%
  as_survey_design(
    ids = id_upm,
    #strata = estrato,
    weights = fexp,
    nest = T
  )

options(survey.lonely.psu = "certainty")



DOMTOTAL_nacional <- d1 %>%  
  filter( ocupado == 1 & p03>=12 & !is.na(domtotal) &  
            tresul == 2) %>%
  group_by(dom) %>% 
  summarise(tiempo_medio_var_2 = survey_sd(domtotal,deff= T, 
                                 vartype = c("se", "ci", "cv", "var"),
                                 na.rm = TRUE))
