feminicide_homicide_total_cases_tbl <- feminicide_homicide_wrangled_tbl %>% 
    select(Provincia, Casos_Totales) %>% 
    
    group_by(Provincia) %>% 
    summarize(total = sum(Casos_Totales)) %>%
    ungroup() %>% 
    
    arrange(desc(total)) %>% 
    
    mutate(Provincia = Provincia %>% as_factor() %>% fct_rev())


feminicide_homicide_total_cases_tbl %>% 
    
    ggplot(aes(x = Provincia, y = total)) +
    geom_col(fill = "#2c3e50") +
    geom_smooth(method = 'lm', se = FALSE) +
    coord_flip() +
    theme_tq() +
    expand_limits(y = 120) +
    labs(
        title = "Feminicidios y Homicidios de Mujeres",
        subtitle = "Por Provincias",
        x = "",
        y = "Provincia"
    )


feminicide_homicide_year_prov_tbl <- feminicide_homicide_wrangled_tbl %>% 
    
    select(Mes_txt, Casos_Totales, Provincia) %>%
    mutate(year  = year(Mes_txt),
           month = month(Mes_txt, label = TRUE, abbr = TRUE)) %>% 
    
    group_by(month, year) %>% 
    summarize(total = sum(Casos_Totales)) %>% 
    ungroup()


feminicide_homicide_year_prov_tbl %>% 
    
    ggplot(aes(x = month, y = total, fill = year)) +
    
    geom_col() +
    facet_wrap(~ year, ncol = 2, nrow = 2, scales = "free_y") +
    theme_tq() +
    labs(
        title = "Feminicidios y Homicidios de Mujeres",
        subtitle = "Por Años",
        x = "",
        y = "Casos"
    )