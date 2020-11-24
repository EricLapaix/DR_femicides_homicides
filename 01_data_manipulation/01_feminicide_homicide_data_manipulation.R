# DATA SCIENCE CLASS ----
# DATA WRANGLING OVERVIEW ----

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(writexl)

# Feminicide and Homicide dataset:

feminicide_homicide_tbl <- read_excel("00_data/feminicidios_homicidios.xlsx")

# Data wrangling ----

feminicide_homicide_wrangled_tbl <- feminicide_homicide_tbl %>%
    
    # Setting names of the columns
    set_names("Provincia",
              "Feminicidio",
              "Homicidio",
              "Circunstancias",
              "Mes",
              "Año") %>%
    
    # Data cleansing
    mutate(
        Provincia      = Provincia %>% str_to_lower() %>% str_to_title(),
        Circunstancias = Circunstancias %>% str_to_lower() %>% str_to_title(),
        Mes_num        = Mes %>% str_to_lower() %>% str_to_title(),
        Mes            = Mes %>% str_to_lower() %>% str_to_title(),
        Año            = as.character(Año),
        Casos_Totales  = Feminicidio + Homicidio
    ) %>%
    
    mutate(Mes_num = case_when(
        Mes_num %>% str_detect("Enero")      ~ 01,
        Mes_num %>% str_detect("Febrero")    ~ 02,
        Mes_num %>% str_detect("Marzo")      ~ 03,
        Mes_num %>% str_detect("Abril")      ~ 04,
        Mes_num %>% str_detect("Mayo")       ~ 05,
        Mes_num %>% str_detect("Junio")      ~ 06,
        Mes_num %>% str_detect("Julio")      ~ 07,
        Mes_num %>% str_detect("Agosto")     ~ 08,
        Mes_num %>% str_detect("Septiembre") ~ 09,
        Mes_num %>% str_detect("Octubre")    ~ 10,
        Mes_num %>% str_detect("Noviembre")  ~ 11,
        Mes_num %>% str_detect("Diciembre")  ~ 12
    )) %>%
    
    mutate(
        Mes_txt =  str_c(Año, "-", Mes_num, "-", 01) %>% ymd() %>% floor_date("months") %>% ymd(),
        Año = year(Mes_txt)
    ) %>% 
    
    mutate(Provincia = case_when(
        Provincia %>% str_detect("Dajabon")                ~ "Dajabón",
        Provincia %>% str_detect("Maria Trinidad Sanchez") ~ "María Trinidad Sánchez",
        Provincia %>% str_detect("San Cristobal")          ~ "San Cristóbal",
        Provincia %>% str_detect("Santiago Rodriguez")     ~ "Santiago Rodríguez",
        Provincia %>% str_detect("San Juan")               ~ "San Juan de la Maguana",
        Provincia %>% str_detect("San Pedro")              ~ "San Pedro de Macorís",
        Provincia %>% str_detect("San Jose De Ocoa")       ~ "San José de Ocoa",
        Provincia %>% str_detect("Santiago")               ~ "Santiago de los Caballeros",
        TRUE ~ Provincia
        )
    ) %>% 
    
    mutate(
        Circunstancias_Generales = case_when(
            
            # Combining "Robo" Situations:
            Circunstancias %>% str_detect("Rob")                  ~ "Robo / Atraco",
            Circunstancias %>% str_detect("Despojo")              ~ "Robo / Atraco",
            Circunstancias %>% str_detect("Intento De")           ~ "Robo / Atraco",
            Circunstancias %>% str_detect("Asalto")               ~ "Robo / Atraco",
            
            
            # Riña:
            Circunstancias %>% str_detect("Riña")                 ~ "Riña",
            
            # Sexual situations:
            Circunstancias %>% str_detect("Feminicidio Intimo")   ~ "Feminicidio Íntimo",
            Circunstancias %>% str_detect("Violacion Sexual")     ~ "Violación Sexual",
            
            #
            Circunstancias %>% str_detect("Droga")                ~ "Drogas",
            Circunstancias %>% str_detect("Sicariato")            ~ "Sicariato / Drogas / Persecución Policial",
            Circunstancias %>% str_detect("Persecusion Policial") ~ "Sicariato / Drogas / Persecución Policial",
            
            # Others:
            Circunstancias %>% str_detect("Accion Legal")         ~ "Acción Legal / Conexión / Bala Perdida",
            Circunstancias %>% str_detect("Conexión")             ~ "Acción Legal / Conexión / Bala Perdida",
            Circunstancias %>% str_detect("Bala Perdida")         ~ "Acción Legal / Conexión / Bala Perdida",
            Circunstancias %>% str_detect("Desconocido")          ~ "Situación Desconocida",
            Circunstancias %>% str_detect("Desconocida")          ~ "Situación Desconocida",
            TRUE ~ Circunstancias
        )
    ) %>%  mutate(
        # Fixing up the mutating of 'Drogas':
        Circunstancias_Generales = case_when(
            
            Circunstancias_Generales %>% str_detect("Drogas") ~ "Sicariato / Drogas / Persecución Policial",
            TRUE ~ Circunstancias_Generales)
        
        )

# Data objects ----

feminicide_homicide_pivot_tbl <- feminicide_homicide_wrangled_tbl %>% 
    
    group_by(Provincia, Circunstancias_Generales) %>% 
    summarise(total = sum(Casos_Totales)) %>% 
    ungroup() %>% 
    
    pivot_wider(names_from = Circunstancias_Generales, values_from = total, values_fill = 0)

# Saving dataframes to RDS/CSV ----

feminicide_homicide_wrangled_tbl %>%
    write_rds("00_data/data_wrangled_feminicide_homicide/feminicide_homicide_wrangled.rds")

feminicide_homicide_pivot_tbl %>% 
    write_xlsx("00_data/data_wrangled_feminicide_homicide/feminicide_homicide_wrangled.xlsx")


# Notes ----

# Previous Pivot Table with spread()

# feminicide_homicide_wrangled_tbl %>% 
#     
#     group_by(Provincia, Circunstancias_Generales) %>% 
#     summarise(total = sum(Casos_Totales)) %>% 
#     ungroup() %>% 
#     
#     spread(key = Circunstancias_Generales, value = total, fill = 0)



