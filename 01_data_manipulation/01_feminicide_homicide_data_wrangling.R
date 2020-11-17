# DATA SCIENCE CLASS ----
# DATA WRANGLING OVERVIEW ----

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)

# Feminicide and Homicide dataset:

feminicide_homicide_tbl <- read_excel("00_data/feminicidios_homicidios.xlsx")


feminicide_homicide_tbl %>% 
    
    # Setting names of the columns
    set_names("Provincias",
              "Feminicidios",
              "Homicidios",
              "Circunstancias",
              "Mes",
              "Año") %>% 
    
    # Mutate columns
    mutate(
        Provincias     = Provincias %>% str_to_lower() %>% str_to_title(), 
        Casos_Totales  = Feminicidios + Homicidios,
        Circunstancias = Circunstancias %>% str_to_lower() %>% str_to_title(),
        Mes            = Mes %>% str_to_lower() %>% str_to_title(),
        Año            = as.character(Año)
    )  %>%
    
    # Formatting 'Mes' as numbers:
    mutate(
        Mes_num = case_when(
            Mes %>% str_detect("Enero")      ~ 01,
            Mes %>% str_detect("Febrero")    ~ 02,
            Mes %>% str_detect("Marzo")      ~ 03,
            Mes %>% str_detect("Abril")      ~ 04,
            Mes %>% str_detect("Mayo")       ~ 05,
            Mes %>% str_detect("Junio")      ~ 06,
            Mes %>% str_detect("Julio")      ~ 07,
            Mes %>% str_detect("Agosto")     ~ 08,
            Mes %>% str_detect("Septiembre") ~ 09,
            Mes %>% str_detect("Octubre")    ~ 10,
            Mes %>% str_detect("Noviembre")  ~ 11,
            Mes %>% str_detect("Diciembre")  ~ 12,
        )
    ) 

