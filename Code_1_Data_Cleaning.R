# -------------------------------------------- 
# CODE_1_Data_Cleaning 
# --------------------------------------------
# Author: Sam Huhnke
# 
# Explanation on code

# --------------------------------------------
# 0) Set working directory (if needed)
# --------------------------------------------

setwd("C:/Users/samhu/Desktop/Code Projects/MxForest")


# --------------------------------------------
# 1) Load necessary packages
# --------------------------------------------

library(data.table) # for fread()
library(readxl)     # for read_xlsx()
library(here)       # for here()
library(tidyverse)  # for data cleaning


# --------------------------------------------
# 2) Load data
#
# NOTE: The 'Arbolado' datasets comprise the Mexican tree inventory data. 
#
# --------------------------------------------


## 2004 - 2007 -> changing "NULL" character values to NA
df_1_raw <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"), na.strings = "NULL")

## 2009 - 2014 -> changing "NULL" character values to NA
df_2_raw <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"), na.strings = "NULL")

## 2015 - 2020
df_3_raw <- fread(here("data", "arbolado", "INFyS_Arbolado_2015_2020.csv"), na.strings = c("NULL", "999991", "999993"))


# -------------------------------------------
# 3) Data Cleaning
#
# NOTE: Each step will be explained in the code. For each dataset, the cleaning happens as one continuos
# NOTE: command. However, the respective step is indicated in the code. 
#
# -------------------------------------------


## df_1 

df_1 <- df_1_raw |>  
  # 1) Normalize names - NOTE: This is done to enable a later merger of the datasets and for convenience
  rename(cgl_sit_reg = cgl_sit_arb,
         CveVeg_S5 = Cve_veg_SV,
         TipoVeg_S5 = Tipo_veg_SV,
         NombreComun = NomComun,
         FormaBiologica = forma_biologica,
         Distancia = distancia,
         AlturaTotal = Altura_total,
         DiametroNormal = Diametro_normal,
         AreaBasal = Area_basal,
         AreaCopa = Area_copa,
         ExposicionCopa = ExposicionLuz,
         VigorEtapa = VigEtapa,
         LongitudAnillos10 = Long10Anillos,
         NumeroAnillos25 = NumAnillos25
  ) |> 
  # Correction of categoric and specific entry mistakes
  mutate(
    # "Estado" Correction - initially 33 -> 32
    Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                       TRUE ~ Estado),
    # "Registro" Correction - initially all values NA -> pull data from string cgl_sit_reg
    Registro = str_extract(cgl_sit_reg, "(?<=_)[^_]+$") |>
      as.integer(),
    # "CveVeg_S5" Correction - replacing "VSaa" with "VSa" to fit Arb.14
    CveVeg_S5 = str_replace(CveVeg_S5, "VSaa", "VSa"),
    # "TipoVeg_S5" Correction - changing names based on CveVeg_S5 data to fit Arb.14
    TipoVeg_S5 = case_when(
      str_detect(CveVeg_S5, "VSA") ~ paste("VEGETACION SECUNDARIA ARBOREA DE ", TipoVeg_S5, sep = ""),
      str_detect(CveVeg_S5, "VSa") ~ paste("VEGETACION SECUNDARIA ARBUSTIVA DE ", TipoVeg_S5, sep = ""),
      str_detect(CveVeg_S5, "VSh") ~ paste("VEGETACION SECUNDARIA HERBACEA DE ", TipoVeg_S5, sep = ""),
      str_detect(CveVeg_S5, "RAP") ~ paste(TipoVeg_S5, " ANUAL Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RAS") ~ paste(TipoVeg_S5, " ANUAL Y SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RA") ~ paste(TipoVeg_S5, " ANUAL", sep = ""),
      str_detect(CveVeg_S5, "RP") ~ paste(TipoVeg_S5, " PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RS") ~ paste(TipoVeg_S5, " SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TAP") ~ paste(TipoVeg_S5, " ANUAL Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TAS") ~ paste(TipoVeg_S5, " ANUAL Y SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TSP") ~ paste(TipoVeg_S5, " SEMIPERMANENTE Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TA") ~ paste(TipoVeg_S5, " ANUAL", sep = ""),
      str_detect(CveVeg_S5, "TP") ~ paste(TipoVeg_S5, " PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TS") ~ paste(TipoVeg_S5, " SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "HA") ~ paste(TipoVeg_S5, " ANUAL", sep = ""),
      TRUE ~ TipoVeg_S5),
    # "VigorEtapa" Correction - NA + class names -> "no capturado" + new names (in line with Arb.14)
    VigorEtapa = case_when(VigorEtapa == "Arbol joven" ~ "Árbol joven",
                           VigorEtapa == "Arbol maduro" ~ "Árbol maduro",
                           VigorEtapa == "Arbol muy joven" ~ "Árbol muy joven",
                           VigorEtapa == "Arbol viejo o supermaduro" ~ "Árbol viejo o súper-maduro",
                           is.na(VigorEtapa) ~ "No capturado",
                           TRUE ~ VigorEtapa),
    # "Edad" Correction - "NULL" + class "character" -> NA + class "numeric" (+ rounding numbers)
    Edad = ifelse(!is.na(Edad),
                  ifelse(as.numeric(Edad) - floor(as.numeric(Edad)) >= 0.5,
                         ceiling(as.numeric(Edad)),
                         floor(as.numeric(Edad))),
                  NA),
    # "Condicion" Correction - values names -> new value names
    Condicion = case_when(Condicion == "Muerto en pie" ~ "Arbol muerto en pie",
                          Condicion == "Vivo" ~ "Arbol vivo",
                          TRUE ~ Condicion),
    # Added Cycle number 
    Cycle = "1"
  ) |>
  # setting initial column order +
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
  ) |>
  # sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)


## df_2: 
