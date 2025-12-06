# ============================================ 
# CODE_1_Data_Cleaning 
# ============================================
# Author: Sam Huhnke
# 
# Explanation on code
#
#
#
#
# ============================================
# 0) Set working directory (if needed)
# ============================================

setwd("C:/Users/samhu/Desktop/Code Projects/MxForest")



# ============================================
# 1) Load necessary packages
# ============================================

library(data.table) # for fread()
library(readxl)     # for read_xlsx()
library(here)       # for here()
library(tidyverse)  # for data cleaning



# ============================================
# 2) Load data
#
# NOTE: The 'Arbolado' datasets comprise the Mexican tree inventory data. 
#
# ============================================

## 2004 - 2007 -> changing "NULL" character values to NA
df_1_raw <- fread(here("data", "Tree_Inventory_Raw", "INFyS_Arbolado_2004_2007.csv"), na.strings = "NULL")

## 2009 - 2014 -> changing "NULL" character values to NA
df_2_raw <- fread(here("data", "Tree_Inventory_Raw", "INFyS_Arbolado_2009_2014.csv"), na.strings = "NULL")

## 2015 - 2020
df_3_raw <- fread(here("data", "Tree_Inventory_Raw", "INFyS_Arbolado_2015_2020.csv"), na.strings = c("NULL", "999991", "999993"))



# ============================================
# 3) Data Cleaning
#
# NOTE: Each step will be explained in the code. For each dataset, the cleaning happens as one continuos
# NOTE: command. However, the respective step is indicated in the code. 
#
# ============================================

# WIP: case_when(Column == 'current name' ~ 'new name')

## df_1 --------------------------------

df_1 <- df_1_raw |> 
  # 1) Normalize names - NOTE: This is done to enable a later merger of the datasets and for convenience
  # WIP: Re-assign all the names to sth english and easily understandable + create change matrix 
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
  # 2) Correction of categorical and specific entry mistakes
  mutate(
    # 2.1) "Estado" Correction: Both 'Distrito Federal' and 'Ciudad de Mexico' describe the same state
    Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                       TRUE ~ Estado),
    # 2.2) "Registro" Correction: initially all values are NA. This pulls the corresponding data from the string of column 'cgl_sit_reg' 
    Registro = str_extract(cgl_sit_reg, "(?<=_)[^_]+$") |>
      as.integer(),
    # 2.3) "CveVeg_S5" Correction: Harmonizing categorical names across all datasets
    CveVeg_S5 = str_replace(CveVeg_S5, "VSaa", "VSa"),
    # 2.4) "TipoVeg_S5" Correction: Harmonizing categorical names across all datasets
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
    # 2.5) "VigorEtapa" Correction: Harmonizing categorical names across all datasets (NAs were turned into 'No capturado')
    VigorEtapa = case_when(VigorEtapa == "Arbol joven" ~ "Árbol joven",
                           VigorEtapa == "Arbol maduro" ~ "Árbol maduro",
                           VigorEtapa == "Arbol muy joven" ~ "Árbol muy joven",
                           VigorEtapa == "Arbol viejo o supermaduro" ~ "Árbol viejo o súper-maduro",
                           is.na(VigorEtapa) ~ "No capturado",
                           TRUE ~ VigorEtapa),
    # 2.6) "Edad" Correction: Define NA values and round numbers 
    # WIP: Look into what exactly I'm changing here... 
    # "NULL" + class "character" -> NA + class "numeric" (+ rounding numbers)
    Edad = ifelse(!is.na(Edad),
                  ifelse(as.numeric(Edad) - floor(as.numeric(Edad)) >= 0.5,
                         ceiling(as.numeric(Edad)),
                         floor(as.numeric(Edad))),
                  NA),
    # 2.7) "Condicion" Correction: Harmonizing categorical names
    Condicion = case_when(Condicion == "Muerto en pie" ~ "Arbol muerto en pie",
                          Condicion == "Vivo" ~ "Arbol vivo",
                          TRUE ~ Condicion),
    # 2.8) Add sample cycle number, so the datasets can later be merged
    Cycle = "1"
  ) |>
  # 3) setting initial column order
  # NOTE: This is purely done for later convenience of working with the data
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
  ) |>
  # 4) sorting for comparison
  # NOTE: This is purely done for later convenience of working with the data
  arrange(Estado, Conglomerado, Sitio, Registro)


## df_2 --------------------------------

df_2 <- df_2_raw |> 
  # 1) Normalize names - NOTE: This is done to enable a later merger of the datasets and for convenience
  # WIP: Re-assign all the names to sth english and easily understandable + create change matrix
  rename(cgl_sit_reg = cgl_sit_arb,
         CveVeg_S5 = Cve_veg_SV,
         TipoVeg_S5 = Tipo_veg_SV,
         NombreComun = NomComun,
         FormaBiologica = Forma_Biologica_1,
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
  # 2) Correction of categoric and specific entry mistakes 
  mutate(
    # 2.1) "Estado" Correction: Both 'Distrito Federal' and 'Ciudad de Mexico' are the same state. The others just correct typos that led to more duplicate states 
    Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                       Estado == "Mexico" ~ "México",
                       Estado == "Michoacan de Ocampo" ~ "Michoacán de Ocampo",
                       Estado == "Nuevo Leon" ~ "Nuevo León",
                       Estado == "Queretaro de Arteaga" ~ "Querétaro",
                       Estado == "San Luis Potosi" ~ "San Luis Potosí",
                       Estado == "Yucatan" ~ "Yucatán",
                       TRUE ~ Estado),
    # 2.2) "Registro" Correction: replaced 2 supspicious entries with NAs
    Registro = case_when(Registro == 316 ~ NA,
                         TRUE ~ Registro),
    Registro = ifelse(cgl_sit_reg == "21314_4_147", NA, Registro),
    # 2.3) "CveVeg_S5" Correction: Harmonizing categorical names across all datasets
    CveVeg_S5 = str_replace(CveVeg_S5, "VSaa", "VSa"),
    # 2.4) "TipoVeg_S5" Correction: Harmonizing categorical names across all datasets
    TipoVeg_S5 = case_when(
      str_detect(CveVeg_S5, "VSA") ~ paste("VEGETACION SECUNDARIA ARBOREA DE ", TipoVeg_S5, sep = ""),
      str_detect(CveVeg_S5, "VSa") ~ paste("VEGETACION SECUNDARIA ARBUSTIVA DE ", TipoVeg_S5, sep = ""),
      str_detect(CveVeg_S5, "VSh") ~ paste("VEGETACION SECUNDARIA HERBACEA DE ", TipoVeg_S5, sep = ""),
      str_detect(CveVeg_S5, "RAP") ~ paste(TipoVeg_S5, " ANUAL Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RAS") ~ paste(TipoVeg_S5, " ANUAL Y SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RSP") ~ paste(TipoVeg_S5, " SEMIPERMANENTE Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RA") ~ paste(TipoVeg_S5, " ANUAL", sep = ""),
      str_detect(CveVeg_S5, "RP") ~ paste(TipoVeg_S5, " PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "RS") ~ paste(TipoVeg_S5, " SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TAP") ~ paste(TipoVeg_S5, " ANUAL Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TAS") ~ paste(TipoVeg_S5, " ANUAL Y SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TSP") ~ paste(TipoVeg_S5, " SEMIPERMANENTE Y PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TA") ~ paste(TipoVeg_S5, " ANUAL", sep = ""),
      str_detect(CveVeg_S5, "TP") ~ paste(TipoVeg_S5, " PERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "TS") ~ paste(TipoVeg_S5, " SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "HAS") ~ paste(TipoVeg_S5, " ANUAL Y SEMIPERMANENTE", sep = ""),
      str_detect(CveVeg_S5, "HA") ~ paste(TipoVeg_S5, " ANUAL", sep = ""),
      TRUE ~ TipoVeg_S5), 
    # 2.5) "FormaFuste" Correction: Harmonizing categorical names across all datasets + turn categorical 'NA' into actual NA
    FormaFuste = str_replace(FormaFuste, "mas", "más"),
    FormaFuste = case_when(FormaFuste == "Arbol cruvo con dos o más fustes" ~ "Arbol curvo con dos o más fustes",
                           FormaFuste == "NA" ~ NA,
                           TRUE ~ FormaFuste),
    # 2.6) "TipoTocon" Correction: Harmonizing categorical names across all datasets
    TipoTocon = case_when(TipoTocon == "Tocon descompuesto (evidencia de tocon)" ~ "Tocón descompuesto (evidencia de tocón)",
                          TipoTocon == "Tocon madera seca (madera dura sin evidencias de descomposicion)" ~ "Tocón madera seca (madera dura sin evidencias de descomposición)",
                          TipoTocon == "Tocon madera seca (madera en proceso de descomposicion pero aun dificil de desprenderse del suelo)" ~ "Tocón madera seca (madera en proceso de descomposición pero aún difícil de desprenderse del suelo)",
                          TipoTocon == "Tocon madera verde (arbol recien cortado)" ~ "Tocón madera verde (árbol recién cortado)",
                          TipoTocon == "Tocon seco (madera muy descompuesta y de facil extraccion del sustrato)" ~ "Tocón seco (madera muy descompuesta y de fácil extracción del sustrato)",
                          TRUE ~ TipoTocon),
    # 2.7) "PosicionCopa" Correction: Turn categorical 'no aplica' into actual NA 
    PosicionCopa = case_when(PosicionCopa == "No aplica" ~ NA,
                             TRUE ~ PosicionCopa),
    # 2.8) "ExposicionCopa" Correction: Harmonizing categorical names across all datasets + turn categorical 'no aplica' into actual NA
    ExposicionCopa = str_replace(ExposicionCopa, "arbol", "árbol"),
    ExposicionCopa = str_replace(ExposicionCopa, "la luz", "luz"),
    ExposicionCopa = str_replace(ExposicionCopa, " solo en un cuarto", " en un solo cuarto"),
    ExposicionCopa = str_replace(ExposicionCopa, " y en un cuarto ", " y un cuarto "),
    ExposicionCopa = case_when(ExposicionCopa == "Arboles que no reciben luz porque se encuentran sombreados por otros árboles  parras  trepadoras u otra vegetacion  arboles que no tienen copa por definicion " ~ 
                                 "Árboles que no reciben luz porque están a la sombra de otra vegetación",
                               ExposicionCopa == "No aplica" ~ NA,
                               TRUE ~ ExposicionCopa),
    # 2.9) "DensidadCopa" Correction: Define NA value + assign value ranges instead of single values
    # NOTE: This is done as part of harmonizing categorical names across all datasets
    DensidadCopa = case_when(DensidadCopa == "" ~ NA,    # NA value
                             DensidadCopa == -9999 ~ NA, # NA value
                             DensidadCopa == "n/a" ~ NA, # NA value
                             DensidadCopa == "00" ~ "Sin parámetro",
                             DensidadCopa == "05" ~ "1 - 5", DensidadCopa == "10" ~ "6 - 10",
                             DensidadCopa == "15" ~ "11 - 15", DensidadCopa == "20" ~ "16 - 20",
                             DensidadCopa == "25" ~ "21 - 25", DensidadCopa == "30" ~ "26 - 30",
                             DensidadCopa == "35" ~ "31 - 35", DensidadCopa == "40" ~ "36 - 40",
                             DensidadCopa == "45" ~ "41 - 45", DensidadCopa == "50" ~ "46 - 50",
                             DensidadCopa == "55" ~ "51 - 55", DensidadCopa == "60" ~ "56 - 60",
                             DensidadCopa == "65" ~ "61 - 65", DensidadCopa == "70" ~ "66 - 70",
                             DensidadCopa == "75" ~ "71 - 75", DensidadCopa == "80" ~ "76 - 80",
                             DensidadCopa == "85" ~ "81 - 85", DensidadCopa == "90" ~ "86 - 90",
                             DensidadCopa == "95" ~ "91 - 95", DensidadCopa == "100" ~ "96 - 100",
                             TRUE ~ DensidadCopa),
    # 2.10) "TransparenciaCopa" Correction: Define NA value + assign value ranges instead of single values
    # NOTE: This is done as part of harmonizing categorical names across all datasets
    TransparenciaCopa = case_when(TransparenciaCopa == "" ~ NA,    # NA value
                                  TransparenciaCopa == -9999 ~ NA, # NA value
                                  TransparenciaCopa == "n/a" ~ NA, # NA value
                                  TransparenciaCopa == "00" ~ "Sin parámetro",
                                  TransparenciaCopa == "05" ~ "1 - 5", TransparenciaCopa == "10" ~ "6 - 10",
                                  TransparenciaCopa == "15" ~ "11 - 15", TransparenciaCopa == "20" ~ "16 - 20",
                                  TransparenciaCopa == "25" ~ "21 - 25", TransparenciaCopa == "30" ~ "26 - 30",
                                  TransparenciaCopa == "35" ~ "31 - 35", TransparenciaCopa == "40" ~ "36 - 40",
                                  TransparenciaCopa == "45" ~ "41 - 45", TransparenciaCopa == "50" ~ "46 - 50",
                                  TransparenciaCopa == "55" ~ "51 - 55", TransparenciaCopa == "60" ~ "56 - 60",
                                  TransparenciaCopa == "65" ~ "61 - 65", TransparenciaCopa == "70" ~ "66 - 70",
                                  TransparenciaCopa == "75" ~ "71 - 75", TransparenciaCopa == "80" ~ "76 - 80",
                                  TransparenciaCopa == "85" ~ "81 - 85", TransparenciaCopa == "90" ~ "86 - 90",
                                  TransparenciaCopa == "95" ~ "91 - 95", TransparenciaCopa == "100" ~ "96 - 100",
                                  TRUE ~ TransparenciaCopa),
    # 2.11) "MuerteRegressiva" Correction: Define NA value + assign value ranges instead of single values 
    # NOTE: This is done as part of harmonizing categorical names across all datasets
    MuerteRegresiva = case_when(MuerteRegresiva == "" ~ NA,    # NA value
                                MuerteRegresiva == -9999 ~ NA, # NA value
                                MuerteRegresiva == "n/a" ~ NA, # NA value
                                MuerteRegresiva == "00" ~ "Sin parámetro",
                                MuerteRegresiva == "05" ~ "1 - 5", MuerteRegresiva == "10" ~ "6 - 10",
                                MuerteRegresiva == "15" ~ "11 - 15", MuerteRegresiva == "20" ~ "16 - 20",
                                MuerteRegresiva == "25" ~ "21 - 25", MuerteRegresiva == "30" ~ "26 - 30",
                                MuerteRegresiva == "35" ~ "31 - 35", MuerteRegresiva == "40" ~ "36 - 40",
                                MuerteRegresiva == "45" ~ "41 - 45", MuerteRegresiva == "50" ~ "46 - 50",
                                MuerteRegresiva == "55" ~ "51 - 55", MuerteRegresiva == "60" ~ "56 - 60",
                                MuerteRegresiva == "65" ~ "61 - 65", MuerteRegresiva == "70" ~ "66 - 70",
                                MuerteRegresiva == "75" ~ "71 - 75", MuerteRegresiva == "80" ~ "76 - 80",
                                MuerteRegresiva == "85" ~ "81 - 85", MuerteRegresiva == "90" ~ "86 - 90",
                                MuerteRegresiva == "95" ~ "91 - 95", MuerteRegresiva == "100" ~ "96 - 100",
                                TRUE ~ MuerteRegresiva),
    # 2.12) "VigorEtapa" Correction: Harmonizing categorical names across all datasets 
    VigorEtapa = case_when(VigorEtapa == "Arbol joven" ~ "Árbol joven",
                           VigorEtapa == "Arbol maduro" ~ "Árbol maduro",
                           VigorEtapa == "Arbol muy joven" ~ "Árbol muy joven",
                           VigorEtapa == "Arbol viejo o supermaduro" ~ "Árbol viejo o súper-maduro",
                           is.na(VigorEtapa) ~ "No capturado",
                           TRUE ~ VigorEtapa),
    # 2.13) "Edad" Correction:
    # WIP: Same as df_1
    Edad = ifelse(!is.na(Edad),
                  ifelse(as.numeric(Edad) - floor(as.numeric(Edad)) >= 0.5,
                         ceiling(as.numeric(Edad)),
                         floor(as.numeric(Edad))
                  ),
                  NA),
    Edad = case_when(Edad == 999 ~ NA,
                     TRUE ~ Edad),
    # 2.14) "Condicion" Correction: Harmonizing categorical names across all datasets
    Condicion = case_when(Condicion == "Muerto en pie" ~ "Arbol muerto en pie",
                          Condicion == "Vivo" ~ "Arbol vivo",
                          TRUE ~ Condicion),
    # 2.15) "Danio1" Correction: Harmonizing categorical names across all datasets + Define NA value
    Danio1 = str_replace(Danio1, "abioticos", "abióticos"),
    Danio1 = str_replace(Danio1, "raiz/tocon", "raíz/tocón"),
    Danio1 = str_replace(Danio1, "pifitas", "pífitas"), 
    Danio1 = str_replace(Danio1, "parasitas", "parásitas"),
    Danio1 = str_replace(Danio1, "Sequia", "Sequía"),
    Danio1 = case_when(Danio1 == "Otros" ~ "No definido",
                       Danio1 == "Insectos" ~ "Insectos en general",
                       Danio1 == "No aplica" ~ NA,   # NA value
                       Danio1 == "No definido" ~ NA, # NA value
                       TRUE ~ Danio1),
    # 2.16) "Severidad1" Correction: Define NA value + correct entry class mistake from categorical to numeric
    Severidad1 = case_when(Severidad1 == "" ~ NA,    # NA value
                           Severidad1 == -9999 ~ NA, # NA value
                           Severidad1 == "n/a" ~ NA, # NA value
                           Severidad1 == "05" ~ 5,   # corrected class mistake
                           TRUE ~ as.numeric(Severidad1)),
    # 2.17) "Danio2" Correction: Harmonizing categorical names across all datasets + Define NA value
    Danio2 = str_replace(Danio2, "abioticos", "abióticos"),
    Danio2 = str_replace(Danio2, "raiz/tocon", "raíz/tocón"),
    Danio2 = str_replace(Danio2, "pifitas", "pífitas"), 
    Danio2 = str_replace(Danio2, "parasitas", "parásitas"),
    Danio2 = str_replace(Danio2, "Sequia", "Sequía"),
    Danio2 = case_when(Danio2 == "Otros" ~ "No definido",
                       Danio2 == "Insectos" ~ "Insectos en general",
                       Danio2 == "No aplica" ~ NA,
                       TRUE ~ Danio2),
    # 2.18) "Severidad2" Correction: Define NA value + correct entry class mistake from categorical to numeric
    Severidad2 = case_when(Severidad2 == "" ~ NA,    # NA value
                           Severidad2 == -9999 ~ NA, # NA value
                           Severidad2 == "n/a" ~ NA, # NA value
                           Severidad2 == "05" ~ 5,   # corrected class mistake
                           TRUE ~ as.numeric(Severidad2)),
    # 2.19) "NumeroTallos" Correction: Define NA value
    NumeroTallos = case_when(NumeroTallos == 999 ~ NA,  # NA value
                             NumeroTallos == 9999 ~ NA, # NA value
                             TRUE ~ NumeroTallos),
    # 2.20) "LongitudAnillos10" Correction: Define NA value
    LongitudAnillos10 = case_when(LongitudAnillos10 == 999 ~ NA, # NA value
                                  TRUE ~ LongitudAnillos10),
    # 2.21) "NumeroAnillos25" Correction: Define NA value
    NumeroAnillos25 = case_when(NumeroAnillos25 == 999 ~ NA, # NA value
                                TRUE ~ NumeroAnillos25),
    # 2.22) "NombreCientifico_APG" Correction: Harmonizing categorical names across all datasets
    NombreCientifico_APG = case_when(NombreCientifico_APG == "ZZ_Desconocido" ~ "ZZ Desconocido",
                                     TRUE ~ NombreCientifico_APG),
    # 2.23) Add sample cycle number, so the datasets can later be merged
    Cycle = "2"
  ) |> 
  # 3) setting initial column order
  # NOTE: This is purely done for later convenience of working with the data
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything() 
  ) |> 
  # 4) sorting for comparison
  # NOTE: This is purely done for later convenience of working with the data
  arrange(Estado, Conglomerado, Sitio, Registro) 


## df_3 --------------------------------

df_3 <- df_3_raw |> 
  # 1) Normalize names - NOTE: This is done to enable a later merger of the datasets and for convenience
  # WIP: Re-assign all the names to sth english and easily understandable + create change matrix
  rename(Anio = Anio_C3,
         Estado = Estado_C3,
         Conglomerado = IdConglomerado,
         Sitio = Sitio_C3,
         Registro = Consecutivo_C3,
         CveVeg_S7 = CVE_S7_C3,
         TipoVeg_S7 = DESCRIP_S7_C3,
         FormaFuste = FormaFuste_C3,
         TipoTocon = TipoTocon_C3,
         Familia_APG = Familia_APG_C3,
         NombreCientifico_APG = NombreCientifico_APG_C3,
         NombreComun = NombreComun_C3,
         FormaBiologica = Forma_Biologica_Cat_C3,
         Distancia = Distancia_C3,
         Azimut = Azimut_C3,
         AlturaTotal = AlturaTotal_C3,
         AlturaFusteLimpio = AlturaFusteLimpio_C3,
         AlturaComercial = AlturaComercial_C3,
         DiametroNormal = DiametroNormal_C3,
         DiametroBasal = DiametroBasalSub_validacion_C3,
         DiametroCopa = DiametroCopa_C3,
         AreaBasal = AreaBasal_C3,
         AreaCopa = AreaCopa_C3,
         PosicionCopa = PosicionCopa_C3,
         ExposicionCopa = ExposicionCopa_C3,
         DensidadCopa = DensidadCopa_C3,
         TransparenciaCopa = TransparenciaFollaje_C3,
         MuerteRegresiva = MuerteRegresiva_C3,
         VigorEtapa = VigorEtapa_C3,
         Edad = Edad_C3,
         Condicion = Condicion_C3,
         Danio1 = AgenteDanio1_C3,
         Severidad1 = Severidad1_C3,
         Danio2 = AgenteDanio2_C3,
         Severidad2 = Severidad2_C3,
         NumeroTallos = NoRama_C3,
         LongitudAnillos10 = LongitudAnillos10_C3,
         NumeroAnillos25 = NumeroAnillos25_C3,
         GrosorCorteza = GrosorCorteza_C3,
         Genero_APG = Genero_APG_C3,
         Especie_APG = Especie_APG_C3,
         X = X_C3,
         Y = Y_C3
  ) |> 
  # 2) Correction of categorical and specific entry mistakes
  mutate(
    # 2.1) "FormaBiologica" Correction: Define NA value 
    FormaBiologica = case_when(FormaBiologica == "Indeterminada" ~ NA, # NA value
                               TRUE ~ FormaBiologica),
    # 2.2) "PosicionCopa" Correction: Define NA value
    PosicionCopa = case_when(PosicionCopa == "No aplicacion" ~ NA, # NA value
                             PosicionCopa == "No aplica" ~ NA,     # NA value
                             PosicionCopa == "No capturado" ~ NA,  # NA value
                             TRUE ~ PosicionCopa),
    # 2.3) "ExposicionCopa" Correction: 
    ExposicionCopa = str_replace(ExposicionCopa, "SI", "Sí"),
    ExposicionCopa = case_when(ExposicionCopa == "No capturado" ~ NA, # NA value
                               ExposicionCopa == "No aplica" ~ NA,    # NA value
                               ExposicionCopa == "Árboles que no reciben luz porque se encuentran sombreados por otros árboles" ~ 
                                 "Árboles que no reciben luz porque están a la sombra de otra vegetación",
                               TRUE ~ ExposicionCopa),
    # 2.4) "DensidadCopa" Correction: Define NA value
    DensidadCopa = case_when(DensidadCopa == "No capturado" ~ NA, # NA value
                             TRUE ~ DensidadCopa),
    # 2.5) "TransparenciaCopa" Correction: Define NA value
    TransparenciaCopa = case_when(TransparenciaCopa == "No capturado" ~ NA, # NA value
                                  TRUE ~ TransparenciaCopa),
    # 2.6) "MuerteRegresiva" Correction: "No capturado" -> NA -> still 3 entry mistakes with 42309, 44682, 44840
    MuerteRegresiva = case_when(MuerteRegresiva == "No capturado" ~ NA, # NA value
                                MuerteRegresiva == "00" ~ "Sin parámetro",
                                MuerteRegresiva == "1-5" ~ "1 - 5", MuerteRegresiva == "6-10" ~ "6 - 10",
                                MuerteRegresiva == "11-15" ~ "11 - 15", MuerteRegresiva == "16-20" ~ "16 - 20",
                                MuerteRegresiva == "21-25" ~ "21 - 25", MuerteRegresiva == "26-30" ~ "26 - 30",
                                MuerteRegresiva == "31-35" ~ "31 - 35", MuerteRegresiva == "36-40" ~ "36 - 40",
                                MuerteRegresiva == "41-45" ~ "41 - 45", MuerteRegresiva == "46-50" ~ "46 - 50",
                                MuerteRegresiva == "51-55" ~ "51 - 55", MuerteRegresiva == "56-60" ~ "56 - 60",
                                MuerteRegresiva == "61-65" ~ "61 - 65", MuerteRegresiva == "66-70" ~ "66 - 70",
                                MuerteRegresiva == "71-75" ~ "71 - 75", MuerteRegresiva == "76-80" ~ "76 - 80",
                                MuerteRegresiva == "81-85" ~ "81 - 85", MuerteRegresiva == "86-90" ~ "86 - 90",
                                MuerteRegresiva == "91-95" ~ "91 - 95", MuerteRegresiva == "96-100" ~ "96 - 100",
                                TRUE ~ MuerteRegresiva),
    # 2.7) "Danio1" Correction: Define NA value
    Danio1 = case_when(Danio1 == "No definido" ~ NA, # NA value
                       TRUE ~ Danio1),
    # 2.8) "Severidad1" Correction: Define NA value + turn values into class 'numeric' 
    Severidad1 = case_when(Severidad1 == "No capturado" ~ NA, # NA value
                           Severidad1 == "No aplica" ~ NA,    # NA value
                           TRUE ~ as.numeric(Severidad1)),
    # 2.9) "Danio2" Correction: Define NA value
    Danio2 = case_when(Danio2 == "No capturado" ~ NA, # NA value
                       TRUE ~ Danio2),
    # 2.10) "Severidad2" Correction: Define NA value + turn values into class 'numeric'
    Severidad2 = case_when(Severidad2 == "No capturado" ~ NA, # NA value
                           Severidad2 == "No aplica" ~ NA,    # NA value
                           TRUE ~ as.numeric(Severidad2)),
    # 2.11) "NombreCientifico" Correction: Harmonizing categorical names across all datasets
    NombreCientifico_APG = case_when(NombreCientifico_APG == "ZZ Genero Desconocido" ~ "ZZ Desconocido",
                                     TRUE ~ NombreCientifico_APG),
    # 2.12) Add sample cycle number, so the datasets can later be merged
    Cycle = "3"
  ) |> 
  # 3) setting initial column order
  # NOTE: This is purely done for later convenience of working with the data
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, CveVeg_S7, TipoVeg_S7, FormaFuste, TipoTocon, Familia_APG,
         NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal, AlturaFusteLimpio, AlturaComercial,
         DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa, PosicionCopa, ExposicionCopa, DensidadCopa,
         TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion, Danio1, Severidad1, Danio2, Severidad2, NumeroTallos,
         LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
  ) |> 
  # 4) sorting for comparison
  # NOTE: This is purely done for later convenience of working with the data
  arrange(Estado, Conglomerado, Sitio, Registro)




# ============================================
# 4) Merge data
#
# NOTE: First, the relevant columns will be extracted. Secondly, the extracted datasets will be merged.
# NOTE: This will create a table in long format, which observations from all cycles in any given column.
#
# ============================================

## df_1
df_1_m <- df_1 |>
  mutate(CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

## df_2
df_2_m <- df_2 |>
  mutate(CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

## df_3
df_3_m <- df_3 |>
  mutate(cgl_sit_reg = NA,
         CveVeg = CveVeg_S7,
         TipoVeg = TipoVeg_S7) |> 
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

## Create merged dataframe df
df_m <- rbind(df_1_m, df_2_m, df_3_m) |> 
  mutate(Plot_ID = paste(Cycle, Conglomerado, Sitio, Anio, sep = "_")) |> 
  mutate(Cluster_ID = Conglomerado) |> 
  select(Cluster_ID, Plot_ID, Cycle, Sitio, Anio, everything(), -c(Conglomerado))


## Save merged df as .csv-file
if (!file.exists('data/Tree_Inventory_Clean/Tree_Inventory_Clean_ES.csv')) {
  write.csv(df_m, 'data/Tree_Inventory_Clean/Tree_Inventory_Clean_ES.csv', row.names = FALSE)
} else {
  message("File already exists — not overwriting.")
}




