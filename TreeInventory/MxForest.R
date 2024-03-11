
### MxForestInventory Data Wrangling Code --------------------------------------
### LAST UPDATED: 06/03/2024 (US)

start.time <- Sys.time()

####################          MAIN CODE             ##########################################################################
#################### 0) LOAD NECESSARY PACKAGES and PREPARATIONS ----------------------------------------------------------

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(tidyverse)  #data tidying
library(ggridges)   #geom_density_ridges()
library(terra)      #geo_spatial coordinates
library(vegan)      #for shannon-index and pielou-eveness
library(svglite)    #to save figures as .svg
library(extrafont)  #to change fonts in my figures
library(RColorBrewer) #for colorpalettes
windowsFonts(TNR = windowsFont("Times New Roman")) #to enable times new roman as a font
windowsFonts(ARL = windowsFont("Arial"))
fonts()
#################### 1) LOAD RAW DATA ------------------------------------------------------------

## 2004 - 2007 -> changing "NULL" character values to NA
Raw.04 <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"), na.strings = "NULL")

## 2009 - 2014 -> changing "NULL" character values to NA
Raw.09 <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"), na.strings = "NULL")

## 2015 - 2020
Raw.14 <- readxl::read_xlsx(here("data", "arbolado", "INFYS_Arbolado_2015_2020.xlsx"), sheet= 1, na = c("NULL", "999991", "999993"))


#################### 2) DATA CLEANING ------------------------------------------------------------

## Arb.04
Arb.04 <- Raw.04 |>  
  # normalizing names
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


## Arb.09 
Arb.09 <- Raw.09 |> 
  # nomralizing names
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
  # Correction of categoric and specific entry mistakes 
  mutate(
    # "Estado" Correction - initially 38 -> 32 
    Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                       Estado == "Mexico" ~ "México",
                       Estado == "Michoacan de Ocampo" ~ "Michoacán de Ocampo",
                       Estado == "Nuevo Leon" ~ "Nuevo León",
                       Estado == "Queretaro de Arteaga" ~ "Querétaro",
                       Estado == "San Luis Potosi" ~ "San Luis Potosí",
                       Estado == "Yucatan" ~ "Yucatán",
                       TRUE ~ Estado),
    # "Registro" Correction + "cgl_sit_arb" Correction - obviously wrong entries -> replaced with NAs
    Registro = case_when(Registro == 316 ~ NA,
                         TRUE ~ Registro),
    Registro = ifelse(cgl_sit_reg == "21314_4_147", NA, Registro),
    # "CveVeg_S5" Correction - replacing "VSaa" with "VSa" to fit Arb.14
    CveVeg_S5 = str_replace(CveVeg_S5, "VSaa", "VSa"),
    # "TipoVeg_S5" Correction - changing names based on CveVeg_S5 data to fit Arb.14
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
    # "FormaFuste" Correction - "NA" + class names -> NA + new names (differ from Arb.14)
    FormaFuste = str_replace(FormaFuste, "mas", "más"),
    FormaFuste = case_when(FormaFuste == "Arbol cruvo con dos o más fustes" ~ "Arbol curvo con dos o más fustes",
                           FormaFuste == "NA" ~ NA,
                           TRUE ~ FormaFuste),
    # "TipoTocon" Correction - class names -> new names (in line with Arb.14)
    TipoTocon = case_when(TipoTocon == "Tocon descompuesto (evidencia de tocon)" ~ "Tocón descompuesto (evidencia de tocón)",
                          TipoTocon == "Tocon madera seca (madera dura sin evidencias de descomposicion)" ~ "Tocón madera seca (madera dura sin evidencias de descomposición)",
                          TipoTocon == "Tocon madera seca (madera en proceso de descomposicion pero aun dificil de desprenderse del suelo)" ~ "Tocón madera seca (madera en proceso de descomposición pero aún difícil de desprenderse del suelo)",
                          TipoTocon == "Tocon madera verde (arbol recien cortado)" ~ "Tocón madera verde (árbol recién cortado)",
                          TipoTocon == "Tocon seco (madera muy descompuesta y de facil extraccion del sustrato)" ~ "Tocón seco (madera muy descompuesta y de fácil extracción del sustrato)",
                          TRUE ~ TipoTocon),
    # "PosicionCopa" Correction - "no aplica" -> NA
    PosicionCopa = case_when(PosicionCopa == "No aplica" ~ NA,
                             TRUE ~ PosicionCopa),
    # "ExposicionCopa" Correction - class names -> new names (in line with Arb.14)
    ExposicionCopa = str_replace(ExposicionCopa, "arbol", "árbol"),
    ExposicionCopa = str_replace(ExposicionCopa, "la luz", "luz"),
    ExposicionCopa = str_replace(ExposicionCopa, " solo en un cuarto", " en un solo cuarto"),
    ExposicionCopa = str_replace(ExposicionCopa, " y en un cuarto ", " y un cuarto "),
    ExposicionCopa = case_when(ExposicionCopa == "Arboles que no reciben luz porque se encuentran sombreados por otros árboles  parras  trepadoras u otra vegetacion  arboles que no tienen copa por definicion " ~ 
                                 "Árboles que no reciben luz porque están a la sombra de otra vegetación",
                               ExposicionCopa == "No aplica" ~ NA,
                               TRUE ~ ExposicionCopa),
    # "DensidadCopa" Correction - "" + -9999 + "n/a" +  + specific -> NA + ranges instead of exact values (in line with)
    DensidadCopa = case_when(DensidadCopa == "" ~ NA,
                             DensidadCopa == -9999 ~ NA,
                             DensidadCopa == "n/a" ~ NA,
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
    # "TransparenciaCopa" Correction - "" + -9999 + "n/a" +  + specific -> NA + ranges instead of exact values (in line with)
    TransparenciaCopa = case_when(TransparenciaCopa == "" ~ NA,
                                  TransparenciaCopa == -9999 ~ NA,
                                  TransparenciaCopa == "n/a" ~ NA,
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
    # "MuerteRegressiva" Correction - "" + -9999 + "n/a" +  + specific -> NA + ranges instead of exact values (in line with)
    MuerteRegresiva = case_when(MuerteRegresiva == "" ~ NA,
                                MuerteRegresiva == -9999 ~ NA,
                                MuerteRegresiva == "n/a" ~ NA,
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
                         floor(as.numeric(Edad))
                  ),
                  NA),
    Edad = case_when(Edad == 999 ~ NA,
                     TRUE ~ Edad),
    # "Condicion" Correction - values names -> new value names (in line with Arb.14)
    Condicion = case_when(Condicion == "Muerto en pie" ~ "Arbol muerto en pie",
                          Condicion == "Vivo" ~ "Arbol vivo",
                          TRUE ~ Condicion),
    # "Danio1" Correction - "No aplica" + class names -> NA + new names (in line with Arb.14)
    Danio1 = str_replace(Danio1, "abioticos", "abióticos"),
    Danio1 = str_replace(Danio1, "raiz/tocon", "raíz/tocón"),
    Danio1 = str_replace(Danio1, "pifitas", "pífitas"), 
    Danio1 = str_replace(Danio1, "parasitas", "parásitas"),
    Danio1 = str_replace(Danio1, "Sequia", "Sequía"),
    Danio1 = case_when(Danio1 == "Otros" ~ "No definido",
                       Danio1 == "Insectos" ~ "Insectos en general",
                       Danio1 == "No aplica" ~ NA,
                       Danio1 == "No definido" ~ NA,
                       TRUE ~ Danio1),
    # "Severidad1" Correction - "" + -9999 + "n/a" + "05" + character -> NA + numeric
    Severidad1 = case_when(Severidad1 == "" ~ NA,
                           Severidad1 == -9999 ~ NA,
                           Severidad1 == "n/a" ~ NA,
                           Severidad1 == "05" ~ 5,
                           TRUE ~ as.numeric(Severidad1)),
    # "Danio2" Correction - "No aplica" + class names -> NA + new names (in line with Arb.14)
    Danio2 = str_replace(Danio2, "abioticos", "abióticos"),
    Danio2 = str_replace(Danio2, "raiz/tocon", "raíz/tocón"),
    Danio2 = str_replace(Danio2, "pifitas", "pífitas"), 
    Danio2 = str_replace(Danio2, "parasitas", "parásitas"),
    Danio2 = str_replace(Danio2, "Sequia", "Sequía"),
    Danio2 = case_when(Danio2 == "Otros" ~ "No definido",
                       Danio2 == "Insectos" ~ "Insectos en general",
                       Danio2 == "No aplica" ~ NA,
                       TRUE ~ Danio2),
    # "Severidad2" Correction - "" + -9999 + "n/a" + "05" + character -> NA + numeric
    Severidad2 = case_when(Severidad2 == "" ~ NA,
                           Severidad2 == -9999 ~ NA,
                           Severidad2 == "n/a" ~ NA,
                           Severidad2 == "05" ~ 5,
                           TRUE ~ as.numeric(Severidad2)),
    # "NumeroTallos" Correction - 999 & 9999 -> NA
    NumeroTallos = case_when(NumeroTallos == 999 ~ NA,
                             NumeroTallos == 9999 ~ NA,
                             TRUE ~ NumeroTallos),
    # "LongitudAnillos10" Correction - 999 -> NA
    LongitudAnillos10 = case_when(LongitudAnillos10 == 999 ~ NA,
                                  TRUE ~ LongitudAnillos10),
    # "NumeroAnillos25" Correction - 
    NumeroAnillos25 = case_when(NumeroAnillos25 == 999 ~ NA,
                                TRUE ~ NumeroAnillos25),
    # "NombreCientifico_APG" Correction - "ZZ_Desconocido" -> NA
    NombreCientifico_APG = case_when(NombreCientifico_APG == "ZZ_Desconocido" ~ "ZZ Desconocido",
                                     TRUE ~ NombreCientifico_APG),
    # Added Cycle number 
    Cycle = "2"
  ) |> 
  # setting initial column order + attaching everything so far not considered to the end
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything() 
  ) |> 
  # sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro) 


## Arb.14 
Arb.14 <- Raw.14 |> 
  # nomralizing names
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
  # mutate() |> ### einfügen von cgl_sit_reg   <----- HIER MUSS NOCH WAS REIN
  # Correction of categoric and specific entry mistakes 
  mutate(
    # "FormaBiologica" Correction - "NULL" -> NA 
    FormaBiologica = case_when(FormaBiologica == "Indeterminada" ~ NA,
                               TRUE ~ FormaBiologica),
    # "PosicionCopa" Correction - "No aplicacion" + "No aplica" + "No capturado" -> NA
    PosicionCopa = case_when(PosicionCopa == "No aplicacion" ~ NA, 
                             PosicionCopa == "No aplica" ~ NA,
                             PosicionCopa == "No capturado" ~ NA,
                             TRUE ~ PosicionCopa),
    # "ExposicionCopa" Correction - "No capturado" + "No aplica" + class names -> NA + new names
    ExposicionCopa = str_replace(ExposicionCopa, "SI", "Sí"),
    ExposicionCopa = case_when(ExposicionCopa == "No capturado" ~ NA,
                               ExposicionCopa == "No aplica" ~ NA,
                               ExposicionCopa == "Árboles que no reciben luz porque se encuentran sombreados por otros árboles" ~ 
                                 "Árboles que no reciben luz porque están a la sombra de otra vegetación",
                               TRUE ~ ExposicionCopa),
    # "DensidadCopa" Correction - "No capturado" -> NA
    DensidadCopa = case_when(DensidadCopa == "No capturado" ~ NA,
                             TRUE ~ DensidadCopa),
    # "TransparenciaCopa" Correction - "No capturado" -> NA
    TransparenciaCopa = case_when(TransparenciaCopa == "No capturado" ~ NA,
                                  TRUE ~ TransparenciaCopa),
    # "MuerteRegresiva" Correction - "No capturado" -> NA -> still 3 entry mistakes with 42309, 44682, 44840
    MuerteRegresiva = case_when(MuerteRegresiva == "No capturado" ~ NA,
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
    # "Danio1" Correction - "No definido" -> NA
    Danio1 = case_when(Danio1 == "No definido" ~ NA,
                       TRUE ~ Danio1),
    # "Severidad1" Correction - "No capturado" + "No aplica" + class "character" -> NA + class "numeric"
    Severidad1 = case_when(Severidad1 == "No capturado" ~ NA,
                           Severidad1 == "No aplica" ~ NA,
                           TRUE ~ as.numeric(Severidad1)),
    # "Danio2" Correction - "No capturado" -> NA
    Danio2 = case_when(Danio2 == "No capturado" ~ NA,
                       TRUE ~ Danio2),
    # "Severidad2" Correction - "No capturado" + "No aplica" + class "character" -> NA + class "numeric"
    Severidad2 = case_when(Severidad2 == "No capturado" ~ NA,
                           Severidad2 == "No aplica" ~ NA,
                           TRUE ~ as.numeric(Severidad2)),
    # "NombreCientifico" Correction - "ZZ Genero Desconocido" -> NA
    NombreCientifico_APG = case_when(NombreCientifico_APG == "ZZ Genero Desconocido" ~ "ZZ Desconocido",
                                     TRUE ~ NombreCientifico_APG),
    # Added Cycle number 
    Cycle = "3"
  ) |> 
  # setting initial column order + attaching everything so far not considered to the end
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, CveVeg_S7, TipoVeg_S7, FormaFuste, TipoTocon, Familia_APG,
         NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal, AlturaFusteLimpio, AlturaComercial,
         DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa, PosicionCopa, ExposicionCopa, DensidadCopa,
         TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion, Danio1, Severidad1, Danio2, Severidad2, NumeroTallos,
         LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
  ) |> 
  # sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)



#################### 3) MERGE CycleS FOR OVERLAPPING VARIABLES ------------------------------------------------------

#Arb.04
M.04 <- Arb.04 |>
  mutate(CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

#Arb.09
M.09 <- Arb.09 |>
  mutate(CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

#Arb.14
M.14 <- Arb.14 |>
  mutate(cgl_sit_reg = NA,
         CveVeg = CveVeg_S7,
         TipoVeg = TipoVeg_S7) |> 
  select(Cycle, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

#merge
merged <- rbind(M.04, M.09, M.14) |> 
  mutate(Plot_ID = paste(Cycle, Conglomerado, Sitio, Anio, sep = "_")) |> 
  mutate(Cluster_ID = Conglomerado) |> 
  select(Cluster_ID, Plot_ID, Cycle, Sitio, Anio, everything(), -c(Conglomerado))


#################### 4) EDA PREPARATION ------------------------------------------------------------------
###### 4.1) SPECIES RICHNESS + INDIVIDUAL TREE COUNT ---------------------------
#### DATA ON CLUSTER LEVEL
C_SpecRich <- merged |> 
  select(Plot_ID, Cycle, Cluster_ID, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(Cycle, Cluster_ID) |> 
  summarise(Cycle = mean(as.integer(Cycle)),
            Cluster_ID = mean(Cluster_ID),
            Anio = mean(Anio),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Cluster_ID)


#### DATA ON PLOT LEVEL - used for later calculation in 5.3)
SpecRich <- merged |> 
  select(Plot_ID, Cycle, Cluster_ID, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(Cycle, Cluster_ID, Sitio) |> 
  summarise(Cycle = mean(as.integer(Cycle)),
            Cluster_ID = mean(Cluster_ID),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            Plot_ID = paste(Cycle, Cluster_ID, Sitio, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Plot_ID)


###### 4.2) SPECIES ABUNDANCES - needed for shannon index and eveness - DATA ON SPECIES LEVEL --------

#### DATA CALCULATED PER CLUSTER
C_SpecAbun <- merged |> 
  select(Cycle, Anio, Cluster_ID, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Cycle, Cluster_ID, NombreCientifico_APG) |> 
  summarise(Cycle = mean(as.integer(Cycle)),
            Cluster_ID = mean(Cluster_ID),
            Anio = mean(Anio),
            abundance=n(),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Cluster_ID)


###### 4.3) SHANNON INDEX H + PIELOU EVENESS J - uses temporary created dataframes! -------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON CLUSTER LEVEL
C_Temp.Shannon <- C_SpecAbun |> 
  ungroup() |> 
  select(Cycle, Cluster_ID, NombreCientifico_APG, abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

C_SpecAbun
C_Temp.Shannon

#### STEP 2: exchange NAs with Zeros 
## DATA ON CLUSTER LEVEL
C_PresenceAbsence <- C_Temp.Shannon |> 
  replace(is.na(C_Temp.Shannon), 0)

C_PresenceAbsence


#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON CLUSTER LEVEL
H <- diversity(C_PresenceAbsence[,-2])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON CLUSTER LEVEL
J <- H/log(specnumber(C_PresenceAbsence[, -2]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
C_Temp.HJ <- data.frame(C_PresenceAbsence$Cycle, C_PresenceAbsence$Cluster_ID, H, J) |> 
  rename(Cycle = C_PresenceAbsence.Cycle,
         Cluster_ID = C_PresenceAbsence.Cluster_ID)


#### STEP 6: merged data table 
## DATA ON CLUSTER LEVEL
ClusterDiagnostics <- left_join(C_SpecRich, C_Temp.HJ, by= c("Cycle","Cluster_ID")) |> 
  select(Cycle, Cluster_ID, Anio, species_count, total_entries, H, J, X, Y)


###### 4.4) TREE MORPHOLOGY -----------------------------------------------

#### DATA ON CLUSTER LEVEL ----- calculated by individual entries (alternatively by means of plot means)
# need more thought going into whether to use means or medians
# Example: for Tree Height Means are on average -20cm compared to Median

C_TreeMorp <- merged |> 
  group_by(Cycle, Cluster_ID) |> 
  summarise(Cycle = mean(as.integer(Cycle)),
            Cluster_ID = mean(Cluster_ID),
            Anio = mean(Anio),
            AvgTreeHeight = mean(AlturaTotal, na.rm = T),
            MedTreeHeight = median(AlturaTotal, na.rm = T),
            AvgDbh = mean(DiametroNormal, na.rm = T),
            MedDbh = median(DiametroNormal, na.rm = T),
            AvgCrownDiameter = mean(DiametroCopa, na.rm = T),
            MedCrownDiameter = median(DiametroCopa, na.rm = T),
            AvgCrownHeight = mean(AlturaTotal - AlturaFusteLimpio, na.rm = T),          # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            MedCrownHeight = median(AlturaTotal - AlturaFusteLimpio, na.rm = T),    # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            AvgCrownArea = mean(AreaCopa, na.rm = T),
            MedCrownArea = median(AreaCopa, na.rm = T),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(AvgCrownHeight = case_when(AvgCrownHeight < 0 ~ NA,
                                    T ~ AvgCrownHeight)) |> 
  relocate(Cluster_ID)

###### 4.5) COMPLETE DIAGNOSTICS DATASET (excluding Biomass; 01/10/2024) ----------------------------

Comp_C_Diagnostics <- left_join(ClusterDiagnostics, C_TreeMorp, by= c("Cycle", "Cluster_ID", "Anio", "X", "Y")) |> 
  relocate(Cycle, Cluster_ID, Anio, species_count, total_entries, H, J, 
           AvgTreeHeight, MedTreeHeight, AvgDbh, MedDbh, AvgCrownDiameter, MedCrownDiameter, AvgCrownHeight, MedCrownHeight, AvgCrownArea, MedCrownArea, X, Y)

#################### 5) METADATA STUFF -------------------------------------------------------------------
###### 5.1) PLOT COUNTS FOR EACH CLUSTER -----------------------------------------

# Adding the Number of Plots per Cluster ("Plots")
PlotCounts <- merged |> 
  group_by(Cycle, Cluster_ID, Sitio, X, Y) |> 
  count() |>
  ungroup() |> 
  select(Cycle, Cluster_ID, Sitio, X, Y) |> 
  group_by(Cycle, Cluster_ID) |> 
  summarise(Cycle = mean(as.numeric(Cycle)),
            Cluster_ID = mean(Cluster_ID),
            X = mean(X),
            Y = mean(Y),
            Plots = n())


# combined dataset
Comp_C_Diagnostics_V2 <- left_join(Comp_C_Diagnostics, PlotCounts, by= c("Cycle", "Cluster_ID")) |> 
  select(everything(), -c("X.y", "Y.y")) |> 
  rename(X = X.x,
         Y = Y.x)

###### 5.2) AVAILABILITY OF PLOTS FOR EACH CLUSTER -----------------------------------------------

# function to count number of cycles with a given number of plots 
summarise_data <- function(data, plot_number) {
  data |> 
    select(Cycle, Cluster_ID, Anio, Plots, X, Y) |> 
    filter(Plots == plot_number) |> 
    group_by(Cluster_ID) |> 
    arrange(Cluster_ID) |> 
    summarise(Cluster_ID = mean(Cluster_ID),
              n = n(),
              X1 = mean(X),
              Y1 = mean(Y)) |> 
    rename(n_cycles_placeholder = n)
}

# combined dataset V3
Comp_C_Diagnostics_V3 <- Comp_C_Diagnostics_V2 %>% 
  left_join(summarise_data(. ,4), by = "Cluster_ID") %>%
  left_join(summarise_data(. ,3), by = "Cluster_ID") %>% 
  left_join(summarise_data(. ,2), by = "Cluster_ID") %>%
  left_join(summarise_data(. ,1), by = "Cluster_ID") %>%
  select(-c("X1.x", "Y1.x", "X1.y", "Y1.y", "X1.x.x", "Y1.x.x", "X1.y.y", "Y1.y.y")) |> 
  rename(cycles_four_plots = n_cycles_placeholder.x,
         cycles_three_plots = n_cycles_placeholder.y,
         cycles_two_plots = n_cycles_placeholder.x.x,
         cycles_one_plots = n_cycles_placeholder.y.y,) |> 
  arrange(Cluster_ID) |> 
  mutate(Consistent =  case_when(cycles_four_plots == 3 ~ T, cycles_four_plots <= 2 ~ F,
                                 cycles_three_plots == 3 ~ T,cycles_three_plots <= 2 ~ F,
                                 cycles_two_plots == 3 ~ T, cycles_two_plots <= 2 ~ F,
                                 cycles_one_plots == 3 ~ T, cycles_one_plots <= 2 ~ F)) |> 
  mutate(cycles_four_plots = ifelse(is.na(cycles_four_plots), 0, cycles_four_plots),
         cycles_three_plots = ifelse(is.na(cycles_three_plots), 0, cycles_three_plots),
         cycles_two_plots = ifelse(is.na(cycles_two_plots), 0, cycles_two_plots),
         cycles_one_plots = ifelse(is.na(cycles_one_plots), 0, cycles_one_plots),
         Cycles = (cycles_four_plots + cycles_three_plots + cycles_two_plots + cycles_one_plots))


###### 5.3) TREE PLOT COUNT MEANS AND MEDIANS BY CLUSTERS ----------------------
## calculate means and medians for each cluster based of total plot entries 
PTC_C <- SpecRich |> 
  group_by(Cycle, Cluster_ID) |> 
  summarise(Cycle = mean(Cycle),
            Cluster_ID = mean(Cluster_ID),
            Anio = mean(Anio),
            Plot_TreeCount_Mean = mean(total_entries),
            Plot_TreeCount_Median = median(total_entries),
            X = mean(X),
            Y = mean(Y)) |> 
  select(Cycle, Cluster_ID, Anio, Plot_TreeCount_Mean, Plot_TreeCount_Median)

Comp_C_Diagnostics_V4 <- left_join(Comp_C_Diagnostics_V3, PTC_C, by = c("Cycle", "Cluster_ID", "Anio"))


Comp_C_Diagnostics_V5 <- left_join(Comp_C_Diagnostics_V4, merged |> select(Cycle, Cluster_ID, Estado, CveVeg, TipoVeg) |> distinct() |> mutate(Cycle = as.numeric(Cycle)),
                                   by = c("Cycle", "Cluster_ID"))

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken1 <- end.time - start.time

###################          METADATA DATASET CODE            ###################################################
################### 1) LOAD RAW DATA ------------------------------------------------------------
## 2004 - 2007
Sec.04 <- readxl::read_xlsx(here("data", "secciones", "INFyS_Secciones_2004_2007_7VCcv7Y.xlsx"), sheet= 1, na = c(""))
## 2004 - 2007
Sec.09 <- readxl::read_xlsx(here("data", "secciones", "INFyS_Secciones_2009_2014_w18bSF1.xlsx"), sheet= 1, na = c(""))
## 2004 - 2007
Sec.14 <- readxl::read_xlsx(here("data", "secciones", "INFyS_Secciones_2015-2020_fRBymGR.xlsx"), sheet= 1, na = c(""))

Sec.09 <- Sec.09 |> 
  mutate(X = as.numeric(X),
         Y = as.numeric(Y))


################### 2) CLUSTER COMPARISON PREPARATION --------------------------------------
# STEP 1: Create Clusterbase
ClusterBase <- rbind(Sec.04 |> 
                       select(Conglomerado),
                     Sec.09 |> 
                       select(Conglomerado),
                     Sec.14 |> 
                       mutate(Conglomerado = IDConglomerado) |> 
                       select(Conglomerado)) |> 
  distinct() |> 
  mutate(Cluster_ID = Conglomerado) |> 
  select(Cluster_ID)


# STEP 2: JOIN CLUSTER_ID + X & Y FROM EACH Cycle
#Cycle 1
Temp <- ClusterBase |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado1 = Conglomerado,
                     X1 = X,
                     Y1 = Y) |> 
              select(Cluster_ID, Conglomerado1, X1, Y1),
            by = "Cluster_ID")
#Cycle 2
Temp <- Temp |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado2 = Conglomerado,
                     X2 = X,
                     Y2 = Y) |> 
              select(Cluster_ID, Conglomerado2, X2, Y2),
            by = "Cluster_ID")
#Cycle 3
MetaBase <- Temp |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Conglomerado3 = IDConglomerado,
                     X3 = X_C3,
                     Y3 = Y_C3) |> 
              select(Cluster_ID, Conglomerado3, X3, Y3),
            by = "Cluster_ID")


# STEP 3: AVERAGE X and Y
PreBase <- MetaBase |> 
  group_by(Cluster_ID) |>
  replace(is.na(MetaBase), 0) |>
  # Averaging X and Y coordinates for each cluster
  mutate(DIV = case_when(X1 != 0 & X2 != 0 & X3 != 0 ~ 3,
                         X1 != 0 & X2 != 0 & X3 == 0 ~ 2,
                         X1 != 0 & X2 == 0 & X3 != 0 ~ 2,
                         X1 == 0 & X2 != 0 & X3 != 0 ~ 2,
                         X1 != 0 & X2 == 0 & X3 == 0 ~ 1,
                         X1 == 0 & X2 != 0 & X3 == 0 ~ 1,
                         X1 == 0 & X2 == 0 & X3 != 0 ~ 1,
                         X1 == 0 & X2 == 0 & X3 == 0 ~ NA
  ),
  X = (X1 + X2 + X3)/DIV,
  Y = (Y1 + Y2 + Y3)/DIV) |> 
  select(Cluster_ID, Conglomerado1, Conglomerado2, Conglomerado3, X, Y) |> 
  # Differences in Clusteravailability - for both sampled and not sampled
  mutate(DIFF12 = Conglomerado2 - Conglomerado1,
         DIFF12 = case_when(DIFF12 > 0 ~ 1,
                            DIFF12 < 0 ~ -1,
                            TRUE ~ DIFF12),
         DIFF13 = Conglomerado3 - Conglomerado1,
         DIFF13 = case_when(DIFF13 > 0 ~ 1,
                            DIFF13 < 0 ~ -1,
                            TRUE ~ DIFF13),
         DIFF23 = Conglomerado3 - Conglomerado2,
         DIFF23 = case_when(DIFF23 > 0 ~ 1,
                            DIFF23 < 0 ~ -1,
                            TRUE ~ DIFF23),
         # Theoretical availability of a cluster for any given cycle - has nothing to do with actual measurement
         Cycle1 = case_when(Conglomerado1 != 0 ~ 1,
                            TRUE ~ 0),
         Cycle2 = case_when(Conglomerado2 != 0 ~ 1,
                            TRUE ~ 0),
         Cycle3 = case_when(Conglomerado3 != 0 ~ 1,
                            TRUE ~ 0),)

#STEP 4: RE-INTRODUCE WHETHER IT WAS SAMPLED
PreBase2 <- PreBase |> 
  # Join Cycle 1
  left_join(Sec.04 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado1 = Muestreado) |> 
              select(Cluster_ID, Muestreado1),
            by = "Cluster_ID"
  ) |> 
  # Join Cycle 2
  left_join(Sec.09 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado2 = Muestreado) |> 
              select(Cluster_ID, Muestreado2),
            by = "Cluster_ID") |> 
  # Join Cycle 3
  left_join(Sec.14 |> 
              select(IDConglomerado, Muestreado_C3) |> 
              mutate(Cluster_ID = IDConglomerado,
                     Muestreado3 = Muestreado_C3) |> 
              select(Cluster_ID, Muestreado3),
            by = "Cluster_ID") |> 
  # changing Muestreado1 values to make them easily computable and compatible with Muestreado2 and Muestreado 3
  mutate(Muestreado1 = case_when(Muestreado1 == "Si" ~ 1,
                                 Muestreado1 == "No" ~ 0)) 


################### 3) CLUSTER AND PLOT STATUS -----------------------------------------------------------
#### 3.1) Cluster Status Filter ------------------------------------------------
# STEP 1: Join Diagnostics Dataset and Metadata
MetaStack <- PreBase2 %>% 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(Cycle == 1) %>%
              rename(Cycle1 = Cycle) %>%
             select("Cluster_ID", "Cycle1"),
             by = "Cluster_ID") %>%
  rename(Cycle1 = Cycle1.y) |> 
  mutate(Cycle1 = case_when(is.na(Cycle1) ~ 100,
                           T ~ Cycle1)) |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(Cycle == 2) %>%
              rename(Cycle2 = Cycle) %>%
              select("Cluster_ID", "Cycle2"),
            by = "Cluster_ID") %>%
  rename(Cycle2 = Cycle2.y) |> 
  mutate(Cycle2 = case_when(is.na(Cycle2) ~ 100,
                           T ~ Cycle2)) |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(Cycle == 3) %>%
              rename(Cycle3 = Cycle) %>%
              select("Cluster_ID", "Cycle3"),
            by = "Cluster_ID") %>%
  rename(Cycle3 = Cycle3.y) |> 
  mutate(Cycle3 = case_when(is.na(Cycle3) ~ 100,
                           T ~ Cycle3))


  
# STEP 2: Claculate Status of Cluster for each Cycle
MetaStack_V2 <- MetaStack |> 
  mutate(Status1 = Muestreado1 - Cycle1,
         Status2 = Muestreado2 - Cycle2,
         Status3 = Muestreado3 - Cycle3) |> 
  mutate(Status1 = case_when(is.na(Status1) ~ -100,
                             Status1 > -99 & Status1 <= 0 ~ 1,
                             T ~ Status1),
         Status2 = case_when(is.na(Status2) ~ -100,
                             Status2 > -99 & Status2 <= 0 ~ 1,
                             T ~ Status2),
         Status3 = case_when(is.na(Status3) ~ -100,
                             Status3 > -99 & Status3 <= 0 ~ 1,
                             T ~ Status3))

#### 3.2) Plot Status Filter ---------------------------------------------------
MetaStack_V3 <- MetaStack_V2 |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S1 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(Cycle == 1) %>%
              ungroup() |> 
              rename(Plot1 = Plots) %>%
              select("Cluster_ID", "Plot1"),
            by = "Cluster_ID") |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S2 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(Cycle == 2) %>%
              ungroup() |> 
              rename(Plot2 = Plots) %>%
              select("Cluster_ID", "Plot2"),
            by = "Cluster_ID") |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Plot_S3 = Sitios_x_cgl_C3) |> 
              select(Cluster_ID, Plot_S3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(Cycle == 3) %>%
              ungroup() |> 
              rename(Plot3 = Plots) %>%
              select("Cluster_ID", "Plot3"),
            by = "Cluster_ID") |> 
  mutate(Plot_Status1 = case_when(Status1 == -100 ~ -100,
                                  Status1 == -99 ~ -99,
                                  Status1 > -99 ~ Plot_S1 - Plot1),
         Plot_Status2 = case_when(Status2 == -100 ~ -100,
                                  Status2 == -99 ~ -99,
                                  Status2 > -99 ~ Plot_S2 - Plot2),
         Plot_Status3 = case_when(Status3 == -100 ~ -100,
                                  Status3 == -99 ~ -99,
                                  Status3 > -99 ~ Plot_S3 - Plot3)
         ) 

##################################     END      ##################################################################
#
################### National Level Species Richenss ###############################################
# STEP 1: What is the cluster base i'll use? ----------

National_Base <- MetaStack_V3 |> 
  select(Cluster_ID, X, Y, Muestreado1, Muestreado2, Muestreado3, Plot_S1, Plot_S2, Plot_S3)


# STEP 2: Calculation for cycle 1, 2, and 3 -----------
# Cycle 1
National_1 <- National_Base |> 
  left_join(merged |> 
              filter(Cycle == 1) |>  
              select(Cluster_ID, NombreCientifico_APG),
            by = "Cluster_ID")



N1 <- National_1 |> 
  ungroup() |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(Plot_S1 == 4 & Plot_S2 == 4 & Plot_S3 == 4) |> 
  distinct(NombreCientifico_APG) |> 
  count() |> 
  mutate(Cycle = 1)

N1

# Cycle 2
National_2 <- National_Base |> 
  left_join(merged |> 
              filter(Cycle == 2) |> 
              select(Cluster_ID, NombreCientifico_APG),
            by = "Cluster_ID")

N2 <- National_2 |> 
  ungroup() |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(Plot_S1 == 4 & Plot_S2 == 4 & Plot_S3 == 4) |> 
  distinct(NombreCientifico_APG) |> 
  count() |> 
  mutate(Cycle = 2)

# Cycle 3
National_3 <- National_Base |> 
  left_join(merged |> 
              filter(Cycle == 3) |> 
              select(Cluster_ID, NombreCientifico_APG),
            by = "Cluster_ID")

N3 <- National_3 |> 
  ungroup() |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(Plot_S1 == 4 & Plot_S2 == 4 & Plot_S3 == 4) |> 
  distinct(NombreCientifico_APG)  |> 
  count() |> 
  mutate(Cycle = 3)




# STEP 3: Cute Barplot ------
# df
National <- rbind(N1, N2, N3)
National

# plot
National |> 
  ggplot(aes(x = Cycle, y = n)) +
  geom_bar(stat = "identity")


##################################     END      ##################################################################
#
############### ECOREGIONS BASED ON SHAPEFILE  ############################################
## STEP 1: load package ---------------
library(sf)

## STEP 2: read shapeCycle "ecoregions" -----

shapefile <- st_read(here("data", "Ecoregions", "ecort08cw.shp"))

## STEP 3: create sf object for coordinates -----
coordinates_df <- National_Base |> 
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84")

coordinates_df


## STEP 4: ensure CRS compatability -----
# Check CRS of both datasets
crs_shapefile <- st_crs(shapefile)
crs_coordinates_df <- st_crs(coordinates_df)

if (crs_shapefile != crs_coordinates_df) {
  coordinates_df <- st_transform(coordinates_df, crs_shapefile)
}

## STEP 5: spatial join ----
joined_data <- st_join(coordinates_df, shapefile)

## STEP 6: drop everything apart from cluster_ID and DESECON  + return to regular df ----
Ecoregions <- st_set_geometry(joined_data |> 
                                select(Cluster_ID, DESECON1, DESECON2, CVEECON2), NULL) |> 
  mutate(DESECON2 = str_replace(DESECON2, "Gofo", "Golfo"))

Ecoregions
##################################     END      ##################################################################
#
##############  BASE FOR ALL CACLULATIONS      #################################################
## STEP 1: JOIN National_Base and Ecoregions based on Cluster_ID ----
Base <- National_Base |> 
  left_join(Ecoregions, by = "Cluster_ID")

Base
##################################     END      ##################################################################

end.time <- Sys.time()
time.taken2 <- end.time - start.time


################### FILTER FOR COMPARABLE CLUSTERS ###############################################################
c123_filter <- function(data) {
  data %>%
    filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1 & 
             Plot_S1 == 4 & Plot_S2 == 4 & Plot_S3 == 4)
}
##################################     END      ##################################################################


###################  UNIVARIATE CLUSTERBASED CHANGE CALCULATIONS  ################################################
# STEP 1: FullStack_V1 ----
FullStack_V1 <- Base |> 
  left_join(Comp_C_Diagnostics_V5 |> 
              ungroup() |> 
              filter(Cycle == 1) |> 
              mutate(DBH1 = AvgDbh,
                     CD1 = AvgCrownDiameter,
                     CH1 = AvgCrownHeight,
                     CA1 = AvgCrownArea,
                     SC1 = species_count,
                     TH1 = AvgTreeHeight,
                     TE1 = total_entries,
                     J1 = J) |> 
              select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 |> 
              ungroup() |> 
              filter(Cycle == 2) |> 
              mutate(DBH2 = AvgDbh,
                     CD2 = AvgCrownDiameter,
                     CH2 = AvgCrownHeight,
                     CA2 = AvgCrownArea,
                     SC2 = species_count,
                     TH2 = AvgTreeHeight,
                     TE2 = total_entries,
                     J2 = J) |> 
              select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 |> 
              ungroup() |> 
              filter(Cycle == 3) |> 
              mutate(DBH3 = AvgDbh,
                     CD3 = AvgCrownDiameter,
                     CH3 = AvgCrownHeight,
                     CA3 = AvgCrownArea,
                     SC3 = species_count,
                     TH3 = AvgTreeHeight,
                     TE3 = total_entries,
                     J3 = J) |> 
              select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3),
            by = "Cluster_ID") |> 
  # Exchanging NAs with Zeros (except for J)
  mutate(
    # Tree Entries
    TE1 = case_when(Muestreado1 == 1 & is.na(TE1) ~ 0,
                    T ~ TE1),
    TE2 = case_when(Muestreado2 == 1 & is.na(TE2) ~ 0,
                    T ~ TE2),
    TE3 = case_when(Muestreado3 == 1 & is.na(TE3) ~ 0,
                    T ~ TE3),
    # Species Count
    SC1 = case_when(Muestreado1 == 1 & is.na(SC1) ~ 0,
                    T ~ SC1),
    SC2 = case_when(Muestreado2 == 1 & is.na(SC2) ~ 0,
                    T ~ SC2),
    SC3 = case_when(Muestreado3 == 1 & is.na(SC3) ~ 0,
                    T ~ SC3),
    # DBH
    DBH1 = case_when(Muestreado1 == 1 & is.na(DBH1) ~ 0,
                     T ~ DBH1),
    DBH2 = case_when(Muestreado2 == 1 & is.na(DBH2) ~ 0,
                     T ~ DBH2),
    DBH3 = case_when(Muestreado3 == 1 & is.na(DBH3) ~ 0,
                     T ~ DBH3),
    # Crown Diameter
    CD1 = case_when(Muestreado1 == 1 & is.na(CD1) ~ 0,
                    T ~ CD1),
    CD2 = case_when(Muestreado2 == 1 & is.na(CD2) ~ 0,
                    T ~ CD2),
    CD3 = case_when(Muestreado3 == 1 & is.na(CD3) ~ 0,
                    T ~ CD3),
    # Crown Height
    CH1 = case_when(Muestreado1 == 1 & is.na(CH1) ~ 0,
                    T ~ CH1),
    CH2 = case_when(Muestreado2 == 1 & is.na(CH2) ~ 0,
                    T ~ CH2),
    CH3 = case_when(Muestreado3 == 1 & is.na(CH3) ~ 0,
                    T ~ CH3),
    # Crown Area
    CA1 = case_when(Muestreado1 == 1 & is.na(CA1) ~ 0,
                    T ~ CA1),
    CA2 = case_when(Muestreado2 == 1 & is.na(CA2) ~ 0,
                    T ~ CA2),
    CA3 = case_when(Muestreado3 == 1 & is.na(CA3) ~ 0,
                    T ~ CA3),
    # Tree Height
    TH1 = case_when(Muestreado1 == 1 & is.na(TH1) ~ 0,
                    T ~ TH1),
    TH2 = case_when(Muestreado2 == 1 & is.na(TH2) ~ 0,
                    T ~ TH2),
    TH3 = case_when(Muestreado3 == 1 & is.na(TH3) ~ 0,
                    T ~ TH3),
  ) |> 
  ungroup()


FullStack_V1 |> 
  c123_filter() |> 
  count()
 
# STEP 2: univariate change calculations -----------------------------------
FullStack_V1 <- FullStack_V1 |> 
  # Calculate univariate changes in TreeCounts (TE) and SpeciesCounts (SC)
  mutate(TE12 = TE2 - TE1,
         TE23 = TE3 - TE2,
         TE13 = TE3 - TE1,
         SC12 = SC2 - SC1,
         SC23 = SC3 - SC2,
         SC13 = SC3 - SC1) |> 
  # aclculate univariate changes in species richness as percentage compared to previous cycle
  mutate(SC12P = case_when(SC1 != 0 ~ (SC12/SC1)*100,
                           SC1 == 0 ~ NA),
         SC23P = case_when(SC2 != 0 ~ (SC23/SC2)*100,
                           SC2 == 0 ~ NA),
         SC13P = case_when(SC1 != 0 ~ (SC13/SC1)*100,
                           SC1 == 0 ~ NA))



# STEP 3: numeric distribution of change calculation ----
FullStack_V1 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(Plot_S2 == 4 & Plot_S3 == 4) |> 
  select(SC23) |> 
  filter(SC23 == 0) |> 
  count()

FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  filter(Plot_S1 == 4 & Plot_S2 == 4) |> 
  select(SC1, SC2) |> 
  filter(SC1 <= 20 & SC2 <= 20) |> 
  count()

FullStack_V1 |> 
  filter(Muestreado1 == 1) |> 
  filter(Plot_S1 == 4) |> 
  summarise(avg = mean(SC1),
            med = median(SC1))

FullStack_V1 |> 
  filter(Muestreado2 == 1) |> 
  filter(Plot_S2 == 4) |> 
  summarise(avg = mean(SC2),
            med = median(SC2))

FullStack_V1 |> 
  filter(Muestreado3 == 1) |> 
  filter(Plot_S3 == 4) |> 
  summarise(avg = mean(SC3),
            med = median(SC3))

FullStack_V1 |> 
  select(DESECON2) |> 
  distinct() |> 
  print(n=100)

FullStack_V1 |> 
  summarise(Max12 = max(SC12, na.rm = T),
            Max23 = max(SC23, na.rm = T),
            Max13 = max(SC13, na.rm = T),
            Min12 = min(SC12, na.rm = T),
            Min23 = min(SC23, na.rm = T),
            Min13 = min(SC13, na.rm = T),
            mean12 = mean(SC12, na.rm = T),
            mean23 = mean(SC23, na.rm = T),
            mean13 = mean(SC13, na.rm = T),
            median12 = median(SC12, na.rm = T),
            median23 = median(SC23, na.rm = T),
            median13 = median(SC13, na.rm = T),
            ) |> 
  print(n = 50)

# status based on PSUs by ecoregion
FullStack_V1 |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC1, na.rm = T),
            Max2 = max(SC2, na.rm = T),
            Max3 = max(SC3, na.rm = T),
            Min1 = min(SC1, na.rm = T),
            Min2 = min(SC2, na.rm = T),
            Min3 = min(SC3, na.rm = T),
            mean1 = mean(SC1, na.rm = T),
            mean2 = mean(SC2, na.rm = T),
            mean3 = mean(SC3, na.rm = T)) |> 
  print(n = 50)


# changes based on PSUs by ecoregion
FullStack_V1 |> 
  group_by(DESECON2) |> 
  summarise(Max12 = max(SC12, na.rm = T),
            Max23 = max(SC23, na.rm = T),
            Max13 = max(SC13, na.rm = T),
            Min12 = min(SC12, na.rm = T),
            Min23 = min(SC23, na.rm = T),
            Min13 = min(SC13, na.rm = T),
            mean12 = mean(SC12, na.rm = T),
            mean23 = mean(SC23, na.rm = T),
            mean13 = mean(SC13, na.rm = T)) |> 
  print(n = 50)

# SR values for PSUs per ecoregion (table s3) ----
# cyle 1
x <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Plot_S1 == 4) |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC1, na.rm = T),
            Min1 = min(SC1, na.rm = T),
            mean1 = mean(SC1, na.rm = T),
            median1 = median(SC1, na.rm = T),
            entries = n()) |> 
  mutate(Cycle = 1)

# cyle 2
y <- FullStack_V1 |> 
  filter(Muestreado2 == 1 & Plot_S2 == 4) |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC2, na.rm = T),
            Min1 = min(SC2, na.rm = T),
            mean1 = mean(SC2, na.rm = T),
            median1 = median(SC2, na.rm = T),
            entries = n()) |> 
  mutate(Cycle = 2)

# cyle 3
z <- FullStack_V1 |> 
  filter(Muestreado3 == 1 & Plot_S3 == 4) |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC3, na.rm = T),
            Min1 = min(SC3, na.rm = T),
            mean1 = mean(SC3, na.rm = T),
            median1 = median(SC3, na.rm = T),
            entries = n()) |> 
  mutate(Cycle = 3)
  
PSU_SR <- rbind(x,y,z) |> 
  arrange(desc(DESECON2), Cycle)

PSU_SR |> 
  print(n=100)

## ENDE ----
# SRC values for PSUs per ecoregion (table s4) ----
# cyle 1
x <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Plot_S1 == 4 & Plot_S2 == 4) |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC12, na.rm = T),
            Min1 = min(SC12, na.rm = T),
            mean1 = mean(SC12, na.rm = T),
            median1 = median(SC12, na.rm = T),
            entries = n()) |> 
  mutate(Comparison = "C12")

# cyle 2
y <- FullStack_V1 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1 & Plot_S2 == 4 & Plot_S3 == 4) |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC23, na.rm = T),
            Min1 = min(SC23, na.rm = T),
            mean1 = mean(SC23, na.rm = T),
            median1 = median(SC23, na.rm = T),
            entries = n()) |> 
  mutate(Comparison = "C23")

# cyle 3
z <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1 & Plot_S1 == 4 & Plot_S3 == 4) |> 
  group_by(DESECON2) |> 
  summarise(Max1 = max(SC13, na.rm = T),
            Min1 = min(SC13, na.rm = T),
            mean1 = mean(SC13, na.rm = T),
            median1 = median(SC13, na.rm = T),
            entries = n()) |> 
  mutate(Comparison = "C13")

PSU_SRC <- rbind(x,y,z) |> 
  mutate(Comparison = factor(Comparison, levels = c("C12", "C23", "C13"))) |> 
  arrange(desc(DESECON2), Comparison)

PSU_SRC |> 
  print(n=100)

## ENDE ----
# OTHER STUFF ----
# table counting PSUs per Ecoregion for species richness
FullStack_V1 |> 
  filter(Muestreado2 == 1) |> 
  filter(Plot_S2 == 4) |> 
  filter(!is.na(DESECON2)) |> 
  group_by(DESECON2) |> 
  select(DESECON2, SC2) |> 
#  filter(SC1 > 0) |> 
  count() |> 
  arrange(desc(DESECON2)) |> 
  print(n = 100)

FullStack_V1 |> 
  filter(Muestreado3 == 1) |> 
  filter(Plot_S3 == 4) |> 
  filter(!is.na(DESECON2)) |> 
  group_by(DESECON2) |> 
  mutate(Type = case_when(SC3 > 20 ~ ">20",
                          SC3 > 10 ~ ">10",
                          SC3 > 5 ~ ">05",
                          SC3 > 0 ~ ">0",
                          SC3 == 0 ~ "0")) |> 
  select(DESECON2, SC3, Type) |> 
  group_by(DESECON2, Type) |> 
  count() |> 
  arrange(desc(DESECON2), desc(Type)) |>
  print(n = 200)



##################################     END      ##################################################################

end.time <- Sys.time()
time.taken3 <- end.time - start.time





##############  NATIONAL BASED CALCULATION         ###############################################
# STEP 1: calculate number of clusters per cycle ----
# Cycle 1
National_C.1 <- National_Base |> 
  filter(Muestreado1 == 1) |> 
  select(Cluster_ID)

National_C.1

National_Calc.1 <- National_C.1 |> 
  left_join(merged |> filter(Cycle == 1) |> select(Cycle, Cluster_ID),
            by = "Cluster_ID") |> 
  ungroup() |> 
  summarise(Cycle = as.character(1),
            number_of_clusters = n_distinct(Cluster_ID))

National_Calc.1

# Cycle 2 
National_C.2 <- National_Base |> 
  filter(Muestreado2 == 1) |> 
  select(Cluster_ID)

National_C.2

National_Calc.2 <- National_C.2 |> 
  left_join(merged |> filter(Cycle == 2) |> select(Cycle, Cluster_ID),
            by = "Cluster_ID") |> 
  ungroup() |> 
  summarise(Cycle = as.character(2),
            number_of_clusters = n_distinct(Cluster_ID))

National_Calc.2

# Cycle 3
National_C.3 <- National_Base |> 
  filter(Muestreado3 == 1) |> 
  select(Cluster_ID)

National_C.3

National_Calc.3 <- National_C.3 |> 
  left_join(merged |> filter(Cycle == 3) |> select(Cycle, Cluster_ID),
            by = "Cluster_ID") |> 
  ungroup() |> 
  summarise(Cycle = as.character(3),
            number_of_clusters = n_distinct(Cluster_ID))

National_Calc.3

# STEP 2: Calculate species abundances per cycle ----
# Cycle 1
National_C.1 <- National_Base |> 
  filter(Muestreado1 == 1) |> 
  select(Cluster_ID)

National_Spec.1 <- National_C.1 |> 
  left_join(merged |> filter(Cycle == 1) |> select(Cycle, Cluster_ID, NombreCientifico_APG),
            by = "Cluster_ID") |>
  select(Cycle, NombreCientifico_APG) |> 
  group_by(Cycle, NombreCientifico_APG) |> 
  summarise(species_abundances = n())

National_Spec.1

# Cycle 2
National_C.2 <- National_Base |> 
  filter(Muestreado2 == 1) |> 
  select(Cluster_ID)

National_Spec.2 <- National_C.2 |> 
  left_join(merged |> filter(Cycle == 2) |> select(Cycle, Cluster_ID, NombreCientifico_APG),
            by = "Cluster_ID") |>
  select(Cycle, NombreCientifico_APG) |> 
  group_by(Cycle, NombreCientifico_APG) |> 
  summarise(species_abundances = n())

National_Spec.2

# Cycle 3
National_C.3 <- National_Base |> 
  filter(Muestreado3 == 1) |> 
  select(Cluster_ID)

National_Spec.3 <- National_C.3 |> 
  left_join(merged |> filter(Cycle == 3) |> select(Cycle, Cluster_ID, NombreCientifico_APG),
            by = "Cluster_ID") |>
  select(Cycle, NombreCientifico_APG) |> 
  group_by(Cycle, NombreCientifico_APG) |> 
  summarise(species_abundances = n())

National_Spec.3

# Step 3: Combine Clusternumbers + Species Abundances ----
# Cylce 1
National_CS.1 <- National_Spec.1 |> 
  left_join(National_Calc.1,
            by = "Cycle") |> 
  relocate(Cycle, number_of_clusters)

# Cycle 2
National_CS.2 <- National_Spec.2 |> 
  left_join(National_Calc.2,
            by = "Cycle") |> 
  relocate(Cycle, number_of_clusters)

# Cycle 3
National_CS.3 <- National_Spec.3 |> 
  left_join(National_Calc.3,
            by = "Cycle") |> 
  relocate(Cycle, number_of_clusters)

# STEP 4: rbind(1,2,3) ----
National_CS <- rbind(National_CS.1, National_CS.2, National_CS.3) |> 
  filter(!is.na(NombreCientifico_APG))

# STEP 5: pivot wider ----
# pivot
National_wide <- National_CS |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = species_abundances)


# exchange all NAs with 0 for calculation
National_wide <- National_wide |> 
  replace(is.na(National_wide), 0)

National_wide

# STEP 6: Rarefaction ----
### IMPORTANT: run this code only with species as cols and samples as rows!!!
m.rar.time <- National_wide[, -c(1:2)]

# preparation step: change integer values to numeric 
m.rar.time <- as.data.frame(lapply(m.rar.time, as.numeric))

National_wide

# rarefaction
a <- min(National_wide$number_of_clusters) # -> 55 days
m3_rarified <- m.rar.time 
for (k in 1:ncol(m.rar.time)) {
  for (i in 1:nrow(m.rar.time)) {
    m3_rarified[i, k] <- m.rar.time[i, k] * (a / National_wide$number_of_clusters[i])
  }
}

# rounding of values
m3_rarified_rd <- trunc(m3_rarified)
commas <- m3_rarified - m3_rarified_rd

upround <- rowSums(commas)

rank <- as.data.frame(t(apply(-commas, 1, order)))

for (k in 1:nrow(m.rar.time)){
  for (i in 1:round(upround[k])) {if(round(upround[k])>0){
    m3_rarified[k, rank[k, i]] <- m3_rarified_rd[k, rank[k, i]] + 1}
  }}

finish <- trunc(m3_rarified)

# return to original dataformat
National_Rare <- cbind(National_wide[1:2], finish)
National_Rare


# pivot to long data format
National_Rare_long <- National_Rare %>% 
  pivot_longer(
    cols = -c(1:2), # Excludes the first two columns
    names_to = "species_names",
    values_to = "values"
  )

# calculate number of species
National_Rare_long |> 
  filter(values != 0) |> 
  group_by(Cycle) |> 
  summarise(n = n())

# STEP 8: Cute Barplot ----
# plot
National_Rare_long |> 
  filter(values != 0) |> 
  group_by(Cycle) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = Cycle, y = n)) +
  geom_bar(stat = "identity")

# STEP 9: Comparison with untouched data and selected conglomerates based ----
# on availability in all datasets + plot availability 

# untouched
merged |> 
  ungroup() |> 
  select(Cycle, NombreCientifico_APG) |> 
  group_by(Cycle) |> 
  summarise(species = n_distinct(NombreCientifico_APG))

#filtered -> calculation in National section
National |> 
  ggplot(aes(x = Cycle, y = n)) +
  geom_bar(stat = "identity")

##################################     END      ##################################################################

##################  HOW DID SUBPLOT SAMPLES CHANGE FOR A GIVEN CONGLOMERATE MAP ##################
# STEP 1: Identifying changed plots ----
Subplot_changes <- Base |> 
  select(Cluster_ID, X, Y, Plot_S1, Plot_S2, Plot_S3) |> 
  mutate(Plot_S12 = Plot_S2 - Plot_S1,
         Plot_23 = Plot_S3 - Plot_S2,
         Plot_13 = Plot_S3 - Plot_S1)

# Step 2: geospatial prep ----
# writeVector(vect(Subplot_changes, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Subplot_changes.shp")

##################################     END      ##################################################################

##################  SPECIES RICHNESS CHANGES MAPS ##################
# STEP 1: Identifying changes per PSU based on available PSUs per cycle and comparison ----
SpeciesRichness <- FullStack_V1 |> 
  mutate(SC1 = case_when(Muestreado1 != 1 | Plot_S1 != 4 ~ NA,
                         T ~ SC1),
         SC2 = case_when(Muestreado2 != 1 | Plot_S2 != 4 ~ NA,
                         T ~ SC2),
         SC3 = case_when(Muestreado3 != 1 | Plot_S3 != 4 ~ NA,
                         T ~ SC3)) |> 
  mutate(SC12 = case_when(Muestreado1 != 1 | Muestreado2 != 1 | Plot_S1 != 4 | Plot_S2 != 4 ~ NA,
                          T ~ SC12),
         SC23 = case_when(Muestreado2 != 1 | Muestreado3 != 1 | Plot_S2 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC23),
         SC13 = case_when(Muestreado1 != 1 | Muestreado3 != 1 | Plot_S1 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC13)) |> 
  mutate(SC12P = case_when(Muestreado1 != 1 | Muestreado2 != 1 | Plot_S1 != 4 | Plot_S2 != 4 ~ NA,
                          T ~ SC12P),
         SC23P = case_when(Muestreado2 != 1 | Muestreado3 != 1 | Plot_S2 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC23P),
         SC13P = case_when(Muestreado1 != 1 | Muestreado3 != 1 | Plot_S1 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC13P)) |> 
  select(X, Y, SC1, SC2, SC3, SC12, SC23, SC13, SC12P, SC23P, SC13P, DESECON2)

# Step 2: geospatial prep ----
# writeVector(vect(SpeciesRichness, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "SpeciesRichness.shp")

##################################     END      ##################################################################



end.time <- Sys.time()
time.taken4 <- end.time - start.time

time.taken1
time.taken2
time.taken3
time.taken4



##############  ECOREGIONS BASED CALCULATION         ###############################################
# STEP 1: add ecoregions to rest of data based on cluster_ID ----
Eco_Calc <- merged |> 
  select(Cluster_ID, Cycle, NombreCientifico_APG) |> 
  left_join(Ecoregions, by = "Cluster_ID")


# STEP 2: Calculate Plots per cluster ----
Plots_1 <- Base |> 
  ungroup() |> 
  select(Cluster_ID, Plot_S1) |> 
  mutate(Cycle = 1,
         number_of_plots = case_when(Plot_S1 == 0 ~ 0,
                                     Plot_S1 == 1 ~ 1,
                                     Plot_S1 == 2 ~ 2,
                                     Plot_S1 == 3 ~ 3,
                                     Plot_S1 == 4 ~ 4,
                                     T ~ Plot_S1)) |> 
  select(-c("Plot_S1"))

# cycle 2
Plots_2 <- Base |> 
  ungroup() |> 
  select(Cluster_ID, Plot_S2) |> 
  mutate(Cycle = 2,
         number_of_plots = case_when(Plot_S2 == 0 ~ 0,
                                     Plot_S2 == 1 ~ 1,
                                     Plot_S2 == 2 ~ 2,
                                     Plot_S2 == 3 ~ 3,
                                     Plot_S2 == 4 ~ 4,
                                     T ~ Plot_S2)) |> 
  select(-c("Plot_S2"))

# cycle 3
Plots_3 <- Base |> 
  ungroup() |> 
  select(Cluster_ID, Plot_S3) |> 
  mutate(Cycle = 3,
         number_of_plots = case_when(Plot_S3 == 0 ~ 0,
                                     Plot_S3 == 1 ~ 1,
                                     Plot_S3 == 2 ~ 2,
                                     Plot_S3 == 3 ~ 3,
                                     Plot_S3 == 4 ~ 4,
                                     T ~ Plot_S3)) |> 
  select(-c("Plot_S3"))

# create long data format
Plots_123 <- rbind(Plots_1, Plots_2, Plots_3)
Plots_123

# STEP 3: Calculate area per cluster ----
Plots_123 <- Plots_123 |> 
  mutate(plot_area = number_of_plots * 400,
         Cycle = as.character(Cycle))

Eco_Calc <- Eco_Calc |> 
  left_join(Plots_123,
            by = c("Cycle", "Cluster_ID"))

Eco_Calc

# STEP 4: Calculate number of clusters per ecoregion ----
Eco_Cluster <- Eco_Calc |> 
  select(Cluster_ID, Cycle, DESECON2, number_of_plots, plot_area) |> 
  group_by(Cycle, DESECON2) |> 
  distinct() |> 
  group_by(Cycle, DESECON2) |> 
  summarise(number_of_clusters = n(),
            total_plots = sum(number_of_plots),
            area = sum(plot_area))

Eco_Calc |> 
  filter(Cycle == "1",
         DESECON2 == "Cuerpos de agua") |> 
  group_by(Cluster_ID) |> 
  count()

Eco_Cluster

# STEP 5: Caculate species abundances per ecoregion ----
Eco_Species <- Eco_Calc |> 
  select(NombreCientifico_APG, Cycle, DESECON2) |> 
  group_by(Cycle, DESECON2, NombreCientifico_APG) |> 
  summarise(species_abundance = n())


# STEP 6: Pivot Eco_Species from long to wide -----
# pivot
Eco_Species_wide <- Eco_Species |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = species_abundance)


# change NA in DESECON to character "NA"
Eco_Species_wide <- Eco_Species_wide |> 
  mutate(DESECON2 = case_when(is.na(DESECON2) ~ "NA",
                              T ~ DESECON2)) 

# exchange all NAs with 0 for calculation
Eco_Species_wide <- Eco_Species_wide |> 
  replace(is.na(Eco_Species_wide), 0)



# STEP 7: Combine Eco_Species and Eco_Cluster + rowSums()----
Eco_wide <- Eco_Cluster |> 
  left_join(Eco_Species_wide,
            by = c("Cycle", "DESECON2")) |> 
  filter(!is.na(DESECON2))

# add number_of_individuals per row
Eco_wide$number_of_individuals <- rowSums(Eco_wide[, -c(1:5)])

# rearange number_of_individuals
Eco_wide <- Eco_wide |> 
  relocate(Cycle, DESECON2, number_of_clusters, total_plots, area, number_of_individuals)
Eco_wide


# STEP 8: Rarefaction Loop Area ---------------
rarefy_ecoregions_area <- function(data) {
  # Extract unique DESECON2 values
  unique_ecoregions <- unique(data$DESECON2)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each ecoregion
  for (ecoregion in unique_ecoregions) {
    # Filter data for the current ecoregion
    Temp <- data |> 
      filter(DESECON2 == ecoregion)
    
    # Rarefaction code adapted from your steps 7 and onward
    m.rar.time <- Temp[, -c(1:6)]
    m.rar.time <- as.data.frame(lapply(m.rar.time, as.numeric))
    a <- min(Temp$area)
    m3_rarified <- m.rar.time 
    for (k in 1:ncol(m.rar.time)) {
      for (i in 1:nrow(m.rar.time)) {
        m3_rarified[i, k] <- m.rar.time[i, k] * (a / Temp$area[i])
      }
    }
    
    # rounding of values
    m3_rarified_rd <- trunc(m3_rarified)
    commas <- m3_rarified - m3_rarified_rd
    
    upround <- rowSums(commas)
    
    rank <- as.data.frame(t(apply(-commas, 1, order)))
    
    for (k in 1:nrow(m.rar.time)){
      for (i in 1:round(upround[k])) {if(round(upround[k])>0){
        m3_rarified[k, rank[k, i]] <- m3_rarified_rd[k, rank[k, i]] + 1}
      }}
    
    finish <- trunc(m3_rarified)
    # Assume 'finish' contains the final data after rarefaction
    
    # Return to original data format (with your specified columns)
    Eco_Temp <- cbind(Temp[1:6], finish)
    
    # Pivot to long format and summarize
    Eco_Temp_Final <- Eco_Temp %>% 
      pivot_longer(cols = -c(1:6), names_to = "species_names", values_to = "values") %>%
      filter(values != 0) %>% 
      group_by(Cycle, DESECON2) %>% 
      summarise(number_of_species = n()) %>% 
      left_join(Eco_Temp |> select(Cycle, DESECON2, number_of_clusters, total_plots, area), by = c("Cycle", "DESECON2"))
    
    # Store the result for this ecoregion
    results_list[[ecoregion]] <- Eco_Temp_Final
  }
  
  # Combine all ecoregion results into one dataframe
  final_result <- bind_rows(results_list)
  
  return(final_result)
}

# Apply the function to your dataset
final_ecoregion_area <- rarefy_ecoregions_area(Eco_wide)
print(final_ecoregion_area, n = 70)


View(final_ecoregion_area |> 
       select(Cycle, DESECON2, total_plots, area, number_of_species))

#write.csv(final_ecoregion_data, file = "Rarefied_Ecoregions_area.csv")
# STEP 11: cute barplot area ----
prepared <- final_ecoregion_area %>%
  filter(DESECON2 == "Sistema Neovolcanico Transversal")

# with SSU text
prepared |> 
  ggplot(aes(x = Cycle, y = number_of_species)) +
  geom_col() +
  geom_text(aes(label = number_of_species), vjust = 1.5, colour = "white") +
  theme_minimal() +
  labs(title = prepared$DESECON2) +
  ylab("# of species") +
  annotate("text", x=Inf, y=Inf, label= sprintf("SSUs: %s ", format(min(prepared$total_plots), big.mark = " ", scientific = FALSE)),
           hjust = 1.25, vjust = 1)



# with area text
prepared |> 
  ggplot(aes(x = Cycle, y = number_of_species)) +
  geom_col() +
  geom_text(aes(label = number_of_species), vjust = 1.5, colour = "white") +
  theme_minimal() +
  labs(title = prepared$DESECON2) +
  ylab("# of species") +
  annotate("text", x=Inf, y=Inf, label= sprintf("Area: %s ", format(min(prepared$area), big.mark = " ", scientific = FALSE)),
           hjust = 1.25, vjust = 1.5) +
  annotate("text", x=Inf, y=Inf, label= "m^2", parse = T, hjust = 1, vjust = 1)

# STEP 9: Rarefaction Loop Individuals ---------------
rarefy_ecoregions_ind <- function(data) {
  # Extract unique DESECON2 values
  unique_ecoregions <- unique(data$DESECON2)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each ecoregion
  for (ecoregion in unique_ecoregions) {
    # Filter data for the current ecoregion
    Temp <- data |> 
      filter(DESECON2 == ecoregion)
    
    # Rarefaction code adapted from your steps 7 and onward
    m.rar.time <- Temp[, -c(1:6)]
    m.rar.time <- as.data.frame(lapply(m.rar.time, as.numeric))
    a <- min(Temp$number_of_individuals)
    m3_rarified <- m.rar.time 
    for (k in 1:ncol(m.rar.time)) {
      for (i in 1:nrow(m.rar.time)) {
        m3_rarified[i, k] <- m.rar.time[i, k] * (a / Temp$number_of_individuals[i])
      }
    }
    
    # rounding of values
    m3_rarified_rd <- trunc(m3_rarified)
    commas <- m3_rarified - m3_rarified_rd
    
    upround <- rowSums(commas)
    
    rank <- as.data.frame(t(apply(-commas, 1, order)))
    
    for (k in 1:nrow(m.rar.time)){
      for (i in 1:round(upround[k])) {if(round(upround[k])>0){
        m3_rarified[k, rank[k, i]] <- m3_rarified_rd[k, rank[k, i]] + 1}
      }}
    
    finish <- trunc(m3_rarified)
    # Assume 'finish' contains the final data after rarefaction
    
    # Return to original data format (with your specified columns)
    Eco_Temp <- cbind(Temp[1:6], finish)
    
    # Pivot to long format and summarize
    Eco_Temp_Final <- Eco_Temp %>% 
      pivot_longer(cols = -c(1:5), names_to = "species_names", values_to = "values") %>%
      filter(values != 0) %>% 
      group_by(Cycle, DESECON2) %>% 
      summarise(number_of_species = n()) %>% 
      left_join(Eco_Temp |> select(Cycle, DESECON2, number_of_clusters, total_plots, area, number_of_individuals), by = c("Cycle", "DESECON2"))
    
    # Store the result for this ecoregion
    results_list[[ecoregion]] <- Eco_Temp_Final
  }
  
  # Combine all ecoregion results into one dataframe
  final_result <- bind_rows(results_list)
  
  return(final_result)
}

# Apply the function to your dataset
final_ecoregion_ind <- rarefy_ecoregions_ind(Eco_wide)
print(final_ecoregion_ind, n = 70)

View(final_ecoregion_ind |> 
       select(Cycle, DESECON2, total_plots, number_of_individuals, number_of_species))

#write.csv(final_ecoregion_data, file = "Rarefied_Ecoregions_ind.csv")


# STEP 10: Rarefaction Loop Individuals rarefy() ----- #needs to still be looped -----

Temp <- Eco_wide |>
  filter(DESECON2 == "Desiertos Calidos")

m.rar.time <- Temp[, -c(1:6)]
m.rar.time <- as.data.frame(lapply(m.rar.time, as.numeric))
a <- min(rowSums(m.rar.time))

rarefied_data <- rarefy(m.rar.time, a)
rarefied_data



rarefy_ecoregions <- function(data) {
  # Extract unique DESECON2 values
  unique_ecoregions <- unique(data$DESECON2)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each ecoregion
  for (ecoregion in unique_ecoregions) {
    # Filter data for the current ecoregion
    Temp <- data |> 
      filter(DESECON2 == ecoregion)
    
    # Rarefaction code adapted from your steps 7 and onward
    m3_rarified <- rarefy(m.rar.time, a)
    
    
    # rounding of values
    m3_rarified_rd <- trunc(m3_rarified)
    commas <- m3_rarified - m3_rarified_rd
    
    upround <- rowSums(commas)
    
    rank <- as.data.frame(t(apply(-commas, 1, order)))
    
    for (k in 1:nrow(m.rar.time)){
      for (i in 1:round(upround[k])) {if(round(upround[k])>0){
        m3_rarified[k, rank[k, i]] <- m3_rarified_rd[k, rank[k, i]] + 1}
      }}
    
    finish <- trunc(m3_rarified)
    # Assume 'finish' contains the final data after rarefaction
    
    # Return to original data format (with your specified columns)
    Eco_Temp <- cbind(Temp[1:6], finish)
    
    # Pivot to long format and summarize
    Eco_Temp_Final <- Eco_Temp %>% 
      pivot_longer(cols = -c(1:6), names_to = "species_names", values_to = "values") %>%
      filter(values != 0) %>% 
      group_by(Cycle, DESECON2) %>% 
      summarise(number_of_species = n()) %>% 
      left_join(Eco_Temp |> select(Cycle, DESECON2, number_of_clusters, total_plots, area, number_of_individuals), by = c("Cycle", "DESECON2"))
    
    # Store the result for this ecoregion
    results_list[[ecoregion]] <- Eco_Temp_Final
  }
  
  # Combine all ecoregion results into one dataframe
  final_result <- bind_rows(results_list)
  
  return(final_result)
}

# Apply the function to your dataset
#final_ecoregion_data <- rarefy_ecoregions(Eco_wide)
#print(final_ecoregion_data, n = 70)

#View(final_ecoregion_data |> 
#      select(Cycle, DESECON2, total_plots, area, number_of_species))

#write.csv(final_ecoregion_data, file = "Rarefied_Ec

#needs to still be looped
# STEP 11: cute barplot ----
prepared <- final_ecoregion_ind %>%
  filter(DESECON2 == "Sistema Neovolcanico Transversal")

# with SSU text
prepared |> 
  ggplot(aes(x = Cycle, y = number_of_species)) +
  geom_col() +
  geom_text(aes(label = number_of_species), vjust = 1.5, colour = "white") +
  theme_minimal() +
  labs(title = prepared$DESECON2) +
  ylab("# of species") +
  annotate("text", x=Inf, y=Inf, label= sprintf("# individuals: %s ", format(min(prepared$number_of_individuals), big.mark = " ", scientific = FALSE)),
           hjust = 1.25, vjust = 1)



# with area text
prepared |> 
  ggplot(aes(x = Cycle, y = number_of_species)) +
  geom_col() +
  geom_text(aes(label = number_of_species), vjust = 1.5, colour = "white") +
  theme_minimal() +
  labs(title = prepared$DESECON2) +
  ylab("# of species") +
  annotate("text", x=Inf, y=Inf, label= sprintf("Area: %s ", format(min(prepared$area), big.mark = " ", scientific = FALSE)),
           hjust = 1.25, vjust = 1.5) +
  annotate("text", x=Inf, y=Inf, label= "m^2", parse = T, hjust = 1, vjust = 1)

# STEP 12: comparison with shared PSUs ----
# create individual calculations
# cycle 1
Test1 <- Base |> left_join(Eco_Calc |>
                             select(Cluster_ID, Cycle, NombreCientifico_APG) |> 
                             filter(Cycle == 1),
                           by = "Cluster_ID") |> 
  c123_filter() |> 
  mutate(Cycle = case_when(is.na(Cycle) ~ "1",
                           T ~ Cycle)) |> 
  ungroup()

Test1 |> c123_filter() |> select(Cluster_ID, DESECON2) |> distinct() |>  group_by(DESECON2) |>  count()

Test1 |> c123_filter() |> group_by(DESECON2) |> 
  summarise(Cycle = mean(as.numeric(Cycle)),
            number_of_cluster = n_distinct(Cluster_ID),
            number_of_species = n_distinct(NombreCientifico_APG)) |> 
  print(n = 100)



# cycle 2
Test2 <- Base |> left_join(Eco_Calc |>
                             select(Cluster_ID, Cycle, NombreCientifico_APG) |> 
                             filter(Cycle == 2),
                           by = "Cluster_ID") |> 
  c123_filter() |> 
  mutate(Cycle = case_when(is.na(Cycle) ~ "2",
                           T ~ Cycle)) |> 
  ungroup()

Test2 |> c123_filter() |> select(Cluster_ID, DESECON2) |> distinct() |>  group_by(DESECON2) |>  count()

Test2 |> c123_filter() |> group_by(DESECON2) |> 
  summarise(Cycle = mean(as.numeric(Cycle)),
            number_of_cluster = n_distinct(Cluster_ID),
            number_of_species = n_distinct(NombreCientifico_APG))


# cycle 3
Test3 <- Base |> left_join(Eco_Calc |>
                             select(Cluster_ID, Cycle, NombreCientifico_APG) |> 
                             filter(Cycle == 3),
                           by = "Cluster_ID") |> 
  c123_filter() |> 
  mutate(Cycle = case_when(is.na(Cycle) ~ "3",
                           T ~ Cycle)) |> 
  ungroup()

Test3 |> 
  select(Cluster_ID) |> 
  distinct() |> 
  count()

Test3 |> c123_filter() |> group_by(DESECON2) |> 
  summarise(Cycle = mean(as.numeric(Cycle)),
            number_of_cluster = n_distinct(Cluster_ID),
            number_of_species = n_distinct(NombreCientifico_APG))

# combine
Test <- rbind(Test1 |> c123_filter() |> group_by(DESECON2) |> 
                summarise(Cycle = mean(as.numeric(Cycle)),
                          number_of_cluster = n_distinct(Cluster_ID),
                          number_of_species = n_distinct(NombreCientifico_APG)),
              Test2 |> c123_filter() |> group_by(DESECON2) |> 
                summarise(Cycle = mean(as.numeric(Cycle)),
                          number_of_cluster = n_distinct(Cluster_ID),
                          number_of_species = n_distinct(NombreCientifico_APG)),
              Test3 |> c123_filter() |> group_by(DESECON2) |> 
                summarise(Cycle = mean(as.numeric(Cycle)),
                          number_of_cluster = n_distinct(Cluster_ID),
                          number_of_species = n_distinct(NombreCientifico_APG))) |> 
  arrange(DESECON2)

# results (+/- 1 for )
Test |> 
  print(n = 100)

# STEP 11: cute barplot v2 ----
prepared <- Test %>%
  mutate(total_plots = number_of_cluster*4) |> 
  filter(DESECON2 == "Sistema Neovolcanico Transversal")


# with SSU text
prepared |> 
  ggplot(aes(x = Cycle, y = number_of_species)) +
  geom_col() +
  geom_text(aes(label = number_of_species), vjust = 1.5, colour = "white") +
  theme_minimal() +
  labs(title = prepared$DESECON2) +
  ylab("# of species") +
  annotate("text", x=Inf, y=Inf, label= sprintf("SSUs: %s ", format(min(prepared$total_plots), big.mark = " ", scientific = FALSE)),
           hjust = 1.25, vjust = 1)

# empty plot for empty ecoregions
ggplot() +
  geom_blank() +
  theme_minimal() +
  scale_x_continuous(breaks=c(1, 2, 3), limits=c(0.55, 3.45)) +
  scale_y_continuous(limits=c(0, 1)) +
  labs(title = "Planicie costera de Texas-Louisiana") +
  ylab("# of species") +
  xlab("Cycle") +
  annotate("text", x=Inf, y=Inf, label= "SSUs:0 ",
           hjust = 1.25, vjust = 1)



# STEP 13: Comparison with shared PSUs - Rarefaction for Individuals ----
# create data base
PSU_long <- rbind(Test1 |> 
                    c123_filter() |> 
                    select(Cycle, DESECON2, NombreCientifico_APG) |> 
                    group_by(Cycle, DESECON2, NombreCientifico_APG) |> 
                    summarise(abundances = n()),
                  Test2 |> 
                    c123_filter() |> 
                    select(Cycle, DESECON2, NombreCientifico_APG) |> 
                    group_by(Cycle, DESECON2, NombreCientifico_APG) |> 
                    summarise(abundances = n()),
                  Test3 |> 
                    c123_filter() |> 
                    select(Cycle, DESECON2, NombreCientifico_APG) |> 
                    group_by(Cycle, DESECON2, NombreCientifico_APG) |> 
                    summarise(abundances = n())) |> 
  arrange(DESECON2)

# pivot wider - shorter because one ecoregion is missing in data 3 and is therefore not shared
PSU_wide <- PSU_long |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundances) |> 
  filter(!is.na(DESECON2))

# exchange all NAs with 0 for calculation
PSU_wide <- PSU_wide |> 
  replace(is.na(PSU_wide), 0)

# add number_of_individuals per row
PSU_wide$number_of_individuals <- rowSums(PSU_wide[, -c(1:2)])

PSU_wide <- PSU_wide |> 
  relocate(Cycle, DESECON2, number_of_individuals)
PSU_wide

rarefy_ecoregions_PSU <- function(data) {
  # Extract unique DESECON2 values
  unique_ecoregions <- unique(data$DESECON2)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each ecoregion
  for (ecoregion in unique_ecoregions) {
    # Filter data for the current ecoregion
    Temp <- data |> 
      filter(DESECON2 == ecoregion)
    
    # Rarefaction code adapted from your steps 7 and onward
    m.rar.time <- Temp[, -c(1:3)]
    m.rar.time <- as.data.frame(lapply(m.rar.time, as.numeric))
    a <- min(Temp$number_of_individuals)
    m3_rarified <- m.rar.time 
    for (k in 1:ncol(m.rar.time)) {
      for (i in 1:nrow(m.rar.time)) {
        m3_rarified[i, k] <- m.rar.time[i, k] * (a / Temp$number_of_individuals[i])
      }
    }
    
    # rounding of values
    m3_rarified_rd <- trunc(m3_rarified)
    commas <- m3_rarified - m3_rarified_rd
    
    upround <- rowSums(commas)
    
    rank <- as.data.frame(t(apply(-commas, 1, order)))
    
    for (k in 1:nrow(m.rar.time)){
      for (i in 1:round(upround[k])) {if(round(upround[k])>0){
        m3_rarified[k, rank[k, i]] <- m3_rarified_rd[k, rank[k, i]] + 1}
      }}
    
    finish <- trunc(m3_rarified)
    # Assume 'finish' contains the final data after rarefaction
    
    # Return to original data format (with your specified columns)
    Eco_Temp <- cbind(Temp[1:3], finish)
    
    # Pivot to long format and summarize
    Eco_Temp_Final <- Eco_Temp %>% 
      pivot_longer(cols = -c(1:3), names_to = "species_names", values_to = "values") %>%
      filter(values != 0) %>% 
      group_by(Cycle, DESECON2) %>% 
      summarise(number_of_species = n()) %>% 
      left_join(Eco_Temp |> select(Cycle, DESECON2, number_of_individuals), by = c("Cycle", "DESECON2"))
    
    # Store the result for this ecoregion
    results_list[[ecoregion]] <- Eco_Temp_Final
  }
  
  # Combine all ecoregion results into one dataframe
  final_result <- bind_rows(results_list)
  
  return(final_result)
}

# Apply the function to your dataset
final_ecoregion_PSU <- rarefy_ecoregions_PSU(PSU_wide)
print(final_ecoregion_PSU, n = 70)

View(final_ecoregion_PSU |> 
       select(Cycle, DESECON2, number_of_individuals, number_of_species))

#write.csv(final_ecoregion_data, file = "Rarefied_Ecoregions_ind.csv")



# STEP 11: cute barplot ----
prepared <- final_ecoregion_PSU %>%
  filter(DESECON2 == "Sistema Neovolcanico Transversal")

# with #individuals text
prepared |> 
  ggplot(aes(x = Cycle, y = number_of_species)) +
  geom_col() +
  geom_text(aes(label = number_of_species), vjust = 1.5, colour = "white") +
  theme_minimal() +
  labs(title = prepared$DESECON2) +
  ylab("# of species") +
  annotate("text", x=Inf, y=Inf, label= sprintf("# individuals: %s ", format(min(prepared$number_of_individuals), big.mark = " ", scientific = FALSE)),
           hjust = 1.25, vjust = 1)
##################################     END      ##################################################################

##############  ECOREGIONS BASED DEGRADATION MAP         ###############################################
Eco_Map <- Base |> 
  select(Cluster_ID, X, Y, DESECON2) |> 
  mutate(Change_Area = case_when(DESECON2 == "Altiplanicie Mexicana" ~ "small gain",
                            DESECON2 == "California Mediterranea" ~ "large gain",
                            DESECON2 == "Cuerpos de agua" ~ "no change",
                            DESECON2 == "Depresiones Intermontanas" ~ "small gain",
                            DESECON2 == "Desiertos Calidos" ~ "large loss",
                            DESECON2 == "Piedemonte de la Sierra Madre Occidental" ~ "large loss",
                            DESECON2 == "Planicie Costera y Lomerios Humedos del Golfo de Mexico" ~ "no change",
                            DESECON2 == "Planicie Costera y Lomerios del Pacifico Sur" ~ "small loss",
                            DESECON2 == "Planicie Costera y Lomerios del Soconusco" ~ "small loss",
                            DESECON2 == "Planicie Costera, Lomerios y Canones del Occidente" ~ "large loss",
                            DESECON2 == "Planicie Noroccidental de la  Peninsula de Yucatan" ~ "small loss",
                            DESECON2 == "Planicie costera de Texas-Louisiana" ~ NA,
                            DESECON2 == "Planicie semiarida de Tamaulipas-Texas" ~ "large loss",
                            DESECON2 == "Planicie y Lomerios de la Peninsula de Yucatan" ~ "no change",
                            DESECON2 == "Planicies Costeras y Lomerios Secos del Golfo de Mexico" ~ "large loss",
                            DESECON2 == "Planicies y Lomerios del Occidente" ~ "large loss",
                            DESECON2 == "Sierra Madre Centroamericana y Altos de Chiapas" ~ "small loss",
                            DESECON2 == "Sierra Madre Occidental" ~ "large loss",
                            DESECON2 == "Sierra Madre Oriental" ~ "small gain",
                            DESECON2 == "Sierra Madre del Sur" ~ "no change",
                            DESECON2 == "Sierra de Los Tuxtlas" ~ "no change",
                            DESECON2 == "Sierra y Planicies de El Cabo" ~ "large loss",
                            DESECON2 == "Sistema Neovolcanico Transversal" ~ "small loss",
                            T ~ NA))

Eco_Map

# writeVector(vect(Eco_Map, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Eco_Map.shp")
##################################     END      ##################################################################






##################          IR-MAD CHANGE DETECTION PREPARATION            ----------------------------------
################## 1) Cycle 1-2 -------------------------------------------
#### STEP 1: dissecting dataframe by Cycle + add Cycle grouping variable (CONSTANT CLUSTERS) -----
## Cycle 1
Cycle1 <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1, X, Y) |> 
  # changing variable names to Cycle-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH1,
         AvgCrownDiameter = CD1,
         AvgCrownHeight = CH1,
         AvgCrownArea = CA1,
         SpeciesCount = SC1,
         AvgTreeHeight = TH1,
         TreeCount = TE1,
         J = J1) |> 
  # add Cycle grouping
  mutate(Cycle = 1) |> 
  relocate(Cycle)

Cycle1

## Cycle 2
Cycle2 <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2, X, Y) |> 
  # changing variable names to Cycle-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH2,
         AvgCrownDiameter = CD2,
         AvgCrownHeight = CH2,
         AvgCrownArea = CA2,
         SpeciesCount = SC2,
         AvgTreeHeight = TH2,
         TreeCount = TE2,
         J = J2) |> 
  #add Cycle grouping
  mutate(Cycle = 2) |> 
  relocate(Cycle)

Cycle2

#### STEP 2: merge into long data format ----
Cycle12 <- rbind(Cycle1, Cycle2)


#### STEP 3: write .csv for python ----

# write.csv(Cycle12, "iMAD_Data_12.csv")
########### CUT -------------------------------------------------------------------------------------------------

################## 2) Comparison of Cycle 2 and 3 - FILTER: Only Clusters that are available for 2 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by Cycle + add Cycle grouping variable (CONSTANT CLUSTERS) -----
## Cycle 2
Cycle2 <- FullStack_V1 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2, X, Y) |> 
  # changing variable names to Cycle-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH2,
         AvgCrownDiameter = CD2,
         AvgCrownHeight = CH2,
         AvgCrownArea = CA2,
         SpeciesCount = SC2,
         AvgTreeHeight = TH2,
         TreeCount = TE2,
         J = J2) |> 
  #add Cycle grouping
  mutate(Cycle = 2) |> 
  relocate(Cycle)

Cycle2

## Cycle 3
Cycle3 <- FullStack_V1 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3, X, Y) |> 
  # changing variable names to Cycle-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH3,
         AvgCrownDiameter = CD3,
         AvgCrownHeight = CH3,
         AvgCrownArea = CA3,
         SpeciesCount = SC3,
         AvgTreeHeight = TH3,
         TreeCount = TE3,
         J = J3) |> 
  #add Cycle grouping
  mutate(Cycle = 3) |> 
  relocate(Cycle)

Cycle3

#### STEP 2: merge into long data format ----
Cycle23 <- rbind(Cycle2, Cycle3)


#### STEP 3: write .csv for python ----

# write.csv(Cycle23, "iMAD_Data_23.csv")
########### CUT -------------------------------------------------------------------------------------------------

################## 3) Comparison of Cycle 1 and 3 - FILTER: Only Clusters that are available for 1 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by Cycle + add Cycle grouping variable (CONSTANT CLUSTERS) -----
## Cycle 1
Cycle1 <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1, X, Y) |> 
  # changing variable names to Cycle-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH1,
         AvgCrownDiameter = CD1,
         AvgCrownHeight = CH1,
         AvgCrownArea = CA1,
         SpeciesCount = SC1,
         AvgTreeHeight = TH1,
         TreeCount = TE1,
         J = J1) |> 
  # add Cycle grouping
  mutate(Cycle = 1) |> 
  relocate(Cycle)

Cycle1

## Cycle 3
Cycle3 <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3, X, Y) |> 
  # changing variable names to Cycle-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH3,
         AvgCrownDiameter = CD3,
         AvgCrownHeight = CH3,
         AvgCrownArea = CA3,
         SpeciesCount = SC3,
         AvgTreeHeight = TH3,
         TreeCount = TE3,
         J = J3) |> 
  #add Cycle grouping
  mutate(Cycle = 3) |> 
  relocate(Cycle)

Cycle3

#### STEP 2: merge into long data format ----
Cycle13 <- rbind(Cycle1, Cycle3)


#### STEP 3: write .csv for python ----

# write.csv(Cycle13, "iMAD_Data_13.csv")
##################################     END      ##################################################################



##################          IR-MAD CHANGE DETECTION RESULTS            ----------------------------------
################## 1) Comparison 1-2 -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_12 <- Raw.04 <- fread(here("data", "iMAD", "[1] iMAD Results", "iMAD_results_12.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_12 <- iMAD_results_12 |> 
  rename(Col_1 = SpeciesCount,
         Col_2 = TreeCount,
         Col_3 = J,
         Col_4 = AvgTreeHeight,
         Col_5 = AvgDbh,
         Col_6 = AvgCrownDiameter,
         Col_7 = AvgCrownHeight,
         Col_8 = AvgCrownArea) |> 
  mutate(Comparison = as.factor("Cycle12")) |> 
  relocate(Comparison)

#### STEP 4: Chi-squared classification - only run from start! -----
summary(iMAD_results_12$chi_squared)

iMAD_results_12 <- iMAD_results_12 |> 
  mutate(chi_squared = case_when(chi_squared <= quantile(chi_squared, 0.90, df=8) ~ 0,
                                 chi_squared > quantile(chi_squared, 0.99, df=8) ~ 3,
                                 chi_squared <= quantile(chi_squared, 0.99, df=8) & chi_squared > quantile(chi_squared, 0.95, df=8) ~ 2,
                                 chi_squared <= quantile(chi_squared, 0.95, df=8) & chi_squared > quantile(chi_squared, 0.90, df=8) ~ 1,
                                 T ~ chi_squared))



#### STEP 5: Direction of change ----
iMAD_results_12 <- iMAD_results_12 |> 
  mutate(direction_of_change = case_when(chi_squared == 0 ~ 0,
                                         chi_squared == 3 & Col_1 > 0 ~ 3,
                                         chi_squared == 3 & Col_1 < 0 ~ -3,
                                         chi_squared == 2 & Col_1 > 0 ~ 2,
                                         chi_squared == 2 & Col_1 < 0 ~ -2,
                                         chi_squared == 1 & Col_1 > 0 ~ 1,
                                         chi_squared == 1 & Col_1 < 0 ~ -1))


#### STEP 6: Geospatial Prep ----
# writeVector(vect(iMAD_results_12, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_12.shp")

########### CUT -------------------------------------------------------------------------------
  
################## 2) Comparison 2-3 -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_23 <- Raw.04 <- fread(here("data", "iMAD", "[1] iMAD Results", "iMAD_results_23.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_23 <- iMAD_results_23 |> 
  rename(Col_1 = SpeciesCount,
         Col_2 = TreeCount,
         Col_3 = J,
         Col_4 = AvgTreeHeight,
         Col_5 = AvgDbh,
         Col_6 = AvgCrownDiameter,
         Col_7 = AvgCrownHeight,
         Col_8 = AvgCrownArea) |> 
  mutate(Comparison = as.factor("Cycle23")) |> 
  relocate(Comparison)

#### STEP 4: Chi-squared classification - only run from start! -----
summary(iMAD_results_23$chi_squared)

iMAD_results_23 <- iMAD_results_23 |> 
  mutate(chi_squared = case_when(chi_squared <= quantile(chi_squared, 0.90, df=8) ~ 0,
                                 chi_squared > quantile(chi_squared, 0.99, df=8) ~ 3,
                                 chi_squared <= quantile(chi_squared, 0.99, df=8) & chi_squared > quantile(chi_squared, 0.95, df=8) ~ 2,
                                 chi_squared <= quantile(chi_squared, 0.95, df=8) & chi_squared > quantile(chi_squared, 0.90, df=8) ~ 1,
                                 T ~ chi_squared))
#### STEP 5: Direction of change ----
iMAD_results_23 <- iMAD_results_23 |> 
  mutate(direction_of_change = case_when(chi_squared == 0 ~ 0,
                                         chi_squared == 3 & Col_1 > 0 ~ 3,
                                         chi_squared == 3 & Col_1 < 0 ~ -3,
                                         chi_squared == 2 & Col_1 > 0 ~ 2,
                                         chi_squared == 2 & Col_1 < 0 ~ -2,
                                         chi_squared == 1 & Col_1 > 0 ~ 1,
                                         chi_squared == 1 & Col_1 < 0 ~ -1))


#### STEP 6: Geospatial Prep ----

# writeVector(vect(iMAD_results_23, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_23.shp")

########### CUT -------------------------------------------------------------------------------------------------

################## 3) Comparison 1-3 -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_13 <- Raw.04 <- fread(here("data", "iMAD", "[1] iMAD Results", "iMAD_results_13.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_13 <- iMAD_results_13 |> 
  rename(Col_1 = SpeciesCount,
         Col_2 = TreeCount,
         Col_3 = J,
         Col_4 = AvgTreeHeight,
         Col_5 = AvgDbh,
         Col_6 = AvgCrownDiameter,
         Col_7 = AvgCrownHeight,
         Col_8 = AvgCrownArea) |> 
  mutate(Comparison = as.factor("Cycle13")) |> 
  relocate(Comparison)

#### STEP 3: Chi-squared classification - only run from start! -----
summary(iMAD_results_13$chi_squared)

iMAD_results_13 <- iMAD_results_13 |> 
  mutate(chi_squared = case_when(chi_squared <= quantile(chi_squared, 0.90, df=8) ~ 0,
                                 chi_squared > quantile(chi_squared, 0.99, df=8) ~ 3,
                                 chi_squared <= quantile(chi_squared, 0.99, df=8) & chi_squared > quantile(chi_squared, 0.95, df=8) ~ 2,
                                 chi_squared <= quantile(chi_squared, 0.95, df=8) & chi_squared > quantile(chi_squared, 0.90, df=8) ~ 1,
                                 T ~ chi_squared))



#### STEP 4: Direction of change ----
iMAD_results_13 <- iMAD_results_13 |> 
  mutate(direction_of_change = case_when(chi_squared == 0 ~ 0,
                                         chi_squared == 3 & Col_1 > 0 ~ 3,
                                         chi_squared == 3 & Col_1 < 0 ~ -3,
                                         chi_squared == 2 & Col_1 > 0 ~ 2,
                                         chi_squared == 2 & Col_1 < 0 ~ -2,
                                         chi_squared == 1 & Col_1 > 0 ~ 1,
                                         chi_squared == 1 & Col_1 < 0 ~ -1))

iMAD_results_13
#### STEP 5: Geospatial Prep ----
# writeVector(vect(iMAD_results_13, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_13.shp")
########### CUT -------------------------------------------------------------------------------------------------

################## 4) CrOSS COMPARISON - FILTER: CONSTANT CLUSTERS -------------------------------------------
## STEP 1: Data Preparation ----
iMAD_results <- rbind(iMAD_results_12, iMAD_results_13, iMAD_results_23) |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13")))
########### CUT -------------------------------------------------------------------------------------------------

end.time <- Sys.time()
time.taken6 <- end.time - start.time

time.taken6





################### PAPER/BA PLOTS (DATA: FullStack_V4 & FullStack_V4_Zeros) -------------------------------------------------------------
#### 1) Violinplots --------------------------------------------
### 1.1) Species Richness Changes ----

# Based on Fullstack_V1 with comparison sample size
FullStack_V1 %>% 
  mutate(SC12 = case_when(Muestreado1 != 1 | Muestreado2 != 1 | Plot_S1 != 4 | Plot_S2 != 4 ~ NA,
                          T ~ SC12),
         SC23 = case_when(Muestreado2 != 1 | Muestreado3 != 1 | Plot_S2 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC23),
         SC13 = case_when(Muestreado1 != 1 | Muestreado3 != 1 | Plot_S1 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC13)) |> 
  select(SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC12", "SC23", "SC13"))) |> 
  ggplot( aes(x= Comparison, y= Species_Richness, fill=Comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_violin(scale = "area", adjust = 1, width = 1) +
  scale_fill_manual(values=c(SC12="#eeeeee", SC23="#aaaaaa", SC13="#666666")) +
  theme_bw() +
  theme(
    panel.background = element_blank(), # Remove panel background
    panel.grid.major = element_line("gray80"), # Keep major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    legend.position = "none", # Remove legend # Add axes lines
  ) +
  scale_y_continuous(breaks = seq(-40, 40, by = 5)) + 
  ylab("Species richness change (# of individuals)") +
  xlab("Comparison") 




#eeeeee


### 1.3) iMAD ----
## iMAD_results_Constant_Zeros 
# STEP 1: Data Preparation 
iMAD_results<- rbind(iMAD_results_12, iMAD_results_13, iMAD_results_23) |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13")))
# STEP 2: Plotting 
## Column 1
iMAD_results |> 
  ggplot(aes(x = Comparison, y = Col_1, fill = Comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_violin(scale = "area", adjust = 1) +
  scale_fill_manual(values=c(Cycle12="#cabad0", Cycle23="#ae96b5", Cycle13="#7c4e87")) +
  theme_bw() +
  theme(
    panel.background = element_blank(), # Remove panel background
    panel.grid.major = element_line("gray80"), # Keep major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    legend.position = "none", # Remove legend # Add axes lines
  ) +
  scale_y_continuous(breaks = seq(-40, 40, by = 10)) + 
  ylab("MAD-Component 1") +
  xlab("") +
  coord_cartesian(ylim = c(-25, 25))



## rest of the columns ----
## Column 1
iMAD_results |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13"))) |> 
  ggplot(aes(x = Comparison, y = Col_1, fill = Comparison)) +
  geom_violin(scale = "area", adjust = 1) +
  coord_cartesian(ylim = c(-25, 25)) +
  scale_fill_manual(values=c(Cycle12="#eeeeee", Cycle23="#aaaaaa", Cycle13="#666666")) +
  theme_bw() +
  theme(
    panel.background = element_blank(), # Remove panel background
    panel.grid.major = element_line("gray80"), # Keep major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    legend.position = "none", # Remove legend # Add axes lines
  )



## Column 3
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_3, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))
## Column 4
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_4, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))
## Column 5
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_5, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))
## Column 6
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_6, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))

## Column 7
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_7, fill = Comparison)) +
  geom_violin()

## Column 8
iMAD_results |> 
  ggplot(aes(x = Comparison, y = Col_8, fill = Comparison)) +
  geom_violin()


#### 2) Scatterplots -------------------------------------------
#### 2.1) Species Richness ----
# Based on Fullstack_V1 with differing sample sizes for each comparison
# Cycle 1 - 2:
FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Plot_S1 == 4 & Plot_S2 == 4) |> 
  ggplot(aes(x = SC1, y = SC2)) +
  geom_point(position = "jitter", color = "black") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red") +
  xlab("Species Richness (Cycle 1)") + ylab("Species Richness (Cycle 2)") +
  theme_light() +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11)) +
  annotate("text", x=10, y=60, label = "n = 17 239", size = 4, hjust = 0.5, vjust = 0.5)

# ggsave(here("Plots", "Thesis Plots", "C12_Scatterplot.svg"), width = 6, height = 6)

# Cycle 2 - 3:
FullStack_V1 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1 & Plot_S2 == 4 & Plot_S3 == 4) |> 
  ggplot(aes(x = SC2, y = SC3)) +
  geom_point(position = "jitter", color = "black") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red") +
  xlab("Species Richness (Cycle 2)") + ylab("Species Richness (Cycle 3)") +
  theme_light() +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11)) +
  annotate("text", x=10, y=60, label = "n = 9 400", size = 4, hjust = 0.5, vjust = 0.5)

# ggsave(here("Plots", "Thesis Plots", "C23_Scatterplot.svg"), width = 6, height = 6)

# Cycle 1 - 3:
FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1 & Plot_S1 == 4 & Plot_S3 == 4) |> 
  ggplot(aes(x = SC1, y = SC3)) +
  geom_point(position = "jitter", color = "black") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red") +
  xlab("Species Richness (Cycle 1)") + ylab("Species Richness (Cycle 3)") +
  theme_light() +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11)) +
  annotate("text", x=10, y=60, label = "n = 8 532", size = 4, hjust = 0.5, vjust = 0.5)

# ggsave(here("Plots", "Thesis Plots", "C13_Scatterplot.svg"), width = 6, height = 6)



#### 3) Ridges ------------------------------------------------- 
## 1.1) Species Richness Distribution ----
# STEP 1: assign levels ----
Lvl1 <- c("California Mediterranea")
Lvl2 <- c("Desiertos Calidos")
Lvl3 <- c("Piedemonte de la Sierra Madre Occidental", "Altiplanicie Mexicana")
Lvl4 <- c("Planicie semiarida de Tamaulipas-Texas", "Planicie costera de Texas-Louisiana")
Lvl5 <- c("Planicie y Lomerios de la Peninsula de Yucatan", "Planicie Costera y Lomerios Humedos del Golfo de Mexico", 
          "Planicies y Lomerios del Occidente", "Planicie Costera y Lomerios del Soconusco", "Sierra de Los Tuxtlas")
Lvl6 <- c("Planicie Costera y Lomerios del Pacifico Sur", "Planicie Costera, Lomerios y Canones del Occidente",
          "Depresiones Intermontanas", "Planicies Costeras y Lomerios Secos del Golfo de Mexico", "Sierra y Planicies de El Cabo",
          "Planicie Noroccidental de la  Peninsula de Yucatan")
Lvl7 <- c("Sierra Madre Centroamericana y Altos de Chiapas", "Sierra Madre Occidental", "Sierra Madre Oriental", 
          "Sierra Madre del Sur", "Sistema Neovolcanico Transversal")
Lvl8 <- c("Cuerpos de agua")

all_levels <- c(Lvl1, Lvl2, Lvl3, Lvl4, Lvl5, Lvl6, Lvl7, Lvl8)

FullStack_V1$DESECON2 <- factor(FullStack_V1$DESECON2, levels = all_levels)
FullStack_V1$DESECON2 <- factor(FullStack_V1$DESECON2, levels = rev(levels(FullStack_V1$DESECON2)))

# STEP 2: assign colors to each level ----
colors_for_values <- c("California Mediterranea" = "#ff6554", 
                       "Desiertos Calidos" = "#ffa114", 
                       "Piedemonte de la Sierra Madre Occidental" = "#fceb02",
                       "Altiplanicie Mexicana" = "#fef396", 
                       "Planicie semiarida de Tamaulipas-Texas" = "#89c7d1", 
                       "Planicie costera de Texas-Louisiana" = "#77b4be",
                       "Planicie y Lomerios de la Peninsula de Yucatan" = "#ceead5", 
                       "Planicie Costera y Lomerios Humedos del Golfo de Mexico" = "#bae0c3", 
                       "Planicies y Lomerios del Occidente" = "#8ecc9e",
                       "Planicie Costera y Lomerios del Soconusco" = "#79b689", 
                       "Sierra de Los Tuxtlas" = "#64a175", 
                       "Planicie Costera y Lomerios del Pacifico Sur" = "#c8e0fa",
                       "Planicie Costera, Lomerios y Canones del Occidente" = "#b0d1f5", 
                       "Depresiones Intermontanas" = "#7bb4ed", 
                       "Planicies Costeras y Lomerios Secos del Golfo de Mexico" = "#649fd7",
                       "Sierra y Planicies de El Cabo" = "#4d8ac1", 
                       "Planicie Noroccidental de la  Peninsula de Yucatan" = "#2c6396", 
                       "Sierra Madre Centroamericana y Altos de Chiapas" = "#e0dbf6",
                       "Sierra Madre Occidental" = "#d1cbee", 
                       "Sierra Madre Oriental" = "#b4aade", 
                       "Sierra Madre del Sur" = "#9c92c5",
                       "Sistema Neovolcanico Transversal" = "#847bac", 
                       "Cuerpos de agua" = "#e9ccdb")


# STEP 3: plot ----
FullStack_V1 %>% 
  mutate(SC1 = case_when(Muestreado1 != 1 | Plot_S1 != 4 ~ NA,
                         T ~ SC1),
         SC2 = case_when(Muestreado2 != 1 | Plot_S2 != 4 ~ NA,
                         T ~ SC2),
         SC3 = case_when(Muestreado3 != 1 | Plot_S3 != 4 ~ NA,
                         T ~ SC3)) |> 
  select(DESECON2, SC1, SC2, SC3) |> 
  pivot_longer(c(SC1, SC2, SC3), names_to = "Cycle", values_to = "Species_Richness") |>
  mutate(Cycle = case_when(Cycle == "SC1" ~ "C1",
                           Cycle == "SC2" ~ "C2",
                           Cycle == "SC3" ~ "C3",
                           T ~ Cycle)) |> 
  mutate(Comparison = factor(Cycle, levels = c("C1", "C2", "C3"))) |> 
  filter(!is.na(DESECON2) & DESECON2 != "Cuerpos de agua") |> 
  ggplot(aes(y = DESECON2, x = Species_Richness, fill = DESECON2)) +
  geom_density_ridges(show.legend = F, scale = 2) +
  scale_fill_manual(values = colors_for_values) +
  facet_grid(~Cycle) +
  theme_minimal() +
  ylab("") +
  xlab("Species richness per PSU (# of individuals)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11))

FullStack_V1 |> 
  select(DESECON1, DESECON2) |> 
  group_by(DESECON1) |>
  distinct() |> 
  arrange(DESECON1) |> 
  print(n = 30)


## 1.1.1) Species Richness Change ----
# STEP 1: assign levels ----
Lvl1 <- c("California Mediterranea")
Lvl2 <- c("Desiertos Calidos")
Lvl3 <- c("Piedemonte de la Sierra Madre Occidental", "Altiplanicie Mexicana")
Lvl4 <- c("Planicie semiarida de Tamaulipas-Texas", "Planicie costera de Texas-Louisiana")
Lvl5 <- c("Planicie y Lomerios de la Peninsula de Yucatan", "Planicie Costera y Lomerios Humedos del Golfo de Mexico", 
          "Planicies y Lomerios del Occidente", "Planicie Costera y Lomerios del Soconusco", "Sierra de Los Tuxtlas")
Lvl6 <- c("Planicie Costera y Lomerios del Pacifico Sur", "Planicie Costera, Lomerios y Canones del Occidente",
          "Depresiones Intermontanas", "Planicies Costeras y Lomerios Secos del Golfo de Mexico", "Sierra y Planicies de El Cabo",
          "Planicie Noroccidental de la  Peninsula de Yucatan")
Lvl7 <- c("Sierra Madre Centroamericana y Altos de Chiapas", "Sierra Madre Occidental", "Sierra Madre Oriental", 
          "Sierra Madre del Sur", "Sistema Neovolcanico Transversal")
Lvl8 <- c("Cuerpos de agua")

all_levels <- c(Lvl1, Lvl2, Lvl3, Lvl4, Lvl5, Lvl6, Lvl7, Lvl8)

FullStack_V1$DESECON2 <- factor(FullStack_V1$DESECON2, levels = all_levels)
FullStack_V1$DESECON2 <- factor(FullStack_V1$DESECON2, levels = rev(levels(FullStack_V1$DESECON2)))

# STEP 2: assign colors to each level ----
colors_for_values <- c("California Mediterranea" = "#ff6554", 
                       "Desiertos Calidos" = "#ffa114", 
                       "Piedemonte de la Sierra Madre Occidental" = "#fceb02",
                       "Altiplanicie Mexicana" = "#fef396", 
                       "Planicie semiarida de Tamaulipas-Texas" = "#89c7d1", 
                       "Planicie costera de Texas-Louisiana" = "#77b4be",
                       "Planicie y Lomerios de la Peninsula de Yucatan" = "#ceead5", 
                       "Planicie Costera y Lomerios Humedos del Golfo de Mexico" = "#bae0c3", 
                       "Planicies y Lomerios del Occidente" = "#8ecc9e",
                       "Planicie Costera y Lomerios del Soconusco" = "#79b689", 
                       "Sierra de Los Tuxtlas" = "#64a175", 
                       "Planicie Costera y Lomerios del Pacifico Sur" = "#c8e0fa",
                       "Planicie Costera, Lomerios y Canones del Occidente" = "#b0d1f5", 
                       "Depresiones Intermontanas" = "#7bb4ed", 
                       "Planicies Costeras y Lomerios Secos del Golfo de Mexico" = "#649fd7",
                       "Sierra y Planicies de El Cabo" = "#4d8ac1", 
                       "Planicie Noroccidental de la  Peninsula de Yucatan" = "#2c6396", 
                       "Sierra Madre Centroamericana y Altos de Chiapas" = "#e0dbf6",
                       "Sierra Madre Occidental" = "#d1cbee", 
                       "Sierra Madre Oriental" = "#b4aade", 
                       "Sierra Madre del Sur" = "#9c92c5",
                       "Sistema Neovolcanico Transversal" = "#847bac", 
                       "Cuerpos de agua" = "#e9ccdb")

# STEP 3: Plot -----
FullStack_V1 %>% 
  mutate(SC12 = case_when(Muestreado1 != 1 | Muestreado2 != 1 | Plot_S1 != 4 | Plot_S2 != 4 ~ NA,
                          T ~ SC12),
         SC23 = case_when(Muestreado2 != 1 | Muestreado3 != 1 | Plot_S2 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC23),
         SC13 = case_when(Muestreado1 != 1 | Muestreado3 != 1 | Plot_S1 != 4 | Plot_S3 != 4 ~ NA,
                          T ~ SC13)) |> 
  select(DESECON2, SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "Species_Richness_Change") |> 
  mutate(Comparison = case_when(Comparison == "SC12" ~ "C12",
                           Comparison == "SC23" ~ "C23",
                           Comparison == "SC13" ~ "C13",
                           T ~ Comparison)) |> 
  mutate(Comparison = factor(Comparison, levels = c("C12", "C23", "C13"))) |> 
  filter(!is.na(DESECON2) & DESECON2 != "Cuerpos de agua") |> 
  ggplot(aes(y = DESECON2, x = Species_Richness_Change, fill = DESECON2)) +
  geom_density_ridges(show.legend = F, scale = 2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual(values = colors_for_values) +
  facet_grid(~Comparison) +
  theme_minimal() +
  ylab("") +
  xlab("Species richness change per PSU (# of individuals)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11))



#### 4) iMAD: mad component plots ----
# Comparison 1-2 ----
## COL 1 - 8
## Column 1
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-20, 20))


## Column 2
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_12 |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))



# Comparison 2-3 ----
# COL 1 - 8 
# Column 1
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-20, 20))


## Column 2
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_23 |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))



# Comparison 1-3 ----
# COL 1 - 8 
# Column 1
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-20, 20))


## Column 2
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_13 |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))


#### 5) Comparison of iMAD with Species Richness
## 5.1 Comparison of iMAD with Species Richness C12 ----
# STEP 1: join iMAD and Fullstack_v1  -----
Fullstack_iMAD_12 <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, X, Y, DESECON2, DESECON1, Plot_S1, Plot_S2, SC12, SC12P) |> 
  mutate(X = round(X, 3),
         Y = round(Y, 3)) |> 
  left_join(iMAD_results_12 |> 
              select(X, Y, Col_1, chi_squared, direction_of_change) |> 
              mutate(X = round(X, 3),
                     Y = round(Y, 3)),
            by = c("X", "Y")) |> 
  mutate(direction_of_change_factor = as.factor(case_when(direction_of_change == -3 ~ "strong loss",
                                         direction_of_change == -2 ~ "moderate loss",
                                         direction_of_change == -1 ~ "small loss",
                                         direction_of_change == 0 ~ "no change",
                                         direction_of_change == 1 ~ "small gain",
                                         direction_of_change == 2 ~ "moderate gain",
                                         direction_of_change == 3 ~ "strong gain")))


# assigning levels
Fullstack_iMAD_12$direction_of_change_factor <- factor(Fullstack_iMAD_12$direction_of_change_factor, 
                                                       levels = c("strong gain", "moderate gain", "small gain",
                                                                  "no change",
                                                                  "small loss", "moderate loss", "strong loss"))
Fullstack_iMAD_12$direction_of_change_factor <- factor(Fullstack_iMAD_12$direction_of_change_factor,
                                                       levels = rev(levels(Fullstack_iMAD_12$direction_of_change_factor)))

# STEP 2: ridglinde plot ----
# total species richness change values
Fullstack_iMAD_12 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC12, y = direction_of_change_factor, fill = direction_of_change_factor)) +
  geom_density_ridges(show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("") +
  xlab("Species richness change per PSU (# of species)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11))

# relative species richness change values
Fullstack_iMAD_12 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC12P, y = direction_of_change_factor, fill = direction_of_change_factor)) +
  geom_density_ridges(show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("") +
  xlab("Species richness change per PSU (# of species)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11)) #+
  coord_cartesian(xlim = c(-101, 1001))

# STEP 3: table with distributions by direction of change ----
Fullstack_iMAD_12 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  group_by(direction_of_change_factor) |> 
  summarise(Mean = mean(SC12),
            Median = median(SC12),
            Max = max(SC12),
            Min = min(SC12))

Fullstack_iMAD_12 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  group_by(direction_of_change_factor) |> 
  summarise(Mean = mean(SC12P),
            Median = median(SC12P),
            Max = max(SC12P),
            Min = min(SC12P)) 


# STEP 4: boxplot ----
#total
Fullstack_iMAD_12 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC12, y = direction_of_change_factor)) +
  geom_boxplot()
#relative
Fullstack_iMAD_12 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC12P, y = direction_of_change_factor)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(-101, 1001))


## 5.2 Comparison of iMAD with Species Richness C23 ----
# STEP 1: join iMAD and Fullstack_v1  -----
Fullstack_iMAD_23 <- FullStack_V1 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, X, Y, DESECON2, DESECON1, Plot_S2, Plot_S3, SC23, SC23P) |> 
  mutate(X = round(X, 3),
         Y = round(Y, 3)) |> 
  left_join(iMAD_results_23 |> 
              select(X, Y, Col_1, chi_squared, direction_of_change) |> 
              mutate(X = round(X, 3),
                     Y = round(Y, 3)),
            by = c("X", "Y")) |> 
  mutate(direction_of_change_factor = as.factor(case_when(direction_of_change == -3 ~ "strong loss",
                                         direction_of_change == -2 ~ "moderate loss",
                                         direction_of_change == -1 ~ "small loss",
                                         direction_of_change == 0 ~ "no change",
                                         direction_of_change == 1 ~ "small gain",
                                         direction_of_change == 2 ~ "moderate gain",
                                         direction_of_change == 3 ~ "strong gain")))


# assigning levels
Fullstack_iMAD_23$direction_of_change_factor <- factor(Fullstack_iMAD_23$direction_of_change_factor, 
                                                       levels = c("strong gain", "moderate gain", "small gain",
                                                                  "no change",
                                                                  "small loss", "moderate loss", "strong loss"))
Fullstack_iMAD_23$direction_of_change_factor <- factor(Fullstack_iMAD_23$direction_of_change_factor,
                                                       levels = rev(levels(Fullstack_iMAD_23$direction_of_change_factor)))


# STEP 2: ridglinde plot ----
# total species richness change values
Fullstack_iMAD_23 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC23, y = direction_of_change_factor, fill = direction_of_change_factor)) +
  geom_density_ridges(show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("") +
  xlab("Species richness change per PSU (# of species)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11))

# relative species richness change values
Fullstack_iMAD_23 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC23P, y = direction_of_change_factor, fill = direction_of_change_factor)) +
  geom_density_ridges(show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("") +
  xlab("Species richness change per PSU (# of species)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11)) #+
  coord_cartesian(xlim = c(-101, 1001))

# STEP 3: table with distributions by direction of change ----
Fullstack_iMAD_23 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  group_by(direction_of_change_factor) |> 
  summarise(Mean = mean(SC23),
            Median = median(SC23),
            Max = max(SC23),
            Min = min(SC23))

Fullstack_iMAD_23 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  group_by(direction_of_change_factor) |> 
  summarise(Mean = mean(SC23P),
            Median = median(SC23P),
            Max = max(SC23P),
            Min = min(SC23P)) 


# STEP 4: boxplot ----
#total
Fullstack_iMAD_23 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC23, y = direction_of_change_factor)) +
  geom_boxplot()
#relative
Fullstack_iMAD_23 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC23P, y = direction_of_change_factor)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(-101, 1001))

  
## 5.3 Comparison of iMAD with Species Richness C13 ----
# STEP 1: join iMAD and Fullstack_v1  -----
Fullstack_iMAD_13 <- FullStack_V1 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, X, Y, DESECON2, DESECON1, Plot_S1, Plot_S3, SC13, SC13P) |> 
    mutate(X = round(X, 3),
           Y = round(Y, 3)) |> 
  left_join(iMAD_results_13 |> 
              select(X, Y, Col_1, chi_squared, direction_of_change) |> 
              mutate(X = round(X, 3),
                     Y = round(Y, 3)),
            by = c("X", "Y")) |> 
  mutate(direction_of_change_factor = as.factor(case_when(direction_of_change == -3 ~ "strong loss",
                                         direction_of_change == -2 ~ "moderate loss",
                                         direction_of_change == -1 ~ "small loss",
                                         direction_of_change == 0 ~ "no change",
                                         direction_of_change == 1 ~ "small gain",
                                         direction_of_change == 2 ~ "moderate gain",
                                         direction_of_change == 3 ~ "strong gain")))


# assigning levels
Fullstack_iMAD_13$direction_of_change_factor <- factor(Fullstack_iMAD_13$direction_of_change_factor, 
                                                       levels = c("strong gain", "moderate gain", "small gain",
                                                                  "no change",
                                                                  "small loss", "moderate loss", "strong loss"))
Fullstack_iMAD_13$direction_of_change_factor <- factor(Fullstack_iMAD_13$direction_of_change_factor,
                                                       levels = rev(levels(Fullstack_iMAD_13$direction_of_change_factor)))

# STEP 2: ridglinde plot ----
# total species richness change values
Fullstack_iMAD_13 |> 
  filter(!is.na(direction_of_change_factor)) |> 
 # filter(Plot_S1 == 4 & Plot_S3 == 4) |> 
  ggplot(aes(x = SC13, y = direction_of_change_factor, fill = direction_of_change_factor)) +
  geom_density_ridges(show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("") +
  xlab("Species richness change per PSU (# of species)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11))

# relative species richness change values
Fullstack_iMAD_13 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC13P, y = direction_of_change_factor, fill = direction_of_change_factor)) +
  geom_density_ridges(show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("") +
  xlab("Species richness change per PSU (# of species)") +
  theme(axis.title.x = element_text(family = "ARL", size = 11),
        axis.title.y = element_text(family = "ARL", size = 11)) +
  coord_cartesian(xlim = c(-101, 1001))

# STEP 3: table with distributions by direction of change ----
Fullstack_iMAD_13 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S1 == 4 & Plot_S3 == 4) |> 
  group_by(direction_of_change_factor) |> 
  summarise(Mean = mean(SC13),
            Median = median(SC13),
            Max = max(SC13),
            Min = min(SC13))

Fullstack_iMAD_13 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  group_by(direction_of_change_factor) |> 
  summarise(Mean = mean(SC13P),
            Median = median(SC13P),
            Max = max(SC13P),
            Min = min(SC13P)) 


# STEP 4: boxplot ----
#total
Fullstack_iMAD_13 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC13, y = direction_of_change_factor)) +
  geom_boxplot()
#relative
Fullstack_iMAD_13 |> 
  filter(!is.na(direction_of_change_factor)) |> 
  ggplot(aes(x = SC13P, y = direction_of_change_factor)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(-101, 1001))



## 5.4 distribution of PSUs per category for each comparison ----
# STEP 1: make sure all preparatory data sets are there ----
Fullstack_iMAD_12
Fullstack_iMAD_23
Fullstack_iMAD_13

# STEP 2: count PSUs per category for each comparison ----
# C12
Fullstack_iMAD_12 |> 
  #filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S1 == 4 & Plot_S2 == 4) |> 
  group_by(direction_of_change_factor) |> 
  summarise(PSUs = n()) |> 
  arrange(desc(direction_of_change_factor))

# C23
Fullstack_iMAD_23 |> 
  # filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S2 == 4 & Plot_S3 == 4) |> 
  group_by(direction_of_change_factor) |> 
  summarise(PSUs = n()) |> 
  arrange(desc(direction_of_change_factor))

# C13
Fullstack_iMAD_13 |> 
  # filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S2 == 4 & Plot_S3 == 4) |> 
  group_by(direction_of_change_factor) |> 
  summarise(PSUs = n()) |> 
  arrange(desc(direction_of_change_factor))



  
## 5.5 distribution of PSUs per Ecoregion and category ----
# STEP 1: make sure all preparatory data sets are there ----
Fullstack_iMAD_12
Fullstack_iMAD_23
Fullstack_iMAD_13
# STEP 2: count PSUs per category for each comparison ----
#C12
Fullstack_iMAD_12 |> 
  #filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S1 == 4 & Plot_S2 == 4) |> 
  group_by(DESECON1, direction_of_change_factor) |> 
  filter(direction_of_change >= 1 | direction_of_change <= -1) |> 
  summarise(PSUs = n()) |>
  arrange(DESECON1, direction_of_change_factor) |> 
  mutate(Regional_Sum = sum(PSUs)) |> 
  ungroup() |> 
  mutate(Total_Sum = sum(PSUs),
         Regional_Percent = Regional_Sum/Total_Sum*100) |> 
  mutate(direction_percent = PSUs/Regional_Sum*100) |> 
  relocate(DESECON1, direction_of_change_factor, PSUs, Regional_Sum, direction_percent) |> 
  arrange(desc(Regional_Percent)) |> 
  print(n = 150)


#C23
Fullstack_iMAD_23 |> 
  #filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S1 == 4 & Plot_S2 == 4) |> 
  group_by(DESECON1, direction_of_change_factor) |> 
  filter(direction_of_change >= 1 | direction_of_change <= -1) |> 
  summarise(PSUs = n()) |>
  arrange(DESECON1, direction_of_change_factor) |> 
  mutate(Regional_Sum = sum(PSUs)) |> 
  ungroup() |> 
  mutate(Total_Sum = sum(PSUs),
         Regional_Percent = Regional_Sum/Total_Sum*100) |> 
  mutate(direction_percent = PSUs/Regional_Sum*100) |> 
  relocate(DESECON1, direction_of_change_factor, PSUs, Regional_Sum, direction_percent) |> 
  arrange(desc(Regional_Percent)) |> 
  print(n = 150)



#C13
Fullstack_iMAD_13 |> 
  #filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S1 == 4 & Plot_S2 == 4) |> 
  group_by(DESECON1, direction_of_change_factor) |> 
  filter(direction_of_change >= 1 | direction_of_change <= -1) |> 
  summarise(PSUs = n()) |>
  arrange(DESECON1, direction_of_change_factor) |> 
  mutate(Regional_Sum = sum(PSUs)) |> 
  ungroup() |> 
  mutate(Total_Sum = sum(PSUs),
         Regional_Percent = Regional_Sum/Total_Sum*100) |> 
  mutate(direction_percent = PSUs/Regional_Sum*100) |> 
  relocate(DESECON1, direction_of_change_factor, PSUs, Regional_Sum, direction_percent) |> 
  arrange(desc(Regional_Percent)) |> 
  print(n = 150)
  
  
  ungroup() |> 
  group_by(DESECON1) |> 
  summarise(total  = sum(PSUs))

Fullstack_iMAD_12 |> 
  ggplot(aes(x = direction_of_change, y = DESECON2)) + 
  geom_density_ridges()

  

##################################     END      ##################################################################
#
################### CLUSTER METADATA DATASET PLOTTING           ##################################################
#### 1) Individual Tree Entries per Cluster (DATA: ALL SAMPLED CLUSTERS) --------------------------------------------------------------

# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1)
# C23
Fullstack_iMAD_23 |> 
  # filter(!is.na(direction_of_change_factor)) |> 
  #filter(Plot_S2 == 4 & Plot_S3 == 4) |> 
  group_by(direction_of_change_factor) |> 
  summarise(PSUs = n()) |> 
  arrange(desc(direction_of_change_factor))C2 <- FullStack_V4 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2)
C3 <- FullStack_V4 |> 
  filter(Muestreado3 == 1) |> 
  select(TE3) |> 
  mutate(Cycle = as.factor(3)) |> 
  rename(TE = TE3)



# STEP 2: Create long datatable with rbind()
TE_ds <- rbind(C1, C2, C3)
TE_ds

# 1a) Density -----
TE_ds |> 
  ggplot(aes(x= TE, color = Cycle)) +
  geom_density()
# 1b) Histogram ----
TE_ds |> 
  ggplot(aes(x= TE, fill = Cycle)) +
  geom_histogram(position = "identity", alpha = 0.3)
# 1c) Frequency ----
TE_ds |> 
  ggplot(aes(x= TE, color = Cycle)) +
  geom_freqpoly()
#### 1.1) Individual Tree Entries per Cluster (DATA: SAMPLED CLUSTERS THROUGH 1-2-3) --------------------------------------------------------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE3, SC3) |> 
  mutate(Cycle = as.factor(3)) |> 
  rename(TE = TE3,
         SC = SC3)

# STEP 2: Create long datatable with rbind()
TE_ds <- rbind(C1, C2, C3)
TE_ds

# 1.1a) Density -----
TE_ds |> 
  ggplot(aes(x= TE, color = Cycle)) +
  geom_density()
# 1.1b) Histogram ----
TE_ds |> 
  ggplot(aes(x= TE, fill = Cycle)) +
  geom_histogram(position = "identity", alpha = 0.3)
# 1.1c) Frequency ----
TE_ds |> 
  ggplot(aes(x= TE, color = Cycle)) +
  geom_freqpoly()

#### 2) Species Richness (Spec Count) per Cluster (DATA: ALL SAMPLED CLUSTERS) --------------------------------------------------------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4 |> 
  filter(Muestreado3 == 1) |> 
  select(TE3, SC3) |> 
  mutate(Cycle = as.factor(3)) |> 
  rename(TE = TE3,
         SC = SC3)

# STEP 2: Create long datatable with rbind()
TE_ds <- rbind(C1, C2, C3)
TE_ds

# 2a) Density -----
FullStack_V4_Zeros |> 
  select(SC1, SC2, SC3) |> 
  pivot_longer(c(SC1, SC2, SC3), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC1", "SC2", "SC3"))) |> 
  ggplot(aes(x= Species_Richness, color=Comparison)) +
  geom_density()
# 2b) Histogram ----
FullStack_V4_Zeros |> 
  select(SC1, SC2, SC3) |> 
  pivot_longer(c(SC1, SC2, SC3), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC1", "SC2", "SC3"))) |> 
  ggplot(aes(x= Species_Richness, fill=Comparison)) +
  geom_histogram(position = "identity", alpha = 0.3)
# 2c) Frequency ----
FullStack_V4_Zeros |> 
  select(SC1, SC2, SC3) |> 
  pivot_longer(c(SC1, SC2, SC3), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC1", "SC2", "SC3"))) |> 
  ggplot(aes(x= Species_Richness, color=Comparison)) +
  geom_freqpoly()





#### 2.1) Species Richness (Spec Count) per Cluster (DATA: SAMPLED CLUSTERS THROUGH 1-2-3) ----------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE3, SC3) |> 
  mutate(Cycle = as.factor(3)) |> 
  rename(TE = TE3,
         SC = SC3)

# STEP 2: Create long datatable with rbind()
TE_ds <- rbind(C1, C2, C3)
TE_ds

# 2a) Density -----
TE_ds |> 
  ggplot(aes(x= SC, color = Cycle)) +
  geom_density()
# 2b) Histogram ----
TE_ds |> 
  ggplot(aes(x= SC, fill = Cycle)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 1)
# 2c) Frequency ----
TE_ds |> 
  ggplot(aes(x= SC, color = Cycle)) +
  geom_freqpoly(binwidth = 1)


#### 3) Avg DBH per Cluster (DATA: ALL SAMPLED CLUSTERS) --------------------------------------------------------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1, SC1, DBH1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1,
         DBH = DBH1)
C2 <- FullStack_V4 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2, SC2, DBH2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2,
         DBH = DBH2)
C3 <- FullStack_V4 |> 
  filter(Muestreado3 == 1) |> 
  select(TE3, SC3, DBH3) |> 
  mutate(Cycle = as.factor(3)) |> 
  rename(TE = TE3,
         SC = SC3,
         DBH = DBH3)

# STEP 2: Create long datatable with rbind()
TE_ds <- rbind(C1, C2, C3)
TE_ds

# 2a) Density -----
TE_ds |> 
  ggplot(aes(x= DBH, color = Cycle)) +
  geom_density()
# 2b) Histogram ----
TE_ds |> 
  ggplot(aes(x= DBH, fill = Cycle)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 1)
# 2c) Frequency ----
TE_ds |> 
  ggplot(aes(x= DBH, color = Cycle)) +
  geom_freqpoly(binwidth = 1)



#### 3.1) Avg DBH per Cluster (DATA: SAMPLED CLUSTERS THROUGH 1-2-3) ----------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1, DBH1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1,
         DBH = DBH1)
C2 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2, DBH2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2,
         DBH = DBH2)
C3 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE3, SC3, DBH3) |> 
  mutate(Cycle = as.factor(3)) |> 
  rename(TE = TE3,
         SC = SC3,
         DBH = DBH3)

# STEP 2: Create long datatable with rbind()
TE_ds <- rbind(C1, C2, C3)
TE_ds

# 2a) Density -----
TE_ds |> 
  ggplot(aes(x= DBH, color = Cycle)) +
  geom_density()
# 2b) Histogram ----
TE_ds |> 
  ggplot(aes(x= DBH, fill = Cycle)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 1)
# 2c) Frequency ----
TE_ds |> 
  ggplot(aes(x= DBH, color = Cycle)) +
  geom_freqpoly(binwidth = 1)



##################################     END      ##################################################################










#################   DONT RUN - TAKES AGES ########################################################################
#################   RAREFACTION OF SPECIESRICHNESS DATA for 3 subplot per conglomerate
# STEP 1: calculate number of subplots for each conglomerate for each cycle ----
# cycle 1
Plots_1 <- Base |> 
  ungroup() |> 
  select(Cluster_ID, Muestreado1, Plot_S1) |> 
  mutate(Cycle = 1,
         number_of_plots = case_when(Plot_S1 == 0 ~ 0,
                                     Plot_S1 == 1 ~ 1,
                                     Plot_S1 == 2 ~ 2,
                                     Plot_S1 == 3 ~ 3,
                                     Plot_S1 == 4 ~ 4,
                                     T ~ Plot_S1))


# cycle 2
Plots_2 <- Base |> 
  ungroup() |> 
  select(Cluster_ID, Muestreado2, Plot_S2) |> 
  mutate(Cycle = 2,
         number_of_plots = case_when(Plot_S2 == 0 ~ 0,
                                     Plot_S2 == 1 ~ 1,
                                     Plot_S2 == 2 ~ 2,
                                     Plot_S2 == 3 ~ 3,
                                     Plot_S2 == 4 ~ 4,
                                     T ~ Plot_S2))

# cycle 3
Plots_3 <- Base |> 
  ungroup() |> 
  select(Cluster_ID, Muestreado3, Plot_S3) |> 
  mutate(Cycle = 3,
         number_of_plots = case_when(Plot_S3 == 0 ~ 0,
                                     Plot_S3 == 1 ~ 1,
                                     Plot_S3 == 2 ~ 2,
                                     Plot_S3 == 3 ~ 3,
                                     Plot_S3 == 4 ~ 4,
                                     T ~ Plot_S3))

Plots_3 |> 
  filter(number_of_plots == 4) |> 
  count()

# STEP 2: Filter out everything that doesn't matter ----
# cycle 1
Plots_1 <- Plots_1 |> 
  filter(Muestreado1 == 1) |> 
  filter(number_of_plots >= 3) |> 
  select(Cluster_ID, Cycle, number_of_plots)

Plots_1 |> count()

# cycle 2
Plots_2 <- Plots_2 |> 
  filter(Muestreado2 == 1) |> 
  filter(number_of_plots >= 3) |> 
  select(Cluster_ID, Cycle, number_of_plots)

Plots_2 |> count()

# cycle 3
Plots_3 <- Plots_3 |> 
  filter(Muestreado3 == 1) |> 
  filter(number_of_plots >= 3) |> 
  select(Cluster_ID, Cycle, number_of_plots)

Plots_3 |> count()

# STEP 3: create long data with rbind() ----
Plots_123 <- rbind(Plots_1, Plots_2, Plots_3)

Plots_123 |> 
  filter(is.na(number_of_plots))

# STEP 4: Re-introduce individual entries ----
Plots_abundances <- Plots_123 |> 
  #some plots are empty -> left join creates NAs for Nombrecientifico
  left_join(C_SpecAbun |> select(Cluster_ID, Cycle, NombreCientifico_APG, abundance),
            by = c("Cycle", "Cluster_ID")) |> 
  relocate(Cluster_ID, Cycle, number_of_plots) |> 
  #exchanging false NAs in NombreCientifico for "empty" and add abundance == 0
  #keeps empty conglomerates for later change calculation
  mutate(NombreCientifico_APG = case_when(is.na(NombreCientifico_APG) & is.na(abundance) ~ "empty",
                                          T ~ NombreCientifico_APG),
         NombreCientifico_APG = case_when(is.na(NombreCientifico_APG) ~ "NA",
                                          T ~ NombreCientifico_APG),
         abundance = case_when(is.na(abundance) ~ 0,
                               T ~ abundance))


# STEP 5: pivot wider ----
# pivot
Plots_wide <- Plots_abundances |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

Plots_wide

# exchange all NAs with 0 for calculation
Plots_wide <- Plots_wide|> 
  replace(is.na(Plots_wide), 0)

Plots_wide

# STEP 6: dissect into single cycles again ----
Plots_wide_1 <- Plots_wide |> 
  filter(Cycle == 1)

Plots_wide_3 <- Plots_wide |> 
  filter(Cycle == 3)

# STEP 7: Rarefaction ----
### IMPORTANT: run this code only with species as cols and samples as rows!!!
m.rar.time <- Plots_wide_3[, -c(1:3)]

# preparation step: change integer values to numeric 
m.rar.time <- as.data.frame(lapply(m.rar.time, as.numeric))

# time measurement
start.time <- Sys.time()

# rarefaction
a <- min(Plots_wide_3$number_of_plots) # -> 3 subplots
m3_rarified <- m.rar.time 
for (k in 1:ncol(m.rar.time)) {
  for (i in 1:nrow(m.rar.time)) {
    m3_rarified[i, k] <- m.rar.time[i, k] * (a / Plots_wide_3$number_of_plots[i])
  }
}

# time measurement
end.time <- Sys.time()
time.taken6 <- end.time - start.time
time.taken6

View(m3_rarified_rd)
View(m.rar.time)
View(Plots_wide |> 
       filter(number_of_plots == 3))



# rounding of values
m3_rarified_rd <- trunc(m3_rarified)
commas <- m3_rarified - m3_rarified_rd

upround <- rowSums(commas)

rank <- as.data.frame(t(apply(-commas, 1, order)))

for (k in 1:nrow(m.rar.time)){
  for (i in 1:round(upround[k])) {if(round(upround[k])>0){
    m3_rarified[k, rank[k, i]] <- m3_rarified_rd[k, rank[k, i]] + 1}
  }}

finish <- trunc(m3_rarified)

# return to original dataformat
Plots_Rare <- cbind(Plots_wide[1:3], finish)
Plots_Rare


# pivot to long data format
Plots_Rare_long <- Plots_Rare %>% 
  pivot_longer(
    cols = -c(1:2), # Excludes the first two columns
    names_to = "species_names",
    values_to = "values"
  )

# calculate number of species
Plots_Rare_long |> 
  group_by(Cycle, Cluster_ID) |> 
  summarise(n = n())



##################################     END      ##################################################################












