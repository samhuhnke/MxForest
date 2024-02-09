
### MxForestInventory Data Wrangling Code --------------------------------------
### LAST UPDATED: 02/08/2024 (US)

start.time <- Sys.time()

####################          MAIN CODE             ##########################################################################
#################### 0) LOAD NECESSARY PACKAGES ----------------------------------------------------------

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(tidyverse)  #data tidying
library(ggridges)   #geom_density_ridges()
library(terra)      #geo_spatial coordinates
library(vegan)      #for shannon-index and pielou-eveness

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
    # Added File number 
    File = "1"
  ) |>
  # setting initial column order +
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
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
    # Added File number 
    File = "2"
  ) |> 
  # setting initial column order + attaching everything so far not considered to the end
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
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
    # Added File number 
    File = "3"
  ) |> 
  # setting initial column order + attaching everything so far not considered to the end
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, CveVeg_S7, TipoVeg_S7, FormaFuste, TipoTocon, Familia_APG,
         NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal, AlturaFusteLimpio, AlturaComercial,
         DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa, PosicionCopa, ExposicionCopa, DensidadCopa,
         TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion, Danio1, Severidad1, Danio2, Severidad2, NumeroTallos,
         LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
  ) |> 
  # sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)



#################### 3) MERGE FILES FOR OVERLAPPING VARIABLES ------------------------------------------------------

#Arb.04
M.04 <- Arb.04 |>
  mutate(CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

#Arb.09
M.09 <- Arb.09 |>
  mutate(CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

#Arb.14
M.14 <- Arb.14 |>
  mutate(cgl_sit_reg = NA,
         CveVeg = CveVeg_S7,
         TipoVeg = TipoVeg_S7) |> 
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

#merge
merged <- rbind(M.04, M.09, M.14) |> 
  mutate(Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_")) |> 
  mutate(Cluster_ID = paste(File, Conglomerado, Anio, sep = "_")) |> 
  select(Cluster_ID, Plot_ID, File, Conglomerado, Sitio, Anio, everything())



#################### 4) EDA PREPARATION ------------------------------------------------------------------
###### 4.1) SPECIES RICHNESS + INDIVIDUAL TREE COUNT ---------------------------
#### DATA ON CLUSTER LEVEL
C_SpecRich <- merged |> 
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Cluster_ID)

#### DATA ON PLOT LEVEL - used for later calculation in 5.3)
SpecRich <- merged |> 
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, Sitio) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Plot_ID)


###### 4.2) SPECIES ABUNDANCES - needed for shannon index and eveness - DATA ON SPECIES LEVEL --------

#### DATA CALCULATED PER CLUSTER
C_SpecAbun <- merged |> 
  select(File, Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, NombreCientifico_APG) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            abundance=n(),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Cluster_ID)


###### 4.3) SHANNON INDEX H + PIELOU EVENESS J - uses temporary created dataframes! -------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON CLUSTER LEVEL
C_Temp.Shannon <- C_SpecAbun |> 
  ungroup() |> 
  select(Cluster_ID, NombreCientifico_APG, abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

#### STEP 2: exchange NAs with Zeros 
## DATA ON CLUSTER LEVEL
C_PresenceAbsence <- C_Temp.Shannon |> 
  replace(is.na(C_Temp.Shannon), 0)

#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON CLUSTER LEVEL
H <- diversity(C_PresenceAbsence[,-1])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON CLUSTER LEVEL
J <- H/log(specnumber(C_PresenceAbsence[, -1]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
C_Temp.HJ <- data.frame(C_PresenceAbsence$Cluster_ID, H, J) |> 
  rename(Cluster_ID = C_PresenceAbsence.Cluster_ID)

#### STEP 6: merged data table 
## DATA ON CLUSTER LEVEL
ClusterDiagnostics <- left_join(C_SpecRich, C_Temp.HJ, by= c("Cluster_ID")) |> 
  select(File, Cluster_ID, Conglomerado, Anio, species_count, total_entries, H, J, X, Y)


###### 4.4) TREE MORPHOLOGY -----------------------------------------------

#### DATA ON CLUSTER LEVEL ----- calculated by individual entries (alternatively by means of plot means)
# need more thought going into whether to use means or medians
# Example: for Tree Height Means are on average -20cm compared to Median

C_TreeMorp <- merged |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            AvgTreeHeight = mean(AlturaTotal, na.rm = T),
            Med_AvgTreeHeight = median(AlturaTotal, na.rm = T),
            AvgDbh = mean(DiametroNormal, na.rm = T),
            Med_AvgDbh = median(DiametroNormal, na.rm = T),
            AvgCrownDiameter = mean(DiametroCopa, na.rm = T),
            Med_AvgCrownDiameter = median(DiametroCopa, na.rm = T),
            AvgCrownHeight = mean(AlturaTotal - AlturaFusteLimpio, na.rm = T),          # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            Med_AvgCrownHeight = median(AlturaTotal - AlturaFusteLimpio, na.rm = T),    # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            AvgCrownArea = mean(AreaCopa, na.rm = T),
            Med_AvgCrownArea = median(AreaCopa, na.rm = T),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(AvgCrownHeight = case_when(AvgCrownHeight < 0 ~ NA,
                                    T ~ AvgCrownHeight)) |> 
  relocate(Cluster_ID)

###### 4.5) COMPLETE DIAGNOSTICS DATASET (excluding Biomass; 01/10/2024) ----------------------------

Comp_C_Diagnostics <- left_join(ClusterDiagnostics, C_TreeMorp, by= c("Cluster_ID", "File", "Conglomerado", "Anio", "X", "Y")) |> 
  relocate(Cluster_ID, File, Conglomerado, Anio, species_count, total_entries, H, J, 
           AvgTreeHeight, Med_AvgTreeHeight, AvgDbh, Med_AvgDbh, AvgCrownDiameter, Med_AvgCrownDiameter, AvgCrownHeight, Med_AvgCrownHeight, AvgCrownArea, Med_AvgCrownArea, X, Y)

# write.csv(Comp_C_Diagnostics, "INFyS_Selection_Cluster.csv")

#################### 5) METADATA STUFF -------------------------------------------------------------------
###### 5.1) PLOT COUNTS FOR EACH CLUSTER -----------------------------------------

# Adding the Number of Plots per Cluster ("Plots")
PlotCounts <- merged |> 
  group_by(File, Conglomerado, Sitio, X, Y) |> 
  count() |>
  ungroup() |> 
  select(File, Conglomerado, Sitio, X, Y) |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.numeric(File)),
            Conglomerado = mean(Conglomerado),
            X = mean(X),
            Y = mean(Y),
            Plots = n())

# combined dataset
Comp_C_Diagnostics_V2 <- left_join(Comp_C_Diagnostics, PlotCounts, by= c("File", "Conglomerado")) |> 
  select(everything(), -c("X.y", "Y.y")) |> 
  rename(X = X.x,
         Y = Y.x)

###### 5.2) AVAILABILITY OF PLOTS FOR EACH CLUSTER -----------------------------------------------

# function to count number of cycles with a given number of plots 
summarise_data <- function(data, plot_number) {
  data |> 
    select(File, Conglomerado, Anio, Plots, X, Y) |> 
    filter(Plots == plot_number) |> 
    group_by(Conglomerado) |> 
    arrange(Conglomerado) |> 
    summarise(Conglomerado = mean(Conglomerado),
              n = n(),
              X1 = mean(X),
              Y1 = mean(Y)) |> 
    rename(n_cycles_placeholder = n)
}

# combined dataset V3
Comp_C_Diagnostics_V3 <- Comp_C_Diagnostics_V2 %>% 
  left_join(summarise_data(. ,4), by = "Conglomerado") %>%
  left_join(summarise_data(. ,3), by = "Conglomerado") %>% 
  left_join(summarise_data(. ,2), by = "Conglomerado") %>%
  left_join(summarise_data(. ,1), by = "Conglomerado") %>%
  select(-c("X1.x", "Y1.x", "X1.y", "Y1.y", "X1.x.x", "Y1.x.x", "X1.y.y", "Y1.y.y")) |> 
  rename(cycles_four_plots = n_cycles_placeholder.x,
         cycles_three_plots = n_cycles_placeholder.y,
         cycles_two_plots = n_cycles_placeholder.x.x,
         cycles_one_plots = n_cycles_placeholder.y.y,) |> 
  arrange(Conglomerado) |> 
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
  group_by(File, Conglomerado) |> 
  summarise(File = mean(File),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            Plot_TreeCount_Mean = mean(total_entries),
            Plot_TreeCount_Median = median(total_entries),
            X = mean(X),
            Y = mean(Y)) |> 
  select(File, Conglomerado, Anio, Plot_TreeCount_Mean, Plot_TreeCount_Median)

Comp_C_Diagnostics_V4 <- left_join(Comp_C_Diagnostics_V3, PTC_C, by = c("File", "Conglomerado", "Anio"))

###### 5.4) ESTADO + TIPO EGETACION FILTER -----------------------------------------------------

Comp_C_Diagnostics_V5 <- left_join(Comp_C_Diagnostics_V4, merged |> select(Cluster_ID, Estado, CveVeg, TipoVeg) |> distinct(),
                                   by = "Cluster_ID")

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####################          METADATA DATASET CODE            ###################################################
#################### 1) LOAD RAW DATA ------------------------------------------------------------
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

# STEP 2: JOIN CLUSTER_ID + X & Y FROM EACH FILE
#File 1
Temp <- ClusterBase |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado1 = Conglomerado,
                     X1 = X,
                     Y1 = Y) |> 
              select(Cluster_ID, Conglomerado1, X1, Y1),
            by = "Cluster_ID")
#File 2
Temp <- Temp |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado2 = Conglomerado,
                     X2 = X,
                     Y2 = Y) |> 
              select(Cluster_ID, Conglomerado2, X2, Y2),
            by = "Cluster_ID")
#File 3
MetaBase <- Temp |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Conglomerado3 = IDConglomerado,
                     X3 = X_C3,
                     Y3 = Y_C3) |> 
              select(Cluster_ID, Conglomerado3, X3, Y3),
            by = "Cluster_ID")

# STEP 3: AVERAGE X and Y
NewBase <- MetaBase |> 
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
NewBase2 <- NewBase |> 
  # Join File 1
  left_join(Sec.04 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado1 = Muestreado) |> 
              select(Cluster_ID, Muestreado1),
            by = "Cluster_ID"
  ) |> 
  # Join File 2
  left_join(Sec.09 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado2 = Muestreado) |> 
              select(Cluster_ID, Muestreado2),
            by = "Cluster_ID") |> 
  # Join File 3
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
FullStack <- NewBase2 %>% 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(File == 1) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File1 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File1"),
            by = "Cluster_ID") %>%
  mutate(File1 = case_when(is.na(File1) ~ 100,
                           T ~ File1)) |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(File == 2) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File2 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File2"),
            by = "Cluster_ID") %>%
  mutate(File2 = case_when(is.na(File2) ~ 100,
                           T ~ File2)) |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(File == 3) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File3 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File3"),
            by = "Cluster_ID") %>%
  mutate(File3 = case_when(is.na(File3) ~ 100,
                           T ~ File3))

# STEP 2: Claculate Status of Cluster for each Cycle
FullStack_V2 <- FullStack |> 
  mutate(Status1 = Muestreado1 - File1,
         Status2 = Muestreado2 - File2,
         Status3 = Muestreado3 - File3) |> 
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
Sec.04 |> 
  mutate(Cluster_ID = Conglomerado,
         Plot1 = Sitios_x_cgl) |> 
  select(Cluster_ID, Plot1)

FullStack_V3 <- FullStack_V2 |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S1 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(File == 1) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot1 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "Plot1"),
            by = "Cluster_ID") |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S2 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(File == 2) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot2 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "Plot2"),
            by = "Cluster_ID") |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Plot_S3 = Sitios_x_cgl_C3) |> 
              select(Cluster_ID, Plot_S3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 %>%
              filter(File == 3) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot3 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
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


################### 4) RE-INTRODUCE ECOREGIONS, TOTAL ENTRIES, SPEC COUNT, AVG DBH PER CLUSTER FOR EACH CYCLE ---------
#### 4.1) FUllStack_V4 ----
FullStack_V4 <- FullStack_V3 |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado) |> 
              select(Cluster_ID, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 |> 
              ungroup() |> 
              filter(File == 1) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH1 = AvgDbh,
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
              filter(File == 2) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH2 = AvgDbh,
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
              filter(File == 3) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH3 = AvgDbh,
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
                         T ~ SC3)
         ) |> 
  ungroup()

#### 4.2) FullStack_V4_Zeros ----
FullStack_V4_Zeros <- FullStack_V3 |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado) |> 
              select(Cluster_ID, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5 |> 
              ungroup() |> 
              filter(File == 1) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH1 = AvgDbh,
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
              filter(File == 2) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH2 = AvgDbh,
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
              filter(File == 3) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH3 = AvgDbh,
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
    TH1 = case_when(Muestreado1 == 1 & is.na(CA1) ~ 0,
                    T ~ CA1),
    TH2 = case_when(Muestreado2 == 1 & is.na(CA2) ~ 0,
                    T ~ CA2),
    TH3 = case_when(Muestreado3 == 1 & is.na(CA3) ~ 0,
                    T ~ CA3),
    ) |> 
  ungroup()


################### 5) UNIVARIATE CHANGE CALCULATIONS -----------------------------------
#### 5.1) FullSack_V4 -----
FullStack_V4 <- FullStack_V4 |> 
  # Calculate univariate changes in TreeCounts (TE) and SpeciesCounts (SC)
  mutate(TE12 = TE2 -TE1,
         TE23 = TE3 - TE1,
         TE13 = TE3 - TE1,
         SC12 = SC2 - SC1,
         SC23 = SC3 - SC2,
         SC13 = SC3 - SC1)

#### 5.1.1) FullSack_V4_Zeros -----
FullStack_V4_Zeros <- FullStack_V4_Zeros |> 
  # Calculate univariate changes in TreeCounts (TE) and SpeciesCounts (SC)
  mutate(TE12 = TE2 -TE1,
         TE23 = TE3 - TE1,
         TE13 = TE3 - TE1,
         SC12 = SC2 - SC1,
         SC23 = SC3 - SC2,
         SC13 = SC3 - SC1)




################### 6) TREE COUNT CHANGE CALCULATION BASED ON ECOREGIONS -----------------------------------

# STEP 1: IDK


# STEP 2: IDK




################### 5) GEOSPATIAL PREPARATION -----------------------------------

  #writeVector(vect(Sec.04, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Clusters04.shp")
  #writeVector(vect(Sec.09, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Clusters09.shp")
  #writeVector(vect(Sec.14, geom = c("X_C3", "Y_C3"), crs = "+proj=longlat +datum=WGS84"), "Clusters14.shp")

  #writeVector(vect(FullStack_V3, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhatClustersChanged_v2.shp")

  #writeVector(vect(FullStack_V4 |> filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1), geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhatClustersChanged_v2.shp")


#DATA: FUllStack_V4 - for cluster based change in tree counts ----
  #writeVector(vect(FullStack_V4, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4.shp")

#DATA: FullStack_V4 - FILTER: only constantly sampled clusters (n = 9 795) ----
  #writeVector(vect(FullStack_V4 |> filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1), geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4_F1.shp")

#DATA: FullStack_V4 ----
#  writeVector(vect(FullStack_V4, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4.shp")

#DATA: FullStack_V4_Zeros ----
#  writeVector(vect(FullStack_V4_Zeros, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4_Zeros.shp")

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


####################          IR-MAD CHANGE DETECTION PREPARATION            ----------------------------------
################## 1) Comparison of Cycle 1 and 2 - FILTER: Only Clusters that are available for 1 and 2 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 1
file1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH1,
         AvgCrownDiameter = CD1,
         AvgCrownHeight = CH1,
         AvgCrownArea = CA1,
         SpeciesCount = SC1,
         AvgTreeHeight = TH1,
         TreeCount = TE1,
         J = J1) |> 
  # add file grouping
  mutate(File = 1) |> 
  relocate(File)

file1

## File 2
file2 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH2,
         AvgCrownDiameter = CD2,
         AvgCrownHeight = CH2,
         AvgCrownArea = CA2,
         SpeciesCount = SC2,
         AvgTreeHeight = TH2,
         TreeCount = TE2,
         J = J2) |> 
  #add file grouping
  mutate(File = 2) |> 
  relocate(File)

file2

#### STEP 2: merge into long data format ----
file12 <- rbind(file1, file2)


#### STEP 3: write .csv for python ----

# write.csv(file12, "iMAD_Data_12_Constant.csv")

################## 1.1) ZEROs - Comparison of Cycle 1 and 2 - FILTER: Only Clusters that are available for 1 and 2 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 1
file1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH1,
         AvgCrownDiameter = CD1,
         AvgCrownHeight = CH1,
         AvgCrownArea = CA1,
         SpeciesCount = SC1,
         AvgTreeHeight = TH1,
         TreeCount = TE1,
         J = J1) |> 
  # add file grouping
  mutate(File = 1) |> 
  relocate(File)

file1

## File 2
file2 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH2,
         AvgCrownDiameter = CD2,
         AvgCrownHeight = CH2,
         AvgCrownArea = CA2,
         SpeciesCount = SC2,
         AvgTreeHeight = TH2,
         TreeCount = TE2,
         J = J2) |> 
  #add file grouping
  mutate(File = 2) |> 
  relocate(File)

file2

#### STEP 2: merge into long data format ----
file12 <- rbind(file1, file2)


#### STEP 3: write .csv for python ----

# write.csv(file12, "iMAD_Data_12_Constant_Zeros.csv")

########### CUT -------------------------------------------------------------------------------------------------
################## 2) Comparison of Cycle 2 and 3 - FILTER: Only Clusters that are available for 2 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 2
file2 <- FullStack_V4 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH2,
         AvgCrownDiameter = CD2,
         AvgCrownHeight = CH2,
         AvgCrownArea = CA2,
         SpeciesCount = SC2,
         AvgTreeHeight = TH2,
         TreeCount = TE2,
         J = J2) |> 
  #add file grouping
  mutate(File = 2) |> 
  relocate(File)

file2

## File 3
file3 <- FullStack_V4 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH3,
         AvgCrownDiameter = CD3,
         AvgCrownHeight = CH3,
         AvgCrownArea = CA3,
         SpeciesCount = SC3,
         AvgTreeHeight = TH3,
         TreeCount = TE3,
         J = J3) |> 
  #add file grouping
  mutate(File = 3) |> 
  relocate(File)

file3

#### STEP 2: merge into long data format ----
file23 <- rbind(file2, file3)


#### STEP 3: write .csv for python ----

# write.csv(file23, "iMAD_Data_23_Constant.csv")

################## 2) Comparison of Cycle 2 and 3 - FILTER: Only Clusters that are available for 2 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 2
file2 <- FullStack_V4_Zeros |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH2,
         AvgCrownDiameter = CD2,
         AvgCrownHeight = CH2,
         AvgCrownArea = CA2,
         SpeciesCount = SC2,
         AvgTreeHeight = TH2,
         TreeCount = TE2,
         J = J2) |> 
  #add file grouping
  mutate(File = 2) |> 
  relocate(File)

file2

## File 3
file3 <- FullStack_V4_Zeros |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH3,
         AvgCrownDiameter = CD3,
         AvgCrownHeight = CH3,
         AvgCrownArea = CA3,
         SpeciesCount = SC3,
         AvgTreeHeight = TH3,
         TreeCount = TE3,
         J = J3) |> 
  #add file grouping
  mutate(File = 3) |> 
  relocate(File)

file3

#### STEP 2: merge into long data format ----
file23 <- rbind(file2, file3)


#### STEP 3: write .csv for python ----

# write.csv(file23, "iMAD_Data_23_Constant_Zeros.csv")

########### CUT -------------------------------------------------------------------------------------------------
################## 3) Comparison of Cycle 1 and 3 - FILTER: Only Clusters that are available for 1 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 1
file1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH1,
         AvgCrownDiameter = CD1,
         AvgCrownHeight = CH1,
         AvgCrownArea = CA1,
         SpeciesCount = SC1,
         AvgTreeHeight = TH1,
         TreeCount = TE1,
         J = J1) |> 
  # add file grouping
  mutate(File = 1) |> 
  relocate(File)

file1

## File 3
file3 <- FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH3,
         AvgCrownDiameter = CD3,
         AvgCrownHeight = CH3,
         AvgCrownArea = CA3,
         SpeciesCount = SC3,
         AvgTreeHeight = TH3,
         TreeCount = TE3,
         J = J3) |> 
  #add file grouping
  mutate(File = 3) |> 
  relocate(File)

file3

#### STEP 2: merge into long data format ----
file13 <- rbind(file1, file3)


#### STEP 3: write .csv for python ----

# write.csv(file13, "iMAD_Data_13_Constant.csv")

################## 3.1) ZEROs - Comparison of Cycle 1 and 3 - FILTER: Only Clusters that are available for 1 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 1
file1 <- FullStack_V4_Zeros |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH1,
         AvgCrownDiameter = CD1,
         AvgCrownHeight = CH1,
         AvgCrownArea = CA1,
         SpeciesCount = SC1,
         AvgTreeHeight = TH1,
         TreeCount = TE1,
         J = J1) |> 
  # add file grouping
  mutate(File = 1) |> 
  relocate(File)

file1

## File 3
file3 <- FullStack_V4_Zeros |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3, X, Y) |> 
  # changing variable names to file-unspecific names -> in order to create long data format 
  rename(AvgDbh = DBH3,
         AvgCrownDiameter = CD3,
         AvgCrownHeight = CH3,
         AvgCrownArea = CA3,
         SpeciesCount = SC3,
         AvgTreeHeight = TH3,
         TreeCount = TE3,
         J = J3) |> 
  #add file grouping
  mutate(File = 3) |> 
  relocate(File)

file3

#### STEP 2: merge into long data format ----
file13 <- rbind(file1, file3)


#### STEP 3: write .csv for python ----

# write.csv(file13, "iMAD_Data_13_Constant_Zeros.csv")

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


####################          IR-MAD CHANGE DETECTION RESULTS            ----------------------------------
################## 1) Comparison of Cycle 1 and 2 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_12_Constant <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_12_Constant.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
View(iMAD_results_12_Constant)

iMAD_results_12_Constant <- iMAD_results_12_Constant |> 
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

#### STEP 3: Plotting ----
## Column 7
# Original
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") 
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-5, 5))

## Column 8
# Original
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") 

# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-5, 5))

#### STEP 4: Geospatial Prep ----
# writeVector(vect(iMAD_results_12_Constant, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_12_Constant.shp")

########### CUT -------------------------------------------------------------------------------------------------
################## 2) Comparison of Cycle 2 and 3 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_23_Constant <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_23_Constant.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_23_Constant <- iMAD_results_23_Constant |> 
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

#### STEP 3: Plotting ----
## Column 7
# Original
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38")
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-5, 5)) 

## Column 8
# Original
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") 

# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-5, 5))

#### STEP 4: Geospatial Prep ----

# writeVector(vect(iMAD_results_23_Constant, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_23_Constant.shp")

########### CUT -------------------------------------------------------------------------------------------------
################## 3) Comparison of Cycle 1 and 3 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_13_Constant <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_13_Constant.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_13_Constant <- iMAD_results_13_Constant |> 
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

#### STEP 3: Plotting ----
## Column 7
# Original
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF")
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-5, 5))

## Column 8
# Original
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") 

# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-20, 20))

#### STEP 4: Geospatial Prep ----

# writeVector(vect(iMAD_results_13_Constant, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_13_Constant.shp")

################## 4) CrOSS COMPARISON - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### Violin Plot ----
## STEP 1: Data Preparation ----
iMAD_results_Constant <- rbind(iMAD_results_12_Constant, iMAD_results_13_Constant, iMAD_results_23_Constant) |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13")))
## STEP 2: Plotting ----
## Column 7
# Original
iMAD_results_Constant |> 
  ggplot(aes(x = Comparison, y = Col_7, fill = Comparison)) +
  geom_violin()
# zoomed
iMAD_results_Constant |> 
  ggplot(aes(x = Comparison, y = Col_7, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))

## Column 8
# Original
iMAD_results_Constant |> 
  ggplot(aes(x = Comparison, y = Col_8, fill = Comparison)) +
  geom_violin()
# Zoomed
iMAD_results_Constant |> 
  ggplot(aes(x = Comparison, y = Col_8, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


################### PAPER/BA PLOTS (DATA: FullStack_V4 & FullStack_V4_Zeros) -------------------------------------------------------------
#### 1) Violinplots --------------------------------------------
## 1.1) Species Richness ----
# FullStack_V4 & FullStack_V4 (both 0s for Species Counts)
FullStack_V4 %>% 
  select(SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "SpecChange") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC12", "SC23", "SC13"))) |> 
  ggplot( aes(x= Comparison, y= SpecChange, fill=Comparison)) +
  geom_violin()
  
## 1.2) Biomass ----
## 1.3) iMAD ----



## 1.4) Total Entries ----
# FullStack_V4 & FullStack_V4 (both 0s for Total Tree Entries)
FullStack_V4 %>% 
  select(TE12, TE23, TE13) |> 
  pivot_longer(c(TE12, TE23, TE13), names_to = "Comparison", values_to = "SpecChange") |> 
  mutate(Comparison = factor(Comparison, levels = c("TE12", "TE23", "TE13"))) |> 
  ggplot( aes(x= Comparison, y= SpecChange, fill=Comparison)) +
  geom_violin()

#### 2) Scatterplots -------------------------------------------
## 1.1) Species Richness ----
# FullStack_V4 & FullStack_V4 (both 0s for Species Counts)
# Cycle 1 - 2:
FullStack_V4 |> 
  ggplot(aes(x = SC2, y = SC1)) +
  geom_point() +
  geom_abline(color = "red")

# Cycle 2 - 3:
FullStack_V4 |> 
  ggplot(aes(x = SC3, y = SC2)) +
  geom_point() +
  geom_abline(color = "red")

# Cycle 1 - 3:
FullStack_V4 |> 
  ggplot(aes(x = SC3, y = SC1)) +
  geom_point() +
  geom_abline(color = "red")
  

## 1.2) Biomass ----
## 1.3) iMAD ----



## 1.4) Total Entries ----
# FullStack_V4 & FullStack_V4 (both 0s for Total Tree Entries)
# Cycle 1 - 2:
FullStack_V4 |> 
  ggplot(aes(x = TE2, y = TE1)) +
  geom_point() +
  geom_abline(color = "red")

# Cycle 2 - 3:
FullStack_V4 |> 
  ggplot(aes(x = TE3, y = TE2)) +
  geom_point() +
  geom_abline(color = "red")

# Cycle 1 - 3:
FullStack_V4 |> 
  ggplot(aes(x = TE3, y = TE1)) +
  geom_point() +
  geom_abline(color = "red")


#### 3) Ridges -------------------------------------------------
## 1.1) Species Richness ----
# FullStack_V4 & FullStack_V4 (both 0s for Species Counts)


## 1.2) Biomass ----

## 1.3) iMAD ----

## 1.4) Total Entries ----
# FullStack_V4 & FullStack_V4 (both 0s for Total Tree Entries)
FullStack_V4 %>% 
  select(TE12, TE23, TE13) |> 
  pivot_longer(c(TE12, TE23, TE13), names_to = "Comparison", values_to = "SpecChange") |> 
  mutate(Comparison = factor(Comparison, levels = c("TE12", "TE23", "TE13"))) |> 
  ggplot( aes(x= Comparison, y= SpecChange, fill=Comparison)) +
  geom_violin() +
  geom_ridges

## 3.1) Species Richness ----
FullStack_V4 %>% 
  select(SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "SpecChange") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC12", "SC23", "SC13"))) |> 
  ggplot( aes(x= Comparison, y= SpecChange, fill=Comparison)) +
  geom_violin()


##################################     END      ##################################################################




################### CLUSTER METADATA DATASET PLOTTING           ##################################################
#### 1) Individual Tree Entries per Cluster (DATA: ALL SAMPLED CLUSTERS) --------------------------------------------------------------

# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1)
C2 <- FullStack_V4 |> 
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





#################### A) EVERYTHING WITH A FILTER FOR TREES ONLY ##################################################
#################### 4) EDA PREPARATION ------------------------------------------------------------------
###### 4.1) SPECIES RICHNESS + INDIVIDUAL TREE COUNT ---------------------------
#### DATA ON CLUSTER LEVEL
C_SpecRich_2 <- merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Cluster_ID)

#### DATA ON PLOT LEVEL - used for later calculation in 5.3)
SpecRich_2 <- merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, Sitio) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Plot_ID)


###### 4.2) SPECIES ABUNDANCES - needed for shannon index and eveness - DATA ON SPECIES LEVEL --------

#### DATA CALCULATED PER CLUSTER
C_SpecAbun_2 <- merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  select(File, Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, NombreCientifico_APG) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            abundance=n(),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Cluster_ID)


###### 4.3) SHANNON INDEX H + PIELOU EVENESS J - uses temporary created dataframes! -------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON CLUSTER LEVEL
C_Temp.Shannon_2 <- C_SpecAbun_2 |> 
  ungroup() |> 
  select(Cluster_ID, NombreCientifico_APG, abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

#### STEP 2: exchange NAs with Zeros 
## DATA ON CLUSTER LEVEL
C_PresenceAbsence_2 <- C_Temp.Shannon_2 |> 
  replace(is.na(C_Temp.Shannon_2), 0)

#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON CLUSTER LEVEL
H_2 <- diversity(C_PresenceAbsence_2[,-1])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON CLUSTER LEVEL
J_2 <- H_2/log(specnumber(C_PresenceAbsence_2[, -1]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
C_Temp.HJ_2 <- data.frame(C_PresenceAbsence_2$Cluster_ID, H_2, J_2) |> 
  rename(Cluster_ID = C_PresenceAbsence_2.Cluster_ID)

#### STEP 6: merged data table 
## DATA ON CLUSTER LEVEL
ClusterDiagnostics_2 <- left_join(C_SpecRich_2, C_Temp.HJ_2, by= c("Cluster_ID")) |> 
  select(File, Cluster_ID, Conglomerado, Anio, species_count, total_entries, H_2, J_2, X, Y)


###### 4.4) TREE MORPHOLOGY -----------------------------------------------

#### DATA ON CLUSTER LEVEL ----- calculated by individual entries (alternatively by means of plot means)
# need more thought going into whether to use means or medians
# Example: for Tree Height Means are on average -20cm compared to Median

C_TreeMorp_2 <- merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            AvgTreeHeight = mean(AlturaTotal, na.rm = T),
            Med_AvgTreeHeight = median(AlturaTotal, na.rm = T),
            AvgDbh = mean(DiametroNormal, na.rm = T),
            Med_AvgDbh = median(DiametroNormal, na.rm = T),
            AvgCrownDiameter = mean(DiametroCopa, na.rm = T),
            Med_AvgCrownDiameter = median(DiametroCopa, na.rm = T),
            AvgCrownHeight = mean(AlturaTotal - AlturaFusteLimpio, na.rm = T),          # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            Med_AvgCrownHeight = median(AlturaTotal - AlturaFusteLimpio, na.rm = T),    # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            AvgCrownArea = mean(AreaCopa, na.rm = T),
            Med_AvgCrownArea = median(AreaCopa, na.rm = T),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Cluster_ID)

###### 4.5) COMPLETE DIAGNOSTICS DATASET (excluding Biomass; 01/10/2024) ----------------------------

Comp_C_Diagnostics_2 <- left_join(ClusterDiagnostics_2, C_TreeMorp_2, by= c("Cluster_ID", "File", "Conglomerado", "Anio", "X", "Y")) |> 
  relocate(Cluster_ID, File, Conglomerado, Anio, species_count, total_entries, H_2, J_2, 
           AvgTreeHeight, Med_AvgTreeHeight, AvgDbh, Med_AvgDbh, AvgCrownDiameter, Med_AvgCrownDiameter, AvgCrownHeight, Med_AvgCrownHeight, AvgCrownArea, Med_AvgCrownArea, X, Y)

# write.csv(Comp_C_Diagnostics, "INFyS_Selection_Cluster.csv")


#################### 5) METADATA STUFF -------------------------------------------------------------------
###### 5.1) PLOT COUNTS FOR EACH CLUSTER -----------------------------------------

# Adding the Number of Plots per Cluster ("Plots")
PlotCounts_2 <- merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  group_by(File, Conglomerado, Sitio, X, Y) |> 
  count() |>
  ungroup() |> 
  select(File, Conglomerado, Sitio, X, Y) |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.numeric(File)),
            Conglomerado = mean(Conglomerado),
            X = mean(X),
            Y = mean(Y),
            Plots = n())

# combined dataset
Comp_C_Diagnostics_V2_2 <- left_join(Comp_C_Diagnostics_2, PlotCounts_2, by= c("File", "Conglomerado")) |> 
  select(everything(), -c("X.y", "Y.y")) |> 
  rename(X = X.x,
         Y = Y.x)

###### 5.2) AVAILABILITY OF PLOTS FOR EACH CLUSTER -----------------------------------------------

# function to count number of cycles with a given number of plots 
summarise_data <- function(data, plot_number) {
  data |> 
    select(File, Conglomerado, Anio, Plots, X, Y) |> 
    filter(Plots == plot_number) |> 
    group_by(Conglomerado) |> 
    arrange(Conglomerado) |> 
    summarise(Conglomerado = mean(Conglomerado),
              n = n(),
              X1 = mean(X),
              Y1 = mean(Y)) |> 
    rename(n_cycles_placeholder = n)
}

# combined dataset V3
Comp_C_Diagnostics_V3_2 <- Comp_C_Diagnostics_V2_2 %>% 
  left_join(summarise_data(. ,4), by = "Conglomerado") %>%
  left_join(summarise_data(. ,3), by = "Conglomerado") %>% 
  left_join(summarise_data(. ,2), by = "Conglomerado") %>%
  left_join(summarise_data(. ,1), by = "Conglomerado") %>%
  select(-c("X1.x", "Y1.x", "X1.y", "Y1.y", "X1.x.x", "Y1.x.x", "X1.y.y", "Y1.y.y")) |> 
  rename(cycles_four_plots = n_cycles_placeholder.x,
         cycles_three_plots = n_cycles_placeholder.y,
         cycles_two_plots = n_cycles_placeholder.x.x,
         cycles_one_plots = n_cycles_placeholder.y.y,) |> 
  arrange(Conglomerado) |> 
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
PTC_C_2 <- SpecRich_2 |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(File),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            Plot_TreeCount_Mean = mean(total_entries),
            Plot_TreeCount_Median = median(total_entries),
            X = mean(X),
            Y = mean(Y)) |> 
  select(File, Conglomerado, Anio, Plot_TreeCount_Mean, Plot_TreeCount_Median)

Comp_C_Diagnostics_V4_2 <- left_join(Comp_C_Diagnostics_V3_2, PTC_C_2, by = c("File", "Conglomerado", "Anio"))

###### 5.4) ESTADO + TIPO EGETACION FILTER -----------------------------------------------------

Comp_C_Diagnostics_V5_2 <- left_join(Comp_C_Diagnostics_V4_2, merged |> 
                                       filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
                                       select(Cluster_ID, Estado, CveVeg, TipoVeg) |> distinct(),
                                   by = "Cluster_ID")

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####################          METADATA DATASET CODE (TREES ONLY FILTER)           ###############################
#################### 1) LOAD RAW DATA ------------------------------------------------------------
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

# STEP 2: JOIN CLUSTER_ID + X & Y FROM EACH FILE
#File 1
Temp <- ClusterBase |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado1 = Conglomerado,
                     X1 = X,
                     Y1 = Y) |> 
              select(Cluster_ID, Conglomerado1, X1, Y1),
            by = "Cluster_ID")
#File 2
Temp <- Temp |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado2 = Conglomerado,
                     X2 = X,
                     Y2 = Y) |> 
              select(Cluster_ID, Conglomerado2, X2, Y2),
            by = "Cluster_ID")
#File 3
MetaBase <- Temp |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Conglomerado3 = IDConglomerado,
                     X3 = X_C3,
                     Y3 = Y_C3) |> 
              select(Cluster_ID, Conglomerado3, X3, Y3),
            by = "Cluster_ID")

# STEP 3: AVERAGE X and Y
NewBase <- MetaBase |> 
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
  # Differences in Clusterabailability - for both sampled and not sampled
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
NewBase2 <- NewBase |> 
  # Join File 1
  left_join(Sec.04 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado1 = Muestreado) |> 
              select(Cluster_ID, Muestreado1),
            by = "Cluster_ID"
  ) |> 
  # Join File 2
  left_join(Sec.09 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado2 = Muestreado) |> 
              select(Cluster_ID, Muestreado2),
            by = "Cluster_ID") |> 
  # Join File 3
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
FullStack_2 <- NewBase2 %>% 
  left_join(Comp_C_Diagnostics_V5_2 %>%
              filter(File == 1) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File1 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File1"),
            by = "Cluster_ID") %>%
  mutate(File1 = case_when(is.na(File1) ~ 100,
                           T ~ File1)) |> 
  left_join(Comp_C_Diagnostics_V5_2 %>%
              filter(File == 2) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File2 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File2"),
            by = "Cluster_ID") %>%
  mutate(File2 = case_when(is.na(File2) ~ 100,
                           T ~ File2)) |> 
  left_join(Comp_C_Diagnostics_V5_2 %>%
              filter(File == 3) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File3 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File3"),
            by = "Cluster_ID") %>%
  mutate(File3 = case_when(is.na(File3) ~ 100,
                           T ~ File3))

# STEP 2: Claculate Status of Cluster for each Cycle
FullStack_V2_2 <- FullStack_2 |> 
  mutate(Status1 = Muestreado1 - File1,
         Status2 = Muestreado2 - File2,
         Status3 = Muestreado3 - File3) |> 
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
FullStack_V3_2 <- FullStack_V2_2 |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S1 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_2 %>%
              filter(File == 1) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot1 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "Plot1"),
            by = "Cluster_ID") |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S2 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_2 %>%
              filter(File == 2) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot2 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "Plot2"),
            by = "Cluster_ID") |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Plot_S3 = Sitios_x_cgl_C3) |> 
              select(Cluster_ID, Plot_S3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_2 %>%
              filter(File == 3) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot3 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
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


################### 4) RE-INTRODUCE ECOREGIONS, TOTAL ENTRIES, SPEC COUNT, AVG DBH PER CLUSTER FOR EACH CYCLE ---------
FullStack_V4_2 <- FullStack_V3_2 |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado) |> 
              select(Cluster_ID, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_2 |> 
              ungroup() |> 
              filter(File == 1) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH1 = AvgDbh,
                     CD1 = AvgCrownDiameter,
                     CH1 = AvgCrownHeight,
                     CA1 = AvgCrownArea,
                     SC1 = species_count,
                     TH1 = AvgTreeHeight,
                     TE1 = total_entries,
                     J1 = J_2) |> 
              select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_2 |> 
              ungroup() |> 
              filter(File == 2) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH2 = AvgDbh,
                     CD2 = AvgCrownDiameter,
                     CH2 = AvgCrownHeight,
                     CA2 = AvgCrownArea,
                     SC2 = species_count,
                     TH2 = AvgTreeHeight,
                     TE2 = total_entries,
                     J2 = J_2) |> 
              select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_2 |> 
              ungroup() |> 
              filter(File == 3) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH3 = AvgDbh,
                     CD3 = AvgCrownDiameter,
                     CH3 = AvgCrownHeight,
                     CA3 = AvgCrownArea,
                     SC3 = species_count,
                     TH3 = AvgTreeHeight,
                     TE3 = total_entries,
                     J3 = J_2) |> 
              select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3),
            by = "Cluster_ID") |> 
  mutate(TE1 = case_when(Muestreado1 == 1 & is.na(TE1) ~ 0,
                         T ~ TE1),
         TE2 = case_when(Muestreado2 == 1 & is.na(TE2) ~ 0,
                         T ~ TE2),
         TE3 = case_when(Muestreado3 == 1 & is.na(TE3) ~ 0,
                         T ~ TE3)) |> 
  # in case you wanted to calculate univariate changes in R 
  mutate(CTE13 = TE3 - TE1,
         SC13 = SC3 - SC1,
         DBH13 = DBH3 -DBH1) |> 
  ungroup()


################### 4) TREE COUNT CHANGE CALCULATION BASED ON ECOREGIONS -----------------------------------

# STEP 1: IDK


# STEP 2: IDK




################### 4) PLACEHOLDER -----------------------------------

##### EMPTY 

################### 5) GEOSPATIAL PREPARATION -----------------------------------

#writeVector(vect(Sec.04, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Clusters04.shp")
#writeVector(vect(Sec.09, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Clusters09.shp")
#writeVector(vect(Sec.14, geom = c("X_C3", "Y_C3"), crs = "+proj=longlat +datum=WGS84"), "Clusters14.shp")

#writeVector(vect(FullStack_V3, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhatClustersChanged_v2.shp")

#writeVector(vect(FullStack_V4 |> filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1), geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhatClustersChanged_v2.shp")


#DATA: FUllStack_V4 - for cluster based change in tree counts
#writeVector(vect(FullStack_V4, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4.shp")

#DATA: FullStack_V4 - FILTER: only constantly sampled clusters (n = 9 795)
#writeVector(vect(FullStack_V4 |> filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1), geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4_F1.shp")

##################################     END      ##################################################################


################### CLUSTER METADATA DATASET PLOTTING (TREES ONLY FILTER)          ###############################
#### 1) Individual Tree Entries per Cluster (DATA: ALL SAMPLED CLUSTERS) --------------------------------------------------------------

# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1)
C2 <- FullStack_V4_2 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2)
C3 <- FullStack_V4_2 |> 
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
C1 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4_2 |> 
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
C1 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4_2 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4_2 |> 
  filter(Muestreado3 == 1) |> 
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





#### 2.1) Species Richness (Spec Count) per Cluster (DATA: SAMPLED CLUSTERS THROUGH 1-2-3) ----------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4_2 |> 
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
C1 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1, SC1, DBH1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1,
         DBH = DBH1)
C2 <- FullStack_V4_2 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2, SC2, DBH2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2,
         DBH = DBH2)
C3 <- FullStack_V4_2 |> 
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
C1 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1, DBH1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1,
         DBH = DBH1)
C2 <- FullStack_V4_2 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2, DBH2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2,
         DBH = DBH2)
C3 <- FullStack_V4_2 |> 
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
#################### A) END ######################################################################################################








#################### B) EVERYTHING WITH A FILTER FOR DBH >= 10 ######################################################################################
#################### 4) EDA PREPARATION ------------------------------------------------------------------
###### 4.1) SPECIES RICHNESS + INDIVIDUAL TREE COUNT ---------------------------
#### DATA ON CLUSTER LEVEL
C_SpecRich_3 <- merged |> 
  filter(DiametroNormal >= 10) |>
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Cluster_ID)

#### DATA ON PLOT LEVEL - used for later calculation in 5.3)
SpecRich_3 <- merged |> 
  filter(DiametroNormal >= 10) |>
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, Sitio) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Plot_ID)


###### 4.2) SPECIES ABUNDANCES - needed for shannon index and eveness - DATA ON SPECIES LEVEL --------

#### DATA CALCULATED PER CLUSTER
C_SpecAbun_3 <- merged |> 
  filter(DiametroNormal >= 10) |>
  select(File, Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, NombreCientifico_APG) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            abundance=n(),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Cluster_ID)


###### 4.3) SHANNON INDEX H + PIELOU EVENESS J - uses temporary created dataframes! -------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON CLUSTER LEVEL
C_Temp.Shannon_3 <- C_SpecAbun_3 |> 
  ungroup() |> 
  select(Cluster_ID, NombreCientifico_APG, abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

#### STEP 2: exchange NAs with Zeros 
## DATA ON CLUSTER LEVEL
C_PresenceAbsence_3 <- C_Temp.Shannon_3 |> 
  replace(is.na(C_Temp.Shannon_3), 0)

#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON CLUSTER LEVEL
H_3 <- diversity(C_PresenceAbsence_3[,-1])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON CLUSTER LEVEL
J_3 <- H_3/log(specnumber(C_PresenceAbsence_3[, -1]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
C_Temp.HJ_3 <- data.frame(C_PresenceAbsence_3$Cluster_ID, H_3, J_3) |> 
  rename(Cluster_ID = C_PresenceAbsence_3.Cluster_ID)

#### STEP 6: merged data table 
## DATA ON CLUSTER LEVEL
ClusterDiagnostics_3 <- left_join(C_SpecRich_3, C_Temp.HJ_3, by= c("Cluster_ID")) |> 
  select(File, Cluster_ID, Conglomerado, Anio, species_count, total_entries, H_3, J_3, X, Y)


###### 4.4) TREE MORPHOLOGY -----------------------------------------------

#### DATA ON CLUSTER LEVEL ----- calculated by individual entries (alternatively by means of plot means)
# need more thought going into whether to use means or medians
# Example: for Tree Height Means are on average -20cm compared to Median

C_TreeMorp_3 <- merged |> 
  filter(DiametroNormal >= 10) |>
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            AvgTreeHeight = mean(AlturaTotal, na.rm = T),
            Med_AvgTreeHeight = median(AlturaTotal, na.rm = T),
            AvgDbh = mean(DiametroNormal, na.rm = T),
            Med_AvgDbh = median(DiametroNormal, na.rm = T),
            AvgCrownDiameter = mean(DiametroCopa, na.rm = T),
            Med_AvgCrownDiameter = median(DiametroCopa, na.rm = T),
            AvgCrownHeight = mean(AlturaTotal - AlturaFusteLimpio, na.rm = T),          # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            Med_AvgCrownHeight = median(AlturaTotal - AlturaFusteLimpio, na.rm = T),    # heuristic calculation: instances in which AT < AFL = negative values for crown height = makes no sense
            AvgCrownArea = mean(AreaCopa, na.rm = T),
            Med_AvgCrownArea = median(AreaCopa, na.rm = T),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Cluster_ID)

###### 4.5) COMPLETE DIAGNOSTICS DATASET (excluding Biomass; 01/10/2024) ----------------------------

Comp_C_Diagnostics_3 <- left_join(ClusterDiagnostics_3, C_TreeMorp_3, by= c("Cluster_ID", "File", "Conglomerado", "Anio", "X", "Y")) |> 
  relocate(Cluster_ID, File, Conglomerado, Anio, species_count, total_entries, H_3, J_3, 
           AvgTreeHeight, Med_AvgTreeHeight, AvgDbh, Med_AvgDbh, AvgCrownDiameter, Med_AvgCrownDiameter, AvgCrownHeight, Med_AvgCrownHeight, AvgCrownArea, Med_AvgCrownArea, X, Y)

# write.csv(Comp_C_Diagnostics, "INFyS_Selection_Cluster.csv")


#################### 5) METADATA STUFF -------------------------------------------------------------------
###### 5.1) PLOT COUNTS FOR EACH CLUSTER -----------------------------------------

# Adding the Number of Plots per Cluster ("Plots")
PlotCounts_3 <- merged |> 
  filter(DiametroNormal >= 10) |>
  group_by(File, Conglomerado, Sitio, X, Y) |> 
  count() |>
  ungroup() |> 
  select(File, Conglomerado, Sitio, X, Y) |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(as.numeric(File)),
            Conglomerado = mean(Conglomerado),
            X = mean(X),
            Y = mean(Y),
            Plots = n())

# combined dataset
Comp_C_Diagnostics_V2_3 <- left_join(Comp_C_Diagnostics_3, PlotCounts_3, by= c("File", "Conglomerado")) |> 
  select(everything(), -c("X.y", "Y.y")) |> 
  rename(X = X.x,
         Y = Y.x)

###### 5.2) AVAILABILITY OF PLOTS FOR EACH CLUSTER -----------------------------------------------

# function to count number of cycles with a given number of plots 
summarise_data <- function(data, plot_number) {
  data |> 
    select(File, Conglomerado, Anio, Plots, X, Y) |> 
    filter(Plots == plot_number) |> 
    group_by(Conglomerado) |> 
    arrange(Conglomerado) |> 
    summarise(Conglomerado = mean(Conglomerado),
              n = n(),
              X1 = mean(X),
              Y1 = mean(Y)) |> 
    rename(n_cycles_placeholder = n)
}

# combined dataset V3
Comp_C_Diagnostics_V3_3 <- Comp_C_Diagnostics_V2_3 %>% 
  left_join(summarise_data(. ,4), by = "Conglomerado") %>%
  left_join(summarise_data(. ,3), by = "Conglomerado") %>% 
  left_join(summarise_data(. ,2), by = "Conglomerado") %>%
  left_join(summarise_data(. ,1), by = "Conglomerado") %>%
  select(-c("X1.x", "Y1.x", "X1.y", "Y1.y", "X1.x.x", "Y1.x.x", "X1.y.y", "Y1.y.y")) |> 
  rename(cycles_four_plots = n_cycles_placeholder.x,
         cycles_three_plots = n_cycles_placeholder.y,
         cycles_two_plots = n_cycles_placeholder.x.x,
         cycles_one_plots = n_cycles_placeholder.y.y,) |> 
  arrange(Conglomerado) |> 
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
PTC_C_3 <- SpecRich_3 |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(File),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            Plot_TreeCount_Mean = mean(total_entries),
            Plot_TreeCount_Median = median(total_entries),
            X = mean(X),
            Y = mean(Y)) |> 
  select(File, Conglomerado, Anio, Plot_TreeCount_Mean, Plot_TreeCount_Median)

Comp_C_Diagnostics_V4_3 <- left_join(Comp_C_Diagnostics_V3_3, PTC_C_3, by = c("File", "Conglomerado", "Anio"))

###### 5.4) ESTADO + TIPO EGETACION FILTER -----------------------------------------------------

Comp_C_Diagnostics_V5_3 <- left_join(Comp_C_Diagnostics_V4_3, merged |> 
                                       filter(DiametroNormal >= 10) |>
                                       select(Cluster_ID, Estado, CveVeg, TipoVeg) |> distinct(),
                                     by = "Cluster_ID")

########## END -------------------------------------------------------------------------------------------


####################          METADATA DATASET CODE (TREES ONLY FILTER)           -----------------------------------------------
#################### 1) LOAD RAW DATA ------------------------------------------------------------
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

# STEP 2: JOIN CLUSTER_ID + X & Y FROM EACH FILE
#File 1
Temp <- ClusterBase |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado1 = Conglomerado,
                     X1 = X,
                     Y1 = Y) |> 
              select(Cluster_ID, Conglomerado1, X1, Y1),
            by = "Cluster_ID")
#File 2
Temp <- Temp |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Conglomerado2 = Conglomerado,
                     X2 = X,
                     Y2 = Y) |> 
              select(Cluster_ID, Conglomerado2, X2, Y2),
            by = "Cluster_ID")
#File 3
MetaBase <- Temp |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Conglomerado3 = IDConglomerado,
                     X3 = X_C3,
                     Y3 = Y_C3) |> 
              select(Cluster_ID, Conglomerado3, X3, Y3),
            by = "Cluster_ID")

# STEP 3: AVERAGE X and Y
NewBase <- MetaBase |> 
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
  # Differences in Clusterabailability - for both sampled and not sampled
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
NewBase2 <- NewBase |> 
  # Join File 1
  left_join(Sec.04 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado1 = Muestreado) |> 
              select(Cluster_ID, Muestreado1),
            by = "Cluster_ID"
  ) |> 
  # Join File 2
  left_join(Sec.09 |> 
              select(Conglomerado, Muestreado) |> 
              mutate(Cluster_ID = Conglomerado,
                     Muestreado2 = Muestreado) |> 
              select(Cluster_ID, Muestreado2),
            by = "Cluster_ID") |> 
  # Join File 3
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
FullStack_3 <- NewBase2 %>% 
  left_join(Comp_C_Diagnostics_V5_3 %>%
              filter(File == 1) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File1 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File1"),
            by = "Cluster_ID") %>%
  mutate(File1 = case_when(is.na(File1) ~ 100,
                           T ~ File1)) |> 
  left_join(Comp_C_Diagnostics_V5_3 %>%
              filter(File == 2) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File2 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File2"),
            by = "Cluster_ID") %>%
  mutate(File2 = case_when(is.na(File2) ~ 100,
                           T ~ File2)) |> 
  left_join(Comp_C_Diagnostics_V5_3 %>%
              filter(File == 3) %>%
              rename(Cluster_ID2 = Cluster_ID,
                     File3 = File) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "File3"),
            by = "Cluster_ID") %>%
  mutate(File3 = case_when(is.na(File3) ~ 100,
                           T ~ File3))

# STEP 2: Claculate Status of Cluster for each Cycle
FullStack_V2_3 <- FullStack_3 |> 
  mutate(Status1 = Muestreado1 - File1,
         Status2 = Muestreado2 - File2,
         Status3 = Muestreado3 - File3) |> 
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
FullStack_V3_3 <- FullStack_V2_3 |> 
  left_join(Sec.04 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S1 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_3 %>%
              filter(File == 1) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot1 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "Plot1"),
            by = "Cluster_ID") |> 
  left_join(Sec.09 |> 
              mutate(Cluster_ID = Conglomerado,
                     Plot_S2 = Sitios_x_cgl) |> 
              select(Cluster_ID, Plot_S2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_3 %>%
              filter(File == 2) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot2 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
              select("Cluster_ID", "Plot2"),
            by = "Cluster_ID") |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado,
                     Plot_S3 = Sitios_x_cgl_C3) |> 
              select(Cluster_ID, Plot_S3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_3 %>%
              filter(File == 3) %>%
              ungroup() |> 
              rename(Cluster_ID2 = Cluster_ID,
                     Plot3 = Plots) %>%
              mutate(Cluster_ID = Conglomerado) %>%
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


################### 4) RE-INTRODUCE ECOREGIONS, TOTAL ENTRIES, SPEC COUNT, AVG DBH PER CLUSTER FOR EACH CYCLE ---------
FullStack_V4_3 <- FullStack_V3_3 |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado) |> 
              select(Cluster_ID, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_3 |> 
              ungroup() |> 
              filter(File == 1) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH1 = AvgDbh,
                     CD1 = AvgCrownDiameter,
                     CH1 = AvgCrownHeight,
                     CA1 = AvgCrownArea,
                     SC1 = species_count,
                     TH1 = AvgTreeHeight,
                     TE1 = total_entries,
                     J1 = J_3) |> 
              select(Cluster_ID, DBH1, CD1, CH1, CA1, SC1, TH1, TE1, J1),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_3 |> 
              ungroup() |> 
              filter(File == 2) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH2 = AvgDbh,
                     CD2 = AvgCrownDiameter,
                     CH2 = AvgCrownHeight,
                     CA2 = AvgCrownArea,
                     SC2 = species_count,
                     TH2 = AvgTreeHeight,
                     TE2 = total_entries,
                     J2 = J_3) |> 
              select(Cluster_ID, DBH2, CD2, CH2, CA2, SC2, TH2, TE2, J2),
            by = "Cluster_ID") |> 
  left_join(Comp_C_Diagnostics_V5_3 |> 
              ungroup() |> 
              filter(File == 3) |> 
              mutate(Cluster_ID2 = Cluster_ID,
                     Cluster_ID = Conglomerado,
                     DBH3 = AvgDbh,
                     CD3 = AvgCrownDiameter,
                     CH3 = AvgCrownHeight,
                     CA3 = AvgCrownArea,
                     SC3 = species_count,
                     TH3 = AvgTreeHeight,
                     TE3 = total_entries,
                     J3 = J_3) |> 
              select(Cluster_ID, DBH3, CD3, CH3, CA3, SC3, TH3, TE3, J3),
            by = "Cluster_ID") |> 
  mutate(TE1 = case_when(Muestreado1 == 1 & is.na(TE1) ~ 0,
                         T ~ TE1),
         TE2 = case_when(Muestreado2 == 1 & is.na(TE2) ~ 0,
                         T ~ TE2),
         TE3 = case_when(Muestreado3 == 1 & is.na(TE3) ~ 0,
                         T ~ TE3)) |> 
  # in case you wanted to calculate univariate changes in R 
  mutate(CTE13 = TE3 - TE1,
         SC13 = SC3 - SC1,
         DBH13 = DBH3 -DBH1) |> 
  ungroup()


################### 4) TREE COUNT CHANGE CALCULATION BASED ON ECOREGIONS -----------------------------------

# STEP 1: IDK


# STEP 2: IDK




################### 4) PLACEHOLDER -----------------------------------

##### EMPTY 

################### 5) GEOSPATIAL PREPARATION -----------------------------------

#writeVector(vect(Sec.04, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Clusters04.shp")
#writeVector(vect(Sec.09, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Clusters09.shp")
#writeVector(vect(Sec.14, geom = c("X_C3", "Y_C3"), crs = "+proj=longlat +datum=WGS84"), "Clusters14.shp")

#writeVector(vect(FullStack_V3, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhatClustersChanged_v2.shp")

#writeVector(vect(FullStack_V4 |> filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1), geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhatClustersChanged_v2.shp")


#DATA: FUllStack_V4 - for cluster based change in tree counts
#writeVector(vect(FullStack_V4, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4.shp")

#DATA: FullStack_V4 - FILTER: only constantly sampled clusters (n = 9 795)
#writeVector(vect(FullStack_V4 |> filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1), geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V4_F1.shp")

########### END -------------------------------------------------------------------------------------------------

################### CLUSTER METADATA DATASET PLOTTING (TREES ONLY FILTER)          -------------------------------------------------------------
#### 1) Individual Tree Entries per Cluster (DATA: ALL SAMPLED CLUSTERS) --------------------------------------------------------------

# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1)
C2 <- FullStack_V4_3 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2)
C3 <- FullStack_V4_3 |> 
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
C1 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4_3 |> 
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
C1 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4_3 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4_3 |> 
  filter(Muestreado3 == 1) |> 
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





#### 2.1) Species Richness (Spec Count) per Cluster (DATA: SAMPLED CLUSTERS THROUGH 1-2-3) ----------------
# STEP 1: Cut data into pieces devided by Cycle
C1 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1)
C2 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2)
C3 <- FullStack_V4_3 |> 
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
C1 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1) |> 
  select(TE1, SC1, DBH1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1,
         DBH = DBH1)
C2 <- FullStack_V4_3 |> 
  filter(Muestreado2 == 1) |> 
  select(TE2, SC2, DBH2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2,
         DBH = DBH2)
C3 <- FullStack_V4_3 |> 
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
C1 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE1, SC1, DBH1) |> 
  mutate(Cycle = as.factor(1)) |> 
  rename(TE = TE1,
         SC = SC1,
         DBH = DBH1)
C2 <- FullStack_V4_3 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(TE2, SC2, DBH2) |> 
  mutate(Cycle = as.factor(2)) |> 
  rename(TE = TE2,
         SC = SC2,
         DBH = DBH2)
C3 <- FullStack_V4_3 |> 
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



########## END ------------------------------------------------------------------------------------------
#################### B) END ######################################################################################################






















#################### AA) FILTER FOR TREES ONLY + EVERYTHING ON ECOREGIONS 4 LEVEL (CONSTANT PLOTS) -------------------------------
###### B2) CREATE ECOREGIONS DATASET (BASED ON FILE 3 DATA) ------------------------------
EcoRegions_2 <- Arb.14 |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  select(Conglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3) |> 
  distinct() 


###### B3) ADD ECOREGIONS 4 + ECO4_ID ----------------------------------------------------
merged_Eco_2 <- merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |>
  left_join(EcoRegions <- Arb.14 |> 
              select(Conglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3) |> 
              distinct(),
            by = "Conglomerado") |> 
  mutate(Eco1_ID = paste(File, DESECON1_C3, sep = "_"),
         Eco2_ID = paste(File, DESECON2_C3, sep = "_"),
         Eco3_ID = paste(File, DESECON3_C3, sep = "_"),
         Eco4_ID = paste(File, DESECON4_C3, sep = "_")) |> 
  relocate(Eco4_ID)


###### B4.1) Species Richness + Individual Tree Count ------------------------------------
Eco4_SpecRich_2 <- merged_Eco_2 |> 
  select(Eco4_ID, File, Conglomerado, DESECON4_C3, NombreCientifico_APG, X, Y) |> 
  group_by(File, Eco4_ID) |> 
  filter(DESECON4_C3 != is.na(DESECON4_C3)) |> 
  summarise(File = mean(as.integer(File)),
            Eco4_species_count = n_distinct(NombreCientifico_APG),
            Eco4_total_entries = n(),
            X = mean(X, na.rm = T),
            Y = mean(Y, na.rm = T)) |> 
  relocate(Eco4_ID)

###### B4.2) Species Abundances  ---------------------------------------------------------
Eco4_SpecAbun_2 <- merged_Eco_2 |> 
  select(Eco4_ID, File, Conglomerado, DESECON4_C3, NombreCientifico_APG, X, Y) |> 
  group_by(File, Eco4_ID, NombreCientifico_APG) |> 
  filter(DESECON4_C3 != is.na(DESECON4_C3)) |> 
  summarise(File = mean(as.integer(File)),
            Eco4_abundance = n(),
            X = mean(X, na.rm = T),
            Y = mean(Y, na.rm = T)) |> 
  relocate(Eco4_ID)

###### B4.3) Shannon Index H + Pielou eveness J ------------------------------------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON CLUSTER LEVEL
Eco4_Temp.Shannon_2 <- Eco4_SpecAbun_2 |> 
  ungroup() |> 
  select(Eco4_ID, NombreCientifico_APG, Eco4_abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = Eco4_abundance)

#### STEP 2: exchange NAs with Zeros 
## DATA ON CLUSTER LEVEL
Eco4_PresenceAbsence_2 <- Eco4_Temp.Shannon_2 |> 
  replace(is.na(Eco4_Temp.Shannon_2), 0)

#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON CLUSTER LEVEL
Eco4_H <- diversity(Eco4_PresenceAbsence_2[,-1])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON CLUSTER LEVEL
Eco4_J <- Eco4_H/log(specnumber(Eco4_PresenceAbsence_2[, -1]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
Eco4_Temp.HJ_2 <- data.frame(Eco4_PresenceAbsence_2$Eco4_ID, Eco4_H, Eco4_J) |> 
  rename(Eco4_ID = Eco4_PresenceAbsence_2.Eco4_ID)

#### STEP 6: merged data table 
## DATA ON CLUSTER LEVEL
Eco4Diagnostics_2 <- left_join(Eco4_SpecRich_2, Eco4_Temp.HJ_2, by= c("Eco4_ID")) |> 
  select(File, Eco4_ID, Eco4_species_count, Eco4_total_entries, Eco4_H, Eco4_J, X, Y)

###### B4.4) Tree Morphology -------------------------------------------------------------

# CURRENTLY EMPTY

###### B4.5) Complete Diagnostics Dataset (excluding Biomass) ----------------------------

# CURRENTLY EMPTY


###### B4.6) Reintroduce Clusters and DESECON4 --------------------------------------------------------
C3_Eco4_Diagnostics_2 <- Comp_C_Diagnostics_V5_2 |> filter(Cycles == 3) |>
  left_join(merged_Eco_2 |> 
              select(Eco4_ID, Cluster_ID, DESECON4_C3) |> 
              distinct(),
            by = "Cluster_ID") |> 
  left_join(Eco4Diagnostics_2 |> 
              ungroup() |> 
              select(Eco4_ID, Eco4_species_count, Eco4_total_entries, Eco4_H, Eco4_J),
            by = "Eco4_ID") |> 
  relocate(Eco4_ID, DESECON4_C3)

View(C3_Eco4_Diagnostics_2)
View(merged_Eco_2)

###### B4.7) Calculating Changes through cycles + geospatial preparation ---------------------------------
C3_Eco4_Changes_2 <- C3_Eco4_Diagnostics_2 |> filter(File == 1) |> ungroup() |> 
  mutate(Eco4_H1 = Eco4_H, Eco4_J1 = Eco4_J, Eco4_SC1 = Eco4_species_count, Eco4_TE1 = Eco4_total_entries) |> 
  select(Conglomerado, Eco4_SC1, Eco4_TE1, Eco4_H1, Eco4_J1, DESECON4_C3, X, Y) |>
  left_join(
    left_join(C3_Eco4_Diagnostics_2 |> filter(File == 2) |> ungroup() |> 
                mutate(Eco4_H2 = Eco4_H, Eco4_J2 = Eco4_J, Eco4_SC2 = Eco4_species_count, Eco4_TE2 = Eco4_total_entries) |> 
                select(Conglomerado, Eco4_SC2, Eco4_TE2, Eco4_H2, Eco4_J2, DESECON4_C3, X, Y),
              C3_Eco4_Diagnostics_2 |> filter(File == 3) |> ungroup() |> 
                mutate(Eco4_H3 = Eco4_H, Eco4_J3 = Eco4_J, Eco4_SC3 = Eco4_species_count, Eco4_TE3 = Eco4_total_entries) |> 
                select(Conglomerado, Eco4_SC3, Eco4_TE3, Eco4_H3, Eco4_J3, DESECON4_C3, X, Y),
              by = c("Conglomerado", "DESECON4_C3")),
    by = c("Conglomerado","DESECON4_C3")) |> 
  select(-c("X.x", "Y.x", "X.y", "Y.y")) |> 
  relocate(X, Y, DESECON4_C3) |> 
  mutate(Eco4_SC12 = Eco4_SC2 - Eco4_SC1,
         Eco4_SC23 = Eco4_SC3 - Eco4_SC2,
         Eco4_SC13 = Eco4_SC3 - Eco4_SC1,
         Eco4_TE12 = Eco4_TE2 - Eco4_TE1,
         Eco4_TE23 = Eco4_TE3 - Eco4_TE2,
         Eco4_TE13 = Eco4_TE3 - Eco4_TE1,
         Eco4_H12 = Eco4_H2 - Eco4_H1,
         Eco4_H23 = Eco4_H3 - Eco4_H2,
         Eco4_H13 = Eco4_H3 - Eco4_H1,
         Eco4_J12 = Eco4_J2 - Eco4_J1,
         Eco4_J23 = Eco4_J3 - Eco4_J2,
         Eco4_J13 = Eco4_J3 - Eco4_J1,) |> 
  select(X, Y, DESECON4_C3, Conglomerado, 
         Eco4_SC12, Eco4_SC23, Eco4_SC13, 
         Eco4_TE12, Eco4_TE23, Eco4_TE13,
         Eco4_H12, Eco4_H23, Eco4_H13,
         Eco4_J12, Eco4_J23, Eco4_J13)

#writeVector(vect(C3_Eco4_Changes_2, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Eco4_Testing_2.shp")


########### END ------------------------------------------------------------------------------------------


#################### BB) EVERYTHING ON ECOREGIONS 4 LEVEL (CONSTANT PLOTS) -------------------------------
###### B2) CREATE ECOREGIONS DATASET (BASED ON FILE 3 DATA) ------------------------------
EcoRegions <- Raw.14 |> 
  select(IdConglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3) |> 
  distinct() |> 
  rename(Conglomerado = IdConglomerado)



###### B3) ADD ECOREGIONS 4 + ECO4_ID ----------------------------------------------------
merged_Eco <- merged |> 
  left_join(EcoRegions <- Raw.14 |> 
              select(IdConglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3) |> 
              distinct() |> 
              rename(Conglomerado = IdConglomerado),
            by = "Conglomerado") |> 
  mutate(Eco1_ID = paste(File, DESECON1_C3, sep = "_"),
         Eco2_ID = paste(File, DESECON2_C3, sep = "_"),
         Eco3_ID = paste(File, DESECON3_C3, sep = "_"),
         Eco4_ID = paste(File, DESECON4_C3, sep = "_")) |> 
  relocate(Eco4_ID)



###### B4.1) Species Richness + Individual Tree Count ------------------------------------
Eco4_SpecRich <- merged_Eco |> 
  select(Eco4_ID, File, Conglomerado, DESECON4_C3, NombreCientifico_APG, X, Y) |> 
  group_by(File, Eco4_ID) |> 
  filter(DESECON4_C3 != is.na(DESECON4_C3)) |> 
  summarise(File = mean(as.integer(File)),
            Eco4_species_count = n_distinct(NombreCientifico_APG),
            Eco4_total_entries = n(),
            X = mean(X, na.rm = T),
            Y = mean(Y, na.rm = T)) |> 
  relocate(Eco4_ID)

###### B4.2) Species Abundances  ---------------------------------------------------------
Eco4_SpecAbun <- merged_Eco |> 
  select(Eco4_ID, File, Conglomerado, DESECON4_C3, NombreCientifico_APG, X, Y) |> 
  group_by(File, Eco4_ID, NombreCientifico_APG) |> 
  filter(DESECON4_C3 != is.na(DESECON4_C3)) |> 
  summarise(File = mean(as.integer(File)),
            Eco4_abundance = n(),
            X = mean(X, na.rm = T),
            Y = mean(Y, na.rm = T)) |> 
  relocate(Eco4_ID)

###### B4.3) Shannon Index H + Pielou eveness J ------------------------------------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON CLUSTER LEVEL
Eco4_Temp.Shannon <- Eco4_SpecAbun |> 
  ungroup() |> 
  select(Eco4_ID, NombreCientifico_APG, Eco4_abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = Eco4_abundance)

#### STEP 2: exchange NAs with Zeros 
## DATA ON CLUSTER LEVEL
Eco4_PresenceAbsence <- Eco4_Temp.Shannon |> 
  replace(is.na(Eco4_Temp.Shannon), 0)

#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON CLUSTER LEVEL
Eco4_H <- diversity(Eco4_PresenceAbsence[,-1])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON CLUSTER LEVEL
Eco4_J <- Eco4_H/log(specnumber(Eco4_PresenceAbsence[, -1]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
Eco4_Temp.HJ <- data.frame(Eco4_PresenceAbsence$Eco4_ID, Eco4_H, Eco4_J) |> 
  rename(Eco4_ID = Eco4_PresenceAbsence.Eco4_ID)

#### STEP 6: merged data table 
## DATA ON CLUSTER LEVEL
Eco4Diagnostics <- left_join(Eco4_SpecRich, Eco4_Temp.HJ, by= c("Eco4_ID")) |> 
  select(File, Eco4_ID, Eco4_species_count, Eco4_total_entries, Eco4_H, Eco4_J, X, Y)

###### B4.4) Tree Morphology -------------------------------------------------------------

# CURRENTLY EMPTY

###### B4.5) Complete Diagnostics Dataset (excluding Biomass) ----------------------------

# CURRENTLY EMPTY


###### B4.6) Reintroduce Clusters and DESECON4 --------------------------------------------------------
C3_Eco4_Diagnostics <- Comp_C_Diagnostics_V5 |> filter(Cycles == 3) |>
  left_join(merged_Eco |> 
              select(Eco4_ID, Cluster_ID, DESECON4_C3) |> 
              distinct(),
            by = "Cluster_ID") |> 
  left_join(Eco4Diagnostics |> 
              ungroup() |> 
              select(Eco4_ID, Eco4_species_count, Eco4_total_entries, Eco4_H, Eco4_J),
            by = "Eco4_ID") |> 
    relocate(Eco4_ID, DESECON4_C3)

View(C3_Eco4_Diagnostics)
View(merged_Eco)

###### B4.7) Calculating Changes through cycles + geospatial preparation ---------------------------------
C3_Eco4_Changes <- C3_Eco4_Diagnostics |> filter(File == 1) |> ungroup() |> 
  mutate(Eco4_H1 = Eco4_H, Eco4_J1 = Eco4_J, Eco4_SC1 = Eco4_species_count, Eco4_TE1 = Eco4_total_entries) |> 
  select(Conglomerado, Eco4_SC1, Eco4_TE1, Eco4_H1, Eco4_J1, DESECON4_C3, X, Y) |>
  left_join(
    left_join(C3_Eco4_Diagnostics |> filter(File == 2) |> ungroup() |> 
                mutate(Eco4_H2 = Eco4_H, Eco4_J2 = Eco4_J, Eco4_SC2 = Eco4_species_count, Eco4_TE2 = Eco4_total_entries) |> 
                select(Conglomerado, Eco4_SC2, Eco4_TE2, Eco4_H2, Eco4_J2, DESECON4_C3, X, Y),
              C3_Eco4_Diagnostics |> filter(File == 3) |> ungroup() |> 
                mutate(Eco4_H3 = Eco4_H, Eco4_J3 = Eco4_J, Eco4_SC3 = Eco4_species_count, Eco4_TE3 = Eco4_total_entries) |> 
                select(Conglomerado, Eco4_SC3, Eco4_TE3, Eco4_H3, Eco4_J3, DESECON4_C3, X, Y),
              by = c("Conglomerado", "DESECON4_C3")),
    by = c("Conglomerado","DESECON4_C3")) |> 
  select(-c("X.x", "Y.x", "X.y", "Y.y")) |> 
  relocate(X, Y, DESECON4_C3) |> 
  mutate(Eco4_SC12 = Eco4_SC2 - Eco4_SC1,
         Eco4_SC23 = Eco4_SC3 - Eco4_SC2,
         Eco4_SC13 = Eco4_SC3 - Eco4_SC1,
         Eco4_TE12 = Eco4_TE2 - Eco4_TE1,
         Eco4_TE23 = Eco4_TE3 - Eco4_TE2,
         Eco4_TE13 = Eco4_TE3 - Eco4_TE1,
         Eco4_H12 = Eco4_H2 - Eco4_H1,
         Eco4_H23 = Eco4_H3 - Eco4_H2,
         Eco4_H13 = Eco4_H3 - Eco4_H1,
         Eco4_J12 = Eco4_J2 - Eco4_J1,
         Eco4_J23 = Eco4_J3 - Eco4_J2,
         Eco4_J13 = Eco4_J3 - Eco4_J1,) |> 
  select(X, Y, DESECON4_C3, Conglomerado, 
         Eco4_SC12, Eco4_SC23, Eco4_SC13, 
         Eco4_TE12, Eco4_TE23, Eco4_TE13,
         Eco4_H12, Eco4_H23, Eco4_H13,
         Eco4_J12, Eco4_J23, Eco4_J13)

#writeVector(vect(C3_Eco4_Changes, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Eco4_Testing.shp")


########### END ------------------------------------------------------------------------------------------


#################### XX) EVERYTHING ON PLOT LEVEL --------------------------------------------------------
###### X4.1) Species Richness + Individual tree count ---------------------------
#### DATA ON PLOT LEVEL
SpecRich <- merged |> 
  select(Plot_ID, File, Conglomerado, Sitio, Anio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, Sitio) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_"),
            species_count = n_distinct(NombreCientifico_APG),
            total_entries = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  relocate(Plot_ID)


###### X4.2) Species Abundances ------------------------------------------------- 
#### DATA CALCULATED PER PLOT 
SpecAbun <- merged |> 
  select(File, Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(File, Conglomerado, Sitio, NombreCientifico_APG) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            abundance=n(),
            Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_"),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Plot_ID)

###### X4.3) Shannon Index H + Pielou eveness J ---------------------------------

#### STEP 1: presence-absence dataset for species per plot - contains NAs -> changed in next step to "0" for further calculations #### TEMPORARY
## DATA ON PLOT LEVEL
Temp.Shannon <- SpecAbun |> 
  ungroup() |> 
  select(Plot_ID, NombreCientifico_APG, abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

#### STEP 2: exchange NAs with Zeros 
## DATA ON PLOT LEVEL
PresenceAbsence <- Temp.Shannon |> 
  replace(is.na(Temp.Shannon), 0)

#### STEP 3: Calculate Shannon-Index H using diversity()  ###### TEMPORARY
## DATA ON PLOT LEVEL
H <- diversity(PresenceAbsence[,-1])

#### STEP 4: calculate Eveness J ######## TEMPORARY
## DATA ON PLOT LEVEL
J <- H/log(specnumber(PresenceAbsence[, -1]))

#### STEP 5: merging H and J into dataframe + renaming ID-Column to be in line with other datasets 
Temp.HJ <- data.frame(PresenceAbsence$Plot_ID, H, J) |> 
  rename(Plot_ID = PresenceAbsence.Plot_ID)

#### STEP 6: merged data table 
## DATA ON PLOT LEVEL
PlotDiagnostics <- left_join(SpecRich, Temp.HJ, by= c("Plot_ID")) |> 
  select(File, Plot_ID, Conglomerado, Sitio, Anio, species_count, total_entries, H, J, X, Y)

###### X4.3.1) DATA TABLE ON SPECIES LEVEL 2 - optional ---------------------------

#NEEDS PLOT LEVEL DATA - SECTION XX)
SpecDiagnostics <- left_join(SpecRich, SpecAbun, Temp.HJ, by= c("Plot_ID")) |> 
  select(File, Plot_ID, Conglomerado, Sitio, Anio, species_count, NombreCientifico_APG, abundance, total_entries, H, J, X.x, Y.y)

###### X4.4) Tree Morphology ----------------------------------------------------

#### DATA ON PLOT LEVEL
# currently: Means -- could also add medians
TreeMorp <- merged |> 
  group_by(File, Conglomerado, Sitio) |> 
  summarise(File = mean(as.integer(File)),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            Anio = mean(Anio),
            Plot_ID = paste(File, Conglomerado, Sitio, Anio, sep = "_"),
            AvgTreeHeight = mean(AlturaTotal, na.rm = T),
            AvgDbh = mean(DiametroNormal, na.rm = T),
            AvgCrownDiameter = mean(DiametroCopa, na.rm = T),
            AvgCrownHeight = mean(AlturaTotal - AlturaFusteLimpio, na.rm = T),
            AvgCrownArea = mean(AreaCopa, na.rm = T),
            X=mean(X),
            Y=mean(Y)) |> 
  relocate(Plot_ID)

###### X7.5) COMPLETE DIAGNOSTICS DATASET (excluding Biomass; 01/10/2024)

Comp_Plot_Diagnostics <- left_join(PlotDiagnostics, TreeMorp, by= c("Plot_ID", "File", "Conglomerado", "Sitio", "Anio", "X", "Y")) |> #still missing median values
  relocate(Plot_ID, File, Conglomerado, Sitio, Anio, species_count, total_entries, H, J, AvgTreeHeight, AvgDbh, AvgCrownDiameter, AvgCrownDiameter, AvgCrownHeight, AvgCrownArea, X, Y)


########### END ------------------------------------------------------------------------------------------



#################### ZZ) Archive Stuff -------------------------------------------------------------------
###### Z1) METADATA ANALYSIS I -------------------------------------------------

# Raw.04
## count all NAs per column
NAs <- Raw.04 |> 
  select(everything()) |>   # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(NAs)

## count all 0s per column
ZEROs <- lapply(Raw.04, function(x){ length(which(x==0))})
View(ZEROs)


## distince values per column (various methods)
# method 1
Raw.04 |>  
  summarise_all(list(~n_distinct(.)))

# method 2
ulst <- lapply(Raw.04, unique)
View(ulst)

## Alternative way for NAs and unsique values
library(Hmisc)
describe(Raw.04)



# Raw.09
## count all NAs per column
NAs <- Raw.09 |> 
  select(everything()) |>   # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(NAs)

## count all 0s per column
ZEROs <- lapply(Raw.09, function(x){ length(which(x==0))})
View(ZEROs)

## distince values per column (various methods)
# method 1
Raw.09 |>  
  summarise_all(list(~n_distinct(.)))

# method 2
ulst <- lapply(Raw.09, unique)
View(ulst)

## Alternative way for NAs and unsique values
library(Hmisc)
describe(Raw.09)



# Raw.14
# count all NAs per column
NAs <- Raw.14 |> 
  select(everything()) |>   # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(NAs)

NAs <- lapply(Raw.14, function(x){ length(which(is.na(x)))})

# count all 0s per column
ZEROs <- lapply(Raw.14, function(x){ length(which(x==0))})
View(ZEROs)

## distince values per column (various methods)
# method 1
Raw.14 |>  
  summarise_all(list(~n_distinct(.)))

# method 2
ulst <- lapply(Raw.14, unique)
View(ulst)

## Alternative way for NAs and unsique values
library(Hmisc)
describe(Raw.14)

###### Z2) FIRST EDA -----------------------------------------------------------

# Fig.1.1 Total most Common Families (by file) 
merged |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 10)) |> # Only keep the 5 most frequent categories and lump the rest into "Other"
  # highlight = fct_other(Familia, keep = "Other", other_level = "Top N Groups")) |>  # making it two tone
  ggplot(aes(x = Familia, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(position = "dodge")

# Fig.1.2 Most common families normalized (over entries per file)s
Normed.Family <- rbind((merged |> 
                          filter(File == 1) |> 
                          group_by(Familia_APG) |> 
                          count() |> 
                          mutate(Normalized = n/1305130,
                                 File = "1")),
                       (merged |> 
                          filter(File == 2) |> 
                          group_by(Familia_APG) |> 
                          count() |> 
                          mutate(Normalized = n/1581022,
                                 File = "2")),
                       (merged |> 
                          filter(File == 3) |> 
                          group_by(Familia_APG) |> 
                          count() |> 
                          mutate(Normalized = n/831331,
                                 File = "3")))

View(Normed.Family)

Normed.Family |> 
  select(File, Familia_APG, Normalized) |>
  ungroup() |> 
  mutate(NormPerc = Normalized*100) |> 
  arrange(desc(NormPerc)) |> 
  mutate(Rank = row_number()) |> 
  filter(Rank <= 40) |> 
  ggplot(aes(x = reorder(Familia_APG, -NormPerc, sum), y = NormPerc, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge") +
  labs(x = "Familiy",
       y = "Relative Abundance [%]",
       title = "Relative Abundances of 15 Most Common Families",
  ) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))





# Fig.2.1 Normalized (over entries per file) biological form abundances by file
Normed.Form <- rbind((merged |> 
                        filter(File == 1) |> 
                        group_by(FormaBiologica) |> 
                        count() |> 
                        mutate(Normalized = n/1305130,
                               File = "1")),
                     (merged |> 
                        filter(File == 2) |> 
                        group_by(FormaBiologica) |> 
                        count() |> 
                        mutate(Normalized = n/1581022,
                               File = "2")),
                     (merged |> 
                        filter(File == 3) |> 
                        group_by(FormaBiologica) |> 
                        count() |> 
                        mutate(Normalized = n/831331,
                               File = "3")))

Normed.Form |> 
  select(File, FormaBiologica, Normalized) |>
  ggplot(aes(x = reorder(FormaBiologica, -Normalized, sum), y = Normalized, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge")





# Fig.3.1 Total disturbances by file
merged |> 
  # subset(!is.na(Danio1)) |> 
  # group_by(Danio1) |> 
  # count()
  ggplot(aes(x = Danio1, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(position = "dodge")

# Fig.3.2 Normalized disturbances by file
Normed.Disturbance1 <- rbind((merged |> 
                                filter(File == 1) |> 
                                subset(!is.na(Danio1)) |> #excluding NAs in calculation
                                group_by(Danio1) |> 
                                count() |> 
                                mutate(Normalized = n/1305130,
                                       File = "1")),
                             (merged |> 
                                filter(File == 2) |> 
                                subset(!is.na(Danio1)) |> #excluding NAs in calculation
                                group_by(Danio1) |> 
                                count() |> 
                                mutate(Normalized = n/571263,
                                       File = "2")),
                             (merged |> 
                                filter(File == 3) |> 
                                subset(!is.na(Danio1)) |> #excluding NAs in calculation
                                group_by(Danio1) |> 
                                count() |> 
                                mutate(Normalized = n/666710,
                                       File = "3")))

Normed.Disturbance1 |> 
  #subset(!is.na(Danio1)) |> 
  select(File, Danio1, Normalized) |>
  ggplot(aes(x = reorder(Danio1, -Normalized, sum), y = Normalized, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge")


# Fig.3.3 Absolute disturbances for comparison
Absolute.Disturbance <- rbind((merged |> 
                                 filter(File == 2) |> 
                                 select(Danio1) |> 
                                 group_by(Danio1) |> 
                                 count() |> 
                                 mutate(File = "2")),
                              (merged |> 
                                 filter(File == 3) |> 
                                 select(Danio1) |> 
                                 group_by(Danio1) |> 
                                 count() |> 
                                 mutate(File = "3"))
)

Absolute.Disturbance |>  
  ggplot(aes(x = reorder(Danio1, -n, sum), y = n, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge")




########### END ------------------------------------------------------------------------------------------



#################### YY) Plotting ------------------------------------------------------------------------
###### Y1) SPECIES RICHNESS + INDIVIDUAL TREE COUNT ----------------------------------------
#### A) Constant clusters ---------------------
##### SPECIES COUNTS PER CLUSTER
Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |>
  filter(Cycles == 3) |> 
  ggplot(aes(x = species_count, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

Comp_C_Diagnostics_V3 |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  ggplot(aes(x = species_count, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

Comp_C_Diagnostics_V3 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  ggplot(aes(x= species_count, colour = File)) +
  geom_density()

##### TREE COUNTS PER CLUSTER
Comp_C_Diagnostics_V5|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  ggplot(aes(x= total_entries, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

Comp_C_Diagnostics_V5|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |>  
  filter(Cycles == 3) |> 
  ggplot(aes(x = total_entries, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()



#### B) Sporadic clusters---------------------
##### SPECIES COUNTS PER CLUSTER
Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  ggplot(aes(x = species_count, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  ggplot(aes(x = species_count, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))


Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |>  
  ggplot(aes(x= species_count, colour = File)) +
  geom_density()

##### TREE COUNTS PER CLUSTER
Comp_C_Diagnostics_V5|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  ggplot(aes(x= total_entries, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

Comp_C_Diagnostics_V5|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |>  
  filter(Cycles != 3) |>  
  ggplot(aes(x = total_entries, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Individual trees per cluster",
       y = "Count") 


Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |>  
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()

#### C) all clusters -----------------------------
##### SPECIES COUNTS PER CLUSTER
Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = species_count, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = species_count, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= species_count, colour = File)) +
  geom_density()

##### TREE COUNTS PER CLUSTER
Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= total_entries, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

Comp_C_Diagnostics_V5 |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |>  
  ggplot(aes(x = total_entries, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()

###### Y2) SPECIES ABUNDANCES --------------------------------------------------

## Plotting - can be skipped
# for combined plots
# read as "there is y entries of a species with a count of x in a given plot" 
# NOT SPECIES SENSITIVE - will plot the same species multiple times as it only counts occurences of a given abundace
C_SpecAbun |>                         # Enter "SpecAbun" or "C_SpecAbun" 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = abundance, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Species Count per Plot",
       y = "Plot Count",
       title = "Frequency Distribution of Species Abundances") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

###### Y3) SHANNON INDEX H + PIELOU EVENESS J ----------------------------------------------
#### A) constant clusters -----------------------
#### SHANNON INDEX
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics" 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |>                      
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_density()


#### PIELOU EVENESS
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_density()



#### B) sporadic clusters -------------------------
#### SHANNON INDEX
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics" 
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |>                      
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_density()


#### PIELOU EVENESS
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles != 3) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_density()




#### C) all clusters ----------------------------------

# plots shannon-index 
Comp_C_Diagnostics_V3 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics" 
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V3 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V3 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  stat_ecdf(geom = "step")

Comp_C_Diagnostics_V3 |> 
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_density()



# plots pielou eveness
Comp_C_Diagnostics_V3 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V3 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V3 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  stat_ecdf(geom = "step")

Comp_C_Diagnostics_V3 |> 
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_density()



#### D) consistent cluster ------------------------------------------------
#### SHANNON INDEX
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics" 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |>  
  filter(Consistent == T) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_density()


#### PIELOU EVENESS
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_density()


#### D) consistent + 4 plots cluster ------------------------------------------------
#### SHANNON INDEX
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics" 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  filter(Plots == 4) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |>  
  filter(Consistent == T) |> 
  filter(Plots == 4) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  filter(Plots == 4) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_density()


#### PIELOU EVENESS
Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  filter(Plots == 4) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  filter(Plots == 4) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  filter(Plots == 4) |> 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_density()


###### Y4) TREE MORPHOLOGY -----------------------------------------------------
# example plot - only some metrics relevant as individual plots
C_TreeMorp |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= AvgDbh, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

C_TreeMorp |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= AvgDbh, color = File)) +
  geom_density()


###### Y5) ECOREGIONS PLOTS ----------------------------------------------------------------
#### A) constant clusters --------------------------------
# A.1) SPECIES RICHNESS  + INDIVIDUAL TREE COUNT ----------------------------------
# SPECIES COUNTS PER ECOREGION
C3_Eco4_Diagnostics |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |>
  ggplot(aes(x = Eco4_species_count, fill = File)) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.3) +
  labs(x = "Species Count per Ecoregion",
       y = "Ecoregion Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

C3_Eco4_Diagnostics |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |>
  ggplot(aes(x = Eco4_species_count, color = File)) +
  geom_freqpoly(binwidth = 10, position = "identity", alpha = 0.3) +
  labs(x = "Species Count per Ecoregion",
       y = "Ecoregion Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

C3_Eco4_Diagnostics |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= Eco4_species_count, colour = File)) +
  geom_density()

# TREE COUNTS PER ECOREGION
C3_Eco4_Diagnostics|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= Eco4_total_entries, fill = File)) +
  geom_histogram(binwidth = 5000, position = "identity", alpha = 0.3) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

C3_Eco4_Diagnostics |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |>
  ggplot(aes(x = Eco4_total_entries, color = File)) +
  geom_freqpoly(binwidth = 5000, position = "identity", alpha = 0.3) +
  labs(x = "Tree Count per Ecoregion",
       y = "Ecoregion Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

C3_Eco4_Diagnostics |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= Eco4_total_entries, colour = File)) +
  geom_density()


# A.2) SHANNON INDEX H + PIELOU EVENESS J ---------------------------------------

# SHANNON
C3_Eco4_Diagnostics |>                                 
  mutate(File = as.factor(File)) |> 
  group_by(Eco4_ID) |>                              
  ggplot(aes(x= Eco4_H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

C3_Eco4_Diagnostics |>                                  
  mutate(File = as.factor(File)) |> 
  group_by(Eco4_ID) |>                             
  ggplot(aes(x= Eco4_H, color = File)) +
  geom_freqpoly(position = "identity", alpha = 0.3)

C3_Eco4_Diagnostics |>                                 
  mutate(File = as.factor(File)) |> 
  group_by(Eco4_ID) |>                             
  ggplot(aes(x= Eco4_H, color = File)) +
  geom_density()

# EVENESS
C3_Eco4_Diagnostics |>                                 
  mutate(File = as.factor(File)) |> 
  group_by(Eco4_ID) |>                              
  ggplot(aes(x= Eco4_J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

C3_Eco4_Diagnostics |>                                  
  mutate(File = as.factor(File)) |> 
  group_by(Eco4_ID) |>                             
  ggplot(aes(x= Eco4_J, color = File)) +
  geom_freqpoly(position = "identity", alpha = 0.3)

C3_Eco4_Diagnostics |>                                 
  mutate(File = as.factor(File)) |> 
  group_by(Eco4_ID) |>                             
  ggplot(aes(x= Eco4_J, color = File)) +
  geom_density()


# A.3) placeholder ---------------------------------------


###### YX) EVERYTHING ELSE POSSIBLY PLOTABLE -----------------------------------------------------

#correlation of number of plots per cluster with total entries per cluster - scatterplot
Comp_C_Diagnostics_V2 |>
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= Plots, y= total_entries, color = File)) +
  geom_point(alpha = 0.3, position = "jitter") 

#correlation of number of plots per cluster with total entries per cluster - boxplots
Comp_C_Diagnostics_V2 |> 
  mutate(Plots = as.factor(Plots),
         File = as.factor(File)) |> 
  ggplot(aes(x = Plots, y = total_entries)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(x = "Plots per Cluster",
       y = "Individual Trees per Cluster")

Comp_C_Diagnostics_V2 |> 
  mutate(Plots = as.factor(Plots),
         File = as.factor(File)) |> 
  ggplot(aes(x = Plots, y = total_entries, fill = File)) +
  geom_boxplot(position = "dodge",
               outlier.alpha = 0.2) +
  labs(x = "Plots per Cluster",
       y = "Individual Trees per Cluster")

#frequency distribution of total entries per cluster by number of plots per cluster - histogram
Comp_C_Diagnostics_V2 |> 
  mutate(Plots = as.factor(Plots)) |> 
  ggplot(aes(x= total_entries)) +
  geom_histogram(binwidth = 1) +
  labs(x= "Individual Trees per Cluster")

#frequency distribution of total entries per cluster by number of plots per cluster - histogram
Comp_C_Diagnostics_V5 |> 
  mutate(Plots = as.factor(Plots)) |> 
  filter(Cycles == 3) |> 
  filter(Consistent == T) |> 
  ggplot(aes(x= total_entries, fill = Plots)) +
  geom_histogram(alpha = 0.3, binwidth = 1) +
  labs(x= "Individual Trees per Cluster")

Comp_C_Diagnostics_V5 |> 
  mutate(Plots = as.factor(Plots),
         File = as.factor(File)) |> 
  filter(Cycles == 3) |> 
 # filter(Consistent == T) |> 
  ggplot(aes(x= total_entries, color = File)) +
  geom_density() +
  labs(x= "Individual Trees per Cluster")



#frequency distribution of total entries per cluster by number of plots per cluster - histogram
Comp_C_Diagnostics_V2 |> 
  mutate(Plots = as.factor(Plots)) |> 
  ggplot(aes(x= total_entries, fill = Plots)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 1) +
  labs(x= "Individual Trees per Cluster") +
  facet_grid(~File)


View(Comp_C_Diagnostics_V2)

#frequency distribution of total entries per cluster by number of plots per cluster - freqpoly
Comp_C_Diagnostics_V2 |> 
  mutate(Plots = as.factor(Plots)) |> 
  ggplot(aes(x= total_entries, color = Plots)) +
  geom_freqpoly()


# Number of clusters by their availability across cycles
Comp_C_Diagnostics_V2 |> 
  select(Cluster_ID, File, Conglomerado, Anio, Plots, X, Y) |> 
  group_by(Conglomerado) |> 
  arrange(Conglomerado) |> 
  #count()
  summarise(Conglomerado = mean(Conglomerado),
            n = n(),
            X = mean(X), 
            Y = mean(Y)) |>
  mutate(n = as.factor(n)) #|> 
  ggplot(aes(x= X, y= Y, color = n)) +
  geom_point(alpha = 0.2)
  

# Clusters by their availability across 1, 2, or 3 cycles
Comp_C_Diagnostics_V5 |> 
  select(Cluster_ID, File, Conglomerado, Plots, Cycles, X, Y) |> 
  mutate(Cycles = as.factor(Cycles)) |> 
  ggplot(aes(x= X, y= Y, color = Cycles)) +
  geom_point() +
  facet_grid(~Cycles)


View(Comp_C_Diagnostics_V3)



Comp_C_Diagnostics_V3 |> 
  filter(Consistent == T) |> 
  select(Cluster_ID, File, Conglomerado, Anio, Plots, X, Y) |> 
  group_by(Conglomerado) |> 
  arrange(Conglomerado) |> 
  summarise(Conglomerado = mean(Conglomerado),
            n = n(),
            X = mean(X), 
            Y = mean(Y)) |>
  mutate(n = as.factor(n)) |> 
  ggplot(aes(x= X, y= Y, color = n)) +
  geom_point() +
  facet_grid(~n)

Comp_C_Diagnostics_V4 |> 
  filter(Consistent == T) |>
  select(File, Conglomerado, Anio, X, Y, cycles, Plots) |> 
  group_by(Conglomerado) |> 
  arrange(Conglomerado) |> 
  summarise(Conglomerado = mean(Conglomerado),
            X = mean(X), 
            Y = mean(Y),
            Plots = mean(Plots),
            cycles = mean(cycles)) |> 
  mutate(Plots = as.factor(Plots)) |> 
  ggplot(aes(x= X, y= Y, color = Plots)) +
  geom_point() +
  facet_grid(~Plots)
  


CycleAvailability <- Comp_C_Diagnostics_V2 |> 
  select(Cluster_ID, File, Conglomerado, Anio, Plots, X, Y) |> 
  group_by(Conglomerado) |> 
  arrange(Conglomerado) |> 
  #count()
  summarise(Conglomerado = mean(Conglomerado),
            n = n(),
            X = mean(X), 
            Y = mean(Y)) |> 
  rename(cycles = n)

CycleAvailability |> 
  filter(cycles == 1)

#plot
Comp_C_Diagnostics_V2 |> 
  select(File, Conglomerado, Anio, Plots, X, Y) |> 
  filter(Plots == 4) |>                           #insert 1, 2, 3, 4
  group_by(Conglomerado) |> 
  arrange(Conglomerado) |> 
  summarise(Conglomerado = mean(Conglomerado),
            n = n(),
            X = mean(X),
            Y = mean(Y)) |> 
  rename(n_cycles_full = n) |> 
  filter(n_cycles_full == 3) |> 
  ggplot(aes(x = X, y = Y)) + 
  geom_point()
  

########### END ------------------------------------------------------------------------------------------



################### AA) PREPARATION CODE FOR GEOSPATIAL ANALYSIS - optional ------------------------------
###### A1) species count per cluster --------------------------------------
## Arb.04
ArbSpat.04 <- Arb.04 |> 
  select(Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado) |> 
  summarise(Conglomerado = mean(Conglomerado),
            species_count=n_distinct(NombreCientifico_APG),
            X=mean(X),
            Y=mean(Y))

ArbSpat.04 <- vect(ArbSpat.04, geom=c("X","Y"), crs="+proj=longlat +datum=WGS84")

plot(ArbSpat.04)
#writeVector(ArbSpat.04, "treeInv_richness_04.shp")

## Arb.09
ArbSpat.09 <- Arb.09 |> 
  select(Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado) |> 
  summarise(Conglomerado = mean(Conglomerado),
            species_count = n_distinct(NombreCientifico_APG),
            X = mean(X),
            Y = mean(Y))

ArbSpat.09 <- vect(ArbSpat.09, geom = c("X", "Y"), crs = "+proj=longlat + datum=WGS84")

plot(ArbSpat.09)
#writeVector(ArbSpat.09, "treeInv_richness_09.shp")

## Arb.14

ARbSpat.14 <- Arb.14 |> 
  select(Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado) |> 
  summarise(Conglomerado = mean(Conglomerado),
            species_count = n_distinct(NombreCientifico_APG),
            X = mean(X),
            Y = mean(Y))

ArbSpat.14 <- vect(ARbSpat.14, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84")

View(ARbSpat.14)

plot(ArbSpat.14)
#writeVector(ArbSpat.14, "treeInv_richness_14.shp")




###### A2) Availability of clusters ---------------------------------------
View(Comp_C_Diagnostics_V3)
##  consistent clusters
Plot.Spat.All <- vect(Comp_C_Diagnostics_V3 |> filter(Consistent == T) |> select(File, Conglomerado, Plots, X, Y), 
                      geom = c("X", "Y"),
                      crs = "+proj=longlat +daum=WGS884")
plot(Plot.Spat.All)
writeVector(Plot.Spat.All, "PlotAvailability_C.shp")

##  inconsistent clusters
Plot.Spat.All <- vect(Comp_C_Diagnostics_V3 |> filter(Consistent == F) |> select(File, Conglomerado, Plots, X, Y), 
                      geom = c("X", "Y"),
                      crs = "+proj=longlat +daum=WGS884")
plot(Plot.Spat.All)
writeVector(Plot.Spat.All, "PlotAvailability_F.shp")

##  all clusters
Plot.Spat.All <- vect(Comp_C_Diagnostics_V3 |> select(File, Conglomerado, Plots, X, Y), 
                      geom = c("X", "Y"),
                      crs = "+proj=longlat +daum=WGS884")
plot(Plot.Spat.All)
writeVector(Plot.Spat.All, "PlotAvailability_A.shp")


## available clusters by file
# all files available
Plot.Spat.3 <- vect(Comp_C_Diagnostics_V5 |> filter(Cycles == 3)  |> select(File, Conglomerado, Estado, Plots, Cycles, Consistent, X, Y),
                    geom = c("X", "Y"),
                    crs = "+proj=longlat +daum=WGS884")

writeVector(Plot.Spat.3, "PlotAvailability_3.shp")

# cycle overview
Plot.Sporadic <- vect(Comp_C_Diagnostics_V5 |> filter(Cycles != 3),
                      geom = c("X", "Y"),
                      crs = "+proj=longlat +daum=WGS884")

writeVector(Plot.Sporadic, "Sporadic_Plots.shp")

Plot.Constant <- vect(Comp_C_Diagnostics_V5 |> filter(Cycles == 3),
                    geom = c("X", "Y"),
                    crs = "+proj=longlat +daum=WGS884")

writeVector(Plot.Constant, "Constant_Plots.shp")

Plot.Constant.Consistent <- vect(Comp_C_Diagnostics_V5 |> filter(Cycles == 3) |> filter(Consistent == T),
                                 geom = c("X", "Y"),
                                 crs = "+proj=longlat +daum=WGS884")

writeVector(Plot.Constant.Consistent, "Constant_Consistent_Plots.shp")

Plot.Spat <- vect(Comp_C_Diagnostics_V5,
                  geom = c("X", "Y"),
                  crs = "+proj=longlat +daum=WGS884")

writeVector(Plot.Spat, "Spatial_Diagnostics.shp")


###### A3) Changes in Species Richness (Consistent + 4 Plots) - BETA --------------------------------------
SR_CP4 <- Comp_C_Diagnostics_V5 |> filter(Consistent == T) |> filter(Plots == 4) |>  filter(File == 1) |> ungroup() |> mutate(One = species_count) |> 
  select(Conglomerado, One, X, Y) |>
  left_join(
    left_join(
      left_join(Comp_C_Diagnostics_V5 |> filter(Consistent == T) |> filter(Plots == 4) |> filter(File == 2) |> ungroup() |> mutate(Two = species_count) |>
                select(Conglomerado, Two, X, Y),
              Comp_C_Diagnostics_V5 |> filter(Consistent == T) |> filter(Plots == 4) |> filter(File == 3) |> ungroup() |> mutate(Three = species_count) |>
                select(Conglomerado, Three, X, Y),
              by = c("Conglomerado")),
        EcoRegions <- Raw.14 |> 
          select(IdConglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3) |> 
          rename(Conglomerado = IdConglomerado) |> 
          left_join(Comp_C_Diagnostics_V5 |> 
                      filter(File == 3) |> 
                      ungroup() |> 
                      select(Conglomerado, Consistent, Plots),
                    by = "Conglomerado") |> 
          filter(Consistent == T & Plots == 4) |> 
          distinct() |> 
          select(Conglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3),
        by = "Conglomerado"),
    by = "Conglomerado") |> 
  mutate(OneTwo = Two - One,
         TwoThree = Three - Two,
         OneThree = Three - One) |> 
  select(-c("X.x", "Y.x", "X.y", "Y.y")) |> 
  relocate(X, Y)

View(SR_CP4)

writeVector(vect(SR_CP4, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Consistent4_SpecRich.shp")

########### END ------------------------------------------------------------------------------------------


################### 9) MULTIVARIATE CHANGE DETECTION - data from python ---------------------

setwd("C:/Users/samhu/Desktop/Projects/MxForest")

Results <- read.csv("./python/iMAD_results.csv", header=FALSE)
hist(Results[,7], breaks=100)







################### 10) PLACEHOLDER - BETA --------------------------------------------------------

### EMPTY



########### BETA SECTION ---------------------------------------------------------------------------------

########### SLIDE 7 - Ind. tree count per plot by plots per cluster (and by file) ---------------------
PTC_P <- SpecRich |> 
  left_join(Comp_C_Diagnostics_V5 |> 
              select(File, Conglomerado, Anio, Plots, Consistent, Cycles), by = c("File", "Conglomerado", "Anio"))

###### SLIDE 7 - FUNCTIONS ------------------------------------------------------------------------
#for consistent + inconsistent plots
summarise_data2 <- function(data, plot_number, consistency) {
  data |> 
    ungroup() |> 
    select(Plots, Consistent, total_entries) |> 
    filter(Plots == plot_number & Consistent == consistency) |> 
    summarise(Consistent = consistency,
              Plots = plot_number,
              Avg = mean(total_entries)
              )
}
#for all plots combined
summarise_data3 <- function(data, plot_number) {
  data |> 
    ungroup() |> 
    select(Plots, total_entries) |> 
    filter(Plots == plot_number) |> 
    summarise(Consistent = NA,
              Plots = plot_number,
              Avg = mean(total_entries)
    )
}
# for comparison between files
summarise_data4 <- function(data, plot_number) {
  data |> 
    ungroup() |> 
    select(File, Plots, total_entries) |> 
    group_by(File) |> 
    filter(Plots == plot_number) |> 
    summarise(File = mean(File),
              Consistent = NA,
              Plots = plot_number,
              Avg = mean(total_entries)
    )
}

###### SLIDE 7 - VALUE CALCULATIONS  -------------------------------------------------------------------
##### BETA 
PTC_P |> summarise_data2(1, T) |> 
  rbind(PTC_P |> summarise_data2(1, F), 
        PTC_P |> summarise_data3(1),
        PTC_P |> summarise_data2(2, T), 
        PTC_P |> summarise_data2(2, F), 
        PTC_P |> summarise_data3(2),
        PTC_P |> summarise_data2(3, T), 
        PTC_P |> summarise_data2(3, F), 
        PTC_P |> summarise_data3(3),
        PTC_P |> summarise_data2(4, T), 
        PTC_P |> summarise_data2(4, F),
        PTC_P |> summarise_data3(4)) |> 
  arrange(desc(Consistent))

# value calculation - constant clusters (cycles == 3) 
PTC_P |> filter(Cycles == 3) |>  summarise_data2(1, T) |> 
  rbind(PTC_P |> filter(Cycles == 3) |> summarise_data2(1, F), 
        PTC_P |> filter(Cycles == 3) |> summarise_data3(1),
        PTC_P |> filter(Cycles == 3) |> summarise_data2(2, T), 
        PTC_P |> filter(Cycles == 3)|> summarise_data2(2, F), 
        PTC_P |> filter(Cycles == 3)|> summarise_data3(2),
        PTC_P |> filter(Cycles == 3)|> summarise_data2(3, T), 
        PTC_P |> filter(Cycles == 3)|> summarise_data2(3, F), 
        PTC_P |> filter(Cycles == 3)|> summarise_data3(3),
        PTC_P |> filter(Cycles == 3)|> summarise_data2(4, T), 
        PTC_P |> filter(Cycles == 3)|> summarise_data2(4, F),
        PTC_P |> filter(Cycles == 3)|> summarise_data3(4)) |> 
  arrange(desc(Consistent))

# SLIDE 7 - value calculation - constant clusters (cycles == 3) -----------------------------------------------------
PTC_P |> filter(Cycles == 3) |> 
  summarise_data4(1) |> 
  rbind(PTC_P |> filter(Cycles == 3) |>  summarise_data4(2),
        PTC_P |> filter(Cycles == 3) |>  summarise_data4(3),
        PTC_P |> filter(Cycles == 3) |>  summarise_data4(4))

# SLIDE 7 - value calculation plot - constant clusters (cycles == 3) --------------------------------------------------------
PTC_P |> 
  filter(Cycles != 3) |> 
  summarise_data4(1) |> 
  rbind(PTC_P |> filter(Cycles != 3) |> summarise_data4(2),
        PTC_P |> filter(Cycles != 3) |> summarise_data4(3),
        PTC_P |> filter(Cycles != 3) |> summarise_data4(4)) |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = Plots, y = Avg, fill = File)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Avg, 2)), colour = "white", size = 2.5,
            vjust = 1.5, position = position_dodge(.9)) +
  labs(y = "Average tree count per plot", x = "Clusters with x number of plots")


# SLIDE 7 - random plots ------------------------
#means
Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == T) |> 
  ggplot(aes(x = Plot_TreeCount_Mean, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 
#medians
Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == T) |> 
  ggplot(aes(x = Plot_TreeCount_Median, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 

# inconsistent plots
#means
Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == F) |> 
  ggplot(aes(x = Plot_Average, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 
#medians
PTC_3 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == F) |> 
  ggplot(aes(x = Plot_Median, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 

# all plots
#means
PTC_3 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  ggplot(aes(x = Plot_Average, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 
#medians
PTC_3 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == F) |> 
  ggplot(aes(x = Plot_Median, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 



# overall averages of trees per plot by amount of plots per cluster - all plots ----------------
PTC_P |> summarise_data2(1, T) |> 
  rbind(PTC_P |> summarise_data2(1, F), 
        PTC_P |> summarise_data3(1),
        PTC_P |> summarise_data2(2, T), 
        PTC_P |> summarise_data2(2, F), 
        PTC_P |> summarise_data3(2),
        PTC_P |> summarise_data2(3, T), 
        PTC_P |> summarise_data2(3, F), 
        PTC_P |> summarise_data3(3),
        PTC_P |> summarise_data2(4, T), 
        PTC_P |> summarise_data2(4, F),
        PTC_P |> summarise_data3(4)) |> 
  arrange(desc(Consistent)) |> 
  ggplot(aes(x = Plots, y = Avg, fill = Consistent)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Avg, 2)), colour = "white", size = 2.5,
            vjust = 1.5, position = position_dodge(.9)) +
  labs(y = "Average tree count per plot", x = "Clusters with x number of plots")

# averages of trees per plot by amount of plots per cluster - constant plots -------------------
PTC_P |> filter(Cycles == 3) |>  summarise_data2(1, T) |> 
  rbind(PTC_P |> filter(Cycles == 3) |> summarise_data2(1, F), 
        PTC_P |> filter(Cycles == 3) |> summarise_data3(1),
        PTC_P |> filter(Cycles == 3) |> summarise_data2(2, T), 
        PTC_P |> filter(Cycles == 3)|> summarise_data2(2, F), 
        PTC_P |> filter(Cycles == 3)|> summarise_data3(2),
        PTC_P |> filter(Cycles == 3)|> summarise_data2(3, T), 
        PTC_P |> filter(Cycles == 3)|> summarise_data2(3, F), 
        PTC_P |> filter(Cycles == 3)|> summarise_data3(3),
        PTC_P |> filter(Cycles == 3)|> summarise_data2(4, T), 
        PTC_P |> filter(Cycles == 3)|> summarise_data2(4, F),
        PTC_P |> filter(Cycles == 3)|> summarise_data3(4)) |> 
  arrange(desc(Consistent)) |> 
  ggplot(aes(x = Plots, y = Avg, fill = Consistent)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Avg, 2)), colour = "white", size = 2.5,
            vjust = 1.5, position = position_dodge(.9)) +
  labs(y = "Average tree count per plot", x = "Clusters with x number of plots")

# overall averages of trees per plot by amount of plots per cluster - divided by file -----------------------
PTC_P |> 
  filter(Cycles == 3) |> 
  summarise_data4(1) |> 
  rbind(PTC_P |> filter(Cycles == 3) |> summarise_data4(2),
        PTC_P |> filter(Cycles == 3) |> summarise_data4(3),
        PTC_P |> filter(Cycles == 3) |> summarise_data4(4)) |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = Plots, y = Avg, fill = File)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Avg, 2)), colour = "white", size = 2.5,
                vjust = 1.5, position = position_dodge(.9)) +
  labs(y = "Average tree count per plot", x = "Clusters with x number of plots")


PTC_P |> 
  summarise_data4(4)

PTC_P

Comp_C_Diagnostics_V5 |> 
  filter(Consistent == T) |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots),
         Avg = mean(total_entries)) |> 
  ggplot(aes(x = Plots, y = Avg, fill = File)) +
  geom_bar(stat = "identity", position = "dodge")


###### 11) PLACEHOLDER - UNASIGNED ---------------------------------------------------------
# how many clusters across all 3 datasets have 3, 2, and 1 plots -----------------------

Comp_C_Diagnostics_V4 |>
  ungroup() |> 
  group_by(Plots) |> 
  filter(Consistent == F) |> 
  distinct(Conglomerado) |> 
  count()  


# is there clusters with zero entries? -------------------------

Comp_C_Diagnostics_V4 |> 
  filter(total_entries == 1)

# how many clusters are available across all 3 cycles with variying amounts of plots ---------------
View(Comp_C_Diagnostics_V5)

Comp_C_Diagnostics_V4 |> 
  ungroup() |> 
  group_by(X, Y) |> 
  distinct() |> 
  #filter(Cycles != 3) |> 
 # filter(Plots == 4) |> 
  #filter(Consistent == F) |> 
  count() |> 
  ungroup() |> 
  distinct() |> 
  count()

Comp_C_Diagnostics_V5 |> 
  filter(Cycles == 2) |> 
  filter(cycles_four_plots == 2) |> 
  #filter(Plots == 4) |> 
  count()





# unique sporadic clusters ------------------------------------------------------------
Comp_C_Diagnostics_V5 |> 
  ungroup() |> 
  select(Conglomerado, Cycles) |> 
  filter(Cycles != 3) |>
  distinct() |> 
  count()



# BETA Tests ------------------------------------------------------------------


#### PILOT: ONLY FOR Consistent == T and Plots == 4 
CP4 <- Comp_C_Diagnostics_V5 |> 
  filter(Consistent == T & Plots == 4) |> 
  left_join(EcoRegions <- Raw.14 |> 
              select(IdConglomerado, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3) |> 
              rename(Conglomerado = IdConglomerado) |> 
              left_join(Comp_C_Diagnostics_V5 |> 
                          filter(File == 3) |> 
                          ungroup() |> 
                          select(Conglomerado, Consistent, Plots),
                        by = "Conglomerado") |> 
              filter(Consistent == T & Plots == 4) |> 
              distinct(),
            by = "Conglomerado")


CP4 |> 
  group_by(File, DESECON3_C3) |> 
  mutate(H_Area = mean(H)) |> 
  select(H_Area) |> 
  distinct()

CP4.2 <- CP4 |> 
  left_join(CP4 |> 
              group_by(File, DESECON3_C3) |> 
              mutate(H_Area = mean(H)) |> 
              select(H_Area) |> 
              distinct(),
            by = c("File", "DESECON3_C3"))

View(CP4.2)

writeVector(vect(CP4.2, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "ChangeDESECON3.shp")


AreaChangeD3_CP4 <- CP4.2 |> filter(File == 1) |> ungroup() |>  mutate(H_Area_1 = H_Area) |> 
  select(Conglomerado, H_Area_1, DESECON3_C3, X, Y) |>
  left_join(
    left_join(CP4.2 |> filter(File == 2) |> ungroup() |> mutate(H_Area_2 = H_Area) |> 
                select(Conglomerado, H_Area_2, DESECON3_C3, X, Y),
              CP4.2 |> filter(File == 3) |> ungroup() |> mutate(H_Area_3 = H_Area) |> 
                select(Conglomerado, H_Area_3,DESECON3_C3, X, Y),
              by = c("Conglomerado", "DESECON3_C3")),
    by = c("Conglomerado", "DESECON3_C3")) |> 
  select(-c("X.x", "Y.x", "X.y", "Y.y")) |> 
  relocate(DESECON3_C3, X, Y) |> 
  mutate(A1A2 = H_Area_2 - H_Area_1,
         A2A3 = H_Area_3 - H_Area_2,
         A1A3 = H_Area_3 - H_Area_1)

View(AreaChangeD3_CP4)

writeVector(vect(AreaChangeD3_CP4, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "ChangesDESECON3.shp")
  


# Biomass beta testing -----------------------------------------------------------------------------------
#1 Available biomass estimations for File 3 --------------------
View(Raw.14)

Raw.14 |> 
  select(AlturaTotal_C3 ,DiametroNormal_C3, biomasa_kg_C3, carbono_kg_C3)

############ CUT -----------------------------------------------------------------------------------------


# OH MY GOD PLS - BETA TESTS --------------------------------------------------------
# How does filtering out everything that's not considered a tree change the dataset? ---------------------------
merged |> 
  group_by(File) |> 
  count()

merged |> 
  filter(FormaBiologica == "Arbol" | FormaBiologica == "Arborescente" | FormaBiologica == "Arbusto" | FormaBiologica == "Palma") |> 
  group_by(Conglomerado) |> 
  count() |> 
  ungroup() |> 
  count()


ClusterDiagnostics_2 |> 
  select(species_count) |> 
  group_by(File) |> 
  summarise(Total = sum(species_count))

ClusterDiagnostics |> 
  select(species_count) |> 
  group_by(File) |> 
  summarise(Total = sum(species_count))

# how does it affect total entries density probability curves?
Comp_C_Diagnostics_V5_2 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()

# how does it change dbh avg?
View(Comp_C_Diagnostics_V5)

## AVG DBH - NO FILTER
Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = AvgDbh, color = File)) +
  geom_density(position = "identity", alpha = 0.3)

## AVG DBH - TREE FILTER
Comp_C_Diagnostics_V5_2 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = AvgDbh, color = File)) +
  geom_density(position = "identity")



## AVG TREEHEIGHT - NO FILTER
Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = AvgTreeHeight, color = File)) +
  geom_density(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = File, y = AvgTreeHeight)) +
  geom_boxplot(position = "identity", alpha = 0.3)

## AVG TREEHEIGHT - TREE FILTER
Comp_C_Diagnostics_V5_2 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = AvgTreeHeight, color = File)) +
  geom_density(position = "identity")

Comp_C_Diagnostics_V5_2 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = File, y = AvgTreeHeight)) +
  geom_boxplot(position = "identity", alpha = 0.3)


## AVG CROWNAREA - NO FILTER
Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = AvgCrownDiameter, color = File)) +
  geom_density(position = "identity", alpha = 0.3)

Comp_C_Diagnostics_V5 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = File, y = AvgCrownDiameter)) +
  geom_boxplot(position = "identity", alpha = 0.3)

## AVG TREEHEIGHT - TREE FILTER
Comp_C_Diagnostics_V5_2 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = AvgCrownDiameter, color = File)) +
  geom_density(position = "identity")

Comp_C_Diagnostics_V5_2 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = File, y = AvgCrownDiameter)) +
  geom_boxplot(position = "identity", alpha = 0.3)


# where do I lose clusters? -------------------------------------------------------------------
writeVector(vect(ClusterDiagnostics_2, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "WhereDoILoseClusters.shp")

# how does the filter change my plot availability? -----------------------------------------------
Comp_C_Diagnostics_V5_2 |> 
  filter(Cycles  == 3 & Consistent == T) |> 
  count()





############ CUT -----------------------------------------------------------------------------------------



