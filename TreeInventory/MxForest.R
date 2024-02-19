
### MxForestInventory Data Wrangling Code --------------------------------------
### LAST UPDATED: 02/10/2024 (US)

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

Comp_C_Diagnostics <- left_join(ClusterDiagnostics, C_TreeMorp, by= c("Cluster_ID", "File", "Conglomerado", "Anio", "X", "Y")) |> 
  relocate(Cluster_ID, File, Conglomerado, Anio, species_count, total_entries, H, J, 
           AvgTreeHeight, MedTreeHeight, AvgDbh, MedDbh, AvgCrownDiameter, MedCrownDiameter, AvgCrownHeight, MedCrownHeight, AvgCrownArea, MedCrownArea, X, Y)

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

View(Comp_C_Diagnostics_V3)

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

###### 5.4) ESTADO + TIPO VEGETACION FILTER -----------------------------------------------------

Comp_C_Diagnostics_V5 <- left_join(Comp_C_Diagnostics_V4, merged |> select(Cluster_ID, Estado, CveVeg, TipoVeg) |> distinct(),
                                   by = "Cluster_ID")

##################################     END      ##################################################################



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

View(FullStack_V3)

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
    TH1 = case_when(Muestreado1 == 1 & is.na(TH1) ~ 0,
                    T ~ TH1),
    TH2 = case_when(Muestreado2 == 1 & is.na(TH2) ~ 0,
                    T ~ TH2),
    TH3 = case_when(Muestreado3 == 1 & is.na(TH3) ~ 0,
                    T ~ TH3),
    ) |> 
  ungroup()


################### 5) UNIVARIATE CHANGE CALCULATIONS -----------------------------------
#### 5.1) FullSack_V4 -----
FullStack_V4 <- FullStack_V4 |> 
  # Calculate univariate changes in TreeCounts (TE) and SpeciesCounts (SC)
  mutate(TE12 = TE2 - TE1,
         TE23 = TE3 - TE2,
         TE13 = TE3 - TE1,
         SC12 = SC2 - SC1,
         SC23 = SC3 - SC2,
         SC13 = SC3 - SC1,
         DBH12 = DBH2 - DBH1,
         DBH23 = DBH3 - DBH2,
         DBH13 = DBH3 - DBH1)

#### 5.1.1) FullSack_V4_Zeros -----
FullStack_V4_Zeros <- FullStack_V4_Zeros |> 
  # Calculate univariate changes in TreeCounts (TE) and SpeciesCounts (SC)
  mutate(TE12 = TE2 - TE1,
         TE23 = TE3 - TE2,
         TE13 = TE3 - TE1,
         SC12 = SC2 - SC1,
         SC23 = SC3 - SC2,
         SC13 = SC3 - SC1)


################### 6) UNIVARIATE CHANGE CALCULATION BASED ON ECOREGIONS -----------------------------------
# STEP 1: Calculate Data based on Ecoregions given in Sec.14 ----
FullStack_V4_Zeros_Eco1 <- FullStack_V4_Zeros |> 
  group_by(DESECON1_C3) |> 
  filter(!is.na(DESECON1_C3) & !is.na(TE1) & !is.na(TE2)) |> 
  summarise(TE1_Eco = sum(TE1, na.rm = T),
            TE2_Eco = sum(TE2, na.rm = T),
            TE3_Eco = sum(TE3, na.rm = T),
            TE12_Eco = TE2_Eco - TE1_Eco,
            TE23_Eco = TE3_Eco - TE2_Eco,
            TE13_Eco = TE3_Eco - TE1_Eco,
            Clusters = n()) 



FullStack_V4_Zeros_Eco2 <- FullStack_V4_Zeros |> 
  group_by(DESECON2_C3) |> 
  filter(!is.na(DESECON2_C3)) |> 
  summarise(TE1_Eco2 = sum(TE1, na.rm = T),
            TE2_Eco2 = sum(TE2, na.rm = T),
            TE3_Eco2 = sum(TE3, na.rm = T),
            TE12_Eco2 = TE2_Eco2 - TE1_Eco2,
            TE23_Eco2 = TE3_Eco2 - TE2_Eco2,
            TE13_Eco2 = TE3_Eco2 - TE1_Eco2)

FullStack_V4_Zeros_Eco3 <- FullStack_V4_Zeros |> 
  group_by(DESECON3_C3) |> 
  filter(!is.na(DESECON3_C3)) |> 
  summarise(TE1_Eco3 = sum(TE1, na.rm = T),
            TE2_Eco3 = sum(TE2, na.rm = T),
            TE3_Eco3 = sum(TE3, na.rm = T),
            TE12_Eco3 = TE2_Eco3 - TE1_Eco3,
            TE23_Eco3 = TE3_Eco3 - TE2_Eco3,
            TE13_Eco3 = TE3_Eco3 - TE1_Eco3)

FullStack_V4_Zeros_Eco4 <- FullStack_V4_Zeros |> 
  group_by(DESECON4_C3) |> 
  filter(!is.na(DESECON4_C3)) |> 
  summarise(TE1_Eco4 = sum(TE1, na.rm = T),
            TE2_Eco4 = sum(TE2, na.rm = T),
            TE3_Eco4 = sum(TE3, na.rm = T),
            TE12_Eco4 = TE2_Eco4 - TE1_Eco4,
            TE23_Eco4 = TE3_Eco4 - TE2_Eco4,
            TE13_Eco4 = TE3_Eco4 - TE1_Eco4)


# STEP 2: Rejoin Main Data: FullStack_V5_Zeros ----
FullStack_V5_Zeros <- FullStack_V4_Zeros |> 
  left_join(FullStack_V4_Zeros_Eco1 , by = c("DESECON1_C3")) |> 
  left_join(FullStack_V4_Zeros_Eco2, by = c("DESECON2_C3")) |> 
  left_join(FullStack_V4_Zeros_Eco3, by = c("DESECON3_C3")) |> 
  left_join(FullStack_V4_Zeros_Eco4, by = c("DESECON4_C3")) 

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

#DATA: 
#DATA: FullStack_V5_Zeros ----
 # writeVector(vect(FullStack_V5_Zeros, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "FullStack_V5_Zeros.shp")

##################################     END      ##################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


###################    FOR MAPS: Total Entries, Species Richness and Biomass Calculation based on Ecoregions  #####################
#### 1) Cluster + Ecoregions Database = Ecoregions_base ----------------
Ecoregions_base <- NewBase2 |> 
  select(-c("Conglomerado1", "Conglomerado2", "Conglomerado3", "DIFF12", "DIFF13", "DIFF23")) |> 
  left_join(Sec.14 |> 
              mutate(Cluster_ID = IDConglomerado) |> 
              select(Cluster_ID, DESECON1_C3, DESECON2_C3, DESECON3_C3, DESECON4_C3),
            by = "Cluster_ID")

#### 2) Add merged values of interest -----
#Ecoregion 1
Ecoregions1_merged <- Ecoregions_base |> 
  left_join(merged |> 
              mutate(Cluster_ID = Conglomerado) |> 
              select(Cluster_ID, File, NombreCientifico_APG),
            by = "Cluster_ID") |> 
  ungroup() |> 
  group_by(File, DESECON1_C3) |> 
  summarise(File = mean(as.integer(File)),
            Eco1_speciesrichness = n_distinct(NombreCientifico_APG),
            Eco1_totalentries = n())

Ecoregions1_merged_ridgeplot <- Ecoregions_base |> 
  left_join(merged |> 
              mutate(Cluster_ID = Conglomerado) |> 
              select(Cluster_ID, File, NombreCientifico_APG),
            by = "Cluster_ID")

# Ecoregion 2
Ecoregions2_merged <- Ecoregions_base |> 
  left_join(merged |> 
              mutate(Cluster_ID = Conglomerado) |> 
              select(Cluster_ID, File, NombreCientifico_APG),
            by = "Cluster_ID") |> 
  ungroup() |> 
  group_by(File, DESECON2_C3) |> 
  summarise(File = mean(as.integer(File)),
            Eco2_speciesrichness = n_distinct(NombreCientifico_APG),
            Eco2_totalentries = n())


#### 3) Rejoin coordinates ---------
# Ecoregion 1
Ecoregions1 <- Ecoregions_base |> 
  left_join(Ecoregions1_merged,
            by = c("DESECON1_C3"))
# Ecoregion 2
Ecoregions2 <- Ecoregions_base |> 
  left_join(Ecoregions2_merged,
            by = c("DESECON2_C3"))


#### 4) To make Maps, filter for Specifications ---- 
# Eco1 ----
Eco1_1 <- Ecoregions1 |> 
  filter(!is.na(DESECON1_C3) & File == 1)
Eco1_1 |> 
  ungroup() |> 
  select(DESECON1_C3, Eco1_speciesrichness, Eco1_totalentries) |> 
  distinct()
# write vector

Eco1_2 <- Ecoregions1 |> 
  filter(!is.na(DESECON1_C3) & File == 2)
Eco1_2 |> 
  ungroup() |> 
  select(DESECON1_C3, Eco1_speciesrichness, Eco1_totalentries) |> 
  distinct()
# write vector

Eco1_3 <- Ecoregions1 |> 
  filter(!is.na(DESECON1_C3) & File == 3)
Eco1_3 |> 
  ungroup() |> 
  select(DESECON1_C3, Eco1_speciesrichness, Eco1_totalentries) |> 
  distinct()
# write vector

Eco1 <- Eco1_1 |> 
  mutate(Eco1_SC1 = Eco1_speciesrichness,
         Eco1_TE1 = Eco1_totalentries) |>
  left_join(Eco1_2 |> ungroup() |> 
              mutate(Eco1_SC2 = Eco1_speciesrichness,
                     Eco1_TE2 = Eco1_totalentries) |>
              select(Cluster_ID, Eco1_SC2, Eco1_TE2) |> 
              distinct(), by = "Cluster_ID") |> 
  left_join(Eco1_3 |> ungroup() |> 
              mutate(Eco1_SC3 = Eco1_speciesrichness,
                     Eco1_TE3 = Eco1_totalentries) |>
              select(Cluster_ID, Eco1_SC3, Eco1_TE3) |> 
              distinct(), by = "Cluster_ID") |> 
  select(-c("Eco1_speciesrichness", "Eco1_totalentries")) |> 
  #univariate changes on ecoregion1 level
  mutate(SC12 = Eco1_SC2 - Eco1_SC1,
         SC23 = Eco1_SC3 - Eco1_SC2,
         SC13 = Eco1_SC3 - Eco1_SC1,
         Avg_SC = (Eco1_SC1 + Eco1_SC2 + Eco1_SC3)/3)



# Eco2 ----
Eco2_1 <- Ecoregions2 |> 
  filter(!is.na(DESECON2_C3) & File == 1)
Eco2_1 |> 
  ungroup() |> 
  select(DESECON2_C3, Eco2_speciesrichness, Eco2_totalentries) |> 
  distinct()
# write vector

Eco2_2 <- Ecoregions2 |> 
  filter(!is.na(DESECON2_C3) & File == 2)
Eco2_2 |> 
  ungroup() |> 
  select(DESECON2_C3, Eco2_speciesrichness, Eco2_totalentries) |> 
  distinct()
# write vector

Eco2_3 <- Ecoregions2 |> 
  filter(!is.na(DESECON2_C3) & File == 3)
Eco2_3 |> 
  ungroup() |> 
  select(DESECON2_C3, Eco2_speciesrichness, Eco2_totalentries) |> 
  distinct()
# write vector

Eco2 <- Eco2_1 |> 
  mutate(Eco2_SC1 = Eco2_speciesrichness,
         Eco2_TE1 = Eco2_totalentries) |>
  left_join(Eco2_2 |> ungroup() |> 
              mutate(Eco2_SC2 = Eco2_speciesrichness,
                     Eco2_TE2 = Eco2_totalentries) |>
              select(Cluster_ID, Eco2_SC2, Eco2_TE2) |> 
              distinct(), by = "Cluster_ID") |> 
  left_join(Eco2_3 |> ungroup() |> 
              mutate(Eco2_SC3 = Eco2_speciesrichness,
                     Eco2_TE3 = Eco2_totalentries) |>
              select(Cluster_ID, Eco2_SC3, Eco2_TE3) |> 
              distinct(), by = "Cluster_ID") |> 
  select(-c("Eco2_speciesrichness", "Eco2_totalentries")) |> 
  #univariate changes on ecoregion1 level
  mutate(SC12 = Eco2_SC2 - Eco2_SC1,
         SC23 = Eco2_SC3 - Eco2_SC2,
         SC13 = Eco2_SC3 - Eco2_SC1,
         Avg_SC = (Eco2_SC1 + Eco2_SC2 + Eco2_SC3)/3)
#### 5) Geospatial Prep ----

# ECO 1
# writeVector(vect(Eco1, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Eco1.shp")

# ECO 2
# writeVector(vect(Eco2, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "Eco2.shp")

##################################     END      ##################################################################



##################          IR-MAD CHANGE DETECTION PREPARATION            ----------------------------------
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
file1 <- FullStack_V4_Zeros |> 
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
file2 <- FullStack_V4_Zeros |> 
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

################## 1.2) NOISE - Comparison of Cycle 1 and 2 - FILTER: Only Clusters that are available for 1 and 2 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 1
file1 <- FullStack_V4_Noise |> 
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
file2 <- FullStack_V4_Noise |> 
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

# write.csv(file12, "iMAD_Data_12_Constant_Noise.csv")



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

################## 2.1) ZEROs - Comparison of Cycle 2 and 3 - FILTER: Only Clusters that are available for 2 and 3 --------------------------------------------
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

################## 2.2) Noise - Comparison of Cycle 2 and 3 - FILTER: Only Clusters that are available for 2 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 2
file2 <- FullStack_V4_Noise |> 
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
file3 <- FullStack_V4_Noise |> 
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

# write.csv(file23, "iMAD_Data_23_Constant_Noise.csv")

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

################## 3.2) Noise - Comparison of Cycle 1 and 3 - FILTER: Only Clusters that are available for 1 and 3 --------------------------------------------
#### STEP 1: dissecting dataframe by file + add file grouping variable (CONSTANT CLUSTERS) -----
## File 1
file1 <- FullStack_V4_Noise |> 
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
file3 <- FullStack_V4_Noise |> 
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

# write.csv(file13, "iMAD_Data_13_Constant_Noise.csv")

##################################     END      ##################################################################


##################          IR-MAD CHANGE DETECTION RESULTS            ----------------------------------
################## 1) Comparison of Cycle 1 and 2 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_12_Constant <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_12_Constant.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_12_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))


## COL 7 + 8 ----
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

################# 1.1) ZEROs - Comparison of Cycle 1 and 2 - FILTER: CONSTANT CLUSTERS ------------------
#### STEP 1: Load data ----
iMAD_results_12_Constant_Zeros <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_12_Constant_Zeros.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_12_Constant_Zeros <- iMAD_results_12_Constant_Zeros |> 
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))


## COL 7 + 8 ----
## Column 7
# Original
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") 
# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-5, 5))

## Column 8
# Original
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") 

# Zoomed
iMAD_results_12_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-5, 5))

#### STEP 4: Geospatial Prep ----
# writeVector(vect(iMAD_results_12_Constant_Zeros, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_12_Constant_Zeros.shp")

################# 1.2) NOISE - Comparison of Cycle 1 and 2 - FILTER: CONSTANT CLUSTERS ------------------
#### STEP 1: Load data ----
iMAD_results_12_Constant_Noise <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_12_Constant_Noise.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_12_Constant_Noise <- iMAD_results_12_Constant_Noise |> 
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-10, 10))


## COL 7 + 8 ----
## Column 7
# Original
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") 
# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-5, 5))

## Column 8
# Original
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") 

# Zoomed
iMAD_results_12_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#F8766D") +
  coord_cartesian(xlim = c(-5, 5))

#### STEP 4: Geospatial Prep ----
# writeVector(vect(iMAD_results_12_Constant_Noise, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_12_Constant_Noise.shp")

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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_23_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## COL 7 + 8 ----
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

################## 2.1) ZEROs -  Comparison of Cycle 2 and 3 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_23_Constant_Zeros <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_23_Constant_Zeros.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_23_Constant_Zeros <- iMAD_results_23_Constant_Zeros |> 
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## COL 7 + 8 ----
## Column 7
# Original
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38")
# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-5, 5)) 

## Column 8
# Original
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") 

# Zoomed
iMAD_results_23_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-5, 5))


#### STEP 4: Geospatial Prep ----

# writeVector(vect(iMAD_results_23_Constant_Zeros, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_23_Constant_Zeros.shp")

################## 2.2) NOISE -  Comparison of Cycle 2 and 3 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_23_Constant_Noise <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_23_Constant_Noise.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_23_Constant_Noise <- iMAD_results_23_Constant_Noise |> 
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-10, 10))

## COL 7 + 8 ----
## Column 7
# Original
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38")
# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-5, 5)) 

## Column 8
# Original
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") 

# Zoomed
iMAD_results_23_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#00BA38") +
  coord_cartesian(xlim = c(-5, 5))


#### STEP 4: Geospatial Prep ----

# writeVector(vect(iMAD_results_23_Constant_Noise, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_23_Constant_Noise.shp")

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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_13_Constant |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))


## COL 7 + 8 ----
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

################## 3.1) ZEROs - Comparison of Cycle 1 and 3 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_13_Constant_Zeros <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_13_Constant_Zeros.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_13_Constant_Zeros <- iMAD_results_13_Constant_Zeros |> 
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))


## COL 7 + 8 ----
## Column 7
# Original
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF")
# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-5, 5))

## Column 8
# Original
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") 

# Zoomed
iMAD_results_13_Constant_Zeros |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-20, 20))

#### STEP 4: Geospatial Prep ----

# writeVector(vect(iMAD_results_13_Constant_Zeros, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_13_Constant_Zeros.shp")

################## 3.2) NOISE - Comparison of Cycle 1 and 3 - FILTER: CONSTANT CLUSTERS -------------------------------------------
#### STEP 1: Load data ----
iMAD_results_13_Constant_Noise <- Raw.04 <- fread(here("data", "iMAD", "[1] Cluster", "iMAD_results_13_Constant_Noise.csv"))
#### STEP 2: Rename Columns + Attach Comparison column ----
iMAD_results_13_Constant_Noise <- iMAD_results_13_Constant_Noise |> 
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
## COL 1 - 8 ----
## Column 1
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_1)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 2
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_2)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 3
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_3)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 4
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_4)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 5
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_5)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 6
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_6)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 7
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))

## Column 8
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-10, 10))


## COL 7 + 8 ----
## Column 7
# Original
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF")
# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_7)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-5, 5))

## Column 8
# Original
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") 

# Zoomed
iMAD_results_13_Constant_Noise |> 
  ggplot(aes(x=Col_8)) +
  geom_histogram(binwidth = 0.1, fill = "#619CFF") +
  coord_cartesian(xlim = c(-20, 20))

#### STEP 4: Geospatial Prep ----

# writeVector(vect(iMAD_results_13_Constant_Noise, geom = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"), "iMAD_results_13_Constant_Noise.shp")

################## 4) CrOSS COMPARISON - FILTER: CONSTANT CLUSTERS -------------------------------------------
## STEP 1: Data Preparation ----
iMAD_results_Constant <- rbind(iMAD_results_12_Constant, iMAD_results_13_Constant, iMAD_results_23_Constant) |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13")))
#### Violin Plot ----
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
### 1.1) Species Richness Changes ----
# FullStack_V4 & FullStack_V4_Zeros (both 0s for Species Counts)
FullStack_V4 %>% 
  select(SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC12", "SC23", "SC13"))) |> 
  ggplot( aes(x= Comparison, y= Species_Richness, fill=Comparison)) +
  geom_violin()

# each with individual clusters  
FullStack_V4_Zeros |> 
  select(SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC12", "SC23", "SC13"))) |> 
  ggplot( aes(x= Comparison, y= Species_Richness, fill=Comparison)) +
  geom_violin()

# comparable clusters
FullStack_V4_Zeros |> 
  filter(Muestreado1 ==  1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  select(SC12, SC23, SC13) |> 
  pivot_longer(c(SC12, SC23, SC13), names_to = "Comparison", values_to = "Species_Richness") |> 
  mutate(Comparison = factor(Comparison, levels = c("SC12", "SC23", "SC13"))) |> 
  ggplot( aes(x= Comparison, y= Species_Richness, fill=Comparison)) +
  geom_violin()


### 1.2) Biomass ----

# currently empty

### 1.3) iMAD ----
## iMAD_results_Constant ----
# STEP 1: Data Preparation 
iMAD_results_Constant <- rbind(iMAD_results_12_Constant, iMAD_results_13_Constant, iMAD_results_23_Constant) |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13")))
# STEP 2: Plotting
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


## iMAD_results_Constant_Zeros ----
# STEP 1: Data Preparation 
iMAD_results_Constant_Zeros <- rbind(iMAD_results_12_Constant_Zeros, iMAD_results_13_Constant_Zeros, iMAD_results_23_Constant_Zeros) |> 
  mutate(Comparison = factor(Comparison, levels = c("Cycle12", "Cycle23", "Cycle13")))
# STEP 2: Plotting 
## Column 1
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_1, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))
## Column 2
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_2, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))
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
# Original
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_7, fill = Comparison)) +
  geom_violin()
# zoomed
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_7, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))

## Column 8
# Original
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_8, fill = Comparison)) +
  geom_violin()
# Zoomed
iMAD_results_Constant_Zeros |> 
  ggplot(aes(x = Comparison, y = Col_8, fill = Comparison)) +
  geom_violin() +
  coord_cartesian(ylim = c(-25, 25))

summary(iMAD_results_Constant_Zeros |> filter(Comparison == "Cycle13") |>  select(Col_8))
summary(iMAD_results_Constant |> filter(Comparison == "Cycle13") |> select(Col_8))

## 1.4) Total Entries ----
# FullStack_V4 & FullStack_V4 (both 0s for Total Tree Entries)
FullStack_V4 %>% 
  select(TE12, TE23, TE13) |> 
  pivot_longer(c(TE12, TE23, TE13), names_to = "Comparison", values_to = "EntryChange") |> 
  mutate(Comparison = factor(Comparison, levels = c("TE12", "TE23", "TE13"))) |> 
  ggplot( aes(x= Comparison, y= EntryChange, fill=Comparison)) +
  geom_violin()

## 1.5) AvgDBH ----
# FullStack_V4
FullStack_V4 %>% 
  select(DBH12, DBH23, DBH13) |> 
  pivot_longer(c(DBH12, DBH23, DBH13), names_to = "Comparison", values_to = "DBH_Change") |> 
  mutate(Comparison = factor(Comparison, levels = c("DBH12", "DBH23", "DBH13"))) |> 
  ggplot( aes(x= Comparison, y= DBH_Change, fill=Comparison)) +
  geom_violin()

# FullStack_V4_Zeros
FullStack_V4_Zeros %>% 
  select(DBH12, DBH23, DBH13) |> 
  pivot_longer(c(DBH12, DBH23, DBH13), names_to = "Comparison", values_to = "DBH_Change") |> 
  mutate(Comparison = factor(Comparison, levels = c("DBH12", "DBH23", "DBH13"))) |> 
  ggplot( aes(x= Comparison, y= DBH_Change, fill=Comparison)) +
  geom_violin()

#### 2) Scatterplots -------------------------------------------
## 1.1) Species Richness ----
# FullStack_V4 & FullStack_V4_Zeros (both 0s for Species Counts)
# Cycle 1 - 2:
# with their individual cluster counts
FullStack_V4 |> 
  ggplot(aes(x = SC1, y = SC2)) +
  geom_point(position = "jitter") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red") 

# with their comparable counts
FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado2 == 1) |> 
  ggplot(aes(x = SC1, y = SC2)) +
  geom_point(position = "jitter") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red")

# Cycle 2 - 3:
FullStack_V4 |> 
  ggplot(aes(x = SC2, y = SC3)) +
  geom_point(position = "jitter") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red") 

# with their comparable counts
FullStack_V4 |> 
  filter(Muestreado2 == 1 & Muestreado3 == 1) |> 
  ggplot(aes(x = SC2, y = SC3)) +
  geom_point(position = "jitter") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red")

# Cycle 1 - 3:
FullStack_V4 |> 
  ggplot(aes(x = SC1, y = SC3)) +
  geom_point(position = "jitter") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red") 

# with their comparable counts
FullStack_V4 |> 
  filter(Muestreado1 == 1 & Muestreado3 == 1) |> 
  ggplot(aes(x = SC1, y = SC3)) +
  geom_point(position = "jitter") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,60)) +
  geom_abline(color = "red")
  

## 1.2) Biomass ----
## 1.3) iMAD ----



## 1.4) Total Entries ----
# FullStack_V4 & FullStack_V4 (both 0s for Total Tree Entries)
# Cycle 1 - 2:
FullStack_V4 |> 
  ggplot(aes(x = TE2, y = TE1)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

# Cycle 2 - 3:
FullStack_V4 |> 
  ggplot(aes(x = TE3, y = TE2)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

# Cycle 1 - 3:
FullStack_V4 |> 
  ggplot(aes(x = TE3, y = TE1)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)


## 1.5) AvgDBH ----
# FullStack_V4 
# Cycle 1 - 2:
FullStack_V4 |> 
  ggplot(aes(x = DBH2, y = DBH1)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

# Cycle 2 - 3:
FullStack_V4 |> 
  ggplot(aes(x = DBH3, y = DBH2)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

# Cycle 1 - 3:
FullStack_V4 |> 
  ggplot(aes(x = DBH3, y = DBH1)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)



# FullStack_V4_Zeros
# Cycle 1 - 2:
FullStack_V4_Zeros |> 
  ggplot(aes(x = DBH2, y = DBH1)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

# Cycle 2 - 3:
FullStack_V4_Zeros |> 
  ggplot(aes(x = DBH3, y = DBH2)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

# Cycle 1 - 3:
FullStack_V4_Zeros |> 
  ggplot(aes(x = DBH3, y = DBH1)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = lm)

#### 3) Ridges ------------------------------------------------- these make sense
## 1.1) Species Richness Distribution ----
# SC1
# Ecoregion 1
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON1_C3)) |> 
  ggplot(aes(y = DESECON1_C3, x = SC1, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

# Comparison with constantly available plots
FullStack_V5_Zeros |>
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(!is.na(DESECON1_C3)) |> 
  ggplot(aes(y = DESECON1_C3, x = SC1, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

# Ecoregion 2
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON2_C3)) |> 
  ggplot(aes(y = DESECON2_C3, x = SC1, fill = DESECON2_C3)) +
  geom_density_ridges(show.legend = F)

# Comparison with constantly available plots
FullStack_V5_Zeros |>
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(!is.na(DESECON2_C3)) |> 
  ggplot(aes(y = DESECON2_C3, x = SC1, fill = DESECON2_C3)) +
  geom_density_ridges(show.legend = F)


# SC2
# Ecoregion 1
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON1_C3)) |>
  ggplot(aes(y = DESECON1_C3, x = SC2, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

# Comparison with constantly available plots
FullStack_V5_Zeros |>
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |> 
  filter(!is.na(DESECON1_C3)) |> 
  ggplot(aes(y = DESECON1_C3, x = SC2, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

# Ecoregion 2
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON2_C3)) |>
  ggplot(aes(y = DESECON2_C3, x = SC2, fill = DESECON2_C3)) +
  geom_density_ridges(show.legend = F)

# SC3
#Ecoregion 1
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON1_C3)) |>
  ggplot(aes(y = DESECON1_C3, x = SC3, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

# Comparison with constantly available plots
FullStack_V5_Zeros |>
  filter(Muestreado1 == 1 & Muestreado2 == 1 & Muestreado3 == 1) |>
  filter(!is.na(DESECON1_C3)) |> 
  ggplot(aes(y = DESECON1_C3, x = SC3, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

#Ecoregion 2
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON2_C3)) |>
  ggplot(aes(y = DESECON2_C3, x = SC3, fill = DESECON2_C3)) +
  geom_density_ridges(show.legend = F)

## 1.1.1) Species Richness Change ----

# SC12
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON1_C3)) |> 
  ggplot(aes(y = DESECON1_C3, x = SC12, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)


# SC23
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON1_C3)) |>
  ggplot(aes(y = DESECON1_C3, x = SC23, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)

# SC13
FullStack_V5_Zeros |> 
  filter(!is.na(DESECON1_C3)) |>
  ggplot(aes(y = DESECON1_C3, x = SC13, fill = DESECON1_C3)) +
  geom_density_ridges(show.legend = F)



## 1.2) Biomass ----

# on it

## 1.3) iMAD ----

# does this even work?

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






















