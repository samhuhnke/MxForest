
### MxForestInventory Data Cleansing --------------------------------------

#### RANDOM NOTES

# categorical values should pot. be represented as factors in R (like VigorEtapa)

# Analysis: TotalDiameter of ds, mean(diameter) of ds, -> sampled by randomly selected rows -> equal data amount of each ds
# -> mutate(Mean_XY = mean(xy, na.rm = T)
# Analysis: heat map of coordniates and tree count (x=x, y=y and color/size of dot = tree count), for example
# Analysis: amount of disturbances by type and severity
# Analysis: Scatterplot by numeric values -> color: VigorEtapa, Family (or sorting by family and color by species?), Estado (categorical values)


# EDA: Geospatial Analysis for low individual clusters -> where are they located?
# EDA: How many of the clusters contain all 4 plots? Is there a correlation between low individual clusters and missing plots?
# EDA: addition to previous line - should also be seen in correlation with Shannon == 0 

##########################################################################################


start.time <- Sys.time()


###### 0) LOAD NECESSARY PACKAGES ----------------------------------------------------------

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(tidyverse)  #data tidying
library(ggridges)   #geom_density_ridges()
library(terra)      #geo_spatial coordinates
library(vegan)      #for shannon-index and pielou-eveness

###### 1) LOAD RAW DATA ------------------------------------------------------------

## 2004 - 2007 -> changing "NULL" character values to NA
Raw.04 <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"), na.strings = "NULL")

## 2009 - 2014 -> changing "NULL" character values to NA
Raw.09 <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"), na.strings = "NULL")

## 2015 - 2020
Raw.14 <- readxl::read_xlsx(here("data", "arbolado", "INFYS_Arbolado_2015_2020.xlsx"), sheet= 1, na = c("NULL", "999991", "999993"))


###### 2) DATA CLEANING ------------------------------------------------------------

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



###### 3) MERGE FILES FOR OVERLAPPING VARIABLES ------------------------------------------------------

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
  select(Plot_ID, File, Conglomerado, Sitio, Anio, everything())



###### 4) EDA PREPARATION ------------------------------------------------------------------
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

### STEP 7: Plots - section YY) PLOTTING


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
  relocate(Cluster_ID)

###### 4.5) COMPLETE DIAGNOSTICS DATASET (excluding Biomass; 01/10/2024) ----------------------------

Comp_C_Diagnostics <- left_join(ClusterDiagnostics, C_TreeMorp, by= c("Cluster_ID", "File", "Conglomerado", "Anio", "X", "Y")) |> 
  relocate(Cluster_ID, File, Conglomerado, Anio, species_count, total_entries, H, J, 
           AvgTreeHeight, Med_AvgTreeHeight, AvgDbh, Med_AvgDbh, AvgCrownDiameter, Med_AvgCrownDiameter, AvgCrownHeight, Med_AvgCrownHeight, AvgCrownArea, Med_AvgCrownArea, X, Y)

# write.csv(Comp_C_Diagnostics, "INFyS_Selection_Cluster.csv")

###### 5) METADATA STUFF -------------------------------------------------------------------
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
                                 cycles_one_plots == 3 ~ T, cycles_one_plots <= 2 ~ F))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


###### XX) EVERYTHING ON PLOT LEVEL --------------------------------------------------------
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

View(SpecRich)

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


###### ZZ) Archive Stuff -------------------------------------------------------------------
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




###### YY) Plotting ------------------------------------------------------------------------
###### Y1) SPECIES RICHNESS + INDIVIDUAL TREE COUNT ----------------------------
## Plotting 
# Individual Species Count per Plot/Cluster
C_SpecRich |>                         # Use "SpecRich" or "C_SpecRich" 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = species_count, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

C_SpecRich |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = species_count, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Species Count per Cluster",
       y = "Cluster Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

C_SpecRich |>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= species_count, colour = File)) +
  stat_ecdf(geom = "step") +
  labs(x = "Species Count per Cluster",
       y = "") 

# Individual Tree Count per Plot/Cluster
C_SpecRich|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= total_entries, fill = File)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

C_SpecRich|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |>  
  ggplot(aes(x = total_entries, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Individual trees per cluster",
       y = "Count") 

C_SpecRich|>                         # Use "SpecRich" or "C_SpecRich"  
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  stat_ecdf(geom = "step") +
  labs(x = "Individual trees per cluster",
       y = "Count") 


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

###### Y3) SHANNON INDEX H + PIELOU EVENESS J ---------------------------------------------------------
# plots shannon-index 
ClusterDiagnostics |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics" 
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

ClusterDiagnostics |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

ClusterDiagnostics |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, H) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= H, colour = File)) +
  stat_ecdf(geom = "step")


# plots pielou eveness
ClusterDiagnostics |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)

ClusterDiagnostics |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()

ClusterDiagnostics |>                                 # Enter "PlotDiagnostics" or "ClusterDiagnostics"  
  mutate(File = as.factor(File)) |> 
  select(File, Cluster_ID, J) |>                      # Enter "Plot_ID" or "Cluster_ID" 
  group_by(Cluster_ID) |>                             # Enter "Plot_ID" or "Cluster_ID" 
  ggplot(aes(x= J, colour = File)) +
  stat_ecdf(geom = "step")


###### Y4) TREE MORPHOLOGY -----------------------------------------------------
# example plot - only some metrics relevant as individual plots
C_TreeMorp |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= AvgCrownArea, fill = File)) +
  geom_histogram(position = "identity", alpha = 0.3)



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
Comp_C_Diagnostics_V2 |> 
  mutate(Plots = as.factor(Plots)) |> 
  ggplot(aes(x= total_entries, fill = Plots)) +
  geom_histogram(alpha = 0.3, binwidth = 1) +
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
  

Comp_C_Diagnostics_V2 |> 
  select(Cluster_ID, File, Conglomerado, Anio, Plots, X, Y) |> 
  group_by(Conglomerado) |> 
  arrange(Conglomerado) |> 
  #count()
  summarise(Conglomerado = mean(Conglomerado),
            n = n(),
            X = mean(X), 
            Y = mean(Y)) |>
  mutate(n = as.factor(n)) |> 
  ggplot(aes(x= X, y= Y, color = n)) +
  geom_point() +
  facet_grid(~n)


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


# most up to date dataset
Comp_C_Diagnostics_V4 <- left_join(Comp_C_Diagnostics_V3, CycleAvailability, by = c("Conglomerado")) |> 
  select(everything(), -c("X", "Y")) |> 
  rename(X = X.x, Y = Y.x) |> 
  arrange(Conglomerado)

View(Comp_C_Diagnostics_V4)


  
  
  

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
  

UnevenPlots_V2

#merge into one big dataset 
Comp_C_Diagnostics_V3 <- left_join(Comp_C_Diagnostics_V2, UnevenPlots, by = c("Conglomerado")) |> 
  select(everything(), -c("X.y", "Y.y")) |> 
  rename(X = X.x, Y = Y.x)

View(Comp_C_Diagnostics_V3)




merged |> 
  select(Conglomerado) |> 
  distinct() |> 
  count()

Comp_C_Diagnostics_V2 |> 
  filter(Plots <= 3) |> 
  mutate(Plots = as.factor(Plots)) |> 
  select(Cluster_ID, File, Conglomerado, Anio, Plots, X, Y) |> 
  ggplot(aes(x = X, y = Y, color = Plots)) +
  geom_point()



###### AA) PREPARATION CODE FOR GEOSPATIAL ANALYSIS - optional -----------------

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

plot(ArbSpat.14)
#writeVector(ArbSpat.14, "treeInv_richness_14.shp")

###### 9) MULTIVARIATE CHANGE DETECTION - data from python ---------------------

setwd("C:/Users/samhu/Desktop/Projects/MxForest")

Results <- read.csv("./python/iMAD_results.csv", header=FALSE)
hist(Results[,7], breaks=100)







###### 10) PLACEHOLDER - BETA --------------------------------------------------------

View(Comp_C_Diagnostics_V3)







### SLIDE 6 - PDF for individual tree entries

# consistent plots
Comp_C_Diagnostics_V3 |> 
  mutate(File = as.factor(File)) |> 
  subset(Consistent == T) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()

# inconsistent plots
Comp_C_Diagnostics_V3 |> 
  mutate(File = as.factor(File)) |> 
  subset(Consistent == F) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()

# all plots
Comp_C_Diagnostics_V3 |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x= total_entries, colour = File)) +
  geom_density()



### SLIDE 7 - Ind. tree count per plot by plots per cluster (and by file)

PTC_P <- SpecRich |> 
  left_join(Comp_C_Diagnostics_V3 |> 
              select(File, Conglomerado, Anio, Plots, Consistent), by = c("File", "Conglomerado", "Anio"))


## calculate means and medians for each cluster based of total plot entries 
PTC_C <- SpecRich |> 
  group_by(File, Conglomerado) |> 
  summarise(File = mean(File),
            Conglomerado = mean(Conglomerado),
            Anio = mean(Anio),
            Cluster_ID = paste(File, Conglomerado, Anio, sep = "_"),
            Plot_Average = mean(total_entries),
            Plot_Median = median(total_entries),
            X = mean(X),
            Y = mean(Y)) |> 
  left_join(Comp_C_Diagnostics_V3 |> 
                        select(File, Conglomerado, Anio, Plots, Consistent), by = c("File", "Conglomerado", "Anio"))


## calculate means per plot by plots per clusters

# functions to count number of cycles with a given number of plots
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
# value calculation
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
# value calculation
PTC_P |> 
  summarise_data4(1) |> 
  rbind(PTC_P |> summarise_data4(2),
        PTC_P |> summarise_data4(3),
        PTC_P |> summarise_data4(4))

# consistent plots
#means
PTC_3 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == T) |> 
  ggplot(aes(x = Plot_Average, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 
#medians
PTC_3 |> 
  mutate(File = as.factor(File),
         Plots = as.factor(Plots)) |> 
  subset(Consistent == T) |> 
  ggplot(aes(x = Plot_Median, fill = Plots)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) 

# inconsistent plots
#means
PTC_3 |> 
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

#overall averages of trees per plot by amount of plots per cluster - divided by consistent
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
  labs(x = "Average tree count per plot", y = "Clusters with x number of plots")

# overall averages of trees per plot by amount of plots per cluster - divided by file
PTC_P |> 
  summarise_data4(1) |> 
  rbind(PTC_P |> summarise_data4(2),
        PTC_P |> summarise_data4(3),
        PTC_P |> summarise_data4(4)) |> 
  mutate(File = as.factor(File)) |> 
  ggplot(aes(x = Plots, y = Avg, fill = File)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Average tree count per plot", y = "Clusters with x number of plots")
