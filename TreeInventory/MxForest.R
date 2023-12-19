
### MxForestInventory Data Cleansing --------------------------------------

# neccesary packages 



library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(tidyverse)  #data tidying
library(ggridges)   #geom_density_ridges()
library(terra)      #geo_spatial coordinates
library(vegan)      #for shannon-index and pielou-eveness


# categorical values should pot. be represented as factors in R (like VigorEtapa)


# Analysis: TotalDiameter of ds, mean(diameter) of ds, -> sampled by randomly selected rows -< equual data amount of each ds
# -> mutate(Mean_XY = mean(xy, na.rm = T)
# Analysis: heat map of coordniates and tree count (x=x, y=y and color/size of dot = tree count), for example
# Analysis: amount of disturbances by type and severity
# Analysis: Scatterplot by numeric values -> color: VigorEtapa, Family (or sorting by family and color by species?), Estado (categorical values)



# tree inventory data from 2004 - 2020 --------------------------------------------

## 2004 - 2007 -> changing "NULL" character values to NA
Raw.04 <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"), na.strings = "NULL")

## 2009 - 2014 -> changing "NULL" character values to NA
Raw.09 <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"), na.strings = "NULL")

## 2015 - 2020
Raw.14 <- readxl::read_xlsx(here("data", "arbolado", "INFYS_Arbolado_2015_2020.xlsx"), sheet= 1, na = c("NULL", "999991", "999993"))


##----------------------------------------------------------------------------------------------------------------
## Arb.04 Data cleaning ------------------------------------------------------------------------------------------

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
                                TRUE ~ Condicion)
        ) |>
# setting initial column order +
  select(Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
         ) |>
# sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)



##----------------------------------------------------------------------------------------------------------------

##----------------------------------------------------------------------------------------------------------------
## Arb.09 Data cleaning ------------------------------------------------------------------------------------------

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
                                           TRUE ~ NombreCientifico_APG)
         ) |> 
# setting initial column order + attaching everything so far not considered to the end
  select(Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything() 
         ) |> 
# sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro) 


##----------------------------------------------------------------------------------------------------------------

##----------------------------------------------------------------------------------------------------------------
## Arb.14 Data cleaning ------------------------------------------------------------------------------------------

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
                                           TRUE ~ NombreCientifico_APG)
         ) |> 
# setting initial column order + attaching everything so far not considered to the end
  select(Anio, Estado, Conglomerado, Sitio, Registro, CveVeg_S7, TipoVeg_S7, FormaFuste, TipoTocon, Familia_APG,
         NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal, AlturaFusteLimpio, AlturaComercial,
         DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa, PosicionCopa, ExposicionCopa, DensidadCopa,
         TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion, Danio1, Severidad1, Danio2, Severidad2, NumeroTallos,
         LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
         ) |> 
# sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)







#-----------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------
## Merging files together for selected variables -----------------------------------------------------------------

M.04 <- Arb.04 |>
  mutate(File = as.factor(1),
         CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

M.09 <- Arb.09 |>
  mutate(File = as.factor(2),
         CveVeg = CveVeg_S5,
         TipoVeg = TipoVeg_S5) |> 
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

M.14 <- Arb.14 |>
  mutate(File = as.factor(3),
         cgl_sit_reg = NA,
         CveVeg = CveVeg_S7,
         TipoVeg = TipoVeg_S7) |> 
  select(File, Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, CveVeg, TipoVeg, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y)

merged <- rbind(M.04, M.09, M.14)

View(merged)

##-----------------------------------------------------------------------------------------------------------

# Exploratory Data Analysis (Figures) ---------------------------------------------------------

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
  
  

## Metadata  analysis ---------------------------------------------------------------------

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

View(Arb.04)
View(Arb.14)







# species richness by cluster + preparation for geospaptial analysis --------------------------------------------------

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


# Species Richness per Plot + Total Species Count per plot -----------------------------------

# 04
PlotSR.04 <- Arb.04 |> 
  select(Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado, Sitio) |> 
  summarise(Anio = mean(Anio),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            species_count=n_distinct(NombreCientifico_APG),
            total_abundance= n(),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(File = "1")

View(PlotSR.04)

PlotSR.04 |> ggplot(aes(x= species_count)) +
  geom_histogram(binwidth = 1) +
  labs(x= "Species Count per Plot",
       y= "Plot Count")

PlotSR.04 |> ggplot(aes(x= species_count)) +
  stat_ecdf(geom = "step")

PlotSR.04 |> ggplot(aes(x= total_abundance)) +
  geom_histogram(binwidth = 1)

PlotSR.04 |> ggplot(aes(x= total_abundance)) +
  stat_ecdf(geom = "step")

# 09
PlotSR.09 <- Arb.09 |> 
  select(Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado, Sitio) |> 
  summarise(Anio = mean(Anio),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            species_count=n_distinct(NombreCientifico_APG),
            total_abundance= n(),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(File = "2")

PlotSR.09 |> ggplot(aes(x= species_count)) +
  geom_histogram(binwidth = 1)

PlotSR.09 |> ggplot(aes(x= species_count)) +
  stat_ecdf(geom = "step")

PlotSR.09 |> ggplot(aes(x= total_abundance)) +
  geom_histogram(binwidth = 1)

PlotSR.09 |> ggplot(aes(x= total_abundance)) +
  stat_ecdf(geom = "step")

# 14
PlotSR.14 <- Arb.14 |> 
  select(Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado, Sitio) |> 
  summarise(Anio = mean(Anio),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            species_count=n_distinct(NombreCientifico_APG),
            total_abundance= n(),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(File = "3")

PlotSR.14 |> ggplot(aes(x= species_count)) +
  geom_histogram(binwidth = 1)

PlotSR.14 |> ggplot(aes(x= species_count)) +
  stat_ecdf(geom = "step")

PlotSR.14 |> ggplot(aes(x= total_abundance)) +
  geom_histogram(binwidth = 1)

PlotSR.14 |> ggplot(aes(x= total_abundance)) +
  stat_ecdf(geom = "step")


#merging
PlotSR <- rbind(PlotSR.04, PlotSR.09, PlotSR.14)

View(PlotSR)

PlotSR |> 
  ggplot(aes(x = species_count, colour = File)) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "Species Count per Plot",
       y = "Plot Count",
       title = "Frequency Distribution of Species Richness") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))

PlotSR |> 
  ggplot(aes(x= species_count, colour = File)) +
  stat_ecdf(geom = "step") +
  labs(x = "Species Count per Plot",
       y = "") 




# Species abundances per plot ----------------------------------------------------------

# 04
PlotSA.04 <- Arb.04 |> 
  select(Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado, Sitio, NombreCientifico_APG) |> 
  summarise(Anio = mean(Anio),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            abundance=n(),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(File = "1")

PlotSA.04 |> ggplot(aes(x= abundance)) +
  geom_histogram(binwidth = 1)

PlotSA.04 |> ggplot(aes(x= abundance)) +
  stat_ecdf(geom = "step")

# 09
PlotSA.09 <- Arb.09 |> 
  select(Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado, Sitio, NombreCientifico_APG) |> 
  summarise(Anio = mean(Anio),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            abundance=n(),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(File = "2")

PlotSA.09 |> ggplot(aes(x= abundance)) +
  geom_histogram(binwidth = 1)

PlotSA.09 |> ggplot(aes(x= abundance)) +
  stat_ecdf(geom = "step")

# 14
PlotSA.14 <- Arb.14 |> 
  select(Anio, Conglomerado, Sitio, NombreCientifico_APG, X, Y) |> 
  group_by(Conglomerado, Sitio, NombreCientifico_APG) |> 
  summarise(Anio = mean(Anio),
            Conglomerado = mean(Conglomerado),
            Sitio = mean(Sitio),
            abundance=n(),
            X=mean(X),
            Y=mean(Y)) |> 
  mutate(File = "3")

PlotSA.14 |> ggplot(aes(x= abundance)) +
  geom_histogram(binwidth = 1)

PlotSA.14 |> ggplot(aes(x= abundance)) +
  stat_ecdf(geom = "step")

#merging
PlotSA <- rbind(PlotSA.04, PlotSA.09, PlotSA.14)
View(PlotSA)


#one data table
Plot <- left_join(PlotSA, PlotSR, by= c("Conglomerado", "Sitio", "Anio", "File")) |>
  mutate(Plot_ID = paste(Conglomerado, Sitio, Anio, sep= "_")) |> 
  select(Plot_ID, Conglomerado, Sitio, Anio, species_count, NombreCientifico_APG, abundance, total_abundance, X.x, Y.y, File)
View(Plot)



## shannon index H + pielous eveness J - all ------------------------------------------------------------- 

## presence-absence dataset for species per plot (vales are abundances)
temp.Shannon <- Plot |> 
  ungroup() |> 
  select(Plot_ID, NombreCientifico_APG, abundance) |> 
  pivot_wider(names_from = NombreCientifico_APG, values_from = abundance)

# exchange NAs with Zeros
df.Shannon <- temp.Shannon |> 
  replace(is.na(temp.Shannon), 0) #|> 
# select(everything(), -Plot_ID)            # not ideal

# Calculate H using diversity() 
H <- diversity(df.Shannon[,-1])
H

# mockup eveness
# calculate J
J <- H/log(specnumber(df.Shannon[, -1]))
J

# mockup table 
diagnostics <- data.frame(df.Shannon$Plot_ID, H, J) |> 
  rename(Plot_ID = df.Shannon.Plot_ID)

Plot2 <- left_join(Plot, diagnostics, by= c("Plot_ID")) |> 
  select(File, Plot_ID, Conglomerado, Sitio, Anio, species_count, NombreCientifico_APG, abundance, total_abundance, H, J, X.x, Y.y)


# plots shannon-index
Plot2 |> 
  select(File, Plot_ID, H) |> 
  group_by(Plot_ID) |> 
  ggplot(aes(x= H, fill = File)) +
  geom_histogram()

Plot2 |> 
  select(File, Plot_ID, H) |> 
  group_by(Plot_ID) |> 
  ggplot(aes(x= H, colour = File)) +
  geom_freqpoly()

# plots pielou eveness
Plot2 |> 
  select(File, Plot_ID, J) |> 
  group_by(Plot_ID) |> 
  ggplot(aes(x = J, fill = File)) +
  geom_histogram()

Plot2 |> 
  select(File, Plot_ID, J) |> 
  group_by(Plot_ID) |> 
  ggplot(aes(x= J, colour = File)) +
  geom_freqpoly()








