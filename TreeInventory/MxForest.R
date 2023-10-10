
### MxForestInventory Data Cleansing --------------------------------------

# neccesary packages 



library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(tidyverse)  #data tidying
library(ggridges)   #geom_density_ridges()


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
                                TRUE ~ Condicion),
        # "NombreCientifico_APG" Correction - "ZZ Desconocido" -> NA
          NombreCientifico_APG = case_when(NombreCientifico_APG == "ZZ Desconocido" ~ NA,
                                           TRUE ~ NombreCientifico_APG)
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
        # "Registro" Correction + "cgl_sit_arb" Correction - obviously wrong entries -> corrected to expected entry 
          Registro = case_when(Registro == 316 ~ 16,
                               TRUE ~ Registro),
          cgl_sit_reg = str_replace(string = cgl_sit_reg, "316$", "16"),
          cgl_sit_reg = str_replace(string = cgl_sit_reg, "4_147$", "4_28"),
          Registro = ifelse(cgl_sit_reg == "21314_4_28", as.integer(str_extract(cgl_sit_reg, "\\d+$")), Registro),
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


##----------------------------------------------------------------------------------------
# Plotting  ----------------------------------------------------------
# Arb.09 PLOTTING (ARCHIVE) ------------------------------------------------------------------------

# Adding a "Rank" for Family Abundance in order to filter easily if needed
T.09 <- Arb.09 |> 
  count(Familia_APG, Estado, sort = TRUE) |> 
  group_by(Familia_APG) |> 
  mutate(Fam_Total = sum(n)) |> 
  ungroup() |>
  select(Fam_Total, Familia_APG) |> 
  distinct() |>
  arrange(desc(Fam_Total), Familia_APG) |> 
  mutate(Familia_APG_Rank = row_number()) |> 
  left_join(Arb.09, by = "Familia_APG") 

View(T.09)

# V1 - Distribution of most common families (using rank size distribution) by state 
T.09 |>  
  ggplot(aes(x = fct_infreq(Familia_APG), fill = Estado)) +
  geom_bar(data = T.09 |> filter(Familia_APG_Rank <= 5))
           
# V2 - Distribution of most common families and "Others" (using rank size distribution) by state - works but not ordered
T.09 |>  
  ggplot(aes(x = fct_infreq(Familia_APG), fill = Estado)) +
  geom_bar(data = T.09 |> filter(Familia_APG_Rank <= 5)) +
  geom_bar(data = T.09 |> filter(Familia_APG_Rank > 5), aes(x = "Others", fill = Estado), position = "stack", na.rm = T)

# V2 - Distribution of most common families and "Others" (using rank size distribution) by state - Filter by fct_lump_n(), no Rank needed
T.09 |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |> # Only keep the 5 most frequent categories and lump the rest into "Other"
        # highlight = fct_other(Familia, keep = "Other", other_level = "Top N Groups")) |>  # making it two tone
  ggplot(aes(x = Familia, fill = Estado)) +
  geom_bar()

# V3 - Distribution of most common families (defined by their rank) by state (relative frequency plot)
T.09 |> 
  group_by(Familia_APG_Rank) |> 
  filter(Familia_APG_Rank <= 5) |> 
  ggplot(aes(x = Familia_APG, fill = Estado)) +
  geom_bar(position = "fill")

# V4 - Distribution of most common families (using rank size distribution) by VigorEtapa 
T.09 |> 
  group_by(Familia_APG_Rank) |> 
  filter(Familia_APG_Rank <= 5) |> 
  ggplot(aes(x = Familia_APG, fill = VigorEtapa)) +
  geom_bar(position = "fill")

# V5 -Distribution of most common families (using rank size distribution) by VigorEtapa (relative frequency plot)
T.09 |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |> # Only keep the 5 most frequent categories and lump the rest into "Other"
  # highlight = fct_other(Familia, keep = "Other", other_level = "Top N Groups")) |>  # making it two tone
  ggplot(aes(x = Familia, fill = VigorEtapa)) +
  geom_bar(position = "fill")

# V6 - Messing around
T.09 |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |> # Only keep the 5 most frequent categories and lump the rest into "Other"
  # highlight = fct_other(Familia, keep = "Other", other_level = "Top N Groups")) |>  # making it two tone
  ggplot(aes(x = Familia, fill = VigorEtapa)) +
  geom_bar(position = "dodge")

# V7 - freqplot
T.09 |> 
  ggplot(aes(x = AlturaTotal, y = after_stat(density))) +
  geom_freqpoly(aes(color = VigorEtapa), binwidth = 1)

# V8 - boxplot
T.09 |> 
  ggplot(aes(x = fct_reorder(VigorEtapa, AreaCopa, median), y = AreaCopa)) +
  geom_boxplot() 

# V9 - normed family abundances

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
  arrange(desc(Normalized)) |> 
  mutate(Rank = row_number()) |> 
  filter(Rank <= 30) |> 
  ggplot(aes(x = reorder(Familia_APG, -Normalized, sum), y = Normalized)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col() + 
  facet_wrap(~File)


#---------------------------------------------------------------------

# Exploratory Data Analysis (Figures) ---------------------------------------------------------



# Fig.1.1 Total most Common Families (by file) 
merged |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 10)) |> # Only keep the 5 most frequent categories and lump the rest into "Other"
  # highlight = fct_other(Familia, keep = "Other", other_level = "Top N Groups")) |>  # making it two tone
  ggplot(aes(x = Familia, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(position = "dodge")



# Fig.1.2 Most common families normalized over entries per file

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
  arrange(desc(Normalized)) |> 
  mutate(Rank = row_number()) |> 
  filter(Rank <= 40) |> 
  ggplot(aes(x = reorder(Familia_APG, -Normalized, sum), y = Normalized, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge")
  

# Fig.2.1 Total biological form abundances by file
merged |> 
  ggplot(aes(x = FormaBiologica, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(position = "dodge")

# Fig.2.2 Normalized biological form abundances by file
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

View(Normed.Form)

Normed.Form |> 
  select(File, FormaBiologica, Normalized) |>
  ggplot(aes(x = reorder(FormaBiologica, -Normalized, sum), y = Normalized, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge")

# Fig.2.3 total most abundant families by FormaBiologica and File
merged |> 
  filter(Familia_APG == c("Fagaceae",  "Fabaceae",  "Pinaceae", "Burseraceae",
                          "Polygonaceae", "Cactaceae", "Rubiaceae")) |> 
  ggplot(aes(x= Familia_APG, fill = FormaBiologica)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar() + 
  facet_wrap(~File)



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

View(Normed.Disturbance1)

Normed.Disturbance1 |> 
  #subset(!is.na(Danio1)) |> 
  select(File, Danio1, Normalized) |>
  ggplot(aes(x = reorder(Danio1, -Normalized, sum), y = Normalized, fill = File)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(position = "dodge")


# Fig.3.3 Absolute disturbances for comparison
merged |> 
  filter(File == 2) |> 
  #subset(!is.na(Danio1)) |> 
  select(File, Danio1) |> 
  group_by(Danio1) |> 
  count() |> 
  ggplot(aes(x = reorder(Danio1, -n, sum), y = n, fill = "")) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col()

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
    






# test: (USELESS) Edad ~ AlturaTotal Correlation --> gap for file 1 & 2
merged |>
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |>
  subset(!is.na(Edad)) |> 
  ggplot(aes(x = Edad, y = AlturaTotal, color = Familia)) +
  geom_point(alpha = 0.3, 
             position = "jitter") + 
  geom_smooth(method = lm) +
  xlim(1, 300) +
  facet_wrap(~File)


# test: AlturaTotal ~ Familia
merged |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |> 
  subset(!is.na(Familia)) |> 
  ggplot(aes(x = AlturaTotal, y = Familia, fill = Familia, color = Familia)) +
  geom_density_ridges(alpha = 0.5)

# test: AlturaFusteLimpio ~ Familia
merged |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |> 
  subset(!is.na(Familia)) |> 
  ggplot(aes(x = AlturaFusteLimpio, y = Familia, fill = Familia, color = Familia)) +
  geom_density_ridges(alpha = 0.5) 

# test: (USELESS) AlturaTotal ~ DiametroNormal
merged |>
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 5)) |>
  subset(!is.na(Edad)) |> 
  ggplot(aes(x = DiametroNormal, y = AlturaTotal, color = Familia)) +
  geom_point(alpha = 0.3, 
             position = "jitter") + 
  ylim(0, 75) +
  geom_smooth(method = lm) 

# Fig.4 Family Abundance by state (all files)
merged |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 10)) |>
  subset(!is.na(Familia)) |> 
  ggplot(aes(x= Familia, fill = Familia)) +
  geom_bar() + 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Estado, scales = "fixed")

# File 1
merged |> 
  filter(File == 1) |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 7)) |>
  subset(!is.na(Familia)) |> 
  ggplot(aes(x= Familia, fill = Familia)) +
  geom_bar() + 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Estado, scales = "fixed")

# File 2
merged |> 
  filter(File == 2) |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 7)) |>
  subset(!is.na(Familia)) |> 
  ggplot(aes(x= Familia, fill = Familia)) +
  geom_bar() + 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Estado, scales = "fixed")

# File 3
merged |> 
  filter(File == 3) |> 
  mutate(Familia = fct_lump_n(fct_infreq(Familia_APG), n = 7)) |>
  subset(!is.na(Familia)) |> 
  ggplot(aes(x= Familia, fill = Familia)) +
  geom_bar() + 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Estado, scales = "fixed")



# test: posicion copa -> interesting for surpimido and codominante
merged |> 
  subset(!is.na(PosicionCopa)) |> 
  ggplot(aes(y = PosicionCopa)) +
  geom_bar() +
  facet_wrap(~File)

# test: comparison of both -> interesting because of how data seems to be assembled in file 3
merged |> 
  subset(!is.na(ExposicionCopa) & !is.na(PosicionCopa)) |> 
  ggplot(aes(x = PosicionCopa, y = ExposicionCopa)) +
  geom_count() +
  facet_wrap(~File) + 
  guides(x = guide_axis(angle = 90))

  
# test: muerte regressiva -> weird mistakes/NAs in File 3
merged |> 
  subset(!is.na(MuerteRegresiva) & MuerteRegresiva != "Sin parámetro") |> 
  ggplot(aes(x = MuerteRegresiva)) +
  geom_bar() +
  facet_grid(File~.)
  
merged |> 
  subset(!is.na(MuerteRegresiva) & MuerteRegresiva != "Sin parámetro") |> 
  ggplot(aes(x = MuerteRegresiva)) +
  geom_bar()
  
# age-class distribution (kinda...) -> right now no area relationship
merged |> 
  mutate(AgeClass = case_when(Edad < 1 ~ "0",
                              Edad >= 1 & Edad <= 20 ~ "1 - 20",
                              Edad >= 21 & Edad <= 40 ~ "21 - 40",
                              Edad >= 41 & Edad <= 60 ~ "41 - 60",
                              Edad >= 61 & Edad <= 80 ~ "61 - 80",
                              Edad >= 81 & Edad <= 100 ~ "81 - 100",
                              Edad >= 101 & Edad <= 120 ~ "101 - 120",
                              Edad >= 121 & Edad <= 140 ~ "121 - 140",
                              Edad >= 141 & Edad <= 160 ~ "141 - 160",
                              Edad > 160 ~ ">160",
                              TRUE ~ NA)) |> 
  mutate(AgeClass = factor(AgeClass, levels = c("0", "1 - 20", "21 - 40", "41 - 60", "61 - 80", 
                                                "81 - 100", "101 - 120", "121 - 140", "141 - 160", ">160"))) |> 
  subset(!is.na(AgeClass)) |> 
  ggplot(aes(x = AgeClass)) +
  geom_bar() +
  facet_wrap(~File)

merged <- merged |> 
  mutate(AgeClass = case_when(Edad < 1 ~ "0",
                              Edad >= 1 & Edad <= 20 ~ "1 - 20",
                              Edad >= 21 & Edad <= 40 ~ "21 - 40",
                              Edad >= 41 & Edad <= 60 ~ "41 - 60",
                              Edad >= 61 & Edad <= 80 ~ "61 - 80",
                              Edad >= 81 & Edad <= 100 ~ "81 - 100",
                              Edad >= 101 & Edad <= 120 ~ "101 - 120",
                              Edad >= 121 & Edad <= 140 ~ "121 - 140",
                              Edad >= 141 & Edad <= 160 ~ "141 - 160",
                              Edad > 160 ~ ">160",
                              TRUE ~ NA)) |> 
  mutate(AgeClass = factor(AgeClass, levels = c("0", "1 - 20", "21 - 40", "41 - 60", "61 - 80", 
                                                "81 - 100", "101 - 120", "121 - 140", "141 - 160", ">160"))) |> 
  subset(!is.na(AgeClass)) |> 
  # subset(AgeClass != 0) |> 
  group_by(AgeClass) |> 
  count() |> 
  mutate(N = as.numeric(n)) |> 
  ungroup() |> 
  mutate(Sum = sum(N),
         AgePercNA = N/Sum) |> 
  ggplot(aes(x = AgeClass, y = AgePercNA)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent)


#### sensible plotting from now on (kind of)

## species richness by state

merged |> 
    group_by(File, Estado, NombreCientifico_APG) |> 
    select(File, Estado, NombreCientifico_APG) |> 
    distinct() |> 
    ungroup() |> 
    group_by(File, Estado) |> 
    arrange(File, Estado, NombreCientifico_APG) |> 
    ggplot(aes(y = Estado)) +
    geom_bar() +
    facet_wrap(~File)


## gamma-diversity by file
  merged |> 
    group_by(File, NombreCientifico_APG) |> 
    select(File, NombreCientifico_APG) |> 
    distinct() |> 
    ggplot(aes(x = File)) +
    geom_bar()

## Species Richness per file 

  # Arb.04 Setup 
  Arb.04.01 <- Arb.04 |> 
    select(Estado, Conglomerado, Sitio) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(PlotCount = n)
  
  Arb.04.02 <- Arb.04 |> 
    select(Estado, NombreCientifico_APG) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(SpeciesCount = n)
  
  Arb.04.RSR <- Arb.04.01 |> 
    left_join(Arb.04.02, by = "Estado") |> 
    mutate(RSR = SpeciesCount/PlotCount,
           RSR = round(RSR, 3),
           File = as.factor(1)) 
View(Arb.04.RSR)
  
  # Arb.09 Setup 
  Arb.09.01 <- Arb.09 |> 
    select(Estado, Conglomerado, Sitio) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(PlotCount = n)
  
  Arb.09.02 <- Arb.09 |> 
    select(Estado, NombreCientifico_APG) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(SpeciesCount = n)
  
  Arb.09.RSR <- Arb.09.01 |> 
    left_join(Arb.09.02, by = "Estado") |> 
    mutate(RSR = SpeciesCount/PlotCount,
           RSR = round(RSR, 3),
           File = as.factor(2)) 
  
  View(Arb.09.RSR)
  
  # Arb.14 Setup 
  Arb.14.01 <- Arb.14 |> 
    select(Estado, Conglomerado, Sitio) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(PlotCount = n)
  
  Arb.14.02 <- Arb.14 |> 
    select(Estado, NombreCientifico_APG) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(SpeciesCount = n)
  
  Arb.14.RSR <- Arb.14.01 |> 
    left_join(Arb.14.02, by = "Estado") |> 
    mutate(RSR = SpeciesCount/PlotCount,
           RSR = round(RSR, 3),
           File = as.factor(3)) 
  
  # final table
  Merged.RSR <- rbind(Arb.04.RSR, Arb.09.RSR, Arb.14.RSR)
  
  View(Merged.RSR)
  
  Merged.RSR |> 
    select(File, Estado, RSR) |> 
    ggplot(aes(x = RSR, y = Estado)) +
    geom_col() +
    facet_wrap(~File)
  




  
  

# Testing --------------------------


## Species Richness per file V2
  
  # Arb.04 Setup 
  Arb.04.01 <- Arb.04 |> 
    select(Estado, Conglomerado, Sitio) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(PlotCount = n)
  
  Arb.04.02 <- Arb.04 |> 
    select(Estado, NombreCientifico_APG) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(SpeciesCount = n)
  
  Arb.04.RSR <- Arb.04.01 |> 
    left_join(Arb.04.02, by = "Estado") |> 
    mutate(RSR = (SpeciesCount*PlotCount)/68108,
           RSR = round(RSR, 3),
           File = as.factor(1)) 
  
  # Arb.09 Setup 
  Arb.09.01 <- Arb.09 |> 
    select(Estado, Conglomerado, Sitio) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(PlotCount = n)
  
  Arb.09.02 <- Arb.09 |> 
    select(Estado, NombreCientifico_APG) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(SpeciesCount = n)
  
  Arb.09.RSR <- Arb.09.01 |> 
    left_join(Arb.09.02, by = "Estado") |> 
    mutate(RSR = (SpeciesCount*PlotCount)/71437,
           RSR = round(RSR, 3),
           File = as.factor(2)) 
  
  View(Arb.09.RSR)
  
  # Arb.14 Setup 
  Arb.14.01 <- Arb.14 |> 
    select(Estado, Conglomerado, Sitio) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(PlotCount = n)
  
  Arb.14.02 <- Arb.14 |> 
    select(Estado, NombreCientifico_APG) |> 
    distinct() |> 
    group_by(Estado) |> 
    count() |> 
    rename(SpeciesCount = n)
  
  Arb.14.RSR <- Arb.14.01 |> 
    left_join(Arb.14.02, by = "Estado") |> 
    mutate(RSR = (SpeciesCount*PlotCount)/34913,
           RSR = round(RSR, 3),
           File = as.factor(3)) 
  
  # final table
  Merged.RSR <- rbind(Arb.04.RSR, Arb.09.RSR, Arb.14.RSR)
  
  Merged.RSR |> 
    select(File, Estado, RSR) |> 
    ggplot(aes(x = RSR, y = Estado)) +
    geom_col() +
    facet_wrap(~File)
  
  



## other testing stuff

Arb.14 |> 
  select(Estado, NombreCientifico_APG) |> 
    group_by(Estado) |> 
  distinct() |> 
  count()

Arb.04 |> 
  select(Conglomerado, Sitio) |> 
  distinct() |> 
  count()

Arb.09 |> 
  select(Conglomerado, Sitio) |> 
  distinct() |> 
  count()

Arb.14 |> 
  select(Conglomerado, Sitio) |> 
  distinct() |> 
  count()







## ---------------------------------------------------------------------

# Raw.04
# count all NAs per column
NAs <- Raw.04 |> 
  select(everything()) |>   # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(NAs)

# count all 0s per column
ZEROs <- lapply(Raw.04, function(x){ length(which(x==0))})
View(ZEROs)

# count all distinct values per column
Raw.04 |>  
  summarise_all(list(~n_distinct(.)))

# 
ulst <- lapply(Raw.04, unique)

View(ulst)

library(Hmisc)

describe(Raw.04)

## --------------------------------------------------------------------
## Checks

Arb.04 |> 
  select(TipoVeg_S5) |> 
  distinct() |> 
  arrange(TipoVeg_S5) |> 
  print(n = 133)


## ---------------------------------------------------------------------

# Raw.09
# count all NAs per column
NAs <- Raw.09 |> 
  select(everything()) |>   # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(NAs)

# count all 0s per column
ZEROs <- lapply(Raw.09, function(x){ length(which(x==0))})
View(ZEROs)

# count all distinct values per column
Raw.09 |>  
  summarise_all(list(~n_distinct(.)))

# 
ulst <- lapply(Raw.09, unique)

View(ulst)

library(Hmisc)

describe(Raw.09)

## ---------------------------------------------------------------------

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

# count all distinct values per column
Raw.14 |>  
  summarise_all(list(~n_distinct(.)))

# 
ulst <- lapply(Raw.14, unique)

View(ulst)

library(Hmisc)

describe(Raw.14)

Raw.14 |> 
  select(es_para_estimacion_ByC) |> 
  distinct()
