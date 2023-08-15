
### MxForestInventory Data Cleansing --------------------------------------

# neccesary packages 

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(dplyr)      #data cleaning
library(tidyr)      #data cleaning
library(stringr)    #data cleaning


# tree inventory data from 2004 - 2020 --------------------------------------------

## 2004 - 2007 -> changing "NULL" character values to NA
Raw.04 <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"), na.strings = "NULL")
ncol(Raw.04); nrow(Raw.04)
str(Raw.04)

## 2009 - 2014
Raw.09 <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"), na.strings = "NULL")
ncol(Raw.09); nrow(Raw.09)
str(Raw.09)

## 2015 - 2020
Raw.14 <- readxl::read_xlsx(here("data", "arbolado", "INFYS_Arbolado_2015_2020.xlsx"), sheet= 1)
ncol(Raw.14); nrow(Raw.14)
str(Raw.14)



# assinging new ds for warngling --------------------------------------------------

Arb.04 <- Raw.04

View(Arb.04)


Arb.09 <- Raw.09

View(Arb.09)


Arb.14 <- Raw.14

View(Arb.14)


##----------------------------------------------------------------------------------------------------------------
## Arb.04 Data cleaning ------------------------------------------------------------------------------------------

Arb.04 <- Raw.04 %>%  
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
         ) %>% 
# setting initial column order + attaching everything so far not considered to the end
  select(Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, Arbol, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
         ) %>%
# Correction of categoric and specific entry mistakes
  mutate(
        # "Estado" Correction - initially 33 -> 32
          Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            TRUE ~ Estado),
        # "Registro" Correction - initially all values NA -> pull data from string cgl_sit_reg
          Registro = str_extract(cgl_sit_reg, "(?<=_)[^_]+$") %>%
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
        # "AlturaFusteLimpio" Correction - class "character" -> class "numeric"
          AlturaFusteLimpio = as.numeric(AlturaFusteLimpio),
        # "AlturaComercial" Correction - class "character" ->  class "numeric"
          AlturaComercial = as.numeric(AlturaComercial),
        # "DiametroBasal" Correction - class "character" ->  class "numeric"
          DiametroBasal = as.numeric(DiametroBasal),
        # "DiametroCopa" Correction - class "character" ->  class "numeric"
          DiametroCopa = as.numeric(DiametroCopa),
        # "AreaBasal" Correction - class "character" ->  class "numeric"
          AreaBasal = as.numeric(AreaBasal),
        # "AreaCopa" Correction - class "character" ->  class "numeric"
          AreaCopa = as.numeric(AreaCopa),
        # "VigorEtapa" Correction - NA + class names -> "no capturado" + new names (in line with Arb.14)
          VigorEtapa = case_when(VigorEtapa == "Arbol joven" ~ "Árbol joven",
                                 VigorEtapa == "Arbol maduro" ~ "Árbol maduro",
                                 VigorEtapa == "Arbol muy joven" ~ "Árbol muy joven",
                                 VigorEtapa == "Arbol viejo o supermaduro" ~ "Arbol viejo o super-maduro",
                                 is.na(VigorEtapa) ~ "No capturado",
                                 TRUE ~ VigorEtapa),
        # "Edad" Correction - "NULL" + class "character" -> NA + class "numeric" (+ rounding numbers)
          Edad = ifelse(!is.na(Edad),
                        ifelse(as.numeric(Edad) - floor(as.numeric(Edad)) >= 0.5,
                               ceiling(as.numeric(Edad)),
                               floor(as.numeric(Edad))),
                        NA),
        # "Condicion" Correction - values names -> new value names
          Condicion = case_when(Condicion == "Muerto en pie" ~ "Árbol muerto en pie",
                                Condicion == "Vivo" ~ "Árbol vivo",
                                TRUE ~ Condicion),
        # "NumeroTallos" Correction - class "character" -> class "numeric"
          NumeroTallos = as.numeric(NumeroTallos),
        # "LongitudAnillos10" Correction - class "character" -> class "numeric"
          LongitudAnillos10 = as.numeric(LongitudAnillos10),
        # "NumeroAnillos25" Correction - class "character" -> class "numeric"
          NumeroAnillos25 = as.numeric(NumeroAnillos25),
        # "GrosorCorteza" Correction - class "character" -> class "numeric"
          GrosorCorteza = as.numeric(GrosorCorteza)) %>% 
# sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)


Test.04 <- Arb.04 %>% 
  select(Edad) %>% 
  distinct() %>%
  arrange(Edad)
View(Test.04)


##----------------------------------------------------------------------------------------------------------------
## Arb.09 Data cleaning ------------------------------------------------------------------------------------------

Arb.09 <- Raw.09 %>% 
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
         ) %>% 
# Correction of categoric and specific entry mistakes 
  mutate(
        # "Estado" Correction - initially 38 -> 32 
          Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
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
        # "FormaFuste" Correction - 
        # "TipoTocon" Correction - 
        # "VigorEtapa" Correction - NA + class names -> "no capturado" + new names (in line with Arb.14)
          VigorEtapa = case_when(VigorEtapa == "Arbol joven" ~ "Árbol joven",
                                 VigorEtapa == "Arbol maduro" ~ "Árbol maduro",
                                 VigorEtapa == "Arbol muy joven" ~ "Árbol muy joven",
                                 VigorEtapa == "Arbol viejo o supermaduro" ~ "Arbol viejo o super-maduro",
                                 is.na(VigorEtapa) ~ "No capturado",
                                 TRUE ~ VigorEtapa),
        # "Edad" Correction - "NULL" + class "character" -> NA + class "numeric" (+ rounding numbers)
          Edad = ifelse(!is.na(Edad),
                        ifelse(as.numeric(Edad) - floor(as.numeric(Edad)) >= 0.5,
                               ceiling(as.numeric(Edad)),
                               floor(as.numeric(Edad))),
                        NA),
        # "Condicion" Correction - values names -> new value names
        Condicion = case_when(Condicion == "Muerto en pie" ~ "Árbol muerto en pie",
                              Condicion == "Vivo" ~ "Árbol vivo",
                              TRUE ~ Condicion),
        # "Danio1" Correction - 
        # "Severidad1" Correction -
        # "Danio2" Correction -
        # "Severidad2" Correction - 
        # "NumeroTallos" Correction - 
        # "LongitudAnillos10" - 
        # "NumeroAnillos25" -
        
         ) %>% 
  
# setting initial column order + attaching everything so far not considered to the end
  select(Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, Arbol, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything() 
         ) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro) 





class(Arb.09$LongitudAnillos10)

Test.09 <- 
  Arb.09 %>% 
  select(GrosorCorteza) %>% 
 # filter(LongitudAnillos10 == 999 ) %>% 
  distinct() %>% 
  arrange(GrosorCorteza) #%>% 
  count()
View(Test.09)

##----------------------------------------------------------------------------------------------------------------
## Arb.14 Data cleaning ------------------------------------------------------------------------------------------

Arb.14 <- Raw.14 %>% 
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
         ) %>% 
# mutate() %>% ### einfügen von cgl_sit_reg   <----- HIER MUSS NOCH WAS REIN
# setting initial column order + attaching everything so far not considered to the end
  select(Anio, Estado, Conglomerado, Sitio, Registro, NoIndividuo_C3, CveVeg_S7, TipoVeg_S7, FormaFuste, TipoTocon, Familia_APG,
         NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal, AlturaFusteLimpio, AlturaComercial,
         DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa, PosicionCopa, ExposicionCopa, DensidadCopa,
         TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion, Danio1, Severidad1, Danio2, Severidad2, NumeroTallos,
         LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
         ) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro)













#---------------------------------------------------------------------------------------------------------------
# PROTOTYPING: REMOVING DATA MISTAKES/INCONSISTENCIES ----------------------------------------------------------



#-----------------------------------------------------------------------------
# Arb.04 (TEMPORARY) ---------------------------------------------------------
View(Arb.04)

# CHANGELOG - Arb.04 ----------------------------------------------------------------------
### Arb.04 - Estado - initially 33 -> changed to 32
### Arb.04 - Registro -  all values "NULL" -> pulling data from string cgl_sit_reg
### Arb.04 - AlturaFusteLimpio - "NULL" + class "character" -> "NA" + class "numeric"
### Arb.04 - AlturaComercial - "NULL" + class "character" -> "NA" + class "numeric"
### Arb.04 - DiametroBasal - "NULL" + class "character" -> "NA" + class "numeric"
### Arb.04 - DiametroCopa - "NULL" + class "character" -> "NA" + class "numeric"
### Arb.04 - AreaBasal - "NULL" + class "character" -> "NA" + class "numeric"
### Arb.04 - AreaCopa - "NULL" + class "character" -> "NA" + class "numeric"
### Arb.04 - VigorEtapa - "NULL" + value names -> "no capturado" + new value names
### Arb.04 - Condicion - value names -> new value names
### Arb.04 - Edad - "NULL" + class "character" -> NA +  class "numeric" (+ rounding numbers)
### Arb.04 - NumeroTallos - "NULL" + class "character" -> NA + class "numeric"
### Arb.04 - LongitudAnillos10 - "NULL" + class "character" -> NA + class "numeric"
### Arb.04 - NumeroAnillos25 - "NULL" + class "character" -> NA + class "numeric"
### Arb.04 - GrosorCorteza - "NULL" + class "character" -> NA + class "numeric"

### Arb.04 - "NULL" character values -> changed them from the start by defining "NULL" as NA-values in fread()

### Arb.04 - CveVeg_S5 - "VSaa" -> "VSa" 

# Testing ------------------------------------
# CURRENT: Cve_veg_S5 & TipoVeg_S5 Corrections



#-----------------------------------------------------------------------------
# Arb.09 (TEMPORARY) ---------------------------------------------------------
View(Arb.09)

# CHANGELOG - Arb.09 ----------------------------------------------------------------------
### Arb.09 - Estado - initially 38
### Arb.09 - Registro -  Mistake in Guerrero 69336 2: 316 - supposedly just 16
### Arb.09 - Registro - Mistake in Chihuahua 21314 4: 147 - supposedly just 28
### Arb.09 - cgl_sit_arb - same issue in string of Conglomerado, Sitio and Registro

T.09 <- Arb.09 %>%
  distinct(Edad) %>% 
  arrange(Edad)

View(T.09)

Age.09 <- Arb.09 %>% 
  mutate(Int = ifelse(Edad != "NULL", as.integer(Edad), NA),
         Num = ifelse(Edad != "NULL", as.numeric(Edad), NA)) %>% 
  select(Edad, Int, Num) %>% 
  filter(!is.na(Int)) %>% 
  arrange(Int)

# -0.098 neben orginal wert
mean(Age.09$Int) #-
mean(Age.09$Num)
# +0.008 neben orginal wert

Age.09 <- Arb.09 %>%
  mutate(Int = ifelse(Edad != "NULL", 
                      ifelse(as.numeric(Edad) - floor(as.numeric(Edad)) >= 0.5,
                             ceiling(as.numeric(Edad)),
                             floor(as.numeric(Edad))),
                      NA),
         Num = ifelse(Edad != "NULL", as.numeric(Edad), NA)) %>% 
  select(Edad, Int, Num) %>% 
  filter(!is.na(Int)) %>% 
  distinct() %>% 
  arrange(Int)

View(Age.09)

mean(Age.09$Int) - mean(Age.09$Num)



#-----------------------------------------------------------------------------
# Arb.14 (TEMPORARY) ---------------------------------------------------------
View(Arb.14)

# CHANGELOG - Arb.09 ----------------------------------------------------------------------








###---------------------------------------------------------------------------------------------------------------
# FURHTER ANALYSIS AND COMPARISON --------------------------------------------------------------------------------


### Comparing CveVeg  and TipoVeg across ds --------------------------------------------------------

# Step 0 - Preperations -> mutate important to adapt values of CveVeg of Arb.04 and Arb.09 to Arb.14 standards

T.04 <- Arb.04 %>%
  select(CveVeg_S5, TipoVeg_S5) %>%
  arrange(CveVeg_S5) %>%
  distinct()



T.09 <- Arb.09 %>% 
  mutate(CveVeg_S5 = str_replace(CveVeg_S5, "VSaa", "VSa"),
         TipoVeg_S5 = ifelse(str_detect(CveVeg_S5, "VSa"), 
                             paste("VEGETACION SECUNDARIA ARBUSTIVA DE ", TipoVeg_S5, sep = ""), 
                             TipoVeg_S5),
         TipoVeg_S5 = ifelse(str_detect(CveVeg_S5, "VSA"),
                             paste("VEGETACION SECUNDARIA ARBOREA DE ", TipoVeg_S5, sep = ""),
                             TipoVeg_S5),
         TipoVeg_S5 = ifelse(str_detect(CveVeg_S5, "VSh"),
                             paste("VEGETACION SECUNDARIA HERBACEA DE ", TipoVeg_S5, sep = ""),
                             TipoVeg_S5)) %>% 
  select(CveVeg_S5, TipoVeg_S5) %>% 
  rename(CveVeg_S5_09 = CveVeg_S5,
         TipoVeg_S5_09 = TipoVeg_S5) %>% 
  arrange(CveVeg_S5_09) %>% 
  distinct()


T.14 <- Arb.14 %>%
  select(CveVeg_S7, TipoVeg_S7) %>%
  mutate(
    TipoVeg_S7 = str_replace_all(TipoVeg_S7, c(
      "Í" = "I",
      "Ó" = "O",
      "Á" = "A",
      "É" = "E",
      "ARBÓREA" = "ARBOREA",
      "VEGETACIÓN" = "VEGETACION",
      "Bosque mesófilo" = "BOSQUE MESOFILIO",
      "HALÓFILA XERÓFILA" = "HALOFILA XEROFILA",
      "Ñ" = "N"
    ))
  ) %>%
  arrange(CveVeg_S7) %>%
  distinct()



# Step 1
distinct_values <- unique(c(T.04$CveVeg_S5, T.09$CveVeg_S5_09, T.14$CveVeg_S7))

T.merged <- data.frame(CveVeg = distinct_values)

View(T.merged)

# Step 2
distinct_combinations <- T.04 %>% 
  select(CveVeg_S5, TipoVeg_S5) %>% 
  distinct()

T.merged <- T.merged %>%
  left_join(distinct_combinations, by = c("CveVeg" = "CveVeg_S5"))

View(T.merged)

# Step 3
distinct_combinations_tipo <- T.09 %>%
  select(CveVeg_S5_09, TipoVeg_S5_09) %>%
  distinct()

T.merged <- T.merged %>%
  left_join(distinct_combinations_tipo, by = c("CveVeg" = "CveVeg_S5_09"))

View(T.merged)

# Step 4
distinct_combinations_tipo <- T.14 %>%
  select(CveVeg_S7, TipoVeg_S7) %>%
  distinct()

T.merged <- T.merged %>%
  left_join(distinct_combinations_tipo, by = c("CveVeg" = "CveVeg_S7")) %>% 
  arrange(CveVeg)

View(T.merged)




#---------------------------------------------------------------------------------
# VALUE CHECKING -----------------------------------------------------------------

# CveVeg_S5 and CveVeg_S7 -----------------

# Arb.04 --------------------


# Arb.09 --------------------


# Arb.14 --------------------


#---------------------------------------------------------------------------------
# ABSOLUTE TESTING BOOTH ---------------------------------------------------------

# testing and screening of ds
Test.04 <- Arb.04
Test.09 <- Arb.09
Test.14 <- Arb.14



























