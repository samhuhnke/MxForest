
### MxForestInventory Data Cleansing --------------------------------------

# neccesary packages 

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(dplyr)      #data cleaning
library(tidyr)      #data cleaning
library(stringr)    #data cleaning


# tree inventory data from 2004 - 2020 --------------------------------------------

## 2004 - 2007
Raw.04 <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"))
ncol(Raw.04); nrow(Raw.04)
str(Raw.04)

## 2009 - 2014
Raw.09 <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"))
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
# setting initial column order
  select(Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, Arbol, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
         ) %>%
# "Estado" Correction - initially 33 -> 32  
  mutate(Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            TRUE ~ Estado),
  ## "Registro" Correction - initially all values "NULL" -> pull data from string cgl_sit_reg
         Registro = str_extract(cgl_sit_reg, "(?<=_)[^_]+$") %>%
           as.integer()) %>%
# sorting for comparison
  arrange(Estado, Conglomerado, Sitio, Registro)








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
# "Estado" Correction - initially 38 -> 32  
  mutate(Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            Estado == "Michoacan de Ocampo" ~ "Michoacán de Ocampo",
                            Estado == "Nuevo Leon" ~ "Nuevo León",
                            Estado == "Queretaro de Arteaga" ~ "Querétaro",
                            Estado == "San Luis Potosi" ~ "San Luis Potosí",
                            Estado == "Yucatan" ~ "Yucatán",
                            TRUE ~ Estado)
         ) %>% 
# "Registro" Correction + "cgl_sit_arb" Correction - obviously wrong entries -> corrected to expected entry   
  mutate(Registro = case_when(Registro == 316 ~ 16,
                              TRUE ~ Registro),
         cgl_sit_reg = str_replace(string = cgl_sit_reg, "316$", "16"),
         cgl_sit_reg = str_replace(string = cgl_sit_reg, "4_147$", "4_28"),
         Registro = ifelse(cgl_sit_reg == "21314_4_28", as.integer(str_extract(cgl_sit_reg, "\\d+$")), Registro)
         ) %>% 
# setting initial column order
  select(Anio, Estado, Conglomerado, Sitio, Registro, cgl_sit_reg, Arbol, CveVeg_S5, TipoVeg_S5, FormaFuste, 
         TipoTocon, Familia_APG, NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal,
         AlturaFusteLimpio, AlturaComercial, DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa,
         PosicionCopa, ExposicionCopa, DensidadCopa, TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion,
         Danio1, Severidad1, Danio2, Severidad2, NumeroTallos, LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything() 
         ) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro) 



## Check-up Arb.09
Arb.09 %>% 
  distinct(Estado) %>% 
  count()

max(Arb.09$Registro)





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
# setting initial column order
  select(Anio, Estado, Conglomerado, Sitio, Registro, NoIndividuo_C3, CveVeg_S7, TipoVeg_S7, FormaFuste, TipoTocon, Familia_APG,
         NombreCientifico_APG, NombreComun, FormaBiologica, Distancia, Azimut, AlturaTotal, AlturaFusteLimpio, AlturaComercial,
         DiametroNormal, DiametroBasal, DiametroCopa, AreaBasal, AreaCopa, PosicionCopa, ExposicionCopa, DensidadCopa,
         TransparenciaCopa, MuerteRegresiva, VigorEtapa, Edad, Condicion, Danio1, Severidad1, Danio2, Severidad2, NumeroTallos,
         LongitudAnillos10, NumeroAnillos25, GrosorCorteza, X, Y,
         everything()
         ) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro)













#--------------------------------------------------------------------------------------------------
# PROTOTYPING: REMOVING DATA MISTAKES/INCONSISTENCIES ----------------------------------------------------------

## Estado - should be 32 distinct values
## Registro - counting individual entries per Site per 'Conglomerado'
## 



#-----------------------------------------------------------------------------
# Arb.04 (TEMPORARY) ---------------------------------------------------------
View(Arb.04)

# CHANGELOG - Arb.04 ----------------------------------------------------------------------
### Arb.04 - Estado - initially 33
### Arb.04 - Registro -  all values "NULL" - pulling data from string cgl_sit_reg













#-----------------------------------------------------------------------------
# Arb.09 (TEMPORARY) ---------------------------------------------------------
View(Arb.09)

# CHANGELOG - Arb.09 ----------------------------------------------------------------------
### Arb.09 - Estado - initially 38
### Arb.09 - Registro -  Mistake in Guerrero 69336 2: 316 - supposedly just 16
### Arb.09 - Registro - Mistake in Chihuahua 21314 4: 147 - supposedly just 28
### Arb.09 - cgl_sit_arb - same issue in string of Conglomerado, Sitio and Registro











#-----------------------------------------------------------------------------
# Arb.14 (TEMPORARY) ---------------------------------------------------------
View(Arb.09)

# CHANGELOG - Arb.09 ----------------------------------------------------------------------
### Arb.09 - Estado - initially 38
### Arb.09 - Registro -  Mistake in Guerrero 69336 2: 316 - supposedly just 16
### Arb.09 - Registro - Mistake in Chihuahua 21314 4: 147 - supposedly just 28
### Arb.09 - cgl_sit_arb - same issue in string of Conglomerado, Sitio and Registro
























#-----------------------------------------------------------------------------
# ABSOLUTE TESTING BOOTH ---------------------------------------------------------

# testing and screening of ds
Test.04 <- Arb.04
Test.09 <- Arb.09
Test.14 <- Arb.14



# ECO - Formato values

Arb.04 %>% 
  select(TransparenciaCopa) %>% 
  distinct(TransparenciaCopa)

Arb.09 %>% 
  select(TransparenciaCopa) %>% 
  distinct(TransparenciaCopa)

Arb.14 %>% 
  select(Estado_C3, IdConglomerado, Sitio_C3, NoIndividuo_C3, Consecutivo_C3, ArboladoID_C3) %>% 
  arrange(ArboladoID_C3) %>% 
  count(ArboladoID_C3)



