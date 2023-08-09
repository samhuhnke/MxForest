
### MxForestInventory Data Cleansing --------------------------------------

# neccesary packages 

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(dplyr)      #data cleaning
library(tidyr)      #data cleaning
library(stringr)    #data cleaning


# tree inventory data from 2004 - 2020 

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



# assinging new ds for warngling

Arb.04 <- Raw.04

View(Arb.04)

Arb.09 <- Raw.09

View(Arb.09)

Arb.14 <- Raw.14

View(Arb.14)


# adapting Arb.04 & Arb.09 variable names

colnames(Arb.04) == colnames(Arb.09) # Varbiablename inconsistent --> forma biologica

## adapt name in Arb.09 to "forma_biologica"
Arb.09 <- Arb.09 %>%  rename(forma_biologica = Forma_Biologica_1)


# adapting Arb.14 to other datasets







# removing entry mistakes

## Estado - should be 32 distinct values
## Registro - counting individual entries per Site per 'Conglomerado'


### Arb.04 - Estado - initially 33
Arb.04 <- Arb.04 %>% 
  mutate(Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            TRUE ~ Estado)) %>% 
  select(Estado, Conglomerado, Sitio, Registro, everything()) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro)

View(Arb.04)


### Arb.04 - Registro - initially all values 'NULL'
Arb.04 <- Arb.04 %>% 
  mutate(Registro = str_sub(string = cgl_sit_arb, -1)) %>% 
  select(Estado, Conglomerado, Sitio, Registro, everything())


View(Arb.04)


### Arb.09 - initially 38
Arb.09 <- Arb.09 %>% 
  mutate(Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            Estado == "Michoacan de Ocampo" ~ "Michoacán de Ocampo",
                            Estado == "Nuevo Leon" ~ "Nuevo León",
                            Estado == "Queretaro de Arteaga" ~ "Querétaro",
                            Estado == "San Luis Potosi" ~ "San Luis Potosí",
                            Estado == "Yucatan" ~ "Yucatán",
                            TRUE ~ Estado)) %>% 
  select(Estado, Conglomerado, Sitio, Registro, everything()) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro)

View(Arb.09)


### Arb.14 - initially 32 
###-----------------------------------



# 
































# testing and screening of ds
Test.04 <- Arb.04
Test.09 <- Arb.09
Test.14 <- Arb.14




# test for "Estado"

## Arb.04

### First part: normalizing names for estado
Test.04 <- Test.04 %>% 
  mutate(Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            TRUE ~ Estado)) %>% 
  select(Estado, Conglomerado, Sitio, Registro, everything()) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro) %>% 
View(Test.04)

Test.04 <- Test.04 %>% 
  select(Estado) %>%
  arrange(Estado)
  distinct(Estado)

## Arb.09

### First part: normalizing names for estado --> only one missing "Ciudad de Mexico" ~ "Distrito federal"
Test.09 <- Test.09 %>% 
  mutate(Estado = case_when(Estado == "Distrito Federal" ~ "Ciudad de México",
                            Estado == "Michoacan de Ocampo" ~ "Michoacán de Ocampo",
                            Estado == "Nuevo Leon" ~ "Nuevo León",
                            Estado == "Queretaro de Arteaga" ~ "Querétaro",
                            Estado == "San Luis Potosi" ~ "San Luis Potosí",
                            Estado == "Yucatan" ~ "Yucatán",
                            TRUE ~ Estado)) %>% 
  select(Estado, Conglomerado, Sitio, Registro, everything()) %>% 
  arrange(Estado, Conglomerado, Sitio, Registro) %>% 
View(Test.09)


## Arb.14 
T.14 <- Arb.14 %>% 
  arrange(Estado_C3) %>% 
  distinct(Estado_C3) %>% 
  mutate(Cve_Estado = row_number())
View(T.14)

## Difference: Arb.09 has duplicates + "Distro Federal" which is missing in Arb.14


Arb.14 <- Arb.14 %>% 
  arrange(IdConglomerado, Sitio_C3, NoRama_C3)
Arb.14 %>% 
  arrange(Cve_Estado_C3) %>% 
  View()

View(Arb.14)


T.09 <- Arb.09 %>%
  arrange(Conglomerado, Sitio, Registro) %>% 
  filter(Conglomerado == 153)
View(T.09)

T.14 <- Arb.14 %>% 
  arrange(IdConglomerado, Sitio_C3, NoIndividuo_C3) %>% 
  filter(IdConglomerado == 153)
View(T.14)

T.14.2 <- Arb.14 %>% 
  arrange(IdConglomerado, Sitio_C3, NoIndividuo_C3) %>%
  filter(FAO_S7_C3 != "NA")

View(T.14.2)