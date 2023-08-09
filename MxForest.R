### MxForestInventory Data Cleansing ########

# neccesary packages

library(data.table) #fread()
library(readxl)     #read_xlsx()
library(here)       #here()
library(dplyr)      #data cleansing
library(tidyr)      #data cleansing


# tree inventory data from 2004 - 2020 

## 2004 - 2007
Arb.04 <- fread(here("data", "arbolado", "INFyS_Arbolado_2004_2007.csv"))
ncol(Arb.04); nrow(Arb.04)
str(Arb.04)

## 2009 - 2014
Arb.09 <- fread(here("data", "arbolado", "INFyS_Arbolado_2009_2014.csv"))
ncol(Arb.09); nrow(Arb.09)
str(Arb.09)

## 2015 - 2020
Arb.14 <- readxl::read_xlsx(here("data", "arbolado", "INFYS_Arbolado_2015_2020.xlsx"), sheet= 1)
ncol(Arb.14); nrow(Arb.14)
str(Arb.14)




# adapting Arb.04 & Arb.09 variable names

colnames(Arb.04) == colnames(Arb.09) # Varbiablename inconsistent --> forma biologica

## adapt name in Arb.09 to "forma_biologica"
Arb.09 <- Arb.09 %>%  rename(forma_biologica = Forma_Biologica_1)


# adapting Arb.14 to other datasets















































# testing and screening of ds

Arb.09 <- Arb.09 %>% 
  arrange(Conglomerado, Sitio, Registro)
View(Arb.09)

Arb.14 <- Arb.14 %>% 
  arrange(IdConglomerado, Sitio_C3, NoRama_C3)
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