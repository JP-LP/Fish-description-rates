############################
## Fish description rates
## Fishbase
############################
rm(list=ls())

library(rfishbase)
library(dplyr)
library(tidyr)
setwd("C:/Users/juanp/Documents/CABRA/BRANDL/Description rates")

##### ---- Max Total length ----
Allfish <- popchar(
  species_list = NULL,
  fields = NULL,
  server = getOption("FISHBASE_API", "fishbase"))

LmaxData <- Allfish %>% select("Species", "Lmax") %>% as.data.frame()
nrow(LmaxData)
head(LmaxData)

SummaryData <- LmaxData %>% 
  group_by(Species) %>%  
  summarise(maxLmax = max(Lmax, na.rm = T)) %>% 
  as.data.frame()

ListSpp <- load_taxa() %>% as.data.frame()
TaxData <- SummaryData %>% left_join(ListSpp, by="Species") %>% as.data.frame()
TaxData$maxLmax <- ifelse(is.finite(TaxData$maxLmax), TaxData$maxLmax , NA) #Change -Inf --> NA
nrow(TaxData) #34721
length(unique(TaxData$Species))#34721 (With 27128 NA)
head(TaxData)
summary(TaxData$maxLmax)

Lmax1 <- TaxData %>% select(Class, Order, Family, Genus, Species, maxLmax) %>% as.data.frame()
Lmax1$maxLmax <- ifelse(is.finite(Lmax1$maxLmax), Lmax1$maxLmax, NA)
head(Lmax1)


GenuslevelData <- Lmax1 %>% group_by(Genus) %>% 
  summarise(MeanmaxLmax_Genus = mean(maxLmax, na.rm = T),
          sdmaxLmax_Genus = sd(maxLmax, na.rm = T)) %>% as.data.frame()


FamilylevelData <- Lmax1 %>% group_by(Family) %>% 
          summarise(MeanmaxLmax_Family = mean(maxLmax, na.rm = T),
            sdmaxLmax_Family = sd(maxLmax, na.rm = T)) %>% as.data.frame()

   
head(GenuslevelData)
head(FamilylevelData)     
write.csv(Lmax1, "SpeciesData.csv")
write.csv(GenuslevelData, "GenusData.csv")
write.csv(FamilylevelData, "FamilyData.csv")

#######
setwd("C:/Users/juanp/Documents/CABRA/BRANDL/Description rates")

Spptable <- read.csv("SpeciesData.csv", header =T)
Genustable <- read.csv("GenusData.csv", header =T)
Familytable <- read.csv("FamilyData.csv", header = T)
head(Spptable)
head(Genustable)
head(Familytable)
nrow(Spptable) #34721
nrow(Genustable) #5177
nrow(Familytable) #602


#Join tables (Spptable as a baseline)

Gen_Data <- left_join(Spptable,Genustable, by="Genus")
Fam_Data <- left_join(Gen_Data, Familytable, by="Family") %>% as.data.frame() %>% 
  mutate(Taxlevel = case_when((!is.na(maxLmax)) ~ "Spp",
                              (is.na(maxLmax) & !is.na(MeanmaxLmax_Genus)) ~ "Genus",
   (is.na(maxLmax) & is.na(MeanmaxLmax_Genus) & !is.na(MeanmaxLmax_Family)) ~ "Family",
   (is.na(maxLmax) & is.na(MeanmaxLmax_Genus) & is.na(MeanmaxLmax_Family)) ~ "CHECK")) 
Fam_Data
nrow(Fam_Data)
length(unique(Fam_Data$Species))
levels(as.factor(Fam_Data$Taxlevel))
length(unique(Fam_Data$Species))
length(unique(Fam_Data$Genus))
length(unique(Fam_Data$Family))
write.csv(Fam_Data, "AgeData.csv")



CheckData <- Fam_Data %>% filter(Taxlevel=="CHECK")
length(unique(CheckData$Species))
length(unique(CheckData$Genus))
length(unique(CheckData$Family))

write.csv(CheckData, "CheckData.csv")



##### ---- Checking families without data
setwd("C:/Users/juanp/Documents/CABRA/BRANDL/Description rates")
CheckData <- read.csv("CheckData.csv", header =T, sep=",")
CheckData

FamNoData <- CheckData %>% group_by(Family) %>% 
             summarise(nGenus = length(unique(Genus)),
                       nSpp = length(unique(Species))) %>% 
  filter(nSpp > 20) %>% as.data.frame()
FamNoData 

write.csv(FamNoData, "FamiliesWithNoData.csv")


##### ---- Max Age ----
Allfish <- popchar(
  species_list = NULL,
  fields = NULL,
  server = getOption("FISHBASE_API", "fishbase"))

tmaxData <- Allfish %>% select("Species", "tmax") %>% as.data.frame()
nrow(tmaxData)
head(tmaxData)

SummaryData <- tmaxData %>% 
  group_by(Species) %>%  
  summarise(maxtmax = max(tmax, na.rm = T)) %>% 
  as.data.frame()

ListSpp <- load_taxa() %>% as.data.frame()
TaxData <- SummaryData %>% left_join(ListSpp, by="Species") %>% as.data.frame()
TaxData$maxtmax <- ifelse(is.finite(TaxData$maxtmax), TaxData$maxtmax , NA) #Change -Inf --> NA
nrow(TaxData) #34721
length(unique(TaxData$Species))#34721 (With 27128 NA)
head(TaxData)
summary(TaxData$maxtmax)

tmax1 <- TaxData %>% select(Class, Order, Family, Genus, Species, maxtmax) %>% as.data.frame()
tmax1$maxtmax <- ifelse(is.finite(tmax1$maxtmax), tmax1$maxtmax, NA)
head(tmax1)


GenuslevelData <- tmax1 %>% group_by(Genus) %>% 
  summarise(Meanmaxtmax_Genus = mean(maxtmax, na.rm = T),
            sdmaxtmax_Genus = sd(maxtmax, na.rm = T)) %>% as.data.frame()


FamilylevelData <- tmax1 %>% group_by(Family) %>% 
  summarise(Meanmaxtmax_Family = mean(maxtmax, na.rm = T),
            sdmaxtmax_Family = sd(maxtmax, na.rm = T)) %>% as.data.frame()


head(GenuslevelData)
head(FamilylevelData)     
write.csv(tmax1, "AgeSpeciesData.csv")
write.csv(GenuslevelData, "AgeGenusData.csv")
write.csv(FamilylevelData, "AgeFamilyData.csv")

####### ---- 

Spptable <- read.csv("AgeSpeciesData.csv", header =T)
Genustable <- read.csv("AgeGenusData.csv", header =T)
Familytable <- read.csv("AgeFamilyData.csv", header = T)
head(Spptable)
head(Genustable)
head(Familytable)
nrow(Spptable) #34721
nrow(Genustable) #5177
nrow(Familytable) #602


#Join tables (Spptable as a baseline)

Gen_Data <- left_join(Spptable,Genustable, by="Genus")
Fam_Data <- left_join(Gen_Data, Familytable, by="Family") %>% as.data.frame() %>% 
  mutate(Taxlevel = case_when((!is.na(maxtmax)) ~ "Spp",
                              (is.na(maxtmax) & !is.na(Meanmaxtmax_Genus)) ~ "Genus",
                              (is.na(maxtmax) & is.na(Meanmaxtmax_Genus) & !is.na(Meanmaxtmax_Family)) ~ "Family",
                              (is.na(maxtmax) & is.na(Meanmaxtmax_Genus) & is.na(Meanmaxtmax_Family)) ~ "CHECK")) 
Fam_Data
nrow(Fam_Data)
length(unique(Fam_Data$Species))
levels(as.factor(Fam_Data$Taxlevel))
length(unique(Fam_Data$Species))
length(unique(Fam_Data$Genus))
length(unique(Fam_Data$Family))
write.csv(Fam_Data, "AgeData.csv")



CheckData <- Fam_Data %>% filter(Taxlevel=="CHECK")
length(unique(CheckData$Species))
length(unique(CheckData$Genus))
length(unique(CheckData$Family))

write.csv(CheckData, "AgeCheckData.csv")


# ---- Add Explanatory variables ----

Odata <- read.csv("DescriptionRatesData.csv", header = T, sep = ",")
length(Odata$Species)

 #Country = Extract number of countries where fish species are reported 
 #per species + freshwater,brackish,saltwater.

Paises <- country(species_list = NULL,
                  fields = NULL,
                  server = getOption("FISHBASE_API", "fishbase"))
NPPS <- Paises %>% select("Species","Freshwater","Brackish","Saltwater","country") %>% 
  group_by(Species) %>% 
  summarise (no.countries = length(country),
             freshwater = max(Freshwater),
             brackish = max(Brackish),
             saltwater = max(Saltwater)) %>% as.data.frame() 
length(NPPS$Species)

Odata.1 <- Odata %>% left_join(NPPS, by="Species") %>% as.data.frame()
length(Odata.1$Species)

##Extract FAO Areas per specie 

distrib <- distribution(species_list = NULL,
                        fields = NULL,
                        server = getOption("FISHBASE_API", "fishbase"))%>%  
  select("Species", "FAO", "Shelf", "AreaCode") %>% group_by(Species) %>% 
  summarise(
    no.FAOAreas = length(unique(FAO)),
    sumFAOAreas = sum(Shelf),
    shelf = case_when((!is.na(Shelf)) ~ 1),
    FAOid = FAO) %>% distinct() %>% spread(key = FAOid, value = shelf, fill = 0) %>% 
  as.data.frame() 

Odata.2 <- Odata.1 %>% left_join(distrib, by="Species") %>% as.data.frame()
length(Odata.2$Species)

## Extract ecosystem per species  
 ecolo <- ecology(species_list = NULL,
                 fields = NULL,
                 server = getOption("FISHBASE_API", "fishbase")) %>%
  select(Species, Neritic,SupraLittoralZone,Saltmarshes,LittoralZone,TidePools,Intertidal,SubLittoral,
         Caves,Oceanic,Epipelagic,Mesopelagic,Abyssopelagic,Hadopelagic,Estuaries,Mangroves,
         MarshesSwamps,CaveAnchialine, Stream, Lakes, Cave) %>% filter(duplicated(Species) == F) %>% as.data.frame()
   
 
 Odata.3 <- Odata.2 %>% left_join(ecolo, by="Species") %>%
   distinct() %>% as.data.frame()
 length(Odata.3$Species)


## Extract General latitudinal information per specie
 ecosys <- ecosystem(species_list = NULL,
                     fields = NULL,
                     server = getOption("FISHBASE_API", "fishbase")) %>% 
   select(Species, Climate) %>% 
   group_by(Species) %>% 
   summarise(ClimateCorr = case_when((Climate == "tropical") ~ "Tropical",
                                     (Climate == "subtropical") ~ "Subtropical",
                                     (Climate == "temperate") ~ "Temperate",
                                     (Climate == "tenperate") ~ "Temperate",
                                     (Climate == "polar") ~ "Polar",
                                     (Climate == "boreal") ~ "Boreal")) %>% 
   as.data.frame()
 
 ecosysData <- ecosys %>% group_by(Species) %>% 
             summarise(ClimValue = case_when((!is.na(ClimateCorr)) ~ 1),
             Clim = ClimateCorr)  %>% 
   distinct() %>% 
   spread(key = Clim, value = ClimValue, fill = 0) %>% 
   as.data.frame() 
 
 Odata.4 <- Odata.3 %>% left_join(ecosysData, by="Species") %>%
   distinct() %>% as.data.frame()
 length(Odata.4$Species) #34182 spp
 
 write.csv(Odata.4, "DRDF.csv")
 
## Merge explanatory variables(+MaxLength) with Age data frame as a baseline
AgeData <- read.csv("AgeData.csv", header = T, sep=",")

DRDF_Age <- AgeData %>% left_join(Odata.4, by = "Species") %>% 
  distinct() %>% as.data.frame()
length(DRDF_Age$Species) #27350 spp

write.csv(DRDF_Age, "DRDF.Age.csv")

# Create unique column with Length and Age values.
DRF_Age <- read.csv("DRDF.Age.csv", header = T, sep=",")
DescRates <- DRF_Age %>% mutate(Age = coalesce(maxtmax, Meanmaxtmax_Genus, Meanmaxtmax_Family)) %>% 
            mutate(TotalLength = coalesce(maxLmax, MeanmaxLmax_Genus, MeanmaxLmax_Family)) %>% 
  as.data.frame()

write.csv(DescRates, "DescRates.csv") 

#---- Converting distance between coordinates (in degrees) into KM units to calculate convex hull areas ----
library(measurements)
library(GeoRange)
OccSpp<- distribution(species_list = NULL,
                      fields = NULL,
                      server = getOption("FISHBASE_API", "fishbase"))


OccSpp$Range = NA
OccSpp=OccSpp %>% drop_na(NorthernLatitude) %>% drop_na(SouthernLatitude) %>% drop_na(WesternLongitude) %>% drop_na(EasternLongitude)
N=ifelse(OccSpp$NorthernLatitudeNS == "N",OccSpp$NorthernLatitude-0.001,-1 *(OccSpp$NorthernLatitude-0.001))
S=ifelse(OccSpp$SouthernLatitudeNS == "N", OccSpp$SouthernLatitude-0.001, -1*(OccSpp$SouthernLatitude-0.001))
W=ifelse(OccSpp$WesternLongitudeEW == "E", OccSpp$WesternLongitude-0.001, -1*(OccSpp$WesternLongitude-0.001))
E=ifelse(OccSpp$EasternLongitudeEW == "E", OccSpp$EasternLongitude-0.001, -1*(OccSpp$EasternLongitude-0.001))


for(q in 1:nrow(OccSpp)){
coords=rbind(
c(N[q],E[q]),
c(S[q],E[q]),
c(S[q],W[q]),
c(N[q],W[q]))

OccSpp$Range[q]=CHullAreaEarth(coords[,2],coords[,1])
}


aggregate(Range~Species,data = OccSpp,FUN=sum)

