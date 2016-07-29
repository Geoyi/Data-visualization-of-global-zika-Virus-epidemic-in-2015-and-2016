library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(rworldmap)

#system("ls ../input")
#zika = read.csv("../input/cdc_zika.csv", stringsAsFactors = F, header = T)

setwd("C:/Data Science Fundation with R/Kraggle/zika-virus-epidemic")
list.files("C:/Data Science Fundation with R/Kraggle/zika-virus-epidemic")
zika <- read.csv('cdc_zika.csv',header=TRUE, fill=TRUE,row.names=NULL)
zika <- data.table(zika)
zika[, c("Country", "Province") := tstrsplit(location, "-", fixed = TRUE)][]
zika$report_date <-as.Date(zika$report_date, "%m/%d/%Y")

zika$Year <- as.numeric(format(zika$report_date, format = "%Y"))


zika %>%
  filter(!is.na(Year)) %>%
  group_by(Country, Year) %>%
  summarise(n = n())-> ZikaOc

names(ZikaOc)[3] <- "cases"

ggplot(ZikaOc, aes(x= Country, y = cases)) +
  geom_bar(stat="identity") +
  coord_flip()+
  facet_wrap(~Year)

data(countryExData,envir=environment(),package="rworldmap")
str(countryExData)
Test <- merge(countryExData, ZikaOc, by = "Country")
sPDF <- joinCountryData2Map(Test, joinCode = "ISO3", nameJoinColumn = "ISO3V10")
mapDevice() #create world map shaped window
mapCountryData(sPDF, nameColumnToPlot='cases')


USA <- zika[grep("United_States", zika$location),]
Mexico <- zika[grep("Mexico", zika$location),]
Panama <- zika[grep("Panama", zika$location),]
Nicaragua <- zika[grep("Nicaragua", zika$location),]
Haiti <- zika[grep("Haiti", zika$location),]
Guatemala <- zika[grep("Guatemala", zika$location),]
El_salvador <- zika[grep("El_Salvador", zika$location),]
Ecuador <- zika[grep("Ecuador", zika$location),]
Dominican_republic <- zika[grep("Dominican_Republic", zika$location),]
Colombia <- zika[grep("Colombia", zika$location),]
Argentina <- zika[grep("Argentina", zika$location),]
Brazil <- zika[grep("Brazil", zika$location),]

USA %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_USA

g1 <- ggplot(g_USA, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'USA: Reported Zika cases', 
       x = 'cases types')

Mexico %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Mexico

g2 <-ggplot(g_Mexico, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Mexico: Reported Zika cases', 
       x = 'cases types')

Panama %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Panama

g3 <-ggplot(g_Panama, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Panama: Reported Zika cases', 
       x = 'cases types')

Nicaragua %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Nicaragua

g4 <-ggplot(g_Nicaragua, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Nicaragua: Reported Zika cases', 
       x = 'cases types')

Haiti %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Haiti

g5 <-ggplot(g_Haiti, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Haiti: Reported Zika cases', 
       x = 'cases types')

Guatemala %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Guatemala

g6 <-ggplot(g_Guatemala, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Guatemala: Reported Zika cases', 
       x = 'cases types')

El_salvador %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_El_salvador

g7 <-ggplot(g_El_salvador, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'El_salvador: Reported Zika cases', 
       x = 'cases types')

Ecuador %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Ecuador

g8 <-ggplot(g_Ecuador, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'g_Ecuador: Reported Zika cases', 
       x = 'cases types')

Dominican_republic %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Dominican_republic

g9 <-ggplot(g_Dominican_republic, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Dominican_republic: Reported Zika cases', 
       x = 'cases types')

Colombia %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Colombia

g10 <-ggplot(g_Colombia, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Colombia: Reported Zika cases', 
       x = 'cases types')

Argentina %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Argentina

g11 <-ggplot(g_Argentina, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Argentina: Reported Zika cases', 
       x = 'cases types')

Brazil %>% 
  group_by(data_field, Year) %>% 
  summarise(Cases = n()) -> g_Brazil

g12 <-ggplot(g_Brazil, aes(x = data_field, y = Cases)) +
  geom_bar(stat = 'identity',colour = 'white') +
  facet_wrap(~ Year) +
  scale_fill_hue() +
  coord_flip() +
  labs(y = 'Brazil: Reported Zika cases', 
       x = 'cases types')

g1 # Frome the cases in USA, most zika cases was reported from travel and local. Local cases mainly were reported from Puerto Rico, New York, Florida, and Virgin Island. 
g2
g3
g4
g5
g6
g7
g8
g9
g10
g11
g12

