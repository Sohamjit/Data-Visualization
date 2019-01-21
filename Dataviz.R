# This project was done from data visualization perspective only. 
# As the purpose of the code was not to do any modelling, no efforts were made on cleaning or replacing missing data

# Group O-1-3 Assignment No.1
# Members:
# - Celine Khoury
# - Elias Azzi
# - Jiachao Pan
# - Sohamjit Mukherjee
# - Wieland Klingspor
# - Sebastian Vasquez

# SELECT AND SAVE YOUR DATA #

Famous_People <- read.csv("https://gist.githubusercontent.com/Brian23ldu/4e2b0543760c167294acb7b5805e3623/raw/849dd0f4abc10cd9c069391693035b5f58658ad9/History_Famous_People.csv", header=TRUE)

# Data Exploratory Analysis
head(Famous_People)
summary(Famous_People)
str(Famous_People)

# Data feature engineering
Famous_People <- Famous_People[order(Famous_People$Birthyear),]

# SIMPLE CHARTS #
if(!"ggplot2" %in% installed.packages()){
  install.packages("ggplot2")} 
if(!"ggthemes" %in% installed.packages()){
  install.packages("ggthemes")}
if(!"ggExtra" %in% installed.packages()){
  install.packages("ggExtra")}
if(!"psych" %in% installed.packages()){
  install.packages("psych")}
if(!"reshape2" %in% installed.packages()){
  install.packages("reshape2")}
if(!"waffle" %in% installed.packages()){
  install.packages("waffle")}
if(!"data.table" %in% installed.packages()){
  install.packages("data.table")}
if(!"ggalt" %in% installed.packages()){
  install.packages("ggalt")}
if(!"dplyr" %in% installed.packages()){
  install.packages("dplyr")}
if(!"tidyr" %in% installed.packages()){
  install.packages("tidyr")}
if(!"plotly" %in% installed.packages()){
  install.packages("plotly")}
if(!"scales" %in% installed.packages()){
  install.packages("scales")}
if(!"leaflet" %in% installed.packages()){
  install.packages("leaflet")}
if(!"tufte" %in% installed.packages()){
  install.packages("tufte")}
# will put the install and call libraries into if statement
library(ggplot2)
library(ggthemes)
library(ggExtra)
library(psych)
library(reshape2)
library(waffle)
library(data.table)
library(ggalt) 
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(leaflet)
library(tufte)

#1. Do a Scatter Plot
ggplot(Famous_People, aes(x=HPI, y=L.star, color=Domain)) + 
  geom_point(size=0.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        title =element_text(size=15))+
  ggtitle( "Famous People HPI vs. L.star")

#2. Do small multiples plots
ggplot(Famous_People, aes(x=HPI, y=L.star, color=Domain)) + 
  scale_colour_brewer(palette = "Set2") +
  geom_point(size=0.4) +
  facet_wrap( ~ Domain, ncol=4) +
  theme_tufte(base_family = "Arial") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        title =element_text(size=15),
        legend.position="None") +
  ggtitle( "Famous People HPI vs. L.star")

#3. Do a density chart plot
ggplot(Famous_People, aes(Domain)) +
  geom_density(aes(fill=Domain), alpha=0.4, color='transparent') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        title =element_text(size=15)) +
  ggtitle( "Famous People distribution") + 
  scale_fill_brewer(palette="Set2")

# TUFTE CHARTS #

# Dataset subsetting

df <- Famous_People[Famous_People$Birthyear>=1950,]

# 1. Do a Tufte box plot
ggplot(df, aes(factor(Birthyear),HPI)) + 
  theme_tufte(ticks=F) +
  geom_tufteboxplot(outlier.colour="transparent", color= "dodgerblue3") + 
  theme(axis.title = element_blank(),
        axis.text = element_blank())+
  annotate("text", x = 49, y = 25, adj=1,  family="Arial",
           label = c("Famous People HPI\n1950 - 2000"),
           size = 8.5)

# 2. Do small multiples Tufte minimal line plot

# Subseting the dataframe and calculating the mean HPI by Birthyear and Domain
df <- Famous_People %>% group_by(Birthyear, Domain) %>% 
  summarise(median_HPI =median(HPI)) 
head(df)  

# Subseting the dataframe and calculating the mean HPI by Birthyear for all Domains
# in order to use it as a Reference Level and compare it with each Domain
df_1 <- Famous_People %>% group_by(Birthyear) %>% 
  summarise(median_HPI = median(HPI)) 
head(df_1) 

# Subseting the dataframes for the same time frame
df <- df[df$Birthyear>=1950,]
df_1 <- df_1[df_1$Birthyear>=1950,]

# Ploting the small multiples of the historical trend of mean HPI by Domain 
# compared to the whole dataset mean HPI
ggplot(df, aes(x=Birthyear, y=median_HPI, color = Domain)) + 
  scale_colour_brewer(palette = "Set2") +
  geom_line(size=0.6) +
  facet_wrap( ~ Domain, ncol=4) +
  theme_tufte(ticks = FALSE, base_size = 10, base_family = "Arial") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15)) + 
  geom_line(data = df_1, colour='gray90')+
  theme(legend.position="None") +
  labs(title = "Famous people median HPI from 1950 - 2000")

# 3. Histogram Scatter Plot
p <- ggplot(Famous_People, aes(x=HPI, y=L.star)) + 
  geom_point(size = 0.5, color = "dodgerblue3") + 
  geom_smooth(alpha = 0.1, color = "gray73") +
  theme_tufte(ticks=F)+ 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=-1),
        title =element_text(size=15, family = "Arial")) +
  ggtitle( "Famous People HPI vs. L.star")

ggMarginal(p, type = "histogram", fill="transparent", color = "dodgerblue3")

# WAFFLE CHARTS #

# convert dataframe to datatable
FP_dt <- data.table(Famous_People)

# for each continent, create one waffle chart
# prepare dataset for filtering each continent
summary(FP_dt[,Continent.Name])

#  subset with respect to continent
FP_Africa <- FP_dt[Continent.Name=='Africa']
FP_Asia <- FP_dt[Continent.Name=='Asia']
FP_Europe <- FP_dt[Continent.Name=='Europe']
FP_NA <- FP_dt[Continent.Name=='North America']
FP_Oceania <- FP_dt[Continent.Name=='Oceania']
FP_SA <- FP_dt[Continent.Name=='South America']

# function for counting numbers with respect to gender
count_gender <- function(dt){
  counts <- c(`Female`=nrow(dt[Gender=='Female']),
              `Male`=nrow(dt[Gender=='Male']))
  return(counts)
}

# apply function
count_Africa <- count_gender(FP_Africa)
count_Asia <- count_gender(FP_Asia)
count_Europe <- count_gender(FP_Europe)
count_NA <- count_gender(FP_NA)
count_Oceania <- count_gender(FP_Oceania)
count_SA <- count_gender(FP_SA)

# this summarize all the info for each continent, then we can decide how to manipulate
# the numbers of each continent to make the waffle charts looks organized
sum(count_Africa)
sum(count_Asia)
sum(count_Europe)
sum(count_NA)
sum(count_Oceania)
sum(count_SA)
# create waffle
waffle(
  count_Africa*100/334, rows = 3, size = 1,
  colors = c("pink2","lightskyblue2"),
  title = "Gender gap of famous people among continents",
  legend_pos = "none",
  xlab = "Africa"
) -> Africa

waffle(
  count_Asia*100/871, rows = 3, size = 1,
  colors = c("pink2","lightskyblue2"),
  title = NULL,
  legend_pos = "none",
  xlab = "Aisa"
) -> Asia

waffle(
  count_Europe*100/5243, rows = 3, size = 1,
  colors = c("pink2","lightskyblue2"),
  title = NULL,
  legend_pos = "none",
  xlab = "Europe"
) -> Europe

waffle(
  count_NA*100/2269, rows = 3, size = 1,
  colors = c("pink2","lightskyblue2"),
  title = NULL,
  legend_pos = "none",
  xlab = "North America"
) -> NorthAmerica

waffle(
  count_Oceania*100/106, rows = 3, size = 1,
  colors = c("pink2","lightskyblue2"),
  title = NULL,
  legend_pos = "none",
  xlab = "Oceania"
) -> Oceania

waffle(
  count_SA*100/300, rows = 3, size = 1,
  colors = c("pink2","lightskyblue2"),
  title = NULL,
  legend_pos = "none",
  xlab = "South America"
) -> SouthAmerica
# call plots
iron(Africa, SouthAmerica, Europe, Asia, NorthAmerica, Oceania)

# DUMBBELL CHART #
# prepare dataset
FP_dt_F <- FP_dt[Gender=='Female']
FP_dt_M <- FP_dt[Gender=='Male']

# check for the domains
summary(FP_dt[,Domain])


domain_type <- c('ARTS','BUSINESS & LAW','EXPLORATION','HUMANITIES',
                 'INSTITUTIONS','PUBLIC FIGURE','SCIENCE & TECHNOLOGY','SPORTS')

# create a function to count for different cats
count_domain <- function(dt){
  counts <- c(`ARTS`=nrow(dt[Domain=='ARTS']),
              `BUSINESS & LAW`=nrow(dt[Domain=='BUSINESS & LAW']),
              `EXPLORATION`=nrow(dt[Domain=='EXPLORATION']),
              `HUMANITIES`=nrow(dt[Domain=='HUMANITIES']),
              `INSTITUTIONS`=nrow(dt[Domain=='INSTITUTIONS']),
              `PUBLIC FIGURE`=nrow(dt[Domain=='PUBLIC FIGURE']),
              `SCIENCE & TECHNOLOGY`=nrow(dt[Domain=='SCIENCE & TECHNOLOGY']),
              `SPORTS`=nrow(dt[Domain=='SPORTS']))
  return(counts)
}

# create datatable FP_count for counting the famous people
FP_count <- data.table(
  domain=domain_type,
  count_female=count_domain(FP_dt_F),
  count_male=count_domain(FP_dt_M)
)

FP_count <- FP_count[order(rank(domain))]

dumbbell <- ggplot(FP_count, aes(x=count_female, xend=count_male, y=domain, group=domain)) +
  geom_dumbbell(colour_x="pink2", size_x=4, colour_xend="lightskyblue2", size_xend = 4, colour='grey60', size=0.6,
                dot_guide=TRUE, dot_guide_size=0.5, dot_guide_colour = "grey80") +
  theme_tufte() +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=-1),
        title =element_text(size=15))


dumbbell + ggtitle('Gender gap in number of famous people') 


## Interactive Map


map <- leaflet(Famous_People) %>%
  addProviderTiles("Esri") %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng=~LON, lat=~LAT, 
                   popup = ~paste(
                     "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                     "<br>","<b>Country : </b>",Country.Name ,
                     "<br>","<b>Occupation : </b>",Occupation ,
                     "<br>","<b>HPI  : </b>", HPI, sep = " "),
                   radius = ~(Average.Views)/100000)

map

## Interactive ScatterPlot

Famous_People <- as.data.table(Famous_People)

Famous_People_Filtered = as.data.table(Famous_People[Birthyear>1800,] %>%
                                         group_by(Gender, Birthyear) %>%
                                         summarise(MeanPageView=mean(Total.Page.Views)
                                         ))
# Converting to nearest decade
Famous_People_Filtered[, BirthyearInDecade := round(Famous_People_Filtered[,Birthyear]/10)*10]

Famous_People_Filtered = as.data.table(Famous_People_Filtered %>%
                                         group_by(Gender, BirthyearInDecade) %>%
                                         summarise(MeanPageViewsByDecade=mean(MeanPageView)
                                         ))

Plot <- ggplot(data=Famous_People_Filtered, 
               aes(y=MeanPageViewsByDecade, x= Gender, color= Gender))+
  geom_point(aes(label = Gender
                 ,size = 5 
                 ,frame = BirthyearInDecade))+
  theme_classic() +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_colour_manual(values =c("pink2","lightskyblue2")) +
  ggtitle("Average Page Views Behaviour") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x= NULL, y = NULL)


ggplotly(Plot) %>%  animation_opts(2000)