library(sf)
library(fmsb)
library(tmap)
library(plotly)
library(maps)
library(tidyr)
library(dplyr)
library(readxl)
library(plotly)
library(rgdal)
library(GGally)
library(mapproj)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(RColorBrewer)


coffee <- read_excel("coffee.xls")
coffee <- na.omit(coffee)

coffee<- coffee %>%
  mutate(height = case_when(altitude_mean_meters < 5000 ~ altitude_mean_meters))

#Plot 1
myplot<-ggplot(data=coffee, aes(x=height, y=Total.Cup.Points))
myplot<-myplot+geom_point(aes(colour=Clean.Cup, size=Cupper.Points),alpha=0.7)+
  labs(x = 'Height in m',
       y = "Total.Cup.Points units",
       title = "Plot of Height v/s Cupper Points ")
myplot


#Plot2
id_count <- aggregate(Serial ~ Country.of.Origin, data=coffee, FUN=length)
names(id_count) <- c("Country.of.Origin", "Country_Count")
head(id_count)
countries <- c(id_count$Country.of.Origin)
number <- (id_count$Country_Count)
cplot <- data.frame(countries,number)
origin_plot <- plot_ly(
  type = "choropleth",
  locations =cplot$countries,
  locationmode = "country names",
  z = cplot$number)%>%layout(title = 'Number of Coffee Plantations', plot_bgcolor = "#e5ecf6")
origin_plot


#Plot 3
table(coffee['Processing.Method'])
data <- data.frame(
  category=c("Semi-washed / Semi-pulped","Natural / Dry","Pulped natural","Washed / Wet","Other"),
  count=c(52,177,9,732,25))

data$fraction <- data$count / sum(data$count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$category, " :\n ", data$count)

#Donut plot for processing Method
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=3, xmin=4, fill=category)) +
  geom_rect() +
  geom_label( x=3.8, aes(y=labelPosition, label=label), size=2.8) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2.2, 4)) +
  theme_void() +
  theme(legend.position = "left")

#Scatter Plot to visualize defects per processing method
myplot<-ggplot(data=coffee, aes(x=height, y=Quakers))
myplot<-myplot+geom_point(aes(colour=Processing.Method, size=Category.Two.Defects),alpha=0.7)+
  labs(x = 'Height in m',
       y = "Defects found",
       title = "Plot of Height v/s Defects ")
myplot


#Plot 4
#Plot for correlating factors to look for
data1 <- subset(coffee, select = c("Balance", "Uniformity","Aroma","Flavor","Aftertaste","Acidity","Sweetness","Clean.Cup","Body"))
ggcorr(data1, method = c("everything", "pearson"))+ ggplot2::labs(title = "Correlation Between Attributes")

#Plot 5.1
#Plot for radar chart of each bean specie
arabica <- read_excel("arabica.xls")
arabica_values <- subset(arabica, select = 
                           c("Balance", "Uniformity","Aroma","Flavor","Aftertaste","Sweetness","Acidity","Clean.Cup"))

a_poly <- as.data.frame(matrix( colMeans(arabica_values) , ncol=8))
colnames(a_poly) <- c("Balance", "Uniformity","Aroma","Flavor","Aftertaste","Acidity","Sweetness","Clean.Cup")
# To use the fmsb package, I have to add 2 lines to the data frame: the max and min of each topic to show on the plot 
#PLEASE RERUN FROM THE A_POLY DF IN THE LINE ABOVE EACH TIME YOU WANT TO GENERATE THE RADAR PLOT 
a_poly<- rbind(rep(20,10) , rep(0,10) , a_poly)

radarchart(a_poly,  axistype=1.5 , 
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
           cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
           title = "Arabica Flavour Profile",
           vlcex=1)

#Plot 5.2
robusta <- read_excel("robusta.xls")
robusta_values <- subset(robusta, 
                         select = c("Balance", "Uniform.Cup","Fragrance...Aroma","Flavor","Aftertaste","Bitter...Sweet","Salt...Acid","Clean.Cup"))

r_poly <- as.data.frame(matrix( colMeans(robusta_values) , ncol=8))
#PLEASE RERUN FROM THE R_POLY DF IN THE LINE ABOVE EACH TIME YOU WANT TO GENERATE THE RADAR PLOT 
colnames(r_poly) <- c("Balance", "Uniformity","Aroma","Flavor","Aftertaste","Acidity","Sweetness","Clean.Cup")
r_poly<- rbind(rep(20,10) , rep(0,10) , r_poly)
radarchart(r_poly,  axistype=1.5 , 
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
           cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
           title="Robusta Flavour Profile",
           vlcex=1)


