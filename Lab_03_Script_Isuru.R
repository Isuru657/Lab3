crime <- read.csv(file.choose())
library(tidyverse)
library(ggplot2)
str(crime)
year <- format(as.Date(crime$Arrest.Date, format="%d/%m/%Y"), "%Y")
crime<- cbind(crime, year)
violent_crime <- c("Homicide", "Rape", "Robbery", "Aggravated Assault")
crime$Charge.Group.Description <- factor(crime$Charge.Group.Description, levels= c("Homicide", "Rape", "Robbery", "Aggravated Assault"))
unique(crime$Charge.Group.Description)
crime_new <- crime%>%
  filter(Charge.Group.Description%in%violent_crime)%>%
  filter(year=="2019")%>%
  select(Age, Sex.Code, Charge.Group.Description, Location, Arrest.Date)%>%
  mutate(Location=gsub(")", "", Location, fixed = TRUE))%>%
  mutate(Location=gsub("(", "", Location, fixed= TRUE))%>%
  mutate(Location=gsub(" ", "", Location, fixed= TRUE))%>%
  separate(Location, into=c("lat", "lon"), sep=",")%>%
  mutate(lat=as.numeric(lat),
         lon=as.numeric(lon),
         Charge.Group.Description=as.factor(Charge.Group.Description))
crime_new
unique(crime_new$Charge.Group.Description)
library(ggmap)
library(RgoogleMaps)
register_google(key="AIzaSyAxmkMWyBEURjGISi0XkX-r0qhZ9ZhuqB8 ")
map2 <-get_map(location="Los Angeles, CA", zoom=10, source="stamen", maptype= "toner-lite")

Age3 <- crime_new%>%
  mutate(Age2=cut(Age, breaks = c(0, 15, 25, 35, 45, 55, 65, 75, 85, 95),
                  labels= c("0-15", "15-25", "25-35", "35-45", "45-55", "55-65", "65-75", "75-85", "85-95")))%>%
  group_by(Sex.Code)%>%
  count(Age2)
Age3
Age3$n <- ifelse(Age3$Sex.Code=="M", -1*Age3$n, Age3$n)

df1 <- data.frame("Point"=c(1,2,3), "lon"= c(-118.43, -118.28, -118.43), "lat"= c(34, 34.05, 34.21))
library(dplyr)
library(tidyverse)
df2 <- df1%>%
  mutate(Point=as.factor(Point))
str(df2)
colors <- c("chocolate2", "limegreen", "navy", "hotpink")
p2 <- ggmap(map2)+
  stat_bin2d(
    aes(x = lon, y = lat,
        fill = Charge.Group.Description,
        color= Charge.Group.Description),
    size = .1, bins = 50, alpha = 0.4,
    data = crime_new)+
  geom_point(data=df2, aes(x=lon, y=lat), shape=17, color="black", fill="black", size=5)+
  labs(fill="Crime:", 
       color="Crime:",
       title="Violent crime in Los Angeles for 2019",
       subtitle = "Where does crime occur?",
       caption = "Marked points indicate highest concentrations")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.box= "horizontal",
        plot.title= element_text(face = "bold", size=13),        
        plot.subtitle = element_text(size=11),
        legend.text = element_text(size=7),
        legend.title = element_text(face="bold", size=10),
        plot.caption = element_text( color="darkgrey", size=7))+
        guides(fill= guide_legend(title.position="top", title.hjust = 0.5))
p2


Age3 <- Age3%>%
  arrange(desc(Sex.Code))
library(ggplot2)

colors <- c("indianred2", "mediumpurple1")
p1 <- ggplot(Age3, aes(x=Age2, y=n, fill=Sex.Code))+
  geom_bar(data=subset(Age3, Sex.Code=="M"), stat="identity", alpha=1)+
  geom_bar(data=subset(Age3, Sex.Code=="F"), stat="identity", alpha=1)+
  coord_flip()+
  scale_y_continuous(breaks=c(-500, -400, -300, -200, -100, 0, 100, 200, 300, 400, 500),
                     labels=c("500", "400", "300", "200", "100", "0", "100", "200", "300", "400", "500"))+
  expand_limits(y=c(-500, 350))+
  labs(fill="Gender:")+
  xlab("Age Group")+
  ylab("Number of criminals")+
  theme_light()+
  theme(axis.text = element_text( size = 7),
        axis.title = element_text( size=9),
        legend.position = "bottom",
        legend.box= "horizontal",
        legend.text = element_text( size=7),
        legend.title = element_text(face="bold", size=10),
        plot.subtitle = element_text(size=11),
        plot.caption = element_text(),
        panel.grid.minor = element_blank())+
  labs(title="           ",
    subtitle="Who commits the crimes?",
       caption= "Source: https://www.data.gov")+
  guides(fill= guide_legend(title.position="top", title.hjust = 0.5, reverse = TRUE))+
  scale_fill_manual(values = colors)
p1
library(gridExtra)
p5 <- grid.arrange(p2, p1, ncol=2)





