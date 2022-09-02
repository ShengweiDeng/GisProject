##covid-19 vaccinations in London
#The UK Government want to ensure as many people are vaccinated for COVID-19 as possible.
#You have been tasked as a consultant to investigate what spatial factors might influence 
#low vaccine uptake in London in particular.

library(tmap)
library(sp)
library(rgdal)
library(mapview)

library(mlbench)
# projection
proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "
projection <- CRS("+proj=longlat +datum=WGS84 +no_defs")

library(dplyr)
### covid-19 cases data
setwd('C:/Users/suechan/Desktop/part_time/data_analysis/220902VACC')
cases <- read.csv('msoa_co_case_data.csv')
# group by borough
casesbyg = cases%>%group_by(UtlaName,date) %>%
  summarise(CasesDateChange = sum(newCasesDateChange),
            CasesDateRollingRate = mean(newCasesDateRollingRate),
            CasesDateRollingSum = sum(newCasesDateRollingSum))

casesbyg$date <- as.Date(casesbyg$date)
casesbyg$month <- format(casesbyg$date, '%Y-%m')
casesbyg$year <- format(casesbyg$date, '%Y')
# group by month
casesbygbym = casesbyg%>%group_by(UtlaName,month) %>%
  summarise(CasesDateChange = sum(CasesDateChange),
            CasesDateRollingRate = mean(CasesDateRollingRate),
            CasesDateRollingSum = sum(CasesDateRollingSum))
# group by year
casesbygbyy = casesbyg%>%group_by(UtlaName,year) %>%
  summarise(CasesDateChange = mean(CasesDateChange),
            CasesDateRollingRate = mean(CasesDateRollingRate),
            CasesDateRollingSum = mean(CasesDateRollingSum))

### covid-19 vaccination datasets
vacc <- read.csv('msoa_co_vaccination_data.csv')
# group by borough
vaccbyg = vacc%>%group_by(areaName,date) %>%
  summarise(new1st = sum(newPeopleVaccinatedFirstDoseByVaccinationDate),
            new2nd = sum(newPeopleVaccinatedSecondDoseByVaccinationDate),
            new3rd = sum(cumPeopleVaccinatedThirdInjectionByVaccinationDate))

vaccbyg$date<-as.Date(vaccbyg$date)
vaccbyg$month <- format(vaccbyg$date, '%Y-%m')
vaccbyg$year <- format(vaccbyg$date, '%Y')
# group by month
vaccbygbym = vaccbyg%>%group_by(areaName,month) %>%
  summarise(new1stsum = sum(new1st),
            new2ndsum = sum(new2nd),
            new3rdsum = sum(new3rd))
# group by year
vaccbygbyy = vaccbyg%>%group_by(areaName,year) %>%
  summarise(new1stmean = mean(new1st),
            new2ndmean = mean(new2nd),
            new3rdmean = mean(new3rd))

# Seperate to years file
case2020 <- casesbygbyy[which(casesbygbyy$year == 2020), ]
case2021 <- casesbygbyy[which(casesbygbyy$year == 2021), ]
case2022 <- casesbygbyy[which(casesbygbyy$year == 2022), ]
vacc2020 <- vaccbygbyy[which(vaccbygbyy$year == 2020), ]
vacc2021 <- vaccbygbyy[which(vaccbygbyy$year == 2021), ]
vacc2022 <- vaccbygbyy[which(vaccbygbyy$year == 2022), ]

C2020.df<-merge(case2020,vacc2020, by.x= 'UtlaName', by.y = 'areaName', sort=TRUE)
C2021.df<-merge(case2021,vacc2021, by.x= 'UtlaName', by.y = 'areaName', sort=TRUE)
C2022.df<-merge(case2022,vacc2022, by.x= 'UtlaName', by.y = 'areaName', sort=TRUE)


### parameters
gwrdata <- read.csv('GWRdata.csv')


# shape files
shape <- readOGR('statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged_disov.shp')
shape.df<-shape@data
shape.df <-shape.df[,c('LB_GSS_CD','BOROUGH')]

# merge covid data(2021\2022, for example)
library(crosstalk)
library(sf)
library(spdep)
library(spData)
library(carData)
library(car)
library(plyr)
library(ggplot2)
library(ggthemes)

library(precrec)
paras.df<-merge(shape.df,C2022.df, by.x= 'BOROUGH', by.y = 'UtlaName', sort=TRUE)
row.names(paras.df)= as.numeric(row.names(paras.df))-1
shape1<-SpatialPolygonsDataFrame(shape,paras.df)

shape.x<-shape1@data
shape.xs<-data.frame(shape1,id.1=seq(0:(length(shape.x[,1])-1) ) )
shape.df <- fortify(shape1)

shape.df$id.1=shape.df$id
shape.df2=shape.df[,-6]
shape_mapdata<-join(shape.df2, shape.xs, type = "full")


library(RColorBrewer)
library(pracma)
#####covid datasets#####
page <- ggplot(shape_mapdata, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(x = long, y = lat, group = group, fill= new3rdmean))+
  labs(title="average numbers in one month of new People Vaccinated Third Dose in 2022")+
  scale_fill_steps(low = "#FFFFBB", high = "#00BBFF")+
  theme_bw()+
  theme(legend.position=c(0.9,0.23),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.background=element_rect(I(0),linetype=1),
        legend.spacing.y = unit(0.0, 'cm'),
        legend.spacing.x = unit(0.0, 'cm'),
        legend.key.height=unit(0.5, 'cm'),
        legend.key.width=unit(0.7, 'cm'),
        legend.key.size=unit(1, 'cm'),
        legend.text=element_text(size = 13),
        axis.text.x =element_text(size=11), axis.text.y=element_text(size=11),
        axis.title.x =element_text(size=13),axis.title.y =element_text(size=13))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name = "Longitude")
  
page

ggsave(
  filename = "2022_3.png", 
  width = 9.5,             
  height = 7,            
  units = "in",          
  dpi = 600              
)


##GWR
library(spgwr)
gwrdata.df <- merge(gwrdata,shape_mapdata, by.x= 'BOROUGH', by.y = 'BOROUGH', sort=TRUE)
gwrdata.df <- subset(gwrdata.df,!is.na(long))
#coordinates(gwrdata.df) <- ~long+lat
#proj4string(gwrdata.df) <- CRS("+init=epsg:27700")
#st_crs(gwrdata.df)$proj4string

#gwrdata.df<-merge(gwrdata,C2021.df, by.x= 'BOROUGH', by.y = 'UtlaName', sort=TRUE)
#write.table(gwrdata.df,"gwrdataplus.csv",row.names=FALSE,col.names=TRUE,sep=",")

form <- new1stmean ~ Area+Popdensity+DEM+DistancetoLondon+EconomicallyInactivePercent+IMDAveragescore

# run linear model
model <- lm(form, data = gwrdata.df)
summary(model)
par(mfrow=c(2,2))
plot(model)

# run gwr model
library(spgwr)
bw <- gwr.sel(formula = form, data = gwrdata.df, coords=cbind(gwrdata.df$long,gwrdata.df$lat),
              gweight = gwr.Gauss,
              method = "cv")

gwr_result <- gwr(formula = form,
                  data = gwrdata.df, bandwidth = bw, coords=cbind(gwrdata.df$long,gwrdata.df$lat),
                  gweight = gwr.Gauss, hatmatrix = TRUE)

