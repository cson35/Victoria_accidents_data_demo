setwd("C:/Users/canso/Downloads/VIC_accident_demo")

########################## Read Data and Check data structure ###################
require('readxl')
data = read_excel('Aggregated-TAC-Hospitalisation-Stats.xlsx', 2)
col = colnames(data)
str(data)  # 392 records, 46 variables
length(unique(data$LGA)) # 79 unique LGA


########################## Data Wrangling #######################################
# missing values
sum(is.na(data))

# delete unneccessary columns
colSums(data[, -c(1, 2, 3)]) 
# the sum of ageUnknown and locUnknown equals to zero, which mean these two columns has no valuable info, so delete
data <- data[ , -which(col %in% c("locUnknown","ageUnknown"))]

# check the start date and end date of the data
unique(data$dateFrom) # start 2011-07-01
unique(data$dateTo) # end 2016-06-30

# check if all LGA have full 5-year-record
table(data$LGA) # only yarriambiack has 4-year-data, Queenscliffe has 3-year
data[data$LGA == 'Queenscliffe', ] # missing 14-15, 15-16
data[data$LGA == 'Yarriambiack', ] # missing 13-14
# the two LGA without completed records only have few accicentds happened in the past in total, treat missing years
# as no accicdent happened

# check if each LGA has unique location (Melbourne or rural)
temp1 <- aggregate(.~ LGA, data[, c('LGA', 'locMelbourne', 'locRural')], sum)
temp1[(temp1$locMelbourne != 0) & (temp1$locRural != 0), ] # unique location
locDic <- vector()
# identify each LGA is in Melbourne or rural
for (i in c(1:nrow(temp1))){
  if (temp1[i, 'locMelbourne'] == 0){
    locDic[[temp1[i, 'LGA']]] = 'Rural'
  }
  else {locDic[[temp1[i, 'LGA']]] = 'Melbourne'}
}

# check if the total number of accidents in different categories mathed
# category: hopital stay days, gender, age, road user type, location, accident type, week day, time
all((data$stayLessEq14 + data$stayGreater14 == data$male + data$female + data$unknownGender) == TRUE)
all((data$stayLessEq14 + data$stayGreater14 == data$age0to17 + data$age18to25 + data$age26to39 + data$age40to59 + data$age60AndOver) == TRUE)
all((data$stayLessEq14 + data$stayGreater14 == data$userBicyclist + data$userDriver + data$userMotorcyclist + data$userPassenger + data$userPedestrian + data$userUnknown) == TRUE)
all((data$stayLessEq14 + data$stayGreater14 == data$locMelbourne + data$locRural) == TRUE)
all((data$stayLessEq14 + data$stayGreater14 == data$crashAdjacentDir + data$crashManoeuvring + data$crashOffCurve +
       data$crashOffStraight + data$crashOnRoad + data$crashOppDir + data$crashOvertake + data$crashPassengerMisc +
       data$crashPedestrian +data$crashSameDir + data$crashUnknown) == TRUE)
all((data$stayLessEq14 + data$stayGreater14 == data$dayMon + data$dayTue + data$dayWed + data$dayThu +
       data$dayFri + data$daySat + data$daySun) == TRUE)
all((data$stayLessEq14 + data$stayGreater14 == data$hr0000to0559 + data$hr0600to1159 + data$hr1200to1759 +data$hr1800to2359 + data$hrUnknown) == TRUE)

# add total amoint of accidents for each year and LGA to the dataframe
data$total <- data$stayLessEq14 + data$stayGreater14


############################# Data visulization ###############################
# mapping
require(leaflet)
require(beepr)
require(maptools)
require(stringr)

mapping <- aggregate(.~LGA, data[, c('LGA', 'total')], sum)
mapping$LGA <- toupper(mapping$LGA)
mapping$LGA <- str_replace(mapping$LGA, "-", " ")

# read shapedile from open source provideing geograpgical info for LGA
gisPath <- file.path("VIC_LGA_POLYGON_shp")
shapeFile <- file.path(gisPath, "VIC_LGA_POLYGON_shp.shp")
sF <- readShapeSpatial(shapeFile)
class(sF)

myDataFromGIS <- sF@data
head(myDataFromGIS)

#calculate the centroids
polys <- as(sF, "SpatialPolygons")
class(polys) # should be SpatialPolygons
length(polys)
slotNames(polys)
Polygon(polys[1])

library(dplyr)
library(purrr)

centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
head(centroids)

myDataFromGIS <- data.frame(myDataFromGIS, centroids)
myDataFromGIS <- myDataFromGIS[, c('VIC_LGA__3', 'long_c', 'lat_c')]
colnames(myDataFromGIS) <- c('LGA', 'longitude', 'latitude')

mapping <- merge(mapping, myDataFromGIS, by = 'LGA', all.x=TRUE)
mapping <- aggregate(.~LGA, mapping, mean)

m <- leaflet(mapping) %>% addTiles() 
m %>% addCircleMarkers(~longitude,
                       ~latitude, 
                       radius = ~total/50, 
                       popup = ~LGA)
# mapping - LGA in Melbourne has more accidents


# check the top ten danergrous LGA
topten <- mapping[order(mapping$total, decreasing = TRUE),][1:10,]
topten <- topten[order(topten$total, decreasing = FALSE),]

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(topten[, 2],names.arg = topten[, 1], horiz=TRUE, main="Top Ten Dangerous LGA", las =1, cex.names=0.7,xlim = c(0, 1500))

names(locDic) <- toupper(names(locDic))
for (i in topten$LGA){
  print (paste(i, locDic[[i]]))
}
# Among the top ten dangerous LGA, only GREATER GEELONG is in rural
sum(topten$total)/sum(data$total) # top ten take account of 34% of total amount of accidents


# distribution for every category 
# hopital stay days, gender, age, road user type, location, accident type, week day, time

# check the distribution for hospital staying days
temp2 <- colSums(data[, c("stayLessEq14", "stayGreater14")])
temp2 <- temp2[order(unname(temp2), decreasing = TRUE)]
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp2,names.arg = names(temp2), main="Hospital Stay Days", las =1, cex.names=0.7, ylim = c(0, 25000))

# check the distribution for gender
temp3 <- colSums(data[, c("male", "female", "unknownGender")])
temp3 <- temp3[order(unname(temp3), decreasing = TRUE)]
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp3,names.arg = names(temp3), main="Gender", las =1, cex.names=0.7, ylim = c(0, 17000))

# check the distribution for age
temp4 <- colSums(data[, c("age0to17", "age18to25", "age26to39", "age40to59", "age60AndOver")])
# temp4 <- temp4[order(unname(temp4), decreasing = TRUE)]
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp4,names.arg = names(temp4), main="AGE", las =1, cex.names=0.7, ylim = c(0, 8000))

# check the distrinution for road type
temp5 <- colSums(data[, c("userBicyclist" , "userDriver", "userMotorcyclist", "userPassenger", "userPedestrian", "userUnknown" )])
temp5 <- temp5[order(unname(temp5), decreasing = FALSE)]
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp5, horiz = TRUE, names.arg = names(temp5), main="Road Type", las =1, cex.names=0.7, xlim = c(0, 14000))

# check the distribution for location
temp6 <- colSums(data[, c("locMelbourne", "locRural")])
temp6 <- temp6[order(unname(temp6), decreasing = TRUE)]
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp6, names.arg = names(temp6), main="Location", las =1, cex.names=0.7, ylim = c(0, 21000))
table(unname(locDic))
# even in data, there are more rural area than Melbourne, but the total amount of acccidents is twice of it in rural area

# check the distribution for accidents type
temp7 <- colSums(data[, c("crashAdjacentDir", "crashManoeuvring", "crashOnRoad", "crashOppDir", 
                                   "crashOvertake", "crashPassengerMisc", "crashPedestrian", "crashOffStraight", 
                                   "crashOffCurve","crashSameDir","crashUnknown")])
temp7 <- temp7[order(unname(temp7), decreasing = FALSE)]
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp7,names.arg = names(temp7), horiz=TRUE, main="Crush Type", las =1, cex.names=0.7, xlim = c(0, 6000))

# check the distribution for week days
temp8 <- colSums(data[, c("dayMon", "dayTue", "dayWed", "dayThu", "dayFri", "daySat", "daySun")])
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp8,names.arg = names(temp8), main="Week Days", las =1, cex.names=0.7, ylim = c(0, 5000))

# check the day seasonility
temp9 <- colSums(data[, c("hr0000to0559", "hr0600to1159", "hr1200to1759", "hr1800to2359", "hrUnknown")])
par(las=2)
par(mar=c(5,8,4,2))
barplot(temp9,names.arg = names(temp9), main="Crush Time", las =1, cex.names=0.7, ylim = c(0, 12000))
# afternoon

# To look at the distribution in top ten dangerous LGA and compare it to general distribution
require(reshape2)
require(ggplot2)
toptenDis <- data[toupper(data$LGA) %in% topten$LGA, ]
toptenDis <- aggregate(. ~ LGA, toptenDis[, -c(2, 3)], sum)

















