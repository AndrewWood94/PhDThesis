library(ggplot2)
library(stringr)
library(cowplot)
library(DescTools)
library(dplyr)
library(Hmisc)
library(sandwich)
library(lmtest)

path = '/Volumes/csce/inf/groups/ANC/Andrew/results/merged/mergedOSM.csv'
OSMData = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
path = '/Volumes/csce/inf/groups/ANC/Andrew/results/merged/mergedHikr.csv'
HikrData = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/merged/'
path=paste0(basepath,'CombinedData.csv')
AllData = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

AllDataDupFinder=AllData[order(AllData$layer),]
dupLayers=sort(unique(AllDataDupFinder$layer[duplicated(AllDataDupFinder[,c("WKT","Start_DateTime", "duration")])]))
AllDataDupes=AllData
AllData=AllData[!(AllData$layer %in% dupLayers),]

OSM_terrains = colnames(AllData[grepl('OSM',colnames(AllData))])
Paved=c("OSM_cycleway","OSM_footway","OSM_living_street",
        "OSM_motorway","OSM_motorway_link","OSM_pedestrian" ,
        "OSM_primary","OSM_primary_link","OSM_residential","OSM_secondary"  ,
        "OSM_secondary_link","OSM_service" ,"OSM_steps","OSM_tertiary"  , 
        "OSM_tertiary_link","OSM_trunk",
        "OSM_trunk_link","OSM_unclassified","OSM_unknown")
AllData$Road = rowSums(is.na(AllData[,grepl('OSM',colnames(AllData))]))!=27
AllData$Paved =rowSums(is.na(AllData[,Paved]))!=19
AllData$Obstruction=!is.na(AllData$avg_obstruction)
AllData$HeavyObstruction = AllData$avg_obstruction>0.1

AllDataReduced = AllData[AllData$speed>=quantile(AllData$speed,0.005)&AllData$speed<=quantile(AllData$speed,0.995),]
AllDataOnRoad = AllDataReduced[AllDataReduced$Road==TRUE,]
AllDataOffRoad = AllDataReduced[AllDataReduced$Road==FALSE,]
AllDataPaved = AllDataOnRoad[AllDataOnRoad$Paved==TRUE,]
AllDataUnpaved = AllDataOnRoad[AllDataOnRoad$Paved==FALSE,]
AllDataObstruction = AllDataOffRoad[AllDataOffRoad$Obstruction==TRUE,]
AllDataNoObstruction = AllDataOffRoad[AllDataOffRoad$Obstruction==FALSE,]

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/All/'
write.csv(AllData,paste0(basepath,'CombinedData.csv'), row.names = FALSE)
write.csv(AllDataReduced,paste0(basepath,'Combined99.csv'), row.names = FALSE)
write.csv(AllDataOnRoad,paste0(basepath,'OnRoad.csv'), row.names = FALSE)
write.csv(AllDataOffRoad,paste0(basepath,'OffRoad.csv'), row.names = FALSE)
write.csv(AllDataPaved,paste0(basepath,'Paved.csv'), row.names = FALSE)
write.csv(AllDataUnpaved,paste0(basepath,'Unpaved.csv'), row.names = FALSE)
write.csv(AllDataObstruction,paste0(basepath,'Obstruction.csv'), row.names = FALSE)
write.csv(AllDataNoObstruction,paste0(basepath,'NoObstruction.csv'), row.names = FALSE)

path = '/Volumes/LaCie/LaCie/LaCie Rugged USB-C/Scotland Data/Recreation/ScotlandLayerLookup.csv'
ScotlandLookup = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScotDataReduced=AllDataReduced[AllDataReduced$layer %in% ScotlandLookup$newlayer,]
ScotDataOnRoad = ScotDataReduced[ScotDataReduced$Road==TRUE,]
ScotDataOffRoad = ScotDataReduced[ScotDataReduced$Road==FALSE,]
ScotDataPaved = ScotDataOnRoad[ScotDataOnRoad$Paved==TRUE,]
ScotDataUnpaved = ScotDataOnRoad[ScotDataOnRoad$Paved==FALSE,]

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/Scotland/'
write.csv(ScotDataReduced,paste0(basepath,'Combined99.csv'), row.names = FALSE)
write.csv(ScotDataOnRoad,paste0(basepath,'OnRoad.csv'), row.names = FALSE)
write.csv(ScotDataOffRoad,paste0(basepath,'OffRoad.csv'), row.names = FALSE)
write.csv(ScotDataPaved,paste0(basepath,'Paved.csv'), row.names = FALSE)
write.csv(ScotDataUnpaved,paste0(basepath,'Unpaved.csv'), row.names = FALSE)

ROUKDataReduced=AllDataReduced[!(AllDataReduced$layer %in% ScotlandLookup$newlayer),]
ROUKDataOnRoad = ROUKDataReduced[ROUKDataReduced$Road==TRUE,]
ROUKDataOffRoad = ROUKDataReduced[ROUKDataReduced$Road==FALSE,]
ROUKDataPaved = ROUKDataOnRoad[ROUKDataOnRoad$Paved==TRUE,]
ROUKDataUnpaved = ROUKDataOnRoad[ROUKDataOnRoad$Paved==FALSE,]
ROUKDataObstruction = ROUKDataOffRoad[ROUKDataOffRoad$Obstruction==TRUE,]
ROUKDataNoObstruction = ROUKDataOffRoad[ROUKDataOffRoad$Obstruction==FALSE,]

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/ROUK/'
write.csv(ROUKDataReduced,paste0(basepath,'Combined99.csv'), row.names = FALSE)
write.csv(ROUKDataOnRoad,paste0(basepath,'OnRoad.csv'), row.names = FALSE)
write.csv(ROUKDataOffRoad,paste0(basepath,'OffRoad.csv'), row.names = FALSE)
write.csv(ROUKDataPaved,paste0(basepath,'Paved.csv'), row.names = FALSE)
write.csv(ROUKDataUnpaved,paste0(basepath,'Unpaved.csv'), row.names = FALSE)
write.csv(ROUKDataObstruction,paste0(basepath,'Obstruction.csv'), row.names = FALSE)
write.csv(ROUKDataNoObstruction,paste0(basepath,'NoObstruction.csv'), row.names = FALSE)

#unique(ScotOffRoad$layer[ScotOffRoad$Obstruction==TRUE])
#path = '/Volumes/LaCie/LaCie/LaCie Rugged USB-C/Scotland Data/Recreation/Modelling/CombinedData.csv'
#OldData = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#names(OldData)[names(OldData) == 'avg_ground_slope_a'] <- 'avg_hill_slope'
#names(OldData)[names(OldData) == 'slopeOS'] <- 'avg_walking_slope'
#old_model = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=OldData, family=gaussian(link="log"))

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/All/'
AllData = read.csv(paste0(basepath,'CombinedData.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataReduced = read.csv(paste0(basepath,'Combined99.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataOnRoad = read.csv(paste0(basepath,'OnRoad.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataOffRoad = read.csv(paste0(basepath,'OffRoad.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataPaved = read.csv(paste0(basepath,'Paved.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataUnpaved = read.csv(paste0(basepath,'Unpaved.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataObstruction = read.csv(paste0(basepath,'Obstruction.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
AllDataNoObstruction = read.csv(paste0(basepath,'NoObstruction.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/Scotland/'
ScotDataReduced = read.csv(paste0(basepath,'Combined99.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScotDataOnRoad = read.csv(paste0(basepath,'OnRoad.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScotDataOffRoad = read.csv(paste0(basepath,'OffRoad.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScotDataPaved = read.csv(paste0(basepath,'Paved.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScotDataUnpaved = read.csv(paste0(basepath,'Unpaved.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/ROUK/'
ROUKDataReduced = read.csv(paste0(basepath,'Combined99.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ROUKDataOnRoad = read.csv(paste0(basepath,'OnRoad.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ROUKDataOffRoad = read.csv(paste0(basepath,'OffRoad.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ROUKDataPaved = read.csv(paste0(basepath,'Paved.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ROUKDataUnpaved = read.csv(paste0(basepath,'Unpaved.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ROUKDataObstruction = read.csv(paste0(basepath,'Obstruction.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ROUKDataNoObstruction = read.csv(paste0(basepath,'NoObstruction.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)


m1 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataReduced, family=gaussian(link='log'))
m2 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataOnRoad, family=gaussian(link='log'))
m3 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataOffRoad, family=gaussian(link='log'))
m4 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataUnpaved, family=gaussian(link='log'))
m5 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataPaved, family=gaussian(link='log'))

mx = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Road+I(Road*avg_hill_slope)+I(Road*avg_walking_slope)+I(Road*avg_walking_slope^2),data=AllDataReduced, family=gaussian(link='log'))
my = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Paved+I(Paved*avg_hill_slope)+I(Paved*avg_walking_slope)+I(Paved*avg_walking_slope^2),data=AllDataOnRoad, family=gaussian(link='log'))

m6 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataObstruction, family=gaussian(link='log'))
m7 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=AllDataNoObstruction, family=gaussian(link='log'))
mz = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Obstruction+I(Obstruction*avg_hill_slope)+I(Obstruction*avg_walking_slope)+I(Obstruction*avg_walking_slope^2),data=AllDataOffRoad, family=gaussian(link='log'))
mw = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+HeavyObstruction+I(HeavyObstruction*avg_hill_slope)+I(HeavyObstruction*avg_walking_slope)+I(HeavyObstruction*avg_walking_slope^2),data=AllDataObstruction, family=gaussian(link='log'))

mr1 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataReduced, family=gaussian(link='log'))
mrx = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Road+I(Road*avg_hill_slope)+I(Road*avg_walking_slope)+I(Road*avg_walking_slope^2),data=ROUKDataReduced, family=gaussian(link='log'))
ms1 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ScotDataReduced, family=gaussian(link='log'))
msx = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Road+I(Road*avg_hill_slope)+I(Road*avg_walking_slope)+I(Road*avg_walking_slope^2),data=ScotDataReduced, family=gaussian(link='log'))

mr2 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataOnRoad, family=gaussian(link='log'))
mr3 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataOffRoad, family=gaussian(link='log'))
mry = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Paved+I(Paved*avg_hill_slope)+I(Paved*avg_walking_slope)+I(Paved*avg_walking_slope^2),data=ROUKDataOnRoad, family=gaussian(link='log'))
ms2 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ScotDataOnRoad, family=gaussian(link='log'))
ms3 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ScotDataOffRoad, family=gaussian(link='log'))
msy = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+Paved+I(Paved*avg_hill_slope)+I(Paved*avg_walking_slope)+I(Paved*avg_walking_slope^2),data=ScotDataOnRoad, family=gaussian(link='log'))

mr4 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataPaved, family=gaussian(link='log'))
mr5 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataUnpaved, family=gaussian(link='log'))
ms4 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ScotDataPaved, family=gaussian(link='log'))
ms5 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ScotDataUnpaved, family=gaussian(link='log'))

mr6 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataObstruction, family=gaussian(link='log'))
mr7 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=ROUKDataNoObstruction, family=gaussian(link='log'))

mr8 = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+HeavyObstruction,data=ROUKDataObstruction, family=gaussian(link='log'))

coeftest(model, vcov = vcovCL, cluster = ~Track)
coeftest(msy, vcov = vcovCL, cluster = ~Track)
coefci(mr3, vcov = vcovCL,cluster = ~Track)
coefci(ms3, vcov = vcovCL,cluster = ~Track)


#Elevation vs Speed

plot(ROUKDataReduced$elevation, ROUKDataReduced$speed, pch='.', col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), xlim=c(0,1000), ylim=c(0,8))
means=vector()
a= seq(0,1000,50)
for (i in a){
  means= c(means,mean(ROUKDataOnRoad$speed[ROUKDataOnRoad$elevation>=i-50 & ROUKDataOnRoad$elevation<i+50]))
}
lines(a, means, col=2, pch='.', type='l', ylim=c(0,8))

plot(ScotDataReduced$elevation, ScotDataReduced$speed, pch='.', col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), xlim=c(0,1000), ylim=c(0,8))
means=vector()
a= seq(0,1000,50)
for (i in a){
  means= c(means,mean(ScotDataOffRoad$speed[ScotDataOffRoad$elevation>=i-50 & ScotDataOffRoad$elevation<i+50]))
}
lines(a, means, col=2, pch='.', type='l', ylim=c(0,8))

#c(bottom, left, top, right)
par(mar=c(5, 4, 1, 1) + 0.1)
hist(ROUKDataReduced$elevation, breaks = c(-10,seq(0,1400,50)), ylim=c(0,0.005), xlab = "Elevation (m)", main=NULL)
hist(ScotDataReduced$elevation, breaks = c(-10,seq(0,1400,50)), ylim=c(0,0.005), xlab = "Elevation (m)", main=NULL)

hist(ROUKDataOffRoad$elevation, breaks = c(-10,seq(0,1400,50)), ylim=c(0,0.005), xlab = "Elevation (m)", main=NULL)
hist(ScotDataOffRoad$elevation, breaks = c(-10,seq(0,1400,50)), ylim=c(0,0.005), xlab = "Elevation (m)", main=NULL)
par(mar=c(5, 4, 4, 2) + 0.1)

ScotDatasets=list(ScotDataReduced, ScotDataOnRoad, ScotDataOffRoad, ScotDataPaved, ScotDataUnpaved)
ROUKDatasets=list(ROUKDataReduced, ROUKDataOnRoad, ROUKDataOffRoad, ROUKDataPaved, ROUKDataUnpaved)
types = c("all", "on","off","paved","unpaved")
for (i in c(1:5)){  
  print(types[i])
  print(sum(ScotDatasets[[i]]$elevation>500)/nrow(ScotDatasets[[i]]))*100
  print(sum(ROUKDatasets[[i]]$elevation>500)/nrow(ROUKDatasets[[i]]))*100
}


#Obstruction vs Speed
a= split(ROUKDataObstruction, cut2(ROUKDataObstruction$avg_obstruction, g=25))
mean_speed = vector()
obstruction = vector()
for (i in c(1:length(a))){
  mean_speed = c(mean_speed,mean(a[[i]]$speed))
  obstruction=c(obstruction,mean(a[[i]]$avg_obstruction))
}

obst_data = data.frame(obstruction,mean_speed)
obst_graph = ggplot(obst_data, aes(obstruction,mean_speed)) + 
  xlab("Obstruction Height (m)") +
  ylab("Walking Speed (km/h)") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-0.05,1.05)) + 
  theme_classic() +
  geom_point()
obst_graph



terrains=sort(colnames(ROUKDataObstruction[!(grepl('OSM',colnames(ROUKDataObstruction)))])[c(14:42,45:47)])
to_ignore=vector()
for (i in terrains){
  results = nrow(ROUKDataObstruction) - sum(is.na(ROUKDataObstruction[[i]]))
  if (results <= 20 ){
    to_ignore = c(to_ignore, i)
  }
}
valid_terrains = terrains[!(terrains %in% to_ignore)]

ROUKDataObstruction$HeavyObstruction2=as.factor(ROUKDataObstruction$HeavyObstruction)
plotslist=list()
count=0
for (i in valid_terrains){
  count=count+1
  label = gsub('  ', ' - ',gsub('\\.', ' ',i))
  p=ggplot(data=ROUKDataObstruction[!(is.na(ROUKDataObstruction[[i]])),], aes(x=HeavyObstruction2)) +
    geom_bar() +
    theme_classic() +
    scale_x_discrete(labels=c("Light Obstruction","Heavy Obstruction"), drop=FALSE)+
    xlab(label) +
    ylab("Count")
  print(p)
  plotslist[[count]]=p
}

combinations=c()
for (i in c(1:nrow(ROUKDataObstruction))){
  types = which(!(is.na(ROUKDataObstruction[i,valid_terrains])))
  if (length(types)>0){
    combo = paste(valid_terrains[types], collapse=', ')
    if (combo %in% names(combinations)){
      combinations[combo]=combinations[combo]+1
    }
    else{
      combinations[combo] = 1
    }
  }
}
badlist=list()
for (i in names(combinations)){
  if(grepl("Nonconiferous.Trees,",i) | grepl("Coniferous.Trees,",i) | grepl("Orchard,",i) | grepl("Scrub,",i)){
    badlist=append(badlist,i)
    }
}
count = 0
for (i in badlist){
  if(grepl("Rough.Grassland,",i) | grepl("Agricultural.Land,",i) | grepl("Boulders,",i) | grepl("Heath,",i)){
    count=count+1
    }
}




#Walk length vs Speed

nameslist=c("layer", "total_time", "break_time", "walking_time","total_distance", "mean_speed")
subtotalsSegment = setNames(data.frame(matrix(ncol = length(nameslist), nrow = 0)), nameslist)

for (i in unique(AllDataReduced$layer)){
  data = AllDataReduced[AllDataReduced$layer==i,c("fid","layer","Start_DateTime","Date","duration","distance","speed")]
  a=max(as.POSIXct(data$Start_DateTime))
  b=min(as.POSIXct(data$Start_DateTime))
  id=max(data$fid)
  
  total_time = as.numeric(difftime(a,b,units="secs"))+data$duration[data$fid==id]
  break_time=total_time-sum(data$duration)
  total_distance = sum(data$distance)
  mean_speed = mean(data$speed)
  walking_time = total_time - break_time
  subtotals[nrow(subtotals) + 1,] = list(i, total_time,break_time,walking_time,total_distance,mean_speed)
}

basepath='/Volumes/csce/inf/groups/ANC/Andrew/results/mergedNoDuplicates/All/'
write.csv(subtotalsSegment,paste0(basepath,'subtotalsSegment.csv'), row.names = FALSE)
write.csv(subtotalsTrack,paste0(basepath,'subtotalsTrack'), row.names = FALSE)
subtotalsSegment = read.csv(paste0(basepath,'subtotalsSegment.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
subtotalsTrack = read.csv(paste0(basepath,'subtotalsTrack'), header = TRUE, sep = ",", stringsAsFactors = FALSE)


AllDataReduced$Date = as.Date(as.POSIXct(AllDataReduced$Start_DateTime),format="%Y-%m-%d")

nameslist=c("Track", "day", "segments","total_time", "break_time", "walking_time","total_distance", "mean_speed")
subtotalsTrack = setNames(data.frame(matrix(ncol = length(nameslist), nrow = 0)), nameslist)
for (i in unique(AllDataReduced$Track)){
  day=0
  trackData = AllDataReduced[AllDataReduced$Track==i,c("fid","layer","Start_DateTime","Date","duration","distance","speed")]
  for (j in unique(trackData$Date)){
    day=day+1
    dayData=trackData[trackData$Date==j,]
    layergroups=list()
    current_max=as.POSIXct.Date(999999999)
    for (k in sort(unique(dayData$layer))){
      layerData=dayData[dayData$layer==k,]
      a=min(as.POSIXct(layerData$Start_DateTime))
      b=max(as.POSIXct(layerData$Start_DateTime))
      if (a<current_max){
        layergroups = append(layergroups,k)
      }
      else{
        layergroups[[length(layergroups)]] = append(layergroups[[length(layergroups)]],k)
      }
      current_max=b
    }
    for (k in layergroups) {
      layerData=dayData[dayData$layer %in% k,]
      a=min(as.POSIXct(layerData$Start_DateTime))
      b=max(as.POSIXct(layerData$Start_DateTime))
      total_time = as.numeric(difftime(b,a,units="secs"))+layerData$duration[layerData$Start_DateTime==b]
      break_time=total_time-sum(layerData$duration)
      total_distance = sum(layerData$distance)
      mean_speed = mean(layerData$speed)
      walking_time = total_time - break_time
      subtotalsTrack[nrow(subtotalsTrack) + 1,] = list(i, day,length(k), total_time,break_time,walking_time,total_distance,mean_speed)
    }
  }
}





a= split(subtotalsTrack, cut2(subtotalsTrack$walking_time, g=500))
bb = vector()
ll = vector()
for (i in c(1:length(a))){
  bb = c(bb,mean(a[[i]]$mean_speed2))
  ll=c(ll,median(a[[i]]$walking_time))
}
plot(ll/3600,bb, ylim=c(0,5.5))
abline(h=4.5)

bb = vector()
ll = vector()
for (i in seq(450,55000,900)){
  bb = c(bb,mean(subtotalsTrack$mean_speed2[subtotalsTrack$walking_time<(i+450)&subtotalsTrack$walking_time>=(i-450)]))
  ll=c(ll,i)
}
plot(ll/3600,bb,ylim=c(0,5.5))


bb = vector()
ll = vector()
for (i in seq(250,44250,500)){
  bb = c(bb,mean(subtotalsTrack$mean_speed[subtotalsTrack$total_distance<(i+250)&subtotalsTrack$total_distance>=(i-250)]))
  ll=c(ll,i)
}
plot(ll/1000,bb,ylim=c(0,5.5))

a= split(subtotalsTrack, cut2(subtotalsTrack$total_distance, g=50))
bb = vector()
ll = vector()
for (i in c(1:length(a))){
  bb = c(bb,mean(a[[i]]$mean_speed2))
  ll=c(ll,median(a[[i]]$total_distance))
}
plot(ll/1000,bb, ylim=c(0,5.5))
abline(h=4.5)



bb = vector()
ll = vector()
for (i in seq(450,55000,900)){
  bb = c(bb,median(subtotalsTrack$break_time[subtotalsTrack$walking_time<(i+450)&subtotalsTrack$walking_time>=(i-450)]))
  ll=c(ll,i)
}
plot(ll/3600,bb/3600)

a= split(subtotalsTrack, cut2(subtotalsTrack$walking_time, g=500))
bb = vector()
ll = vector()
for (i in c(1:length(a))){
  bb = c(bb,median(a[[i]]$break_time))
  ll=c(ll,median(a[[i]]$walking_time))
}
plot(ll/3600,bb/3600,)
abline(h=4.5)




obst_data = data.frame(ll,bb)
obst_graph = ggplot(obst_data, aes(ll/1000,bb)) + 
  xlab("Walking Distance (km)") +
  ylab("Average Walking Speed (km/h)") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,50)) + 
  theme_classic() +
  geom_point()
obst_graph

par(mar=c(5, 4, 1, 1) + 0.1)
hist(subtotalsTrack$walking_time/3600, ylim=c(0,4000), xlab = "Walking Time (h)", main=NULL)

hist(ROUKDataOffRoad$elevation, breaks = c(-10,seq(0,1400,50)), ylim=c(0,0.005), xlab = "Elevation (m)", main=NULL)
hist(ScotDataOffRoad$elevation, breaks = c(-10,seq(0,1400,50)), ylim=c(0,0.005), xlab = "Elevation (m)", main=NULL)
par(mar=c(5, 4, 4, 2) + 0.1)





#Season vs Speed
ScotData$season=floor((as.numeric(format(as.POSIXct(ScotData$Start_DateTime),"%m"))%%12)/3)
t1=as.POSIXct(ScotDataReduced$Start_DateTime[1])
t2=as.POSIXct(ScotDataReduced$Start_DateTime[2])


BigSample=function(UKdataset, scotlandmodel, sampleNo, sampleSize){
  useable_models=list()
  for (i in c(1:sampleNo)){
    sampletracknumbers = sample(unique(UKdataset$Track),sampleSize)
    sampledataset = UKdataset[UKdataset$Track %in% sampletracknumbers,]
    samplemodel = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=sampledataset, family=gaussian(link='log'))
    useable_models = append(useable_models,list(samplemodel))
  }
  useable_models=append(useable_models,list(scotlandmodel))
  return(useable_models)
}

plotClimb=function(datagrid, legend){
  names=colnames(datagrid[2:ncol(datagrid)])
  line_comparison = ggplot(datagrid) + 
    xlab("Walking Slope (degrees)") +
    ylab("Walking Speed (km/h)") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) + 
    scale_x_continuous(limits = c(-50,50), breaks = seq(-50,50,10)) + 
    theme_classic() +
    theme(legend.key.height=unit(legend[[6]],"line"))
  for (i in c(1:length(legend[[1]]))){
  line_comparison = line_comparison +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[i],color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i])))
  }
  line_comparison = line_comparison +
    scale_color_manual(name = "", breaks = legend[[1]], values = legend[[2]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_linetype_manual(name = "", breaks = legend[[1]], values = legend[[3]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_size_manual(name = "", breaks = legend[[1]], values = legend[[4]], labels = function(x) str_wrap(x, width = legend[[5]]))
  line_comparison
}

plotTraverse=function(datagrid, legend){
  names=colnames(datagrid[2:ncol(datagrid)])
  line_comparison = ggplot(datagrid) + 
    xlab("Hill Slope (degrees)") +
    ylab("Walking Speed (km/h)") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) + 
    scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) + 
    theme_classic() +
    theme(legend.key.height=unit(legend[[6]],"line"))
  for (i in c(1:length(legend[[1]]))){
    line_comparison = line_comparison +  
      geom_line(data = datagrid, aes_string(x = 'slope', y = names[i],color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i])))
  }
  line_comparison = line_comparison +
    scale_color_manual(name = "", breaks = legend[[1]], values = legend[[2]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_linetype_manual(name = "", breaks = legend[[1]], values = legend[[3]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_size_manual(name = "", breaks = legend[[1]], values = legend[[4]], labels = function(x) str_wrap(x, width = legend[[5]]))
  line_comparison
}

plotSampleClimb=function(datagrid, legend){
  names=colnames(datagrid[2:ncol(datagrid)])
  line_comparison = ggplot(datagrid) + 
    xlab("Walking Slope (degrees)") +
    ylab("Walking Speed (km/h)") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5.5)) + 
    scale_x_continuous(limits = c(-50,50), breaks = seq(-50,50,10)) + 
    theme_classic() +
    theme(legend.key.height=unit(legend[[6]],"line"))
  for (i in c(1:(length(names)-1))){
    line_comparison = line_comparison +  
      geom_line(data = datagrid, aes_string(x = 'slope', y = names[i], color=shQuote(legend[[1]][1]), linetype=shQuote(legend[[1]][1]), size=shQuote(legend[[1]][1])))
  }
  line_comparison = line_comparison +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[length(names)],color=shQuote(legend[[1]][2]), linetype=shQuote(legend[[1]][2]), size=shQuote(legend[[1]][2])))
  line_comparison = line_comparison +
    scale_color_manual(name = "", breaks = legend[[1]], values = legend[[2]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_linetype_manual(name = "", breaks = legend[[1]], values = legend[[3]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_size_manual(name = "", breaks = legend[[1]], values = legend[[4]], labels = function(x) str_wrap(x, width = legend[[5]]))
  line_comparison
}

plotSampleTraverse=function(datagrid, col1){
  names=colnames(datagrid[2:ncol(datagrid)])
  line_comparison = ggplot(datagrid) + 
    xlab("Hill Slope (degrees)") +
    ylab("Walking Speed (km/h)") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5.5)) + 
    scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) + 
    theme_classic() +
    theme(legend.key.height=unit(legend[[6]],"line"))
  for (i in c(1:(length(names)-1))){
    line_comparison = line_comparison +  
      geom_line(data = datagrid, aes_string(x = 'slope', y = names[i], color=shQuote(legend[[1]][1]), linetype=shQuote(legend[[1]][1]), size=shQuote(legend[[1]][1])))
  }
  line_comparison = line_comparison +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[length(names)],color=shQuote(legend[[1]][2]), linetype=shQuote(legend[[1]][2]), size=shQuote(legend[[1]][2])))
  line_comparison = line_comparison +
    scale_color_manual(name = "", breaks = legend[[1]], values = legend[[2]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_linetype_manual(name = "", breaks = legend[[1]], values = legend[[3]], labels = function(x) str_wrap(x, width = legend[[5]])) +
    scale_size_manual(name = "", breaks = legend[[1]], values = legend[[4]], labels = function(x) str_wrap(x, width = legend[[5]]))
  line_comparison
}


#Sample width 0.25, old width 0.5, colour redbrick, cyan
legendBreaks = c('Off Road Obstruction Available', 
                 'Off Road No Obstruction Available')
legendColours = c("red","blue")
legendStyle = c("solid","dashed")
legendSize = c(1,1)
legendWidth=11
legendHeight=3

legend=list(legendBreaks, 
            legendColours, 
            legendStyle, 
            legendSize,
            legendWidth=11,
            legendHeight=3)

set.seed(5)
useable_models=BigSample(AllDataObstruction,m7, 100, 500)

useable_models=list(m6, m7)

travgrid=TraverseGrid(useable_models, FALSE)
climbgrid=ClimbGrid(useable_models, TRUE)

plotTraverse(travgrid, legend)
plotClimb(climbgrid, legend)

plotSampleTraverse(travgrid, legend)
plotSampleClimb(climbgrid, legend)


ROUKDataReduced$HeavyObstruction[is.na(ROUKDataReduced$HeavyObstruction)]=FALSE
ROUKDataReduced$OffRoadObstruction = ROUKDataReduced$Obstruction & !ROUKDataReduced$Road
ROUKDataReduced$OffRoadHeavyObstruction = ROUKDataReduced$HeavyObstruction & !ROUKDataReduced$Road

model = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+ # (Off Road No Obst Data)
               +OffRoadObstruction+OffRoadHeavyObstruction+  # (Off Road Obst Data)
               I(Paved*avg_walking_slope)+I(Paved*avg_walking_slope^2)+ #Paved Road
               Road+I(Road*avg_hill_slope)+I(Road*avg_walking_slope^2)      #Other Road (Unpaved)
             ,data=ROUKDataReduced, family=gaussian(link='log'))


coeftest(model, vcov = vcovCL, cluster = ~Track)

ScotDataReduced$HeavyObstruction[is.na(ScotDataReduced$HeavyObstruction)]=FALSE
ScotDataReduced$OffRoadObstruction = ScotDataReduced$Obstruction & !ScotDataReduced$Road
ScotDataReduced$OffRoadHeavyObstruction = ScotDataReduced$HeavyObstruction & !ScotDataReduced$Road

models = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+ # (Off Road No Obst Data)
               Paved+I(Paved*avg_hill_slope)+I(Paved*avg_walking_slope)+ #Paved Road
               Road+I(Road*avg_hill_slope)+I(Road*avg_walking_slope)+I(Road*avg_walking_slope^2)      #Other Road (Unpaved)
             ,data=ScotDataReduced, family=gaussian(link='log'))

coeftest(models, vcov = vcovCL, cluster = ~Track)

AllDataReduced$HeavyObstruction[is.na(AllDataReduced$HeavyObstruction)]=FALSE
AllDataReduced$OffRoadObstruction = AllDataReduced$Obstruction & !AllDataReduced$Road
AllDataReduced$OffRoadHeavyObstruction = AllDataReduced$HeavyObstruction & !AllDataReduced$Road

modela = glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+ # (Off Road No Obst Data)
                +OffRoadObstruction+OffRoadHeavyObstruction+  # (Off Road Obst Data)
                Road+I(Road*avg_hill_slope)+I(Road*avg_walking_slope^2)+     #Other Road (Unpaved)
              I(Paved*avg_hill_slope)+I(Paved*avg_walking_slope)+I(Paved*avg_walking_slope^2) #Paved Road
                ,data=AllDataReduced, family=gaussian(link='log'))

coeftest(modela, vcov = vcovCL, cluster = ~Track)

useable_models = list(model, models, modela)
save(useable_models, file="Final_models.RData")
load("Final_models.RData")
model=useable_models[[1]]
models=useable_models[[2]]
modela=useable_models[[3]]

climbgridPavedRoad=ClimbGrid(useable_models, 
                               obstruction_data = FALSE,
                               heavy_obstruction = FALSE,
                               road = TRUE,
                               paved = TRUE)
climbgridUnpavedRoad=ClimbGrid(useable_models, 
                                      obstruction_data = FALSE,
                                      heavy_obstruction = FALSE,
                                      road = TRUE,
                                      paved = FALSE)
climbgridUnknownObstruction=ClimbGrid(useable_models, 
                                      obstruction_data = FALSE,
                                      heavy_obstruction = FALSE,
                                      road = FALSE,
                                      paved = FALSE)
climbgridLightObstruction=ClimbGrid(useable_models, 
                                    obstruction_data = TRUE,
                                    heavy_obstruction = FALSE,
                                    road = FALSE,
                                    paved = FALSE)
climbgridHeavyObstruction=ClimbGrid(useable_models, 
                                    obstruction_data = TRUE,
                                    heavy_obstruction = TRUE,
                                    road = FALSE,
                                    paved = FALSE)
travgridUnpavedRoad=TraverseGrid(useable_models, 
                               obstruction_data = FALSE,
                               heavy_obstruction = FALSE,
                               road = TRUE,
                               paved = FALSE)
travgridUnknownObstruction=TraverseGrid(useable_models, 
                                      obstruction_data = FALSE,
                                      heavy_obstruction = FALSE,
                                      road = FALSE,
                                      paved = FALSE)
travgridLightObstruction=TraverseGrid(useable_models, 
                                    obstruction_data = TRUE,
                                    heavy_obstruction = FALSE,
                                    road = FALSE,
                                    paved = FALSE)
travgridHeavyObstruction=TraverseGrid(useable_models, 
                                    obstruction_data = TRUE,
                                    heavy_obstruction = TRUE,
                                    road = FALSE,
                                    paved = FALSE)
Deg30GridLightObstruction=DegGrid(useable_models, 
                                  obstruction_data = TRUE,
                                  heavy_obstruction = FALSE,
                                  road = FALSE,
                                  paved = FALSE,
                                  30)
Deg30GridHeavyObstruction=DegGrid(useable_models, 
                                  obstruction_data = TRUE,
                                  heavy_obstruction = TRUE,
                                  road = FALSE,
                                  paved = FALSE,
                                  30)
Deg40GridLightObstruction=DegGrid(useable_models, 
                                    obstruction_data = TRUE,
                                    heavy_obstruction = FALSE,
                                    road = FALSE,
                                    paved = FALSE,
                                  40)
Deg40GridHeavyObstruction=DegGrid(useable_models, 
                                    obstruction_data = TRUE,
                                    heavy_obstruction = TRUE,
                                    road = FALSE,
                                    paved = FALSE,
                                    40)







for (i in names(results[,1])){
  if(!(grepl("slope",i))){
    print(results[,1][i])
  }
}
for (i in names(results[,1])){
  if((grepl("avg_walking_slope",i))&&!(grepl("2",i))){
    print(results[,1][i])
  }
}
for (i in names(results[,1])){
  if((grepl("avg_walking_slope",i))&&(grepl("2",i))){
    print(results[,1][i])
  }
}
for (i in names(results[,1])){
  if(grepl("avg_hill_slope",i)){
    print(results[,1][i])
  }
}




