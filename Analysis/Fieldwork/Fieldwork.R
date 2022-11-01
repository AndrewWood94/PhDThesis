library(ggplot2)
library(stringr)
library(cowplot)
library(DescTools)
library(dplyr)
library(Hmisc)

basepath='/Volumes/csce/inf/groups/ANC/Andrew/EddieDATA/Processed/Scouts5NewBreak/merged/'
path=paste0(basepath,'CombinedData25Manual.csv')
ScoutData = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

#ScoutData$total_walking_slope = atan(ScoutData$elevation_change/ScoutData$length)*180/pi
#ScoutData$mean_speed = (ScoutData$distance/1000)/(ScoutData$duration/3600)

OSM_terrains = colnames(ScoutData[grepl('OSM',colnames(ScoutData))])
Paved=c("OSM_cycleway","OSM_footway",
        "OSM_motorway","OSM_motorway_link","OSM_pedestrian" ,
        "OSM_primary","OSM_residential","OSM_secondary"  ,
        "OSM_service" ,"OSM_steps","OSM_tertiary", 
        "OSM_trunk", "OSM_trunk_link","OSM_unclassified")
ScoutData$Road = rowSums(is.na(ScoutData[,grepl('OSM',colnames(ScoutData))]))!=length(OSM_terrains)
ScoutData$Paved =rowSums(is.na(ScoutData[,Paved]))!=length(Paved)

ScoutData$TestNumber = 0
ScoutData$PureTime = as.POSIXct(format(as.POSIXct(ScoutData$Start_DateTime),"%H:%M:%S"),format = "%H:%M:%S")

T1start = "8:40:00"
T1stop = "8:48:00"
Start=as.POSIXct(T1start, format = "%H:%M:%S")
Stop=as.POSIXct(T1stop, format = "%H:%M:%S")
ScoutData$TestNumber[ScoutData$PureTime>Start & ScoutData$PureTime<Stop]=1

T1start = "9:05:00"
T1stop = "9:12:30"
Start=as.POSIXct(T1start, format = "%H:%M:%S")
Stop=as.POSIXct(T1stop, format = "%H:%M:%S")
ScoutData$TestNumber[ScoutData$PureTime>Start & ScoutData$PureTime<Stop]=2

T1start = "9:12:30"
T1stop = "9:35:00"
Start=as.POSIXct(T1start, format = "%H:%M:%S")
Stop=as.POSIXct(T1stop, format = "%H:%M:%S")
ScoutData$TestNumber[ScoutData$PureTime>Start & ScoutData$PureTime<Stop]=3

T1start = "13:22:15"
T1stop = "13:32:00"
Start=as.POSIXct(T1start, format = "%H:%M:%S")
Stop=as.POSIXct(T1stop, format = "%H:%M:%S")
ScoutData$TestNumber[ScoutData$PureTime>Start & ScoutData$PureTime<Stop]=4

T1start = "13:30:50"
T1stop = "13:36:00"
Start=as.POSIXct(T1start, format = "%H:%M:%S")
Stop=as.POSIXct(T1stop, format = "%H:%M:%S")
ScoutData$TestNumber[ScoutData$PureTime>Start & ScoutData$PureTime<Stop]=5

tracks = sort(unique(ScoutData$Track))
for (i in tracks){
  ScoutData$TrackNum[ScoutData$Track==i]=which(i==tracks)
}

ScoutData$Road[ScoutData$TestNumber %in% c(1,2,3,4,5)] = FALSE
ScoutData$OffRoadHeavyObstruction = FALSE
ScoutData$OffRoadObstruction = FALSE
ScoutData$NoObstPredictions = predict(model, newdata = ScoutData, type= 'response')

#COMPARE TO TRUE OBSTRUCTION MODELS
ScoutData$OffRoadObstruction[!ScoutData$Road] = TRUE
ScoutData$OffRoadHeavyObstruction[ScoutData$TestNumber %in% c(4,5) & !ScoutData$Road] = TRUE
ScoutData$ObstPredictions = predict(model, newdata = ScoutData, type= 'response')
ScoutData$NoObstResidual = ScoutData$speed - ScoutData$NoObstPredictions
ScoutData$ObstResidual = ScoutData$speed - ScoutData$ObstPredictions

write.csv(ScoutData,path, row.names = FALSE)

basepath='/Volumes/csce/inf/groups/ANC/Andrew/EddieDATA/Processed/ScoutsNewBreak/merged/'
ScoutData50 = read.csv(paste0(basepath,'CombinedData50Geom.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScoutData50Road = read.csv(paste0(basepath,'CombinedData50GeomRoads.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScoutData50Manual = read.csv(paste0(basepath,'CombinedData50Manual.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)



basepath='/Volumes/csce/inf/groups/ANC/Andrew/EddieDATA/Processed/Scouts5NewBreak/merged/'
ScoutData5sec50 = read.csv(paste0(basepath,'CombinedData50Manual.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
ScoutData5sec25 = read.csv(paste0(basepath,'CombinedData25Manual.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)



ScoutData=ScoutData50Road
ScoutData=ScoutData[,c(1:14,45:62)]

#Basic model Normal Walking Conditions, ignoring Driving Sections

Standard_walk = ScoutData[ScoutData$TestNumber ==0,]
Stop=as.POSIXct("15:00:00", format = "%H:%M:%S")
Standard_walk = Standard_walk[as.POSIXct(substr(Standard_walk$Start_DateTime,12,19), format = "%H:%M:%S")<Stop,]

Standard_walk$Naismith = sapply(Standard_walk$avg_walking_slope, naismith)
Standard_walk$Tobler = sapply(Standard_walk$avg_walking_slope, tobler)
Standard_walk$NaismithResidual = Standard_walk$speed - Standard_walk$Naismith
Standard_walk$ToblerResidual = Standard_walk$speed - Standard_walk$Tobler

Standard_walk$cam50 = sapply(Standard_walk$avg_walking_slope, campbell, 50)
Standard_walk$cam75 = sapply(Standard_walk$avg_walking_slope, campbell, 75)
Standard_walk$cam95 = sapply(Standard_walk$avg_walking_slope, campbell, 95)
Standard_walk$cam50Residual = Standard_walk$speed - Standard_walk$cam50
Standard_walk$cam75Residual = Standard_walk$speed - Standard_walk$cam75
Standard_walk$cam95Residual = Standard_walk$speed - Standard_walk$cam95

residplot = baseplot(dataset = Standard_walk, 
                             xvar = Standard_walk$avg_walking_slope, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
                             yvar = Standard_walk$cam95Residual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
residplot + geom_hline(yintercept=0, size=0.5)
residplot = baseplot(dataset = Standard_walk, 
                          xvar = Standard_walk$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
                          yvar = Standard_walk$cam50Residual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
residplot + geom_hline(yintercept=0, size=0.5)




hillplot = baseplot(dataset = Standard_walk, 
                    xvar = Standard_walk$avg_walking_slope, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
                    yvar = Standard_walk$speed, ylim = c(0,10), ybreaks = seq(0,10,1))
hillplot + geom_line(data = climbgridUnpavedRoad, aes(x = slope, y = model1), color = 'red')

travplot = baseplot(dataset = Standard_walk, 
                    xvar = Standard_walk$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
                    yvar = Standard_walk$speed, ylim = c(0,10), ybreaks = seq(0,10,1))
travplot + geom_line(data = travgridUnpavedRoad, aes(x = slope, y = model1), color = 'red')

modelresidplot = baseplot(dataset = Standard_walk, 
                          xvar = Standard_walk$avg_walking_slope, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
                          yvar = Standard_walk$ObstResidual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
modelresidplot + geom_hline(yintercept=0, size=0.5)

modelresidplot = baseplot(dataset = Standard_walk, 
                          xvar = Standard_walk$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
                          yvar = Standard_walk$ObstResidual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
modelresidplot + geom_hline(yintercept=0, size=0.5)

naismithresidplot = baseplot(dataset = Standard_walk, 
                             xvar = Standard_walk$avg_walking_slope, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
                             yvar = Standard_walk$NaismithResidual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
naismithresidplot + geom_hline(yintercept=0, size=0.5)

naismithresidplot = baseplot(dataset = Standard_walk, 
                             xvar = Standard_walk$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
                             yvar = Standard_walk$NaismithResidual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
naismithresidplot + geom_hline(yintercept=0, size=0.5)

toblerresidplot = baseplot(dataset = Standard_walk, 
                           xvar = Standard_walk$avg_walking_slope, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
                           yvar = Standard_walk$ToblerResidual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
toblerresidplot + geom_hline(yintercept=0, size=0.5)

toblerresidplot = baseplot(dataset = Standard_walk, 
                           xvar = Standard_walk$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
                           yvar = Standard_walk$ToblerResidual, ylabel = 'Residuals (km/h)', ylim = c(-6,6), ybreaks = seq(-6,6,2))
toblerresidplot + geom_hline(yintercept=0, size=0.5)

#COMPARE RESIDUALS

legendBreaks = c('Model', 
                 'Naismith',
                 'Tobler',
                 'cam50',
                 'cam75',
                 'cam95')
legendColours = c("red","blue", "green", "pink", "purple", "orange")
legendStyle = c("solid","dashed", "dashed","solid","solid","solid")
legendSize = c(1,1,1,1,1,1)

legend=list(legendBreaks, 
            legendColours, 
            legendStyle, 
            legendSize,
            legendWidth=11,
            legendHeight=1)

res=data.frame(matrix(NA, ncol = 6, nrow = 0))
for (i in seq(-18,16,2)){
  data = Standard_walk[Standard_walk$avg_walking_slope>i & Standard_walk$avg_walking_slope<=i+2,c('ObstResidual','NaismithResidual','ToblerResidual','cam50Residual','cam75Residual','cam95Residual')]
  res = rbind(res, c(colMeans(data), sqrt(colMeans(data^2))))
}
res$slope = seq(-17,17,2)
res=na.omit(res[,c(13,1:12)])

resplot = baseplot(dataset = res, 
         xvar = NULL, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
         yvar = NULL, ylabel = 'Mean Residuals (km/h)', ylim = c(-3,3), ybreaks = seq(-3,3,1))
resplot = resplot + geom_hline(yintercept=0, size=0.5)
for (i in c(1:6)){
  resplot = resplot + geom_line(aes_string(x = 'slope', y = colnames(res)[i+1], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)

resplot = baseplot(dataset = res, 
                   xvar = NULL, xlabel = 'Walking Slope (degrees)', xlim = c(-20,20), xbreaks = seq(-20,20,10), 
                   yvar = NULL, ylabel = 'RMSE Residuals (km/h)', ylim = c(0,3), ybreaks = seq(0,3,1))
for (i in c(1:6)){
  resplot = resplot + geom_line(aes_string(x = 'slope', y = colnames(res)[i+7], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)

res=data.frame(matrix(NA, ncol = 6, nrow = 0))
for (i in seq(0,30,2)){
  data = Standard_walk[Standard_walk$avg_hill_slope>i & Standard_walk$avg_hill_slope<=i+2,c('ObstResidual','NaismithResidual','ToblerResidual','cam50Residual','cam75Residual','cam95Residual')]
  res = rbind(res, c(colMeans(data),sqrt(colMeans(data^2))))
}
res$slope = seq(1,31,2)
res=na.omit(res[,c(13,1:12)])

resplot = baseplot(dataset = res, 
         xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
         yvar = NULL, ylabel = 'Mean Residuals (km/h)', ylim = c(-2,2), ybreaks = seq(-2,2,1))
resplot = resplot + geom_hline(yintercept=0, size=0.5)
for (i in c(1:6)){
  resplot = resplot + geom_line(aes_string(x = 'slope', y = colnames(res)[i+1], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)

resplot = baseplot(dataset = res, 
                   xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(0,35), xbreaks = seq(0,40,10), xexpand = c(0,0),
                   yvar = NULL, ylabel = 'Residuals (km/h)', ylim = c(0,2), ybreaks = seq(0,2,1))
for (i in c(1:6)){
  resplot = resplot + geom_line(aes_string(x = 'slope', y = colnames(res)[i+7], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)

BigRes = Standard_walk[abs(Standard_walk$ObstResidual)>=2,]

#Investigate Steep Walking Slope

ScoutData=ScoutData50Manual
ScoutData=ScoutData5sec50
ScoutData=ScoutData5sec25


Downhill = ScoutData[ScoutData$TestNumber %in% c(2,3,5),]

plot = baseplot(dataset = Downhill, 
                 xvar = Downhill$avg_walking_slope, xlabel = 'Walking Slope (degrees)', xlim = c(-35,35), xbreaks = seq(-30,30,10), 
                 yvar = Downhill$speed, ylim = c(0,5), ybreaks = seq(0,5,1))
plot + geom_point(data = Downhill[Downhill$TrackNum==7,], aes(x=avg_walking_slope, y=speed), color = "red", size=1)


plot = baseplot(dataset = Downhill, 
                xvar = Downhill$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(0,40), xbreaks = seq(0,40,5), xexpand = c(0,0),
                yvar = Downhill$avg_walking_slope, ylabel = 'Walking Slope (degrees)', ylim = c(-35,35), ybreaks = seq(-35,55,5), yexpand =c(0.05,0))
plot + geom_hline(yintercept=28, linetype="dashed", color = "blue", size=0.5) +
      geom_hline(yintercept=-28, linetype="dashed", color = "blue", size=0.5) +
      geom_abline(intercept = 0, slope = 1, size=0.5) +
      geom_abline(intercept = 0, slope = -1, size=0.5)

means=data.frame(matrix(NA, ncol = 6, nrow = 0))
for (i in seq(0,40,2)){
  data = Downhill[Downhill$avg_hill_slope>i & Downhill$avg_hill_slope<=i+2,'avg_walking_slope']
  means = rbind(means, c(mean(data[data>0]),mean(data[data<0])))
}
means$slope = seq(0,40,2)

meanplot = baseplot(dataset = means, 
                   xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(0,40), xbreaks = seq(0,40,5), xexpand = c(0,0),
                   yvar = NULL, ylabel = 'Average Walking Slope (degrees)', ylim = c(-35,35), ybreaks = seq(-35,55,5), yexpand =c(0.05,0))
for (i in c(1:2)){
  meanplot = meanplot + geom_point(aes_string(x = 'slope', y = colnames(means)[i]))
}
meanplot + geom_hline(yintercept=28, linetype="dashed", color = "blue", size=0.5) +
  geom_hline(yintercept=-28, linetype="dashed", color = "blue", size=0.5) +
  geom_abline(intercept = 0, slope = 1, size=0.5) +
  geom_abline(intercept = 0, slope = -1, size=0.5)

#CLIMBING THE HILL
  
ScoutData=ScoutData5sec25
Downhill = ScoutData[ScoutData$TestNumber %in% c(2,3,5), c(1:14,44:56)]
Downhill$Naismith = sapply(Downhill$avg_walking_slope, naismith)
Downhill$Tobler = sapply(Downhill$avg_walking_slope, tobler)
Downhill$NaismithResidual = Downhill$speed - Downhill$Naismith
Downhill$ToblerResidual = Downhill$speed - Downhill$Tobler
Downhill$Explanation = 1

Downhill$cam50 = sapply(Downhill$avg_walking_slope, campbell, 50)
Downhill$cam75 = sapply(Downhill$avg_walking_slope, campbell, 75)
Downhill$cam95 = sapply(Downhill$avg_walking_slope, campbell, 95)
Downhill$cam50Residual = Downhill$speed - Downhill$cam50
Downhill$cam75Residual = Downhill$speed - Downhill$cam75
Downhill$cam95Residual = Downhill$speed - Downhill$cam95


basepath='/Volumes/csce/inf/groups/ANC/Andrew/EddieDATA/Processed/Scouts5NewBreak/merged/'
path=paste0(basepath,'Downhill25Manual.csv')
Downhill = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
Downhill$PredTime = ((Downhill$distance/1000) / Downhill$ObstPredictions)*3600

plot = baseplot(dataset = Downhill, 
                xvar = NULL, xlabel = 'Walking Slope (degrees)', xlim = c(-35,35), xbreaks = seq(-40,40,5), xexpand = c(0.05,0),
                yvar = NULL, ylabel = 'Walking Speed (km/h)', ylim = c(0,5), ybreaks = seq(0,5,1), yexpand =c(0,0))
plot + geom_point(data  = Downhill[Downhill$TestNumber==2,], aes(x=avg_walking_slope, y=speed), size=1, color = 'black') +
  geom_point(data  = Downhill[Downhill$TestNumber==3,], aes(x=avg_walking_slope, y=speed), size=1, color = 'black') +
  geom_point(data  = Downhill[Downhill$TestNumber==5,], aes(x=avg_walking_slope, y=speed), size=1, color = 'red') +
    theme(legend.position = 'none') +
  geom_line(data  = climbgridHeavyObstruction, aes(slope, model1), size = 0.5, color = 'red') +
  geom_line(data  = climbgridLightObstruction, aes(slope, model1), size = 0.5, color = 'black')

means=data.frame(matrix(NA, ncol = 6, nrow = 0))
for (i in seq(-40,0,2)){
  data = Downhill[Downhill$avg_walking_slope>i & Downhill$avg_walking_slope<=i+2,c('TestNumber','speed')]
  means = rbind(means, c(mean(data$speed[data$TestNumber==2]),mean(data$speed[data$TestNumber==5])))
}
means$slope = seq(-40,0,2)

meanplot = baseplot(dataset = Downhill, 
                    xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(-40,0), xbreaks = seq(-40,0,5), xexpand = c(0.05,0),
                    yvar = NULL, ylabel = 'Walking Speed (km/h)', ylim = c(0,5), ybreaks = seq(0,5,1), yexpand =c(0,0))
meanplot + geom_point(data = means, aes_string(x = 'slope', y = colnames(means)[1]), color = 'black') +
  geom_point(data = means, aes_string(x = 'slope', y = colnames(means)[2]), color = 'red') +
  geom_line(data  = climbgridHeavyObstruction, aes(slope, model1), size = 0.5, color = 'red') +
  geom_line(data  = climbgridLightObstruction, aes(slope, model1), size = 0.5, color = 'black')




modelresidplot = baseplot(dataset = Downhill, 
                          xvar = NULL, xlabel = 'Walking Slope (degrees)', xlim = c(-35,35), xbreaks = seq(-40,40,5), xexpand = c(0.05,0),
                          yvar = NULL, ylabel = 'Residuals (km/h)', ylim = c(-4,4), ybreaks = seq(-4,4,2))

modelresidplot + geom_hline(yintercept=0, size=0.5) +
geom_point(data  = Downhill[Downhill$TestNumber==2,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'black') +
geom_point(data  = Downhill[Downhill$TestNumber==3,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'black') +
geom_point(data  = Downhill[Downhill$TestNumber==5,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'red')

modelresidplot + geom_hline(yintercept=0, size=0.5) +
geom_point(data  = Downhill[Downhill$TestNumber==2 & Downhill$Explanation != 3,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'black') +
geom_point(data  = Downhill[Downhill$TestNumber==3 & Downhill$Explanation != 3,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'black') +
geom_point(data  = Downhill[Downhill$TestNumber==5 & Downhill$Explanation != 3,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'red')

modelresidplot + geom_hline(yintercept=0, size=0.5) +
geom_point(data  = Downhill[Downhill$Explanation == 1,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'black') +
geom_point(data  = Downhill[Downhill$Explanation == 2,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'red') +
geom_point(data  = Downhill[Downhill$Explanation == 3,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'green') +
geom_point(data  = Downhill[Downhill$Explanation == 4,], aes(x=avg_walking_slope, y=ObstResidual), size=1, color = 'blue')

total = 0
for (i in unique(Downhill$TrackNum[Downhill$TestNumber ==3]))
{
  print(i)
  pred = sum(Downhill$PredTime[Downhill$TrackNum == i & Downhill$TestNumber ==3 & Downhill$Explanation != 3])
  real = sum(Downhill$duration[Downhill$TrackNum == i & Downhill$TestNumber ==3 & Downhill$Explanation != 3])
  diff = ((pred - real)/real*100)
  print(diff)
  total = total + diff
}
total/5






res=data.frame(matrix(NA, ncol = 6, nrow = 0))
res1=data.frame(matrix(NA, ncol = 6, nrow = 0))
res2=data.frame(matrix(NA, ncol = 6, nrow = 0))

for (i in seq(-35,35,2)){
  data = Downhill[Downhill$avg_walking_slope>i & Downhill$avg_walking_slope<=i+2,c('TestNumber','ObstResidual','NaismithResidual','ToblerResidual','cam50Residual','cam75Residual','cam95Residual')]
  res = rbind(res, c(colMeans(data[,c(2:7)]), sqrt(colMeans(data[,c(2:7)]^2))))
  res1 = rbind(res1, c(colMeans(data[data$TestNumber==2 | data$TestNumber==5 ,c(2:7)]), sqrt(colMeans(data[data$TestNumber==2| data$TestNumber==5 ,c(2:7)]^2))))
  res2 = rbind(res2, c(colMeans(data[data$TestNumber==3,c(2:7)]), sqrt(colMeans(data[data$TestNumber==3,c(2:7)]^2))))
}
res$slope = seq(-34,36,2)
res=na.omit(res[,c(13,1:12)])
res1$slope = seq(-34,36,2)
res1=na.omit(res1[,c(13,1:12)])
res2$slope = seq(-34,36,2)
res2=na.omit(res2[,c(13,1:12)])

resplot = baseplot(dataset = res, 
                   xvar = NULL, xlabel = 'Walking Slope (degrees)', xlim = c(-35,35), xbreaks = seq(-40,40,5), 
                   yvar = NULL, ylabel = 'Mean Residuals (km/h)', ylim = c(-3,3), ybreaks = seq(-3,3,1))
resplot = resplot + geom_hline(yintercept=0, size=0.5)
for (i in c(1:6)){
  resplot = resplot + geom_line(data = res1, aes_string(x = 'slope', y = colnames(res)[i+1], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
  resplot = resplot + geom_line(data = res2, aes_string(x = 'slope', y = colnames(res)[i+1], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)

resplot = baseplot(dataset = res, 
                   xvar = NULL, xlabel = 'Walking Slope (degrees)', xlim = c(-35,35), xbreaks = seq(-40,40,5), 
                   yvar = NULL, ylabel = 'RMSE Residuals (km/h)', ylim = c(0,3), ybreaks = seq(0,3,1))
for (i in c(1:6)){
  resplot = resplot + geom_line(data = res1, aes_string(x = 'slope', y = colnames(res)[i+7], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
  resplot = resplot + geom_line(data = res2, aes_string(x = 'slope', y = colnames(res)[i+7], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)


Downhill$Explanation = 1
Downhill$Explanation[Downhill$avg_hill_slope<15] = 2
Downhill$Explanation[(Downhill$avg_hill_slope-abs(Downhill$avg_walking_slope))>15] = 3

plot(Downhill$avg_hill_slope, Downhill$avg_walking_slope, xlim=c(0,40), col=Downhill$Explanation, pch=20)
plot(Downhill$avg_walking_slope, Downhill$speed, ylim=c(0,5), xlim=c(-35,35), col=Downhill$Explanation, pch=20)

plot(Downhill$avg_walking_slope, Downhill$ObstResidual, col=Downhill$Explanation, pch=20, ylim=c(-4,4))
abline(h=0)

basepath='/Volumes/csce/inf/groups/ANC/Andrew/EddieDATA/Processed/Scouts5NewBreak/merged/'
path=paste0(basepath,'Downhill25Manual.csv')
write.csv(Downhill,path, row.names = FALSE)
Downhill = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

Downhill = Downhill[Downhill$Explanation == 1,]

plot(Downhill$avg_hill_slope, Downhill$avg_walking_slope, xlim=c(0,40), col=Downhill$TestNumber, pch=20)
plot(Downhill$avg_walking_slope, Downhill$speed, ylim=c(0,5), xlim=c(-35,35), col=Downhill$TestNumber, pch=20)

plot(Downhill$avg_walking_slope, Downhill$ObstResidual, col=Downhill$TestNumber, pch=20, ylim=c(-4,4))
plot(Downhill$avg_hill_slope, Downhill$ObstResidual, col=Downhill$TestNumber, pch=20, ylim=c(-4,4))

abline(h=0)
points(Downhill$avg_walking_slope, Downhill$ObstPredictions, col = 1, pch = 20)

plot(Downhill$avg_walking_slope, Downhill$ObstResidual, col=Downhill$Explanation, pch=20, ylim=c(-4,4))


#TRAVERSING THE HILL

ScoutData=ScoutData5sec25

Crosshill = ScoutData[ScoutData$TestNumber %in% c(1,4),]


plot = baseplot(dataset = Crosshill, 
                xvar = Crosshill$avg_hill_slope, xlabel = 'Hill Slope (degrees)', xlim = c(20,40), xbreaks = seq(0,40,5), xexpand = c(0.05,0),
                yvar = Crosshill$avg_walking_slope, ylabel = 'Walking Slope (degrees)', ylim = c(-30,30), ybreaks = seq(-35,55,5), yexpand =c(0.05,0))

plot = baseplot(dataset = Crosshill, 
                xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(20,40), xbreaks = seq(0,40,5), xexpand = c(0.05,0),
                yvar = NULL, ylabel = 'Walking Speed (km/h)', ylim = c(0,5), ybreaks = seq(0,5,1), yexpand =c(0,0))
plot + geom_point(data  = Crosshill[Crosshill$TestNumber==1,], aes(x=avg_hill_slope, y=speed), size=1, color = 'black') +
  geom_point(data  = Crosshill[Crosshill$TestNumber==4,], aes(x=avg_hill_slope, y=speed), size=1, color = 'red') +
  theme(legend.position = 'none') +
  geom_line(data  = travgridHeavyObstruction, aes(slope, model1), size = 0.5, color = 'red') +
  geom_line(data  = travgridLightObstruction, aes(slope, model1), size = 0.5, color = 'black')

means=data.frame(matrix(NA, ncol = 6, nrow = 0))
for (i in seq(20,40,2)){
  data = Crosshill[Crosshill$avg_hill_slope>i & Crosshill$avg_hill_slope<=i+2,c('TestNumber','speed')]
  means = rbind(means, c(mean(data$speed[data$TestNumber==1]),mean(data$speed[data$TestNumber==4])))
}
means$slope = seq(20,40,2)

meanplot = baseplot(dataset = Crosshill, 
                xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(20,40), xbreaks = seq(0,40,5), xexpand = c(0.05,0),
                yvar = NULL, ylabel = 'Walking Speed (km/h)', ylim = c(0,5), ybreaks = seq(0,5,1), yexpand =c(0,0))
meanplot + geom_point(data = means, aes_string(x = 'slope', y = colnames(means)[1]), color = 'black') +
geom_point(data = means, aes_string(x = 'slope', y = colnames(means)[2]), color = 'red') +
geom_line(data  = travgridHeavyObstruction, aes(slope, model1), size = 0.5, color = 'red') +
geom_line(data  = travgridLightObstruction, aes(slope, model1), size = 0.5, color = 'black')

Crosshill$Naismith = sapply(Crosshill$avg_walking_slope, naismith)
Crosshill$Tobler = sapply(Crosshill$avg_walking_slope, tobler)
Crosshill$NaismithResidual = Crosshill$speed - Crosshill$Naismith
Crosshill$ToblerResidual = Crosshill$speed - Crosshill$Tobler

Crosshill$cam50 = sapply(Crosshill$avg_walking_slope, campbell, 50)
Crosshill$cam75 = sapply(Crosshill$avg_walking_slope, campbell, 75)
Crosshill$cam95 = sapply(Crosshill$avg_walking_slope, campbell, 95)
Crosshill$cam50Residual = Crosshill$speed - Crosshill$cam50
Crosshill$cam75Residual = Crosshill$speed - Crosshill$cam75
Crosshill$cam95Residual = Crosshill$speed - Crosshill$cam95


plot(Crosshill$avg_hill_slope, Crosshill$ObstResidual, col=Crosshill$TestNumber, pch=20, ylim=c(-6,6))
plot(Crosshill$avg_hill_slope, Crosshill$NaismithResidual, col=Crosshill$TestNumber, pch=20, ylim=c(-6,6))
plot(Crosshill$avg_hill_slope, Crosshill$ToblerResidual, col=Crosshill$TestNumber, pch=20, ylim=c(-6,6))

modelresidplot = baseplot(dataset = Crosshill, 
                          xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(20,40), xbreaks = seq(0,40,5), xexpand = c(0.05,0),
                          yvar = NULL, ylabel = 'Residuals (km/h)', ylim = c(-4,4), ybreaks = seq(-6,6,2), yexpand = c(0.05,0))

modelresidplot + geom_hline(yintercept=0, size=0.5) +
geom_point(data  = Crosshill[Crosshill$TestNumber==1,], aes(x=avg_hill_slope, y=ObstResidual), size=1, color = 'black') +
geom_point(data  = Crosshill[Crosshill$TestNumber==4,], aes(x=avg_hill_slope, y=ObstResidual), size=1, color = 'red')

modelresidplot + geom_hline(yintercept=0, size=0.5) +
geom_point(data  = Crosshill[Crosshill$TestNumber==1,], aes(x=avg_hill_slope, y=NaismithResidual), size=1, color = 'black') +
geom_point(data  = Crosshill[Crosshill$TestNumber==4,], aes(x=avg_hill_slope, y=NaismithResidual), size=1, color = 'red')

modelresidplot + geom_hline(yintercept=0, size=0.5) +
geom_point(data  = Crosshill[Crosshill$TestNumber==1,], aes(x=avg_hill_slope, y=ToblerResidual), size=1, color = 'black') +
geom_point(data  = Crosshill[Crosshill$TestNumber==4,], aes(x=avg_hill_slope, y=ToblerResidual), size=1, color = 'red')


res=data.frame(matrix(NA, ncol = 6, nrow = 0))
res1=data.frame(matrix(NA, ncol = 6, nrow = 0))
res2=data.frame(matrix(NA, ncol = 6, nrow = 0))
for (i in seq(20,40,2)){
  data = Crosshill[Crosshill$avg_hill_slope>i & Crosshill$avg_hill_slope<=i+2,c('TestNumber','ObstResidual','NaismithResidual','ToblerResidual','cam50Residual','cam75Residual','cam95Residual')]
  res = rbind(res, c(colMeans(data[,c(2:7)]), sqrt(colMeans(data[,c(2:7)]^2))))
  res1 = rbind(res1, c(colMeans(data[data$TestNumber==1,c(2:7)]), sqrt(colMeans(data[data$TestNumber==1,c(2:7)]^2))))
  res2 = rbind(res2, c(colMeans(data[data$TestNumber==4,c(2:7)]), sqrt(colMeans(data[data$TestNumber==4,c(2:7)]^2))))
}
res$slope = seq(21,41,2)
res=na.omit(res[,c(13,1:12)])
res1$slope = seq(21,41,2)
res1=na.omit(res1[,c(13,1:12)])
res2$slope = seq(21,41,2)
res2=na.omit(res2[,c(13,1:12)])

plot(res$slope, res[,2], type = 'l', ylim=c(-1,1), col = 2, xlim=c(20,40))
lines(res1$slope, res1[,2], type = 'l', col=1)
lines(res1$slope, res2[,2], type = 'l', col=4)


resplot = baseplot(dataset = res, 
                   xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(20,40), xbreaks = seq(0,40,10), xexpand = c(0.05,0),
                   yvar = NULL, ylabel = 'Mean Residuals (km/h)', ylim = c(-3,3), ybreaks = seq(-3,2,1), yexpand = c(0.05,0))
resplot = resplot + geom_hline(yintercept=0, size=0.5)
for (i in c(1:6)){
  resplot = resplot + geom_line(aes_string(x = 'slope', y = colnames(res)[i+1], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)

resplot = baseplot(dataset = res, 
                   xvar = NULL, xlabel = 'Hill Slope (degrees)', xlim = c(20,40), xbreaks = seq(0,40,10), xexpand = c(0.05,0),
                   yvar = NULL, ylabel = 'Residuals (km/h)', ylim = c(0,3), ybreaks = seq(0,3,1), yexpand = c(0,0))
for (i in c(1:6)){
  resplot = resplot + geom_line(aes_string(x = 'slope', y = colnames(res)[i+7], color=shQuote(legend[[1]][i]), linetype=shQuote(legend[[1]][i]), size=shQuote(legend[[1]][i]))) 
}
addLegend(resplot, legend)







plot = ggplot(Crosshill, aes(avg_hill_slope, speed)) +
  xlab('Walking Slope (degrees)') +
  ylab('Speed (km/h)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) + 
  scale_x_continuous(limits = c(0,35), breaks = seq(-40,40,10)) + 
  theme_classic() +
  geom_point(size=1, alpha = 1)
plot = plot + geom_point(data = Crosshill, aes(x=avg_hill_slope, y=NoObstPredictions), size=1, color="green")

plot(AllDataReduced$avg_hill_slope,AllDataReduced$avg_walking_slope, pch='.', col=1, xlim=c(0,40), ylim=c(-40,40))

mean(abs(AllData50$total_walking_slope[AllData50$TestNumber ==0]-AllData50$avg_walking_slope[AllData50$TestNumber ==0]))
mean(abs(AllData50$total_walking_slope[AllData50$TestNumber %in% c(1,4)]-AllData50$avg_walking_slope[AllData50$TestNumber %in% c(1,4)]))

plot(ScoutData50$avg_hill_slope,AllData50$speed, pch=20, col=1)
plot(AllData50$avg_hill_slope,AllData50$avg_walking_slope, pch=20, col=1, xlim=c(0,40), ylim=c(-40,40))
plot(AllData50$avg_hill_slope,AllData50$total_walking_slope, pch=20, col=1, xlim=c(0,40), ylim=c(-40,40))

abline(a=0, b=1)
abline(a=-20, b=0)


lines(climbgridUnknownObstruction$slope, climbgridUnknownObstruction$model1, col = 1)
lines(climbgridLightObstruction$slope, climbgridLightObstruction$model1, col = 1)
lines(climbgridHeavyObstruction$slope, climbgridHeavyObstruction$model1, col = 1)

lines(Deg30GridLightObstruction$slope, Deg30GridLightObstruction$model1, col = 1)
lines(Deg30GridHeavyObstruction$slope, Deg30GridHeavyObstruction$model1, col = 2)
lines(Deg40GridLightObstruction$slope, Deg40GridLightObstruction$model1, col = 3)
lines(Deg40GridHeavyObstruction$slope, Deg40GridHeavyObstruction$model1, col = 4)


for(i in seq(-40,40,5)){
  points(i,mean(Downhill$speed[Downhill$avg_walking_slope>(i-2.5) & Downhill$avg_walking_slope<(i+2.5)]), col=4, pch=20)
}

ToInvestigate = Downhill[Downhill$speed < (3.2-(0.1*Downhill$avg_walking_slope)) & Downhill$avg_walking_slope >0,]
plot(ToInvestigate$avg_walking_slope, ToInvestigate$speed, ylim=c(0,3), xlim=c(0,20), col=ToInvestigate$TrackNum, pch=20)
text(ToInvestigate$avg_walking_slope, ToInvestigate$speed,label= ToInvestigate$fid, pos=2, cex=0.8)







baseplot = function(dataset, xvar, yvar, xlabel, ylim, ybreaks, xlim, xbreaks, ylabel = 'Speed (km/h)', xexpand = c(0.05, 0), yexpand = c(0, 0)){
  plot = ggplot(dataset, aes(xvar, yvar)) +
    xlab(xlabel) +
    ylab(ylabel) +
    scale_y_continuous(expand = yexpand, limits = ylim, breaks = ybreaks) + 
    scale_x_continuous(expand = xexpand, limits = xlim, breaks = xbreaks) + 
    theme_classic()
  if (!(is.null(xvar))){
    plot = plot + geom_point(size=1, alpha = 1)
  }
  return(plot)
}

addLegend = function(plot, legend){
  plot = plot +
  theme(legend.key.height=unit(legend[[6]],"line"))  +
  scale_color_manual(name = "", breaks = legend[[1]], values = legend[[2]], labels = function(x) str_wrap(x, width = legend[[5]])) +
  scale_linetype_manual(name = "", breaks = legend[[1]], values = legend[[3]], labels = function(x) str_wrap(x, width = legend[[5]])) +
  scale_size_manual(name = "", breaks = legend[[1]], values = legend[[4]], labels = function(x) str_wrap(x, width = legend[[5]]))
  return(plot)
}