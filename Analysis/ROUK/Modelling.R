library(rgl)
library(visreg)
library(mgcv)
library(caret)
library(ggplot2)
source("Eddie/Speed formulas.R")

#HIKR DATA
hikrdata = read.csv("/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/HikrOutput/HikrOutputTerrain/MergedHikr.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#OSM DATA
osmdata = read.csv('/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/OSM Output/OSMTerrain/MergedFinal/MergedOSM.csv', header = TRUE, sep = ",", stringsAsFactors = FALSE)

modellingdata = rbind(hikrdata, osmdata)

filepath = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/UK Data/Modelling/"
write.csv(modellingdata,paste0(filepath,'CombinedData.csv'), row.names = FALSE)
modellingdata = read.csv(paste0(filepath,'CombinedData.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)


#CROSS VALIDATION
#Create 10 folds in dataset
folds = createFolds(modellingdata$layer, k = 10, list = TRUE, returnTrain = FALSE)
save(folds, file="folds.RData")
load("folds.RData")

#Set up results dataframe
nameslist=c("model", "MSE", "RMSE", "MAE", "Rsquared")
modelresults = setNames(data.frame(matrix(ncol = length(nameslist), nrow = 0)), nameslist)

#Get values for existing models
existing_models = c("naismith", "tobler", "irtenkauf", "langmuir")
for (model_name in existing_models){
  truevalues = modellingdata$speed
  predictions = sapply(modellingdata$avg_walking_slope,model_name)
  MSE = mean((truevalues - predictions)^2)
  MAE = mean(abs(truevalues - predictions))
  TSS = sum((truevalues - mean(truevalues))^2)
  RSS = sum((truevalues - predictions)^2)
  Rsquared = 1-(RSS/TSS)
  RMSE = sqrt(MSE)
  modelresults[nrow(modelresults) + 1,] = list(model_name, MSE,RMSE,MAE,Rsquared)
}

modellist=vector()
#linear hill
m1 = 'glm(speed~avg_hill_slope+avg_walking_slope,data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m2 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m3 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m4 = 'glm(speed~avg_hill_slope+avg_walking_slope,data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))'
m5 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))'
m6 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))'
#quadratic hill
m7 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope,data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m8 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m9 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m10 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))'
m11 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))'
#cubic hill
m12 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope,data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m13 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m14 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
modellist = c(modellist, m1, m2,m3,m4, m5, m6, m7,m8,m9,m10,m11,m12,m13,m14)
rm(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)

count = 0
for (modeltext in modellist){
  count= count + 1
  print(c(count, modeltext))
  MSE = vector()
  MAE = vector()
  Rsquared = vector()
  for (i in c(1:1)){
    print(i)
    model = eval(parse(text = modeltext))
    truevalues = modellingdata$speed[folds[[i]]]
    predictions = predict(model, newdata = modellingdata[folds[[i]],], type = "response")
    MSE = c(MSE, mean((truevalues - predictions)^2))
    MAE = c(MAE, mean(abs(truevalues - predictions)))
    TSS = sum((truevalues - mean(truevalues))^2)
    RSS = sum((truevalues - predictions)^2)
    Rsquared = c(Rsquared, 1-(RSS/TSS))
  }
  RMSE = sqrt(MSE)
  modelresults[nrow(modelresults) + 1,] = list(modeltext, mean(MSE),mean(RMSE),mean(MAE),mean(Rsquared))
}

write.csv(modelresults,paste0(filepath,'ModelComparison.csv'), row.names = FALSE)
modelresults = read.csv(paste0(filepath,'ModelComparison.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)

#EXPLORE ALL MODELS

modellist=vector()
#linear hill
m1 = 'glm(speed~avg_hill_slope+avg_walking_slope,data=modellingdata, family=gaussian(link="log"))'
m2 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata, family=gaussian(link="log"))'
m3 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata, family=gaussian(link="log"))'
m4 = 'glm(speed~avg_hill_slope+avg_walking_slope,data=modellingdata, family=Gamma(link="inverse"))'
m5 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata, family=Gamma(link="inverse"))'
m6 = 'glm(speed~avg_hill_slope+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata, family=Gamma(link="inverse"))'
#quadratic hill
m7 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope,data=modellingdata, family=gaussian(link="log"))'
m8 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata, family=gaussian(link="log"))'
m9 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata, family=gaussian(link="log"))'
#cubic hill
m10 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope,data=modellingdata, family=gaussian(link="log"))'
m11 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata, family=gaussian(link="log"))'
m12 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata, family=gaussian(link="log"))'
m13 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope,data=modellingdata, family=Gamma(link="inverse"))'
m14= 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope+I(avg_walking_slope^2),data=modellingdata, family=Gamma(link="inverse"))'
m15 = 'glm(speed~avg_hill_slope+I(avg_hill_slope^2)+I(avg_hill_slope^3)+avg_walking_slope+I(avg_walking_slope^2)+I(avg_walking_slope^3),data=modellingdata, family=Gamma(link="inverse"))'
modellist = c(modellist, m1, m2,m3,m4, m5, m6, m7,m8,m9,m10,m11,m12,m13,m14,m15)
rm(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)

model_collection_full = list()
for (i in (1:length(modellist))){
  print(c(i,modellist[i]))
  model_collection_full[[i]] = eval(parse(text=modellist[i]))
}
model_collection = model_collection_full

save(model_collection_full, file="all_models.RData")
load('all_models.RData')

#Model elimination
#Hill direction
x = 0
y =  c(0:85)
datagrid = expand.grid(avg_walking_slope = x, avg_hill_slope = y)
count = 2
for (i in c(1:length(model_collection))){
  if (!(is.null(model_collection[[i]]))){
    count=count+1
    datagrid = cbind(datagrid, predict(model_collection[[i]], datagrid, type = 'response'))
    colnames(datagrid)[count] = i
  }
}
increasing = colnames(datagrid)[which(sapply(datagrid, function(x) any(diff(x) > 0)))][- 1]
for (i in c(1:length(model_collection))){
  if (i %in% as.numeric(increasing)){
    model_collection[i] = list(NULL)
  }
}
#Walk Direction
#Increasing function
#Walking Uphill
x = (seq(10, 80, 1))
y = (seq(10, 80, 1))
datagrid = data.frame(avg_walking_slope = x, avg_hill_slope = y)
count = 2
for (i in c(1:length(model_collection))){
  if (!(is.null(model_collection[[i]]))){
    count=count+1
    datagrid = cbind(datagrid, predict(model_collection[[i]], datagrid, type = 'response'))
    colnames(datagrid)[count] = i
  }
}
increasing = colnames(datagrid)[which(sapply(datagrid, function(x) any(diff(x) > 0)))][- c(1,2)]
for (i in c(1:length(model_collection))){
  if (i %in% as.numeric(increasing)){
    model_collection[i] = list(NULL)
  }
}
#Walking Downhill
x = (seq(-10, -80, -1))
y = (seq(10, 80, 1))
datagrid = data.frame(avg_walking_slope = x, avg_hill_slope = y)
count = 2
for (i in c(1:length(model_collection))){
  if (!(is.null(model_collection[[i]]))){
    count=count+1
    datagrid = cbind(datagrid, predict(model_collection[[i]], datagrid, type = 'response'))
    colnames(datagrid)[count] = i
  }
}
increasing = colnames(datagrid)[which(sapply(datagrid, function(x) any(diff(x) > 0)))][- 1]
for (i in c(1:length(model_collection))){
  if (i %in% as.numeric(increasing)){
    model_collection[i] = list(NULL)
  }
}
#Critical gradient
x = (seq(-30, 30, 1))
y =  (30)
datagrid = expand.grid(avg_walking_slope = x, avg_hill_slope = y)
count=2
for (i in c(1:(length(model_collection)))){
  if (!(is.null(model_collection[[i]]))){
    count=count+1
    pred_speed = matrix(predict(model_collection[[i]], datagrid, type = 'response'))
    #horizontal distance needed to gain same height as 1 horizontal unit in slope direction
    dist_required = (tan(datagrid$avg_hill_slope*pi/180)/tan(datagrid$avg_walking_slope*pi/180))
    time = abs(dist_required/pred_speed)
    datagrid = cbind(datagrid, pred_speed)
    
    minval=which.min(time[datagrid$avg_walking_slope>0])
    topslope=datagrid[datagrid$avg_walking_slope>0,1][minval]
    datagrid[datagrid$avg_walking_slope>topslope,count]=NA
    
    minval=which.min(time[datagrid$avg_walking_slope<0])
    bottomslope=datagrid[datagrid$avg_walking_slope<0,1][minval]
    datagrid[datagrid$avg_walking_slope<bottomslope,count]=NA
    colnames(datagrid)[count] = i
  }
}
no_critical = colnames(datagrid)[!(is.na(datagrid[1,])) | !(is.na(datagrid[48,])) ][- c(1,2)]
for (i in c(1:length(model_collection))){
  if (i %in% as.numeric(no_critical)){
    model_collection[i] = list(NULL)
  }
}



obstruction_data = FALSE
heavy_obstruction = FALSE
road = FALSE
paved = FALSE
x = (seq(-35, 35, 1))
y = (35)

y = (seq(0, 45, 0.2))

datagrid = data.frame(expand.grid(avg_walking_slope = x,avg_hill_slope = y))
datagrid$OffRoadObstruction = obstruction_data
datagrid$OffRoadHeavyObstruction = heavy_obstruction
datagrid$Road = road
datagrid$Paved = paved
pred_speed= predict(model, datagrid, type = 'response')
datagrid = cbind(datagrid, pred_speed)
dist_required = (tan(datagrid$avg_hill_slope*pi/180)/tan(datagrid$avg_walking_slope*pi/180))
datagrid = cbind(datagrid, dist_required)
time = abs(dist_required/pred_speed)
datagrid = cbind(datagrid, time)
minval=which.min(time[datagrid$avg_walking_slope>0])
datagrid[datagrid$avg_walking_slope>0,1][minval]
minval=which.min(time[datagrid$avg_walking_slope<0])
datagrid[datagrid$avg_walking_slope<0,1][minval]


open3d()
persp3d(x, y, datagrid$pred_speed, col = 2,lit =TRUE, add = TRUE)










useable_models=list()
count=0
for (i in c(1:length(model_collection))){
  if (!(is.null(model_collection[[i]]))){
    count=count+1
    useable_models[[count]]=model_collection[[i]]
  }
}

save(useable_models, file="models.RData")
load("models.Rdata")

#Final model
useable_models=list()
useable_models[[1]] = models[[1]]

comparison=list()
comparison[[1]]=m2
comparison[[2]]=useable_models[[1]]
comparison[[3]]=useable_models[[3]]
useable_models=comparison
#Visualisation

open3d()
for (i in c(1:length(useable_models))){
  visreg2d(model, "avg_walking_slope", "avg_hill_slope", plot.type="rgl", scale='response', color = i+2, add = TRUE, lit =FALSE)
}
aspect3d(1,1,1)
box3d()
axis3d('x--',labels=T,tick=T, nticks = 8)
axis3d('y+-',labels=T,tick=T)
axis3d('z++',labels=T,tick=T)

#VISUALISATION POSSIBLE AREA

slopes = (seq(-50, 50, 0.2))
avgs =  (seq(0, 60, 0.2))

open3d()
for (i in c(1:(length(useable_models)))){
  datagrid = expand.grid(avg_walking_slope = slopes, avg_hill_slope = avgs)
  pred_speed = matrix(predict(useable_models[[i]], datagrid, type = 'response'))
  dist_required = (tan(datagrid$avg_hill_slope*pi/180)/tan(datagrid$avg_walking_slope*pi/180))
  time = abs(dist_required/pred_speed)
  datagrid = cbind(datagrid, pred_speed)
  #OPTIMAL
  #minval=which.min(time[datagrid$avg_hill_slope==max(avgs)&datagrid$avg_walking_slope>0])
  #topslope=datagrid[datagrid$avg_hill_slope==max(avgs)&datagrid$avg_walking_slope>0,1][minval]
  #datagrid[datagrid$avg_walking_slope>topslope,3]=NA
  
  #minval=which.min(time[datagrid$avg_hill_slope==max(avgs)&datagrid$avg_walking_slope<0])
  #bottomslope=datagrid[datagrid$avg_hill_slope==max(avgs)&datagrid$avg_walking_slope<0,1][minval]
  #datagrid[datagrid$avg_walking_slope<bottomslope,3]=NA
  #------
  datagrid[abs(datagrid$avg_walking_slope)>datagrid$avg_hill_slope,]=NA
  persp3d(slopes, avgs, datagrid[,3], col = i+2,lit =TRUE, add = TRUE)
}

aspect3d(1,1,1)
box3d()

axis3d('x--',labels=T,tick=T, nticks = 8)
axis3d('y+-',labels=T,tick=T)
axis3d('z++',labels=T,tick=T)
title3d(zlab = 'Speed (km/h)', line = 3, pos = c(50,60,NA))
title3d(xlab = 'Walking Slope (degrees)', line = 3, pos = c(NA,0,0))
title3d(ylab = 'Hill Slope (degrees)', line = 3, pos = c(50,NA,0))
#legend3d("topright", legend=c("GAM, 3 knot","GAM, 4 knot", "GLM, quadratic", "GLM, cubic"), cex=1,  lty=c(1, 1,1,1), col = c(3,4,5,6), inset=c(0.02)) 


legendBreaks = c("Old Model", "All Data", "Off Path", "On Path", "Off Path Obst","Off Path No Obst", "Paved Road", "Unpaved Road")
legendStyle = c("dashed", 'solid', 'solid', 'solid', 'solid', 'solid',"dashed","solid")
legendColours = (c("red","blue", "green", "magenta", "yellow", "red","blue", "green"))
legendShape = c(1,1,1,1,1,1,1,1)
legendSize = c(2,2,2,2,2,2,2,2)


#Hill Traverse
modellingdata=UnPaved

plotdata = modellingdata[abs(modellingdata$avg_walking_slope)<5,]

#Model data
x = 0
y =  c(0:40)
datagrid = expand.grid(avg_walking_slope = x, avg_hill_slope = y, avg_obstruction=0)
names = vector()
for (i in c(1:length(useable_models))){
  datagrid = cbind(datagrid, predict(useable_models[[i]], datagrid, type = 'response'))
  names = c(names, paste0('model',i))
}
datagrid = data.frame(datagrid[,-c(1,3)])
colnames(datagrid)=c("slope", names)

#Existing Functions data
datagrid = cbind(datagrid, naismith(0), tobler(0))
colnames(datagrid)=c("slope", names, "nai","tob")

#Mean and standard error traverse data
means=vector()
seplus = vector()
seminus=vector()
bin_width = 10
bins=vector()
for (i in seq(0,65,bin_width)){
  bin_data = plotdata[plotdata$avg_hill_slope >= i & +
                        plotdata$avg_hill_slope <= i+bin_width, 'speed']
  binmean = mean(bin_data)
  stddev = sd(bin_data)
  stderr = stddev/sqrt(length(bin_data))
  
  means = c(means,binmean)
  seplus = c(seplus, binmean+stderr)
  seminus = c(seminus, binmean-stderr)
  bins = c(bins, i+bin_width/2)
}
meanlines = data.frame(cbind(bins, means, seplus, seminus))

#Plot Data
hill_traverse = ggplot(plotdata, aes(avg_hill_slope, speed)) +
  xlab('Hill Slope (degrees)') +
  ylab('Speed (km/h)') +
  scale_x_continuous(expand = c(0, 0), limits = c(0,40)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) + 
  theme_classic() +
  geom_point(size=0.01, alpha = 0.3) 

#Add mean lines
hill_traverse = hill_traverse + 
  geom_line(data = meanlines, aes(x = bins, y = seplus, colour = "Data Standard Error", linetype = "Data Standard Error")) +
  geom_line(data = meanlines, aes(x = bins, y = seminus, colour = "Data Standard Error", linetype = "Data Standard Error")) +
  geom_line(data = meanlines, aes(x = bins, y = means, colour = "Data Mean", linetype = "Data Mean"))

#Add existing functions
hill_traverse = hill_traverse +
  geom_line(data = datagrid, aes(x = slope, y = tob, colour = "Tobler", linetype = "Tobler")) +
  geom_line(data = datagrid, aes(x = slope, y = nai, colour = "Naismith", linetype = "Naismith"))

#Add single model line

hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model1, colour = 'Old Model', linetype = 'Old Model'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model2, colour = 'All Data', linetype = 'All Data'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model3, colour = 'Off Path', linetype = 'Off Path'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model3, colour = 'On Path', linetype = 'On Path'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model5, colour = 'Off Path Obst', linetype = 'Off Path Obst'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model6, colour = 'Off Path No Obst', linetype = 'Off Path No Obst'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model4, colour = 'Unpaved Road', linetype = 'Unpaved Road'))
hill_traverse = hill_traverse +  
  geom_line(data = datagrid, aes(x = slope, y = model5, colour = 'Paved Road', linetype = 'Paved Road'))

#Add multiple model lines

for (i in c(1:length(useable_models))){
  hill_traverse = hill_traverse +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[i]), colour = (palette()[i%%length(palette())+1]), linetype = 'solid')
} 

hill_traverse = hill_traverse +  
  scale_linetype_manual(name = "", breaks = legendBreaks, values = legendStyle, labels = function(x) str_wrap(x, width = 5)) +
  scale_color_manual(name = "", breaks = legendBreaks, values = legendColours, labels = function(x) str_wrap(x, width = 5))
hill_traverse = hill_traverse + theme(legend.position = 'none')
hill_traverse = 
  hill_traverse + theme(axis.text=element_text(size=10),axis.title=element_text(size=12),legend.text=element_text(size=12))

#Plot hill climb
plotdata = modellingdata[modellingdata$avg_hill_slope-abs(modellingdata$avg_walking_slope)<5,]

#Model data
x = (seq(-80, 80, 1))
y = c(seq(80, 0, -1),seq(1, 80, 1))
datagrid = data.frame(cbind(avg_walking_slope = x, avg_hill_slope = y),avg_obstruction=0)
names = vector()
for (i in c(1:length(useable_models))){
  datagrid = cbind(datagrid, predict(useable_models[[i]], datagrid, type = 'response'))
  names = c(names, paste0('model',i))
}
datagrid = datagrid[,-c(2,3)]
colnames(datagrid)=c("slope", names)

#Existing Functions data
nas = sapply(x,naismith)
tob = sapply(x,tobler)
datagrid = cbind(datagrid, nas, tob)
colnames(datagrid)=c("slope", names, "nai","tob")


#Mean and standard error traverse data
means=vector()
seplus = vector()
seminus=vector()
bin_width = 10
bins=vector()
for (i in seq(-50,50,bin_width)){
  bin_data = plotdata[plotdata$avg_walking_slope >= i & +
                        plotdata$avg_walking_slope <= i+bin_width, 'speed']
  binmean = mean(bin_data)
  stddev = sd(bin_data)
  stderr = stddev/sqrt(length(bin_data))
  
  means = c(means,binmean)
  seplus = c(seplus, binmean+stderr)
  seminus = c(seminus, binmean-stderr)
  bins = c(bins, i+bin_width/2)
}
meanlines = data.frame(cbind(bins, means, seplus, seminus))


#Plot Data
hill_climb = ggplot(plotdata, aes(avg_walking_slope, speed)) +
  xlab('Walking Slope (degrees)') +
  ylab('Speed (km/h)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) + 
  scale_x_continuous(limits = c(-40,40), breaks = seq(-40,40,10)) + 
  theme_classic() +
  geom_point(size=0.01, alpha = 0.2)

#Add mean lines
hill_climb = hill_climb + 
  geom_line(data = meanlines, aes(x = bins, y = seplus, colour = "Data Standard Error", linetype = "Data Standard Error")) +
  geom_line(data = meanlines, aes(x = bins, y = seminus, colour = "Data Standard Error", linetype = "Data Standard Error")) +
  geom_line(data = meanlines, aes(x = bins, y = means, colour = "Data Mean", linetype = "Data Mean"))

#Add existing functions
hill_climb = hill_climb +
  geom_line(data = datagrid, aes(x = slope, y = tob, colour = "Tobler", linetype = "Tobler")) +
  geom_line(data = datagrid, aes(x = slope, y = nai, colour = "Naismith", linetype = "Naismith"))

#Add single model line
hill_climb = hill_climb +  
  geom_line(data = datagrid, aes(x = slope, y = model1, colour = 'Old Model', linetype = 'Old Model'))
hill_climb = hill_climb +  
  geom_line(data = datagrid, aes(x = slope, y = model2, colour = 'All Data', linetype = 'All Data'))
hill_climb = hill_climb +  
  geom_line(data = datagrid, aes(x = slope, y = model3, colour = 'Off Path', linetype = 'Off Path'))
hill_climb = hill_climb +  
  geom_line(data = datagrid, aes(x = slope, y = model3, colour = 'On Path', linetype = 'On Path'))
hill_climb = hill_climb +  
  geom_line(data = datagrid, aes(x = slope, y = model4, colour = 'Unpaved Road', linetype = 'Unpaved Road'))
hill_climb = hill_climb +  
  geom_line(data = datagrid, aes(x = slope, y = model5, colour = 'Paved Road', linetype = 'Paved Road'))


#Add multiple model lines

for (i in c(1:length(useable_models))){
  hill_climb = hill_climb +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[i]), colour = (palette()[i%%length(palette())+1]), linetype = 'solid')
} 

hill_climb = hill_climb + theme(legend.position = 'none')
hill_climb = hill_climb +  
  scale_linetype_manual(name = "", breaks = legendBreaks, values = legendStyle, labels = function(x) str_wrap(x, width = 5)) +
  scale_color_manual(name = "", breaks = legendBreaks, values = legendColours, labels = function(x) str_wrap(x, width = 5))


hill_climb = 
  hill_climb + theme(axis.text=element_text(size=10),axis.title=element_text(size=12),legend.text=element_text(size=12))




#Traverse
TraverseGrid=function(useable_models, 
                       obstruction_data = FALSE,
                       heavy_obstruction = FALSE,
                       road = FALSE,
                       paved = FALSE){
  x = 0
  y =  c(0:40)
  datagrid = expand.grid(avg_hill_slope = y, avg_walking_slope = x)
  datagrid$OffRoadObstruction = obstruction_data
  datagrid$OffRoadHeavyObstruction = heavy_obstruction
  datagrid$Road = road
  datagrid$Paved = paved
  names = vector()
  for (i in c(1:length(useable_models))){
    datagrid = cbind(datagrid, predict(useable_models[[i]], cbind(datagrid,HeavyObstruction=FALSE), type = 'response'))
    names = c(names, paste0('model',i))
  }
  datagrid = datagrid[,c(1,7:ncol(datagrid))]
  colnames(datagrid)=c("slope", names)
  return(datagrid)
}

#Climb
ClimbGrid=function(useable_models, 
                   obstruction_data = FALSE,
                   heavy_obstruction = FALSE,
                   road = FALSE,
                   paved = FALSE){
  x = (seq(-50, 50, 1))
  y = c(seq(50, 0, -1),seq(1, 50, 1))
  
  datagrid = data.frame(cbind(avg_walking_slope = x,avg_hill_slope = y))
  datagrid$OffRoadObstruction = obstruction_data
  datagrid$OffRoadHeavyObstruction = heavy_obstruction
  datagrid$Road = road
  datagrid$Paved = paved
  
  names = vector()
  for (i in c(1:length(useable_models))){
    datagrid = cbind(datagrid, predict(useable_models[[i]], datagrid, type = 'response'))
    names = c(names, paste0('model',i))
  }
  datagrid = datagrid[,c(1,7:ncol(datagrid))]
  colnames(datagrid)=c("slope", names)
  return(datagrid)
}


DegGrid=function(useable_models, 
                   obstruction_data = FALSE,
                   heavy_obstruction = FALSE,
                   road = FALSE,
                   paved = FALSE, 
                  hill_slope){
  x = (seq(-50, 50, 1))
  y = rep(hill_slope,101)
  
  datagrid = data.frame(cbind(avg_walking_slope = x,avg_hill_slope = y))
  datagrid$OffRoadObstruction = obstruction_data
  datagrid$OffRoadHeavyObstruction = heavy_obstruction
  datagrid$Road = road
  datagrid$Paved = paved
  
  names = vector()
  for (i in c(1:length(useable_models))){
    datagrid = cbind(datagrid, predict(useable_models[[i]], datagrid, type = 'response'))
    names = c(names, paste0('model',i))
  }
  datagrid = datagrid[,c(1,7:ncol(datagrid))]
  colnames(datagrid)=c("slope", names)
  return(datagrid)
}

#Bonus
x = 0
y =  seq(0,1000,50)
datagrid = expand.grid(avg_walking_slope = x, avg_hill_slope = 0, elevation=y, HeavyObstruction=FALSE)
names = vector()
for (i in c(1:length(useable_models))){
  datagrid = cbind(datagrid, predict(useable_models[[i]], datagrid, type = 'response'))
  names = c(names, paste0('model',i))
}
datagrid = data.frame(datagrid[,-c(1,2,4)])
colnames(datagrid)=c("slope", names)




line_comparison = ggplot(datagrid)+ylim(0,5)
for (i in c(1:length(useable_models))){
  line_comparison = line_comparison +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[i]), colour = (palette()[i]), linetype = 'solid')
}

line_comparison = ggplot(datagrid)+ylim(0,5)
for (i in c(12)){
  line_comparison = line_comparison +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[i]), colour = (palette()[((i+1)%%2)+1]), linetype = 'solid')
}
line_comparison

for (i in c(1:2)){
  line_comparison = line_comparison +  
    geom_line(data = datagrid, aes_string(x = 'slope', y = names[i]), colour = (palette()[(i%%2)+1]), linetype = 'dotted')
}
line_comparison

palette()
