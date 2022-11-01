library(rgl)
library(visreg)
library(mgcv)
library(caret)
source("Speed formulas.R")

#HIKR DATA
filepath = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/Recreation/Hikr/"
hikrdata = read.csv(paste0(filepath,"Merged/DataPurgedBreaks.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
#OSM DATA
filepath = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/Recreation/Identifiable/"
identifiabledata = read.csv(paste0(filepath,"Merged/DataPurgedBreaks.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
filepath = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/Recreation/Trackable/"
trackabledata = read.csv(paste0(filepath,"Merged/DataPurgedBreaks.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

hikrdata$Segment.No = paste0('H', hikrdata$Segment.No)
identifiabledata$Segment.No = paste0('I', identifiabledata$Segment.No)
trackabledata$Segment.No = paste0('T', trackabledata$Segment.No)
modellingdata = rbind(hikrdata, identifiabledata, trackabledata)

filepath = "/Volumes/LaCie/LaCie/LaCie Rugged USB-C/Scotland Data/Recreation/Modelling/"
write.csv(modellingdata,paste0(filepath,'CombinedDataBreaks.csv'), row.names = FALSE)
modellingdata = subset(modellingdata, modellingdata$OnBreak==0)
write.csv(modellingdata,paste0(filepath,'CombinedData.csv'), row.names = FALSE)
modellingdata2 = read.csv(paste0(filepath,'CombinedData.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
modellingdata2 = read.csv(paste0(filepath,'CombinedDataBreaks.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)


#CROSS VALIDATION

#Create 10 folds in dataset
#folds = createFolds(modellingdata$Segment.No, k = 10, list = TRUE, returnTrain = FALSE)
#save(folds, file="folds.RData")
load("folds.RData")

#Set up results dataframe
nameslist=c("model", "MSE", "RMSE", "MAE", "Rsquared")
modelresults = setNames(data.frame(matrix(ncol = length(nameslist), nrow = 0)), nameslist)

#Get values for existing models
existing_models = c("naismith", "tobler", "irtenkauf", "langmuir")
for (model_name in existing_models){
  truevalues = modellingdata$speed
  predictions = sapply(modellingdata$slopeOS,model_name)
  MSE = mean((truevalues - predictions)^2)
  MAE = mean(abs(truevalues - predictions))
  TSS = sum((truevalues - mean(truevalues))^2)
  RSS = sum((truevalues - predictions)^2)
  Rsquared = 1-(RSS/TSS)
  RMSE = sqrt(MSE)
  modelresults[nrow(modelresults) + 1,] = list(model_name, MSE,RMSE,MAE,Rsquared)
}

modellist=vector()
for (i in seq(3,9,3)){
  for(j in seq(3,9,3)){
    m1 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,'), data=modellingdata[-folds[[i]],], family=gaussian(link="log"))')
    m2 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,'), data=modellingdata[-folds[[i]],], family=gaussian(link="log"))')
    m3 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,', bs="cr"), data=modellingdata[-folds[[i]],], family=gaussian(link="log"))')
    m4 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,', bs="cr"), data=modellingdata[-folds[[i]],], family=gaussian(link="log"))')
    m5 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,'), data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))')
    m6 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,'), data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))')
    m7 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,', bs="cr"), data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))')
    m8 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,', bs="cr"), data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))')
    modellist = c(modellist, m1, m2,m3,m4, m5, m6, m7, m8)
  }
}

m1 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS+I(slopeOS^2),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m2 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS+I(slopeOS^2),data=modellingdata[-folds[[i]],], family=gaussian(link="log"))'
m2 = 'glm(speed~avg_ground_slope_a+slopeOS+I(slopeOS^2),data=modellingdata[-folds[[i]],], family=Gamma(link="inverse"))'
modellist = c(modellist, m1, m2)

count = 0
for (modeltext in modellist){
  count= count + 1
  print(c(count, modeltext))
  MSE = vector()
  MAE = vector()
  Rsquared = vector()
  for (i in c(1:10)){
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
#GAM knots 3 - 9
for (i in seq(3,9)){
  for(j in seq(3,9)){
    m1 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,'), data=modellingdata, family=gaussian(link="log"))')
    m2 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,'), data=modellingdata, family=gaussian(link="log"))')
    m3 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,', bs="cr"), data=modellingdata, family=gaussian(link="log"))')
    m4 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,', bs="cr"), data=modellingdata, family=gaussian(link="log"))')
    m5 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,'), data=modellingdata, family=Gamma(link="inverse"))')
    m6 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,'), data=modellingdata, family=Gamma(link="inverse"))')
    m7 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+s(slopeOS, k=',j,', bs="cr"), data=modellingdata, family=Gamma(link="inverse"))')
    m8 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+s(slopeOS, k=',j,', bs="cr"), data=modellingdata, family=Gamma(link="inverse"))')
    modellist = c(modellist, m1, m2,m3,m4, m5, m6, m7, m8)
  }
}

#GAM 1 linear term
for (i in seq(3,9)){
  m1 = paste0('gam(speed~avg_ground_slope_a+s(slopeOS, k=',i,'), data=modellingdata, family=gaussian(link="log"))')
  m2 = paste0('gam(speed~avg_ground_slope_a+s(slopeOS, k=',i,', bs="cr"), data=modellingdata, family=gaussian(link="log"))')
  m3 = paste0('gam(speed~avg_ground_slope_a+s(slopeOS, k=',i,'), data=modellingdata, family=Gamma(link="inverse"))')
  m4 = paste0('gam(speed~avg_ground_slope_a+s(slopeOS, k=',i,', bs="cr"), data=modellingdata, family=Gamma(link="inverse"))')
  m5 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+slopeOS, data=modellingdata, family=gaussian(link="log"))')
  m6 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+slopeOS, data=modellingdata, family=gaussian(link="log"))')
  m7 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ')+slopeOS, data=modellingdata, family=Gamma(link="inverse"))')
  m8 = paste0('gam(speed~s(avg_ground_slope_a, k=', i, ', bs="cr")+slopeOS, data=modellingdata, family=Gamma(link="inverse"))')
  modellist = c(modellist, m1, m2,m3,m4, m5, m6, m7, m8)
}

#GLM
#linear hill
m1 = 'glm(speed~avg_ground_slope_a+slopeOS,data=modellingdata, family=gaussian(link="log"))'
m2 = 'glm(speed~avg_ground_slope_a+slopeOS+I(slopeOS^2),data=modellingdata2, family=gaussian(link="log"))'
m3 = 'glm(speed~avg_ground_slope_a+slopeOS+I(slopeOS^2)+I(slopeOS^3),data=modellingdata, family=gaussian(link="log"))'
m4 = 'glm(speed~avg_ground_slope_a+slopeOS,data=modellingdata, family=Gamma(link="inverse"))'
m5 = 'glm(speed~avg_ground_slope_a+slopeOS+I(slopeOS^2),data=modellingdata, family=Gamma(link="inverse"))'
m6 = 'glm(speed~avg_ground_slope_a+slopeOS+I(slopeOS^2)+I(slopeOS^3),data=modellingdata, family=Gamma(link="inverse"))'
modellist = c(modellist, m1, m2,m3,m4, m5, m6)
#quadratic hill
m1 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS,data=modellingdata, family=gaussian(link="log"))'
m2 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS+I(slopeOS^2),data=modellingdata, family=gaussian(link="log"))'
m3 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS+I(slopeOS^2)+I(slopeOS^3),data=modellingdata, family=gaussian(link="log"))'
m4 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS,data=modellingdata, family=Gamma(link="inverse"))'
m5 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS+I(slopeOS^2),data=modellingdata, family=Gamma(link="inverse"))'
m6 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+slopeOS+I(slopeOS^2)+I(slopeOS^3),data=modellingdata, family=Gamma(link="inverse"))'
modellist = c(modellist, m1, m2,m3,m4, m5, m6)
#cubic hill
m1 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS,data=modellingdata, family=gaussian(link="log"))'
m2 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS+I(slopeOS^2),data=modellingdata, family=gaussian(link="log"))'
m3 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS+I(slopeOS^2)+I(slopeOS^3),data=modellingdata, family=gaussian(link="log"))'
m4 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS,data=modellingdata, family=Gamma(link="inverse"))'
m5 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS+I(slopeOS^2),data=modellingdata, family=Gamma(link="inverse"))'
m6 = 'glm(speed~avg_ground_slope_a+I(avg_ground_slope_a^2)+I(avg_ground_slope_a^3)+slopeOS+I(slopeOS^2)+I(slopeOS^3),data=modellingdata, family=Gamma(link="inverse"))'
modellist = c(modellist, m1, m2,m3,m4, m5, m6)
rm(m1,m2,m3,m4,m5,m6,m7,m8)

model_collection_full = list()
for (i in (1:length(modellist))){
  print(c(i,modellist[i]))
  gam_collection_full[[i]] = eval(parse(text=modellist[i]))
}
model_collection = model_collection_full

save(model_collection_full, file="all_models.RData")


#Model elimination
#Hill direction
x = 0
y =  c(0:85)
datagrid = expand.grid(slopeOS = x, avg_ground_slope_a = y)
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
datagrid = data.frame(slopeOS = x, avg_ground_slope_a = y)
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
datagrid = data.frame(slopeOS = x, avg_ground_slope_a = y)
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
x = (seq(-21, 21, 1))
y =  (30)
datagrid = expand.grid(slopeOS = x, avg_ground_slope_a = y)
count=2
for (i in c(1:(length(model_collection)))){
  if (!(is.null(model_collection[[i]]))){
    count=count+1
    pred_speed = matrix(predict(model_collection[[i]], datagrid, type = 'response'))
    #horizontal distance needed to gain same height as 1 horizontal unit in slope direction
    dist_required = (tan(datagrid$avg_ground_slope_a*pi/180)/tan(datagrid$slopeOS*pi/180))
    time = abs(dist_required/pred_speed)
    datagrid = cbind(datagrid, pred_speed)
    
    minval=which.min(time[datagrid$slopeOS>0])
    topslope=datagrid[datagrid$slopeOS>0,1][minval]
    datagrid[datagrid$slopeOS>topslope,count]=NA
    
    minval=which.min(time[datagrid$slopeOS<0])
    bottomslope=datagrid[datagrid$slopeOS<0,1][minval]
    datagrid[datagrid$slopeOS<bottomslope,count]=NA
    colnames(datagrid)[count] = i
  }
}
no_critical = colnames(datagrid)[!(is.na(datagrid[1,])) | !(is.na(datagrid[48,])) ][- c(1,2)]
for (i in c(1:length(model_collection))){
  if (i %in% as.numeric(no_critical)){
    model_collection[i] = list(NULL)
  }
}

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

#GLM Only
models=list()
count=0
for (i in c(18:21)){
  count=count+1
  models[[count]]=useable_models[[i]]
}

#Final model
useable_models=list()
useable_models[[1]] = models[[1]]


#Visualisation

open3d()
for (i in c(1:length(useable_models))){
  visreg2d(useable_models[[i]], "slopeOS", "avg_ground_slope_a", plot.type="rgl", scale='response', color = i+2, add = TRUE, lit =FALSE)
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
  datagrid = expand.grid(slopeOS = slopes, avg_ground_slope_a = avgs)
  pred_speed = matrix(predict(useable_models[[i]], datagrid, type = 'response'))
  dist_required = (tan(datagrid$avg_ground_slope_a*pi/180)/tan(datagrid$slopeOS*pi/180))
  time = abs(dist_required/pred_speed)
  datagrid = cbind(datagrid, pred_speed)
  #OPTIMAL
  #minval=which.min(time[datagrid$avg_ground_slope_a==max(avgs)&datagrid$slopeOS>0])
  #topslope=datagrid[datagrid$avg_ground_slope_a==max(avgs)&datagrid$slopeOS>0,1][minval]
  #datagrid[datagrid$slopeOS>topslope,3]=NA
  
  #minval=which.min(time[datagrid$avg_ground_slope_a==max(avgs)&datagrid$slopeOS<0])
  #bottomslope=datagrid[datagrid$avg_ground_slope_a==max(avgs)&datagrid$slopeOS<0,1][minval]
  #datagrid[datagrid$slopeOS<bottomslope,3]=NA
  #------
  datagrid[abs(datagrid$slopeOS)>datagrid$avg_ground_slope_a,]=NA
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


legendBreaks = c("GLM", "Naismith","Tobler","Data Mean", "Data Standard Error")
legendStyle = c("solid", 'longdash', 'twodash', "dashed", "dashed")
legendColours = (c("blue","green", "magenta",'red', 'cyan'))
legendShape = c(1,1,1,1,1)
legendSize = c(2,2,2,2,2)


#Hill Traverse
plotdata = modellingdata[abs(modellingdata$slopeOS)<5,]

#Model data
x = 0
y =  c(0:85)
datagrid1 = expand.grid(slopeOS = x, avg_ground_slope_a = y)
names = vector()
for (i in c(1:length(useable_models))){
  datagrid = cbind(datagrid, predict(useable_models[[1]], datagrid1, type = 'response'))
  names = c(names, paste0('model',1))
}
datagrid = data.frame(datagrid[,-1])
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
  bin_data = plotdata[plotdata$avg_ground_slope_a >= i & +
                        plotdata$avg_ground_slope_a <= i+bin_width, 'speed']
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
hill_traverse = ggplot(plotdata, aes(avg_ground_slope_a, speed)) +
  xlab('Hill Slope (degrees)') +
  ylab('Speed (km/h)') +
  scale_x_continuous(expand = c(0, 0), limits = c(0,60)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) + 
  theme_classic() +
  geom_point(size=0.01, alpha = 0.05) 

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
  geom_line(data = datagrid, aes(x = slope, y = model1, colour = 'GLM', linetype = 'GLM'))

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
plotdata = modellingdata[modellingdata$avg_ground_slope_a-abs(modellingdata$slopeOS)<5,]

#Model data
x = (seq(-80, 80, 1))
y = c(seq(80, 0, -1),seq(1, 80, 1))
datagrid1 = data.frame(cbind(slopeOS = x, avg_ground_slope_a = y))
names = vector()
for (i in c(1:length(useable_models))){
  datagrid = cbind(datagrid, predict(useable_models[[1]], datagrid1, type = 'response'))
  names = c(names, paste0('model',1))
}
datagrid = datagrid[,-2]
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
  bin_data = plotdata[plotdata$slopeOS >= i & +
                        plotdata$slopeOS <= i+bin_width, 'speed']
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
hill_climb = ggplot(plotdata, aes(slopeOS, speed)) +
  xlab('Walking Slope (degrees)') +
  ylab('Speed (km/h)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) + 
  scale_x_continuous(limits = c(-40,40), breaks = seq(-40,40,10)) + 
  theme_classic() +
  geom_point(size=0.01, alpha = 0.05)

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
  geom_line(data = datagrid, aes(x = slope, y = model1, colour = 'GLM', linetype = 'GLM'))


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

