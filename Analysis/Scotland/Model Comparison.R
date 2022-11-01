source("Speed formulas.R")
install.packages("plotrix")
library(plotrix)
library(mgcv)

useable_models = list(useable_models[[2]])


combo_model_comparison <- data.frame(matrix(ncol = 4+length(useable_models), nrow = 0))
combo_model_comparison = setNames(combo_model_comparison,c('segment', 'duration','naismith','tobler'))

modellingdata$Track = (do.call(rbind,strsplit(modellingdata$Segment.No, "[.]"))[,1])
for (i in unique(modellingdata$Track)){
  segment = modellingdata[modellingdata$Track==i, c('fid', 'duration', 'distance', 'speed', 'slopeOS', 'avg_ground_slope_a')]
  pred_speeds = cbind(sapply(segment$slopeOS, naismith), sapply(segment$slopeOS, tobler))
  for (model in useable_models){
    pred_speeds = cbind(pred_speeds, predict(model, newdata = segment, type="response"))
  }
  pred_times = ((segment$distance/1000)/pred_speeds)*3600
  combo_model_comparison[nrow(combo_model_comparison) + 1,] = c(i, sum(segment$duration), colSums(pred_times))
}
for (i in 2:length(combo_model_comparison)){
  combo_model_comparison[,i] = as.numeric(combo_model_comparison[,i])
}
for (i in c(1:length(useable_models))){
  colnames(combo_model_comparison)[i+4] = i
}
for (i in c(3:length(combo_model_comparison))){
  combo_model_comparison = cbind(combo_model_comparison, (combo_model_comparison$duration - combo_model_comparison[,i]))
  colnames(combo_model_comparison)[length(combo_model_comparison)] = colnames(combo_model_comparison[i])
}




detail_speed_comparison = modellingdata[, c('fid', 'Segment.No', 'slopeOS', 'avg_ground_slope_a', 'speed')]
detail_time_comparison = modellingdata[, c('fid', 'Segment.No', 'slopeOS', 'avg_ground_slope_a', 'duration')]

pred_speed = cbind(sapply(modellingdata$slopeOS, naismith), sapply(modellingdata$slopeOS, tobler))
pred_time = (modellingdata$distance*3.6)/pred_speed
detail_speed_comparison = cbind(detail_speed_comparison, pred_speed)
detail_time_comparison = cbind(detail_time_comparison, pred_time)
detail_speed_comparison = setNames(detail_speed_comparison,c('fid','segment', 'walking slope', 'hill slope', 'speed', 'naismith','tobler'))
detail_time_comparison = setNames(detail_time_comparison,c('fid','segment', 'walking slope', 'hill slope', 'duration', 'naismith','tobler'))

for (i in c(1:length(useable_models))){
  pred_speed = predict(useable_models[[i]], newdata = modellingdata, type="response")
  pred_time = (modellingdata$distance*3.6)/pred_speed
  detail_speed_comparison = cbind(detail_speed_comparison, pred_speed)
  colnames(detail_speed_comparison)[length(detail_speed_comparison)] = i
  detail_time_comparison = cbind(detail_time_comparison, pred_time)
  colnames(detail_time_comparison)[length(detail_time_comparison)] = i
}

for (j in c(6:length(detail_speed_comparison))){
  detail_speed_comparison = cbind(detail_speed_comparison, (detail_speed_comparison$speed - detail_speed_comparison[,j]))
  detail_time_comparison = cbind(detail_time_comparison, (detail_time_comparison$duration - detail_time_comparison[,j]))
  colnames(detail_speed_comparison)[length(detail_speed_comparison)] = colnames(detail_speed_comparison[j])
  colnames(detail_time_comparison)[length(detail_time_comparison)] = colnames(detail_time_comparison[j])
}

end = length(detail_speed_comparison)
start = end - (length(detail_speed_comparison)-5)/2 + 1

error_check = detail_speed_comparison[,c(5,start:end)]
error_check = detail_time_comparison[,c(5,start:end)]

end = length(combo_model_comparison)
start = end - (length(combo_model_comparison)-2)/2 + 1
error_check = combo_model_comparison[,c(2,start:end)]

#avg % error
perc_error = colMeans(abs(error_check[,2:length(error_check)])/error_check[,1]*100)
#MSE
MSE = colMeans(error_check[,2:length(error_check)]^2)
#RMSE
RMSE = sqrt(colMeans(error_check[,2:length(error_check)]^2))
#Rsq
TSS = sum((error_check[,1] - mean(error_check[,1]))^2)
RSS = colSums(error_check[,2:length(error_check)]^2)
Rsq = 1-(RSS/TSS)

speed_errors = rbind(perc_error, MSE, RMSE, Rsq)
time_errors = rbind(perc_error, MSE, RMSE, Rsq)
combo_errors = rbind(perc_error, MSE, RMSE, Rsq)
colnames(speed_errors)[which.min(speed_errors[1,])[[1]]]
colnames(speed_errors)[which.min(speed_errors[2,])[[1]]]
colnames(speed_errors)[which.min(speed_errors[3,])[[1]]]
colnames(speed_errors)[which.max(speed_errors[4,])[[1]]]
colnames(time_errors)[which.min(time_errors[1,])[[1]]]
colnames(time_errors)[which.min(time_errors[2,])[[1]]]
colnames(time_errors)[which.min(time_errors[3,])[[1]]]
colnames(time_errors)[which.max(time_errors[4,])[[1]]]
colnames(combo_errors)[which.min(combo_errors[1,])[[1]]]
colnames(combo_errors)[which.min(combo_errors[2,])[[1]]]
colnames(combo_errors)[which.min(combo_errors[3,])[[1]]]
colnames(combo_errors)[which.max(combo_errors[4,])[[1]]]


RMSE= vector()
bin_width = 10
bins=vector()
for (i in seq(0,65,bin_width)){
  bin_data = detail_speed_comparison[ +
    detail_speed_comparison$`hill slope` >= i & +
    detail_speed_comparison$`hill slope` <= i+bin_width & +
    abs(detail_speed_comparison$`walking slope`)<5,c(5,start:end)]
  RMSE = rbind(RMSE, sqrt(colMeans(bin_data[,2:length(bin_data)]^2)))
  bins = c(bins, i+bin_width/2)
}
RMSE = data.frame(cbind(bins, RMSE))
colnames(RMSE)[4]='GLM'



hill_traverse_rmse = ggplot(RMSE, aes(x = bins)) +
  xlab('Hill Slope (degrees)') +
  ylab('RMSE (km/h)') +
  theme_classic() +
  scale_y_continuous(limits = c(0.8,2.51), expand = c(0,0), 
                     breaks = c(0.8, seq(1,3,0.5)), labels = c(0, seq(1,3,0.5))) + 
  theme(axis.line.y = element_blank(),panel.grid.major.x = element_line(size=.1, color="grey")) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 1, yend = Inf, size=1) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  0.8, yend = 1,linetype = "dotted", color = "black", size=1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,60)) + 
  geom_line(aes(y=naismith, colour = "Naismith", linetype = "Naismith")) +
  geom_line(aes(y=tobler, colour = "Tobler", linetype = "Tobler")) +
  geom_line(aes(y=GLM, colour = 'GLM', linetype = 'GLM')) +
  geom_point(aes(y=naismith,colour = "Naismith", shape="Naismith", size = "Naismith")) +
  geom_point(aes(y=tobler,colour = "Tobler", shape="Tobler", size = "Tobler")) +
  geom_point(aes(y=GLM,colour = "GLM", shape="GLM", size = "GLM"))

hill_traverse_rmse = hill_traverse_rmse +
  scale_linetype_manual(name = "", breaks = legendBreaks, values = legendStyle, labels = function(x) str_wrap(x, width = 5)) +
  scale_color_manual(name = "", breaks = legendBreaks, values = legendColours, labels = function(x) str_wrap(x, width = 5)) +
  scale_shape_manual(name = "", breaks = legendBreaks, values = legendShape, labels = function(x) str_wrap(x, width = 5)) +
  scale_size_manual(name = "", breaks = legendBreaks, values = legendSize, labels = function(x) str_wrap(x, width = 5))
hill_traverse_rmse = hill_traverse_rmse + theme(legend.position = 'none')

hill_traverse_rmse + theme(axis.text=element_text(size=10),axis.title=element_text(size=12),legend.text=element_text(size=12))


RMSE= vector()
bin_width = 10
bins=vector()
for (i in seq(-50,50,bin_width)){
  bin_data = detail_speed_comparison[ +
                           detail_speed_comparison$`walking slope` >= i & +
                           detail_speed_comparison$`walking slope` <= i+bin_width & +
                           (detail_speed_comparison$`hill slope`-abs(detail_speed_comparison$`walking slope`)<5), +
                           c(5,start:end)]
  RMSE = rbind(RMSE, sqrt(colMeans(bin_data[,2:length(bin_data)]^2)))
  bins = c(bins, i+bin_width/2)
}
RMSE = data.frame(cbind(bins, RMSE))
colnames(RMSE)[4]='GLM'


hill_climb_rmse = ggplot(RMSE, aes(x = bins)) +
  xlab('Walking Slope (degrees)') +
  ylab('RMSE (km/h)') +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) + 
  scale_x_continuous(limits = c(-40,40), breaks = seq(-40,40,10)) + 
  theme(panel.grid.major.x = element_line(size=.1, color="grey")) +
  geom_line(aes(y=naismith, colour = "Naismith", linetype = "Naismith")) +
  geom_line(aes(y=tobler, colour = "Tobler", linetype = "Tobler")) +
  geom_line(aes(y=GLM, colour = 'GLM', linetype = 'GLM')) +
  geom_point(aes(y=naismith,colour = "Naismith", shape="Naismith", size = "Naismith")) +
  geom_point(aes(y=tobler,colour = "Tobler", shape="Tobler", size = "Tobler")) +
  geom_point(aes(y=GLM,colour = "GLM", shape="GLM", size = "GLM"))

hill_climb_rmse = hill_climb_rmse +
  scale_linetype_manual(name = "", breaks = legendBreaks, values = legendStyle, labels = function(x) str_wrap(x, width = 5)) +
  scale_color_manual(name = "", breaks = legendBreaks, values = legendColours, labels = function(x) str_wrap(x, width = 5)) +
  scale_shape_manual(name = "", breaks = legendBreaks, values = legendShape, labels = function(x) str_wrap(x, width = 5)) +
  scale_size_manual(name = "", breaks = legendBreaks, values = legendSize, labels = function(x) str_wrap(x, width = 5))
hill_climb_rmse = hill_climb_rmse + theme(legend.position = 'none')


hill_climb_rmse + theme(axis.text=element_text(size=10),axis.title=element_text(size=12),legend.text=element_text(size=12))







legend <- get_legend(hill_traverse + theme(legend.box.margin = margin(0, 6, 0, 6)))
plots = plot_grid(hill_traverse_rmse, hill_climb_rmse, (hill_traverse + theme(legend.position = 'none')), hill_climb, ncol=2, align = "v", labels = c('A', 'C','B','D'))
p <- plot_grid(plots, legend, ncol = 2, rel_widths = c(1, .2))


