source("Speed formulas.R")
install.packages("plotrix")
library(plotrix)
library(mgcv)


final_model = model
final_dataset = ROUKDataReduced

final_dataset$modelspeed = predict(final_model, newdata = final_dataset, type="response")
final_dataset$naismithspeed = sapply(final_dataset$avg_walking_slope, naismith)
final_dataset$toblerspeed = sapply(final_dataset$avg_walking_slope, tobler)

final_dataset$modeltime = ((final_dataset$distance/1000)/final_dataset$modelspeed)*3600
final_dataset$naismithtime = ((final_dataset$distance/1000)/final_dataset$naismithspeed)*3600
final_dataset$toblertime = ((final_dataset$distance/1000)/final_dataset$toblerspeed)*3600

combo_model_comparison <- data.frame(matrix(ncol = 5, nrow = 0))
combo_model_comparison = setNames(combo_model_comparison,c('segment', 'duration', 'new', 'naismith','tobler'))
for (i in unique(final_dataset$layer)){
  segment = final_dataset[final_dataset$layer==i, c('duration', 'modeltime', 'naismithtime', 'toblertime')]
  combo_model_comparison[nrow(combo_model_comparison) + 1,] = c(i, colSums(segment))
}
combo_model_comparison[,2:length(combo_model_comparison)] = sapply(combo_model_comparison[,2:length(combo_model_comparison)],as.numeric)
combo_model_comparison = cbind(combo_model_comparison, (combo_model_comparison$duration - combo_model_comparison[,3:length(combo_model_comparison)]))
end = length(combo_model_comparison)
start = end - (length(combo_model_comparison)-2)/2 + 1
error_check = combo_model_comparison[,c(2,start:end)]

detail_speed_comparison = final_dataset[, c('speed', 'modelspeed', 'naismithspeed', 'toblerspeed')]
detail_time_comparison = final_dataset[, c('duration', 'modeltime', 'naismithtime', 'toblertime')]
detail_speed_comparison = cbind(detail_speed_comparison, (detail_speed_comparison$speed - detail_speed_comparison[,2:length(detail_speed_comparison)]))
detail_time_comparison = cbind(detail_time_comparison, (detail_time_comparison$duration - detail_time_comparison[,2:length(detail_time_comparison)]))
end = length(detail_speed_comparison)
start = end - (length(detail_speed_comparison)-1)/2 + 1

error_check = detail_speed_comparison[,c(1,start:end)]

error_check = detail_time_comparison[,c(1,start:end)]
for (i in c(2:length(error_check))){
  if(i==2){
    error_check_99 = error_check[error_check[,i]>=quantile(error_check[,i],0.0005)&error_check[,i]<=quantile(error_check[,i],0.9995),c(1,i)]
  }
  else{
    error_check_99 = cbind(error_check_99, error_check[error_check[,i]>=quantile(error_check[,i],0.0005)&error_check[,i]<=quantile(error_check[,i],0.9995),c(1,i)])
    }
}
perc_error_99=c()
MSE_99=c()
RMSE_99=c()
Rsq_99=c()
for (i in seq(2,length(error_check_99),2)){
  perc_error_99[colnames(error_check_99[i])] = mean(abs(error_check_99[,i])/error_check_99[,i-1]*100)
  MSE_99[colnames(error_check_99[i])] = mean(abs(error_check_99[,i]^2))
  RMSE_99[colnames(error_check_99[i])] = sqrt(MSE_99[colnames(error_check_99[i])])
  TSS = sum((error_check_99[,i-1] - mean(error_check_99[,i-1]))^2)
  RSS = sum(error_check_99[,i]^2)
  Rsq_99[colnames(error_check_99[i])] = 1-(RSS/TSS)
}



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
time_errors_99 = rbind(perc_error_99, MSE_99, RMSE_99, Rsq_99)
combo_errors = rbind(perc_error, MSE, RMSE, Rsq)
combo_errors_99 = rbind(perc_error_99, MSE_99, RMSE_99, Rsq_99)

colnames(speed_errors)[which.min(speed_errors[1,])[[1]]]
colnames(speed_errors)[which.min(speed_errors[2,])[[1]]]
colnames(speed_errors)[which.min(speed_errors[3,])[[1]]]
colnames(speed_errors)[which.max(speed_errors[4,])[[1]]]
colnames(time_errors)[which.min(time_errors[1,])[[1]]]
colnames(time_errors)[which.min(time_errors[2,])[[1]]]
colnames(time_errors)[which.min(time_errors[3,])[[1]]]
colnames(time_errors)[which.max(time_errors[4,])[[1]]]
colnames(time_errors_99)[which.min(time_errors_99[1,])[[1]]]
colnames(time_errors_99)[which.min(time_errors_99[2,])[[1]]]
colnames(time_errors_99)[which.min(time_errors_99[3,])[[1]]]
colnames(time_errors_99)[which.max(time_errors_99[4,])[[1]]]
colnames(combo_errors)[which.min(combo_errors[1,])[[1]]]
colnames(combo_errors)[which.min(combo_errors[2,])[[1]]]
colnames(combo_errors)[which.min(combo_errors[3,])[[1]]]
colnames(combo_errors)[which.max(combo_errors[4,])[[1]]]











end = length(detail_speed_comparison)
start = end - (length(detail_speed_comparison)-1)/2 + 1

RMSE= vector()
bin_width = 10
bins=vector()
for (i in seq(0,50,bin_width)){
  bin_data = detail_speed_comparison[final_dataset$avg_hill_slope >= i & +
                              final_dataset$avg_hill_slope <= i+bin_width & +
                              abs(final_dataset$avg_walking_slope)<5,
                              c(start:end)]
  RMSE = rbind(RMSE, sqrt(colMeans(bin_data^2)))
  bins = c(bins, i+bin_width/2)
}
RMSE = data.frame(cbind(bins, RMSE))

hill_traverse_rmse = ggplot(RMSE, aes(x = bins)) +
  xlab('Hill Slope (degrees)') +
  ylab('RMSE (km/h)') +
  theme_classic() +
  geom_line(aes(y=naismithspeed, colour = "Naismith")) +
  geom_line(aes(y=toblerspeed, colour = "Tobler")) +
  geom_line(aes(y=modelspeed, colour = 'GLM'))

hill_traverse_rmse

RMSE= vector()
bin_width = 10
bins=vector()
for (i in seq(-40,30,bin_width)){
  bin_data = detail_speed_comparison[ +
                                        final_dataset$avg_walking_slope >= i & +
                                        final_dataset$avg_walking_slope <= i+bin_width & +
                                        (final_dataset$avg_hill_slope-abs(final_dataset$avg_walking_slope)<5), +
                                        c(start:end)]
  RMSE = rbind(RMSE, sqrt(colMeans(bin_data^2)))
  bins = c(bins, i+bin_width/2)
}
RMSE = data.frame(cbind(bins, RMSE))

hill_climb_rmse = ggplot(RMSE, aes(x = bins)) +
  xlab('Walking Slope (degrees)') +
  ylab('RMSE (km/h)') +
  theme_classic() +
  geom_line(aes(y=naismithspeed, colour = "Naismith")) +
  geom_line(aes(y=toblerspeed, colour = "Tobler")) +
  geom_line(aes(y=modelspeed, colour = 'GLM'))

hill_climb_rmse
