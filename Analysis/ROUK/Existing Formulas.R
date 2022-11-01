naismith=function(slope){
  if(is.na(slope)){
   return(NA)
  }
  if(slope<0){
    5
  }
  else{
    1/(0.2+tan(slope*pi/180)*10/6)
  }
}
fast=function(slope){
  if(is.na(slope)){
    return(NA)
  }
  else{
    5-max(slope,0)/18
  }
}
tobler =function(slope){
  if(is.na(slope)){
    return(NA)
  }
  6*exp(-3.5*abs(tan(slope*pi/180)+0.05))
}
irtenkauf = function(slope){
  if(is.na(slope)){
    return(NA)
  }
  (0.11 +  exp(-(slope+5)^2/(2*30^2)))*3.6
}
langmuir=function(slope,y = 5){
  if(is.na(slope)){
    return(NA)
  }
  if(slope > 0){ 
    3*y/(3+5*y*tan(slope*pi/180))
  }
  else if((-5 >= slope) & (slope >= -12)){
    9*y/(9+5*y*tan(slope*pi/180))
  }
  else if(slope < -12){
    9*y/(9-5*y*tan(slope*pi/180))
  }
  else{
    y
  }
}
arnet=function(slope,y,f,d){
  if(is.na(slope)){
    return(NA)
  }
  if(slope<0){
    a=0
  }
  else{
    a=7
  }
  y/(1/f+a*d*tan(slope*pi/180)+(tan(slope*pi/180))^2*7.5)
}

campbell = function(slope, perc){
  s= slope
  if (perc == 50){
    a= -1.4579
    b= 22.0787
    c= 76.3271
    d= 0.0525
    e= -0.00032002
  }
  else if (perc == 75){
    a= -1.2842
    b= 23.1097
    c= 75.7319
    d= 0.1852
    e= -0.0010541
  }
  else if (perc == 95){
    a= -1.7612
    b= 16.4733
    c= 31.5181
    d= 0.7733
    e= -0.0010611
  }
  r=c*(1/(pi*b*(1+((s-a)/b)^2)))+d+(e*s)
  return(r*3.6)
}



SpeedGraph = function(){
  
  x = -80:80
  x1 = c(-80:-13, -12.1)
  x2 = -12:-5
  x3 = c(-4.9, -4:80)

  irt=sapply(x, irtenkauf)
  n=sapply(x,naismith)
  t=sapply(x,tobler)
  aFF = sapply(x,arnet,5,0.9,0)
  aUF = sapply(x,arnet,5,0.9,1)
  aFR = sapply(x,arnet,5,1,0)
  aUR = sapply(x,arnet,5,1,1)
  aFOB = sapply(x,arnet,5,0.4,0)
  aUOB = sapply(x,arnet,5,0.4,1)
  l1=sapply(x1,langmuir,5)
  l2=sapply(x2,langmuir,5)
  l3=sapply(x3,langmuir,5)
  l11=sapply(x1,langmuir,4)
  l22=sapply(x2,langmuir,4)
  l33=sapply(x3,langmuir,4)
  
  existing = data.frame(cbind(x, n, t, 0.6*t))
  lang1 = data.frame(cbind(x1, l1, l11))
  lang2 = data.frame(cbind(x2, l2, l22))
  lang3 = data.frame(cbind(x3, l3, l33))
  
  colnames(existing) = c('angle', 'naismith','tobler','tobler (off-path)')
  colnames(lang1) =c('angle', 'langmuir', 'langmuir (off-path)')
  colnames(lang2) =c('angle', 'langmuir', 'langmuir (off-path)')
  colnames(lang3) =c('angle', 'langmuir', 'langmuir (off-path)')
  
    plot(NULL,xlim=c(-80,80), ylim=c(0,12), ylab='Walking Speed (km/h)', xlab='Slope Angle (degrees)')
  #naismith
  lines(x, n, col=2, lwd=1.5, lty=1)
  #langmuir
  lines(x1,l1,col=4, lwd=1.5, lty = 2)
  lines(x2,l2,col=4, lwd=1.5, lty = 2)
  lines(x3,l3,col=4, lwd=1.5, lty = 2)
  #langmuir off-road
  lines(x1,l11,col="cornflowerblue", lty="dashed")
  lines(x2,l22,col="cornflowerblue", lty="dashed")
  lines(x3,l33,col="cornflowerblue", lty="dashed")
  #tobler
  lines(x, t, col=3, lwd=1.5)
  #tobler off-road
  lines(x,0.6*t,col="forestgreen", lty =2)
  #arnet (flat/uphill)
  #forest
  lines(x,aFF,col="green4")
  lines(x,aUF,col="green3", lty="dashed")
  #road
  lines(x,aFR,col="darkorange3")
  lines(x,aUR,col="darkorange2", lty="dashed")
  #obstruction
  lines(x,aFOB,col="deeppink")
  lines(x,aUOB,col="deeppink4", lty="dashed")
  
  legend(25, 12, legend=c("Naismith", "Langmuir", "Langmuir (off-path)", "Tobler", "Tobler (off-path)"),
         col=c(2, 4, "cornflowerblue", 3, "forestgreen"), lty=c(1, 2, 2, 1, 2), cex=0.8)





  
  p = ggplot(existing, aes(x=angle)) +
    xlab('Walking Slope (degrees)') +
    ylab('Speed (km/h)') +
    scale_x_continuous(expand = c(0, 0), limits = c(-60,60)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) + 
    theme_classic() +
    geom_line(data = lang1, aes(y=langmuir, colour = 'Langmuir', linetype = 'Langmuir')) +
    geom_line(data = lang1, aes(y=`langmuir (off-path)`, colour = 'Langmuir (off-path)', linetype = 'Langmuir (off-path)')) +
    geom_line(data = lang2, aes(y=langmuir, colour = 'Langmuir', linetype = 'Langmuir')) +
    geom_line(data = lang2, aes(y=`langmuir (off-path)`, colour = 'Langmuir (off-path)', linetype = 'Langmuir (off-path)')) +
    geom_line(data = lang3, aes(y=langmuir, colour = 'Langmuir', linetype = 'Langmuir')) +
    geom_line(data = lang3, aes(y=`langmuir (off-path)`, colour = 'Langmuir (off-path)', linetype = 'Langmuir (off-path)')) +
    geom_line(aes(y=naismith, colour = 'Naismith', linetype = 'Naismith')) +
    geom_line(aes(y=tobler, colour = 'Tobler', linetype = 'Tobler')) +
    geom_line(aes(y=`tobler (off-path)`, colour = 'Tobler (off-path)', linetype = 'Tobler (off-path)'))
  
    
  p+scale_linetype_manual(name = "", breaks = c("Naismith", "Tobler","Tobler (off-path)","Langmuir", "Langmuir (off-path)"),values = c('solid', 'twodash', 'dotted', "dashed", "dashed")) +
  scale_color_manual(name = "", breaks = c("Naismith", "Tobler","Tobler (off-path)","Langmuir", "Langmuir (off-path)"),values = ColToGray(c('black', 'red', 'magenta', "blue", "cyan"))) +
    theme(legend.position = c(0.8,0.8), legend.text=element_text(size=12),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12))
  
  
  p+scale_linetype_manual(name = "", breaks = c("Naismith", "Tobler","Tobler (off-path)","Langmuir", "Langmuir (off-path)"),values = c('solid', 'twodash', 'dotted', "dashed", "dashed")) +
    scale_color_manual(name = "", breaks = c("Naismith", "Tobler","Tobler (off-path)","Langmuir", "Langmuir (off-path)"),values = (c('black', 'red', 'magenta', "blue", "cyan"))) +
    theme(legend.position = c(0.8,0.8), legend.text=element_text(size=12),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12))
  
  legend <- get_legend(hill_traverse + theme(legend.box.margin = margin(0, 6, 0, 6)))
  hill_traverse = hill_traverse + theme(legend.position = 'none')
  

}












