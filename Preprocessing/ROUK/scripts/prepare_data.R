#!/usr/bin/env Rscript

#Script to filter walking gps tracks and combine into 50m segments for use in modelling
walkingSlope = function(df){
  #Calculate walking slope values & set NA to zero
  df$walking_slope = atan(df$elevation_change/df$distance)*180/pi
  df$walking_slope[is.na(df$walking_slope)]=0
  return(df)
}

findbreaks = function(df, getlengths, minbreak=NULL, speedcutoff = Inf){
  # if getlengths = true, find lengths of all breaks in dataframe
  # if getlengths = false, tag all breaks > minbreak or containing speed > speedcutoff with OnBreak = 2

  breaklengths=vector()
  Onbreak=FALSE
  HighSpeedBreak = FALSE
  duration = 0
  for (i in 1:nrow(df)){    #For each segment, loop over the points
    #If breakpoint
    if ((df[ i, 'OnBreak'] == 1) | (df[ i, 'OnBreak'] == 2)) {
      if(duration==0){          #If point is first breakpoint, note start
        Onbreak = TRUE
        BreakStart=i
      }
      #If break is at start of segment, or contains a high speed point, tag it
      if(i == 1 | df [i, 'speed'] >= speedcutoff){
        HighSpeedBreak = TRUE
      }
      duration = duration + df [i, 'duration']  #Calculate break duration
    }

    else {
      #If previously on break
      if(Onbreak) {
        breaklengths = c(breaklengths, duration)  #Save length of previous break
        Onbreak = FALSE
        if (!getlengths){                         #If not finding breaklengths
          if (duration >= minbreak | HighSpeedBreak == TRUE) {   #Tag all breaks above minimum length
            df[BreakStart:(i-1), 'OnBreak'] = 2
          }
        }
        duration = 0
        HighSpeedBreak = FALSE
      }
    }
  }
  #If final points in segment are a break, classify them anyway
  if(Onbreak){
    breaklengths = c(breaklengths, duration)
    Onbreak = FALSE
    if (!getlengths){
      df[BreakStart:i, 'OnBreak'] = 2
    }
  }

  #If getting lengths return vector of breaklengths
  if (getlengths){
    return(breaklengths)
  }
  #Otherwise return dataframe with tagged long breaks
  else{
    return(df)
  }
}

# merge data into intervals, split on variable [type], minimum interval size [condition]
datamerge=function(df,type, condition, terrain_names = list()){

  nameslist=c("WKT", "fid", "Track", "Segment.No", "Start_DateTime", "duration", "distance", "speed","elevation",
              "avg_hill_slope", "avg_walking_slope", "avg_obstruction", "OnBreak", terrain_names)
  merged = setNames(data.frame(matrix(ncol = length(nameslist), nrow = 0)), nameslist)

  variables = list('wkt'='', 'fid'=1,
                   'dateTime'='','duration'=0,'distance'=0,"elevation"=0,
                   "avg_hill_slope"=0, "avg_walking_slope"=0, "avg_obstruction"=0,
                   'OnBreak'= 0)

  variables$dateTime=df$time[1]

  start_point = strsplit(strsplit(df$WKT[1], ',')[[1]][1],'\\(')[[1]][2]
  variables$wkt = paste0("LINESTRING (" , start_point)
  merged_terrain=vector()

  for (i in 1:nrow(df)){
    #If next point is part of a long break, save the previous point to output
    if(df$OnBreak[i]==2){
      if (!variables$OnBreak){

        start_point = strsplit(strsplit(df$WKT[i], ',')[[1]][1],'\\(')[[1]][2]
        variables$wkt = paste0(variables$wkt, ',',start_point, ')')

        if (variables[[type]]<condition){
          added = savePoint(merged, variables, merged_terrain, alteration = TRUE)
        }
        else{
          added = savePoint(merged, variables, merged_terrain, alteration = FALSE)
        }

        merged = added[[1]]
        variables = added[[2]]
        variables$dateTime=df$time[i]
        variables$elevation=df$elevation[i]
        variables$wkt = paste0("LINESTRING (" , start_point)
        merged_terrain=vector()

        variables$OnBreak = 1
      }
      variables$duration = variables$duration + df$duration[i]
      variables$distance = variables$distance + df$distance[i]
    }

    else{
      #If end of break, or threshold met, save break or previous section
      if (variables$OnBreak | variables[[type]]>=condition){
        start_point = strsplit(strsplit(df$WKT[i], ',')[[1]][1],'\\(')[[1]][2]
        variables$wkt = paste0(variables$wkt, ',',start_point, ')')

        added = savePoint(merged, variables, merged_terrain, alteration = FALSE)
        merged = added[[1]]
        variables = added[[2]]
        variables$dateTime=df$time[i]
        variables$elevation=df$elevation[i]
        variables$wkt = paste("LINESTRING (" , start_point, sep='')
        merged_terrain=vector()
      }

      variables$duration = variables$duration + df$duration[i]
      variables$distance = variables$distance + df$distance[i]

      #Calculate slopes and obstruction as weighted average of time spent on slope
      variables$avg_hill_slope = variables$avg_hill_slope + (df$hill_slope[i]*df$duration[i])
      variables$avg_walking_slope = variables$avg_walking_slope + (df$walking_slope[i]*df$duration[i])
      variables$avg_obstruction = variables$avg_obstruction + (df$obstruction[i]*df$duration[i])

      for (item in terrain_names) {
        if (!is.na(df[[item]][i])) {
          merged_terrain=c(merged_terrain,item)
        }
      }
    }
  }
  end_point = strsplit(df$WKT[i], ',')[[1]][2]
  variables$wkt = paste(variables$wkt, end_point, sep=',')

  if (variables$OnBreak || variables[[type]]>=condition){
    added = savePoint(merged, variables, merged_terrain, alteration = FALSE)
  }
  else{
    added = savePoint(merged, variables, merged_terrain, alteration = TRUE)
  }

  merged = added[[1]]
  merged$speed=(merged$distance/1000)/(merged$duration/3600)
  merged$avg_hill_slope = merged$avg_hill_slope / merged$duration
  merged$avg_walking_slope = merged$avg_walking_slope/merged$duration
  merged$avg_obstruction = merged$avg_obstruction/merged$duration
  merged$Track = df$track_name[1]
  merged$Segment.No = df$segment_no[1]
  return(merged)
}

savePoint = function(df, variables, terrain, alteration){

  if(variables$duration >0){
    k = variables$fid

    #If start of segment, or < threshold between two breaks, classify as break
    if (k==1 && alteration){
      variables$OnBreak = 1
      alteration = FALSE
    }
    if (alteration && (df[k-1,]$OnBreak != variables$OnBreak)){
      variables$OnBreak = 1
      alteration = FALSE
    }

    if (alteration) {
      k = k - 1
      df[k,]$WKT = paste(strsplit(df[k,]$WKT, ',')[[1]][1], strsplit(variables$wkt, ',')[[1]][2], sep=",")
      df[k,]$duration = variables$duration + df$duration[k]
      df[k,]$distance = variables$distance + df$distance[k]
      df[k,]$avg_hill_slope = variables$avg_hill_slope + df$avg_hill_slope[k]
      df[k,]$avg_walking_slope = variables$avg_walking_slope + df$avg_walking_slope[k]
      df[k,]$avg_obstruction = variables$avg_obstruction + df$avg_obstruction[k]
    }

    else{
      df[k,]$WKT = variables$wkt
      df[k,]$fid = k
      df[k,]$Start_DateTime = variables$dateTime
      df[k,]$duration = variables$duration
      df[k,]$distance = variables$distance
      df[k,]$elevation = variables$elevation
      df[k,]$avg_hill_slope = variables$avg_hill_slope
      df[k,]$avg_walking_slope = variables$avg_walking_slope
      df[k,]$avg_obstruction = variables$avg_obstruction
      df[k,]$OnBreak = variables$OnBreak
    }

    for (item in unique(terrain)) {
      df[k,][[item]]=1
    }

    variables = list('wkt'= '', 'fid'=k+1,
                     'dateTime'='','duration'=0,'distance'=0,
                     "avg_hill_slope"=0, "avg_walking_slope"=0,'avg_obstruction'=0,
                     'OnBreak'= 0)
  }
  return (list(df, variables))
}
#Filter data using known walking data as criteria

DataRemoval = function(df, known_values){
  #Filter data using known walking data as criteria

  # Find extreme points which separate walking/drving sections
  fids = c(min(df$fid), max(df$fid))
  #DURATION 600 -> 180
  fids = c(fids, df$fid[(df$distance>500 | df$duration>180 | df$speed>100)])
  fids = sort(unique(fids))

  #Segments with extreme speeds
  if(length(fids)>2){
    for (j in (1:(length(fids)-1))){
      #Single points between extremes are set to be a break
      if (length(df$speed[df$fid>=fids[j] & df$fid<=fids[j+1] & df$OnBreak == 0])==1){
        df$OnBreak[df$fid>=fids[j] & df$fid<=fids[j+1] & df$OnBreak == 0]=1
      }
      else if (length(df$speed[df$fid>=fids[j] & df$fid<=fids[j+1] & df$OnBreak == 0])>1){
        quant=quantile(df$speed[df$fid>=fids[j] & df$fid<=fids[j+1] & df$OnBreak == 0])
        #If median speed > upper quartile of known maximum speed, set segment section to be a break
        if (quant[[3]] > known_values$Q3max){
          df$OnBreak[df$fid>=fids[j] & df$fid<=fids[j+1]]=1
        }
      }
    }
  }

  #Ignore Segments where there is less than 2.5min or 250m of useable data
  if ((sum(df$duration[df$OnBreak==0])<=150) | (sum(df$distance[df$OnBreak==0])<=250)) {
    return(FALSE)
  }

  #If median speed > upper quartile of known maximum speed
  #minimum speed > median of known medians
  #upper quartile > whiskers of known max, ignore
  #top whisker < minimum of known upper quartiles, ignore
  to_remove = vector()
  quant=quantile(df$speed[df$OnBreak==0])
  top_whisker = boxplot(df$speed[df$OnBreak==0], plot=FALSE)$stats[5]
  if (quant[3] > known_values$Q3max){
    return(FALSE)
  }
  else if (quant[1] > known_values$medmed){
    return(FALSE)
  }
  else if (quant[4] > known_values$whiskermax){
    return(FALSE)
  }
  else if (top_whisker < known_values$minQ3){
    return(FALSE)
  }
  return(df)
}

highspeedcheck = function(df, highspeed){
  #Find points adjacent to break points, gaps, or start/end of segment where speed > highspeed and mark as break
  fast = df$fid[df$speed>highspeed & df$OnBreak==0]
  for (i in fast){
    #If previous/next point in segment is a break point
    if (any(df$OnBreak[(df$fid == (i+1) | df$fid == (i-1))]==1) |
        #If previous/next point doesnt exist (end of segment or gap in data)
        length(df$speed[(df$fid == (i+1) | df$fid == (i-1))]) < 2){
      df$OnBreak[df$fid == i] = 1
    }
  }
  return(df)
}

prepare = function(path, parameters){
  #Process & filter merged csv track to remove non valid sections

  parameters = lapply(parameters,as.character)
  valid_breaks = parameters$valid_breaks
  combo50 = parameters$combo_50
  processed_breaks_name = parameters$processed_breaks
  processed_name = parameters$processed_output
  short_segments_name = parameters$short_segments_output
  filetype = parameters$filetype
  hikr_path = parameters$hikr_path

  if (filetype != "hikr"){

    hikrdata = read.csv(hikr_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

    known_max_speed = vector()
    known_median_speed = vector()
    known_q3_speed = vector()

    for(i in sort(unique(hikrdata$layer))){
      quants=quantile(hikrdata$speed[hikrdata$layer==i])
      known_max_speed = c(known_max_speed, quants[[5]])
      known_q3_speed = c(known_q3_speed, quants[[4]])
      known_median_speed = c(known_median_speed, quants[[3]])
    }

    HikrValues = list(Q3max = quantile(known_max_speed, 0.75)[[1]],
                      whiskermax = boxplot(known_max_speed, plot=FALSE)$stats[5],
                      medmed = median(known_median_speed),
                      minQ3 = min(known_q3_speed))
    #HikrValues = list(Q3max = 5.869302,whiskermax = 7.490433,medmed = 3.02698,minQ3 = 2.448512)
  }

  AllData = read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

  if (!("obstruction" %in% colnames(AllData))){
    AllData$obstruction = NA
    a=which(colnames(AllData)=='obstruction')
    AllData = AllData[,c(1:10,a,11:(ncol(AllData)-1))]
  }

  #make sure OnBreak is the last column
  a=which(colnames(AllData)=='OnBreak')
  if (a<ncol(AllData)){
    AllData = AllData[,c(1:(a-1),(a+1):ncol(AllData),a)]
  }

  name = basename(path)
  if(ncol(AllData)>12){
    terrain=colnames(AllData)[12:(ncol(AllData)-1)]
  }
  else{
    terrain=list()
  }

  #Calculate walking slope values & set zero movement points to breaks

  AllData=walkingSlope(AllData)
  AllData$OnBreak[is.na(AllData$OnBreak)]=0

  AllData$OnBreak[AllData$distance==0]=1

  #Tag any points with over 1km travel or 180s duration as a fixed break
  AllData$OnBreak[AllData$distance>=1000]=2
  AllData$OnBreak[AllData$duration>=180]=2

  #Tag points >3min duration followed by high-speed point as a fixed break
  onbreak=which((AllData$duration>=180)[-length(AllData$duration)]&(AllData$speed>10)[-1])
  onbreak=c(onbreak, onbreak+1)
  AllData$OnBreak[onbreak]=2

  ##Get lengths of breaks in the data
  #breaklengths = findbreaks(AllData, getlengths = TRUE)

  #Tag all breaks longer than minimum threshold
  AllData = findbreaks(AllData, getlengths = FALSE, minbreak = 30, speedcutoff = 10)
  #Save as new file
  if (!(valid_breaks=="")){
    write.csv(AllData,paste0(valid_breaks,'/',name), row.names = FALSE)
  }
  #Merge data into 50m intervals
  AllData50m = datamerge(AllData, 'distance', 50, terrain)

  #Save as new file
  if (!(combo50=="")){
    write.csv(AllData50m,paste0(combo50,'/',name), row.names = FALSE)
  }

  if (filetype=="hikr"){
    AllData50m = highspeedcheck(AllData50m, highspeed = 10)
    #Remove segments with mean speed above 10km/h
    mean_speed = mean(AllData50m$speed[AllData50m$OnBreak==0])
    if (is.na(mean_speed) | mean_speed > 10){
      return(FALSE)
    }
    AllData50mNoBreaks=subset(AllData50m, AllData50m$OnBreak==0)
  }
  else if (filetype=="osm"){
    #Filter dataset to remove tracks which don't match known walking profile
    AllData50m = DataRemoval(AllData50m, HikrValues)
    if (isFALSE(AllData50m)){
      return(FALSE)
    }
    AllData50mNoBreaks = subset(AllData50m, AllData50m$OnBreak==0)

    #Loop to remove high speed / short points
    current_length = Inf
    while (length(AllData50mNoBreaks[,1])<current_length){
      current_length=length(AllData50mNoBreaks[,1])
      AllData50m = highspeedcheck(AllData50m, highspeed = 10)
      AllData50m = DataRemoval(AllData50m, HikrValues)
      if (isFALSE(AllData50m)){
        return(FALSE)
      }
      AllData50mNoBreaks = subset(AllData50m, AllData50m$OnBreak==0)
    }
  }
  if (!(short_segments_name=="")){
    for (i in which(AllData50m$OnBreak==1)){
      start = AllData50m[i,'Start_DateTime']
      duration = AllData50m[i,'duration']
      j=which(AllData$time==start)
      length=0
      datarange = nrow(AllData)
      while (length<duration & j <= datarange){
        AllData[j,'OnBreak']=2
        length = length + AllData[j,'duration']
        j=j+1
      }
    }
    AllData10m = datamerge(AllData, 'distance', 10, terrain)
    AllData10mNoBreaks = subset(AllData10m, AllData10m$OnBreak==0)
    write.csv(AllData10mNoBreaks,paste0(short_segments_name,'/',name), row.names = FALSE)
  }

  if (!(processed_breaks_name=="")){
    write.csv(AllData50m,paste0(processed_breaks_name,'/',name), row.names = FALSE)
  }
  write.csv(AllData50mNoBreaks,paste0(processed_name,'/',name), row.names = FALSE)
  return(TRUE)
}
