

####### User specified parameters #############

# filedir: The location of the beiwe formatted gps data (post interpolation). 
#          This should match the "outdir" variable in InterpolateGPS.py. 
filedir="C:/Users/ianja/Dropbox/MayneGPS/data/Beiweformat" 

# rawdata_filepath: The file path of the original raw GPS data containing all GPS over the sample
rawdata_filepath = "C:/Users/ianja/Dropbox/MayneGPS/data/mayne_aware_export_04202022.csv"

###############################################

library(GPSmobility)
IID_v=list.files(filedir)


GetMaxMinLatLon =function(IID,rawdat){
  rIDs=which(rawdat[,3]==IID)
  minlat=min(rawdat[rIDs,4])
  maxlat=max(rawdat[rIDs,4])
  minlon=min(rawdat[rIDs,5])
  maxlon=max(rawdat[rIDs,5])
  return(c(minlat,maxlat,minlon,maxlon))
}

XY2LatLong = function(mobmat,latlonlims,R = 6.371 * 10^6){
  ph0 = latlonlims[1]
  ph1 = latlonlims[2]
  th0 = latlonlims[3]
  th1 = latlonlims[4]
  d1 = 2 * pi * R * ((ph1 - ph0) * 2 * pi/360)/(2 * pi)
  d2 = 2 * pi * (R * sin(pi/2 - ph1 * 2 * pi/360)) * ((th1 - th0) * 2 * pi/360)/(2 * pi)
  d3 = 2 * pi * (R * sin(pi/2 - ph0 * 2 * pi/360)) * ((th1 - th0) * 2 * pi/360)/(2 * pi)
  outmat = mobmat
  for(i in 1:nrow(mobmat)){
    if(!is.na(mobmat[i,2]) && !is.na(mobmat[i,3])){
      x=mobmat[i,2]
      y=mobmat[i,3]
      w1 = y/ (d1 * sin(acos(abs((d3 - d2)/(2 * d1)))))
      w2 = (x - w1 * abs(d3 - d2)/2)/(d3 * (1 - w1) +d2 * w1)
      outmat[i,2]=w2*(th1 - th0) + th0 #lon-x
      outmat[i,3]=w1*(ph1 - ph0) + ph0 #lat-y
    }
    if(!is.na(mobmat[i,5]) && !is.na(mobmat[i,6])){
      x=mobmat[i,5]
      y=mobmat[i,6]
      w1 = y/ (d1 * sin(acos(abs((d3 - d2)/(2 * d1)))))
      w2 = (x - w1 * abs(d3 - d2)/2)/(d3 * (1 - w1) +d2 * w1)
      outmat[i,5]=w2*(th1 - th0) + th0 #lon-x
      outmat[i,6]=w1*(ph1 - ph0) + ph0 #lat-y
    }
  }
  colnames(outmat)=c("EventType","Latitute_start","Longitude_start","timestamp_start","Latitute_end","Longitude_end","timestamp_end")
  return(outmat)
}

rawdat=read.csv(rawdata_filepath)
for(i in 1:length(IID_v)){
  IID = IID_v[i]
  cat(paste("Processing GPS data for: ",IID,"\n"))
  filename=paste("GPSimp-",substr(IID,0,6),sep="")
  fildir=paste(filedir,"/",IID,sep="")
  try1 = try(MobilityFeatures(filename,fildir), silent = TRUE)
  if (class(try1) == "try-error") {
    cat("Insufficient data.\n\n")
    warning(paste("Insufficient GPS data for ID:",IID))
  }else{
    load(paste(fildir,"/",filename,".Rdata",sep=""))
    imputed_traceXY=SimulateMobilityGaps(mobmat,obj,wtype="GLR",spread_pars=c(10,1))
    save(file=paste(fildir,"/imputedXY-",substr(IID,0,6),".Rdata",sep=""),imputed_traceXY)
    # get min/max lon/lat for IID
    latlonlims=GetMaxMinLatLon(IID,rawdat)
    imputed_traceLatLon=XY2LatLong(imputed_traceXY,latlonlims)
    save(file=paste(fildir,"/imputedLatLon-",substr(IID,0,6),".Rdata",sep=""),imputed_traceLatLon)
  }
  #
}

# LatLong2XY: need to write inverse function

# load saved file, then feed it into


