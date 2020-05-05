trip_statitics <- function(data){
  
  distance_ortho_robuste<-function(lat1m,lat2m,lon1m,lon2m){
    R <- 6377726
    dist.m<-R*2*asin(
      ((sin((lat1m*pi/180-lat2m*pi/180)/2))^2+
         cos(lat1m*pi/180)*cos(lat2m*pi/180)*(sin((lon1m*pi/180-lon2m*pi/180)/2))^2)^.5)
    return(dist.m)
    end
  }
  
  cap<-function(lat1,lat2,lon1,lon2){
    
    #Transfo des lat lon en radians
    lat1<-lat1*pi/180
    lat2<-lat2*pi/180
    lon1<-lon1*pi/180
    lon2<-lon2*pi/180
    delta.lon<-lon2-lon1
    
    cap<-atan2(sin(delta.lon)*cos(lat2),
               cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(delta.lon))
    
    #Pour obtenir valeurs entre 0 et 2pi (et pas -pi et +pi)
    cap<-cap%%(2*pi)
    
    #transfo cap radians en cap degres
    cap<-cap*180/pi
    return(cap)
    end
  }
  
  
  
  n1<-nrow(data)
  data$step_duration <- c(NA,diff(data$datetime, units="secs"))
  data$step_length <- c(NA,distance_ortho_robuste(data$lat[1:n1-1],data$lat[2:n1],data$lon[1:n1-1],data$lon[2:n1]))
  data$range <- c(NA,distance_ortho_robuste(data$lat[1],data$lat[2:n1],data$lon[1],data$lon[2:n1]))
  data$step_speed<-data$step_length/data$step_duration
  
  data$cap<-c(NA,cap(data$lat[2:n1],data$lat[1:n1-1],data$lon[2:n1],data$lon[1:n1-1]))
  
  
  # correction when sep_length == 0
  data$step_direction <- c(NA, ifelse(diff(data$cap)%%360 > 180, diff(data$cap)%%360 - 360, diff(data$cap)%%360))
  data$step_direction[data$step_length == 0] <- 0
  
  return(data)
  
}