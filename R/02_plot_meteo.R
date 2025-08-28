library(raster)
library(lubridate)
library(dplyr)
library(ncdf4)
library(reshape2)
library(sf)
library(tmap)
library(purrr)
library(stringr)
library(cols4all)
library(basemaps)

##define coordinate systems
latlong = "+init=epsg:4326"
rdnew = "+init=epsg:28992" ## metres coordinate system - Dutch

# create data frame of trip period
start_date <-  "2025-06-20 10:00:00"

end_date <- "2025-07-07 14:00:00"

kr8_d8 <- data.frame(time = seq(
  from=as.POSIXct(start_date, tz="UTC"),
  to=as.POSIXct(end_date, tz="UTC"),
  by="hour"
) )

 # mutate(time_UTC = paste0("X",gsub(" ", ".", gsub("-", ".", time_UTC))))
  

# define the variables that were downloaded
variablez <- c("10m_u_component_of_wind",
               "10m_v_component_of_wind",
               "2m_temperature",
               "mean_sea_level_pressure",
               "sea_surface_temperature",
               "total_precipitation",
               "surface_solar_radiation_downwards",
               "precipitation_type",
                #"coefficient_of_drag_with_waves",
               "mean_direction_of_total_swell",
               "mean_direction_of_wind_waves",
               "k_index")

# loop through and create a raster brick for each variable
d8_list <- list()
nam_list <- list()
for (v in variablez){

    ## find all the variable files
    ECMWF_files <- list.files("out/", pattern = v, full.names = TRUE)
    
    for (e in ECMWF_files){
    
    ECMWF_v <- nc_open(e)
    
    short_nam <- names(ECMWF_v$var)[NROW(ECMWF_v$var)]
    
    longitude <- ncvar_get(ECMWF_v, "longitude")
    latitude <- ncvar_get(ECMWF_v, "latitude")
    
    ##create min x and y
    x_min <- min(longitude)
    x_max <- max(longitude)
    y_min <- min(latitude)
    y_max <- max(latitude)
    ##determine number of lat and lon points
    ny <- NROW(latitude)
    nx <- NROW(longitude)
    
    d_ecmwf <- raster(xmn=x_min, xmx=x_max, ymn=y_min, ymx=y_max, nrows=ny, ncols = nx, vals = 1, crs = 4326)
    
    bb <- extent(d_ecmwf)
    
    thyme <- ncvar_get(ECMWF_v, "valid_time")
    #d8 <- date(ymd_h(as.POSIXct(thyme, origin="1900-01-01 00:00")))
    d8 <- lubridate::ymd("1970-01-01") + lubridate::seconds(thyme)
    
    # d8_in <- colsplit(d8, " ", c("date", "time")) %>%
    #   mutate(hour_no = seq(1:NROW(d8))) %>% 
    #   filter(date %in% kr8_d8)
    
    ##extract no2 variables for entire domain for 1 time step and altitude
    var_in <- ncvar_get(ECMWF_v, short_nam, start = c(1,1, 1), count = c(nx,ny, NROW(d8)))
    
    nc_close(ECMWF_v)
    ## brick it
    ru <- t(brick(var_in))
    
    
    ##define the extent
    extent(ru) <- bb
    #ru <- setExtent(ru, bb,  keepres=FALSE)
    ##define the crs
    raster::crs(ru) <- 4326
    
    names(ru) <- as.character(d8)
    
    if(unique(month(d8))==6){
      ru <- ru[[467:720]]
    } else {
      ru <- ru[[1:159]]
    }
    
    assign(paste0(short_nam, "_", month(d8)), ru)
    
    d8_list[[e]] <- data.frame(format(d8))
  
    print(paste(e, sep = "_"))

    }
    

    nam_list[[v]] <- data.frame(v,short_nam)

}

# make a df of the short variable names
nams_df <- do.call(rbind,nam_list)

# convert J to W
ssrd_W_6 <- ssrd_6/3600
ssrd_W_7 <- ssrd_7/3600

# so night time plot is empty make 0 = NA
values(ssrd_W_6)[values(ssrd_W_6)<1] <- NA
values(ssrd_W_7)[values(ssrd_W_7)<1] <- NA

# function for calculating wd from u and v
windDir <-function(u,v){
  (270-atan2(u,v)*180/pi)%%360 
}

# calculate ws from u and v
ws_6 = sqrt(u10_6^2 + v10_6^2)
# calc wd
wd_6 = windDir(u = u10_6, v = v10_6)

ws_7 = sqrt(u10_7^2 + v10_7^2)
wd_7 = windDir(u = u10_7, v = v10_7)

# add the dates back into the layers
names(ws_6) <- names(u10_6)
names(ws_7) <- names(u10_7)
names(wd_6) <- names(u10_6)
names(wd_7) <- names(u10_7)

# convert temp to deg C
t2m_c_6 <- t2m_6-273.14
t2m_c_7 <- t2m_7-273.14

# same for surface temp
sst_c_6 <- sst_6-273.14
sst_c_7 <- sst_7-273.14

# convert rain from m to mm
tp_6 <- tp_6*1000
tp_7 <- tp_7*1000

# import the track, round dates to hour, remove duplicates and add a column of UTC to match with ECMWF data
track_sf <- st_read("data/activity_19676867656.gpx", layer = "track_points") |> 
  select(time,geometry) |> 
  mutate(time =as.POSIXct(floor_date(time, unit = "hour")), tz="CET") |> 
  distinct(time, .keep_all = TRUE) |> 
  mutate(time_UTC = with_tz(time, "UTC"))
  
# I used this to manually go through each parameter and guage the breaks which are in the table below
  bks_min <- min(mdww_6@data@min, mdww_7@data@min)
  bks_max <- max(mdts_6@data@max, mdts_7@data@max)

# import a background map to use for each plot
bg <- basemaps::basemap_raster(ext=kx_6, map_service = "carto", map_type = "light")
#for (l in 1:nlayers(kx_6)){

# create parameters for each plot
meteo_vars <- data.frame(met_var = c("ws", "wd", "tp", "t2m_c", "sst_c", "ssrd_W", "mdww", "mdts", "kx"),
                         pal = c("kovesi.linear_yl_rd_bk","hcl.dark3", "brewer.blues","viridis.turbo", "tol.incandescent", "viridis.inferno",
                                 "hcl.dark3","hcl.dark3", "poly.glasbey"),
                         bks_min = c(0,0,0,0,11,0,0,0,0),
                         bks_max = c(16,360,12,40,30,1000,360, 360,0),
                         bks_int = c(2,45,0.5,3,2,100,45,45,0),
                         labels = c("FALSE", "TRUE","FALSE", "FALSE","FALSE", "FALSE","TRUE", "TRUE","TRUE"),
                         title = c("wind speed (m/s)", "wind direction (degrees)", "total precipitation\nin grid cell (mm)",
                                  "air temperature (°C)", "sea surface\ntemperature (°C)", "solar surface\nirradiation downwards (W/m2)",
                                  "mean direction\nwind waves (degrees)", "mean direction\ntidal surge (degrees)", "k factor\n(thunderstorm chance)"))

dir.create("out/pngs")

# loop through and generate an image for each hour for each variable
for (m in meteo_vars$met_var){
  
  met_df <- filter(meteo_vars, met_var == m)
  
  bks <- seq(met_df$bks_min, met_df$bks_max, by = met_df$bks_int)
  
  if(m == "tp"){
    bks <- c(0,0.01,0.02,0.04,0.08,0.15,0.3,0.6,1.2,2.4,5,10,15)
  }
  
  pal <- cols4all::c4a(met_df$pal, NROW(bks))
  
  for (ss in c(6,7)){
    
    r <- get(paste0(m, "_", ss))
  
  for (l in 1:nlayers(r)){
    
    meteo_UTC <- ymd_hms(gsub("X", "",names(r[[l]])))
    
    if(is.na(meteo_UTC)){
    meteo_UTC <- ymd_hms(paste0(gsub("X", "",names(r[[l]])),"00:00:00"))
    }
    
    closest_index <- which.min(abs(track_sf$time_UTC - meteo_UTC))
    
    # Retrieve the closest time
    pnt <- track_sf[closest_index,]
    
    meteo_CET <- format(meteo_UTC+7200)

    if(met_df$labels == TRUE){
      
      if(m == "kx"){
      
      labelz <- c("No thunderstorm",
                     "Isolated thunderstorms",
                     "Widely scattered thunderstorms",
                     "Scattered thunderstorms",
                     "Numerous thunderstorms")
      
      
      bks <- c(-Inf, 20, 25, 30, 35, Inf)  # include -Inf and Inf to capture all
      
      pal <- cols4all::c4a(met_df$pal, NROW(bks))
      
      } else {
      
      labelz <- c("N",
                     "NE",
                     "E",
                     "SE",
                     "S",
                     "SW",
                     "W",
                     "NW")
      }
    
p1 <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(r[[l]])+
  tm_raster(palette = pal, alpha = 0.5, breaks = bks, labels = labelz, title = met_df$title, legend.reverse = TRUE)+
  tm_shape(pnt)+
  tm_dots(col = 'orange', size = 0.7,title = "Tres Hombres", legend.show = TRUE)+
  tm_layout(legend.outside = FALSE, title.size = 1, panel.show = FALSE, main.title = meteo_CET, legend.bg.color = "white",
            bg.color = "white", legend.frame = TRUE)+
  tm_legend(position = c("right", "bottom"))
} else {

p1 <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(r[[l]])+
  tm_raster(palette = pal, alpha = 0.5, breaks = bks, title = met_df$title, legend.reverse = TRUE)+
  tm_shape(pnt)+
  tm_dots(col = 'orange', size = 0.7, title = "Tres Hombres", legend.show = TRUE)+
  tm_layout(legend.outside = FALSE, title.size = 1, panel.show = FALSE, main.title = meteo_CET, legend.bg.color = "white",
            bg.color = "white", legend.frame = TRUE)+
  tm_legend(position = c("right", "bottom"))


}
    
    file_nam <- str_sub(gsub(" ", "_", gsub(":", "", gsub("-", "", meteo_CET))),1,-3)

tmap_save(p1, filename = paste0("out/pngs/",m,"_", file_nam,".png"))

print(file_nam)
  }
    
  }
  
}

