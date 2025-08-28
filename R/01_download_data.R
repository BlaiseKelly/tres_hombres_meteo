# load functions
source("R/functions/get_era.R")

select <- dplyr::select

# get the variables from ecmwf for download
variablez <- c("10m_u_component_of_wind",
               "10m_v_component_of_wind",
               "2m_temperature",
               "mean_sea_level_pressure",
               "sea_surface_temperature",
               "total_precipitation",
               "surface_solar_radiation_downwards",
               "precipitation_type",
               "coefficient_of_drag_with_waves",
               "mean_direction_of_total_swell",
               "mean_direction_of_wind_waves",
               "k_index")

# import gps track of journey
track_sf <- st_read("data/activity_19676867656.gpx", layer = "track_points") |> 
  select(time,geometry) |> 
  st_coordinates()

# import track range and add on a bit so the boats location is not right on the edge
min_lon <- min(track_sf[,1])-1
max_lon <- max(track_sf[,1])+1
min_lat <- min(track_sf[,2])-1
max_lat <- max(track_sf[,2])+1

# generate boundary box for ecmwf request
ecmwf_format_bb <- paste0(min_lat, "/", min_lon, "/", max_lat, "/", max_lon)

path <- "./"

# make requests
vzs <- get_era(bb = ecmwf_format_bb,months = c(6,7), location = "out",
                  variables = variablez, dataset = "single-level", api_key = "", path_out = "./", years = 2025)




