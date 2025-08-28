# this script imports the pngs for each variable and stiches them into one movie animation
library(magick)

dir.create("out/video/segments/")

# variable short names
varz <- c("ws", "wd", "tp", "t2m_c", "sst_c", "ssrd_W", "mdts", "kx")

# import all the file paths for each
for (v in varz){
  
  v_files <- list.files("out/pngs", pattern = v, full.names = TRUE)[-1]
  
  assign(paste0(v, "_files"),v_files)
  
}
  
# how many frames will the movie have?
n_layrs <- NROW(v_files)

# split up into movie segments to be pieced together later
items <- 1:n_layrs
chunks <- split(items, ceiling(seq_along(items)/5))
#n_layers <- 100
for (c in rev(chunks)){

  lyrz <- c
  
  # read in all the files for the first frame
  for (v in varz){
  
  v_f <- get(paste0(v,"_files"))

  g <- image_read(v_f[lyrz[1]])
  
  assign(paste0("g", which(v == varz)),g)
  
}
  # create first frame
  new_mov <- image_append(c(image_append(c(g1, g2, g3, g7), stack = FALSE), 
                             image_append(c(g4,g5,g6,g8))), stack = TRUE)
  
  # loop through the rest of the layers
  for(i in lyrz[-1]){
    
    for (v in varz){
      
      v_f <- get(paste0(v,"_files"))
  
  g <- image_read(v_f[i])
  
  assign(paste0("g", which(v == varz)),g)
  
  }
  
  # create each frame
  combined <- image_append(c(image_append(c(g1, g2, g3, g7), stack = FALSE), 
                             image_append(c(g4,g5,g6,g8))), stack = TRUE)
  
  new_mov <- c(new_mov, combined)
  print(i)
}

  # create a file for each video chunk
magick::image_write_video(new_mov, framerate = 3, paste0("out/video/", sprintf("%03d", min(lyrz)),"_",sprintf("%03d", max(lyrz)), ".mp4"))

}


