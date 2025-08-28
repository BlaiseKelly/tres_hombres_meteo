# this script imports the movie chunks written out in previous script into one file

library(av)

dir.create("out/video/final/")

videos<-list.files("out/video/segments/", full.names = TRUE)

#encoding
av_encode_video(
  videos,
  output=paste0("out/video/final/output.mp4"),
  framerate=1,
  vfilter="null",
  codec="libx264rgb",
  audio=NULL,
  verbose=TRUE
)
