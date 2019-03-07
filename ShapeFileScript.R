# Code adapted from tutorial at http://cdmaps.polisci.ucla.edu/tut/mapping_congress_in_R.html

library(sf)

get_congress_map <- function(cong=113) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}

for(i in 81:114){
  shapefile = get_congress_map(i)
  filepath= paste0("Data/districts/shape",i,".Rdata")
  save(shapefile, file = filepath)
}
