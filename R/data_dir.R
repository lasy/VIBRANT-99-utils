
data_dir <- function(){
  
  if (str_detect(getwd(), "laurasymul"))
    data_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT data UCLouvain/actual data/"
  else if (str_detect(getwd(), "/laura/")) #vermeren
    data_dir <- "/Users/laura/OneDrive - UCL/11-VIBRANT/VIBRANT data UCLouvain/actual data/"
  else
    stop("You need to specify the path to the data directory in `VIBRANT-99-utils/R/data_dir.R`")
  data_dir
}


dropbox_dir <- function(){
  if (str_detect(getwd(), "laurasymul"))
    dropbox_dir <- "/Users/laurasymul/Dropbox/Academia/Projects/VIBRANT Study Files/"
  else if (str_detect(getwd(), "/laura/"))
    dropbox_dir <- "/Users/laura/Dropbox/VIBRANT Study Files/"
  else
    stop("Hello! You need to specify the path to the VIBRANT Dropbox directory in the function `dropbox_dir` in the VIBRANT-99-utils/R/data_dir.R file.")
  dropbox_dir
}
