file_name <- "ProgAssignment3-data"

target_dir <- paste(getwd(), "/Data/", sep = "")
source_zip_file <- paste(getwd(), paste("/Data/", file_name, ".zip", sep = ""), sep = "")

unzip(source_zip_file, exdir = target_dir)