file_name <- "UCI HAR Dataset"
dir_name <- "course3-project"
target_dir <- paste(getwd(), "/Data/", dir_name, sep = "")

if (!file.exists(target_dir)) {
  dir.create(target_dir)
}

source_zip_file <- paste(getwd(), paste("/Data/", file_name, ".zip", sep = ""), sep = "")

unzip(source_zip_file, exdir = target_dir)
