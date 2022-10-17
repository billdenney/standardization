# Load raw data from .csv file
ae <- read.csv("data-raw/SDTM_example_files/Harvard-directory/ae.csv")
usethis::use_data(ae)

dm <- read.csv("data-raw/SDTM_example_files/Harvard-directory/dm.csv")
usethis::use_data(dm)

pc <- read.csv("data-raw/SDTM_example_files/Harvard-directory/pc.csv")
usethis::use_data(pc)

ex <- read.csv("data-raw/SDTM_example_files/Harvard-directory/ex.csv")
usethis::use_data(ex)

lb <- read.csv("data-raw/SDTM_example_files/Harvard-directory/lb.csv")
usethis::use_data(lb)

eg <- read.csv("data-raw/SDTM_example_files/Harvard-directory/eg.csv")
usethis::use_data(eg)

specs <- "data-raw/specs.xlsx"
usethis::use_data(specs)

