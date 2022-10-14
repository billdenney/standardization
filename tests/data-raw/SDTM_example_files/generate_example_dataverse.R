library(devtools)
library(usethis)

# Generate example SDTM datasets here using the Harvard Dataverse datasets.  The
# name of the dataset should be "sdtm_example_dataverse_" followed by the SDTM
# domain.  The long name prefix (sdtm_example_dataverse_) anticipates that
# additional example may be available in the future.

# AE Domain
sdtm_example_dataverse_ae <-
  read.csv(file="data-raw/SDTM_example_files/Harvard-directory/ae.csv", stringsAsFactors=FALSE)
# Confirm that all columns match the expected SDTM type (e.g. character vs
# numeric) here.  To know the class, refer to the SDTM standard for the domain.
sdtm_example_dataverse_ae$AESPID <- as.character(sdtm_example_dataverse_ae$AESPID)
use_data(sdtm_example_dataverse_ae)
str(sdtm_example_dataverse_ae)

#DM Domain
sdtm_example_dataverse_dm <-
  read.csv(file="data-raw/SDTM_example_files/Harvard directory/dm.csv", stringsAsFactors=FALSE)
#Checking class of each variable
str(sdtm_example_dataverse_dm)
#changing the class of mismatching variable
sdtm_example_dataverse_dm$SUBJID <- as.character(sdtm_example_dataverse_dm$SUBJID)
use_data(sdtm_example_dataverse_dm)

#EG Domain
sdtm_example_dataverse_eg <-
  read.csv(file="data-raw/SDTM_example_files/Harvard-directory/eg.csv", stringsAsFactors=FALSE)
#Checking class of each variable
str(sdtm_example_dataverse_eg)
#class of each varible matching with SDTMIG3.2 type
use_data(sdtm_example_dataverse_eg)

#LB Domain
sdtm_example_dataverse_lb <-
  read.csv(file="data-raw/SDTM_example_files/Harvard-directory/lb.csv", stringsAsFactors=FALSE)
#Checking class of each variable
str(sdtm_example_dataverse_lb)
#changing the class of mismatching variable
sdtm_example_dataverse_lb$LBORNRLO <- as.character(sdtm_example_dataverse_lb$LBORNRLO)
sdtm_example_dataverse_lb$LBORNRHI <- as.character(sdtm_example_dataverse_lb$LBORNRHI)
use_data(sdtm_example_dataverse_lb)


#MH Domain
sdtm_example_dataverse_mh <-
  read.csv(file="data-raw/SDTM_example_files/Harvard-directory/mh.csv", stringsAsFactors=FALSE)
#Checking class of each variable
str(sdtm_example_dataverse_mh)
#class of each varible matching with SDTMIG3.2 type
use_data(sdtm_example_dataverse_mh)

#PE Domain
sdtm_example_dataverse_pe <-
  read.csv(file="data-raw/SDTM_example_files/Harvard-directory/pe.csv", stringsAsFactors=FALSE)
#Checking class of each variable
str(sdtm_example_dataverse_pe)
#class of each varible matching with SDTMIG3.2 type
use_data(sdtm_example_dataverse_pe)

#VS Domain
sdtm_example_dataverse_vs <-
  read.csv(file="data-raw/SDTM_example_files/Harvard-directory/vs.csv", stringsAsFactors=FALSE)
#Checking class of each variable
str(sdtm_example_dataverse_vs)
#class of each varible matching with SDTMIG3.2 type
use_data(sdtm_example_dataverse_vs)

