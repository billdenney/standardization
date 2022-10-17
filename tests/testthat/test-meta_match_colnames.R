
dm <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/dm.csv")
#specs <- "../data-raw/specs.xlsx"
specs<-import_list("../data-raw/specs.xlsx")
cols<-c("STUDYID","USUBJID","SEX","AGE","AGEU","RACE","ETHNIC")
labels<-c("Study Identifier","Unique Subject Identifier","Sex","Age","Age Units","Race","Ethnicity")

assign_var<-function(data,columns,label){

  for(i in seq_along(cols)){
    labelled::var_label(dm[,cols[i]]) <- labels[i]
  }

  return(dm)
}

test_that("meta_match_colnames works", {

dm_mod<-assign_var(dm,columns=cols,label=labels)

match_cols<-meta_match_colnames(dm_mod,specs,c("Demographics","Common"))

expect_setequal(names(match_cols),c("AGE","AGEU","RACE","ETHNIC","STUDYID", "USUBJID"))
expect_error(meta_match_colnames(dm_mod,specs,c("Demographics","Common")),NA)

})
