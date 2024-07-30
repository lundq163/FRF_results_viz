library(abcddata)
setwd("/home/rando149/shared/data/Collection_3165_Supporting_Documentation/abcd-tabulated-data-release-5.1/")
dtf_ABCD_long_form = abcddata_initialize_long_form()
sui_data = abcddata_add.substance_use(dtf_ABCD_long_form)
sui_data$SBS.CHR.CS.ALC.Type_of_use
attr(sui_data$SBS.CHR.CS.ALC.Type_of_use, "abcd_codebook")