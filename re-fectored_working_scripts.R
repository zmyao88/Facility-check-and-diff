setwd('c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/')
source('C:/Users/zmyao/Documents/GitHub/Facility-check-and-diff/re-fectored_source_functions.R')


##################
# Importing data #
##################

# Old data source from snap shot
old_hospital <- read.csv('Facility list snapshot/facility_list_hospitals_May_06.csv', stringsAsFactors=F)
row.names(old_hospital) <- NULL
old_hospital$order <- as.numeric(row.names(old_hospital))
old_hospital_copy <- old_hospital

old_school <- read.csv('Facility list snapshot/FACILITY_LIST_schools_May_06.csv', stringsAsFactors=F)
row.names(old_school) <- NULL
old_school$order <- as.numeric(row.names(old_school))


# new data just come into formhub
new_hospital <- read.csv('facility_list_hospitals.csv', stringsAsFactors=F)
new_school <- read.csv('FACILITY_LIST_schools.csv', stringsAsFactors=F)

# mapping dictionary for long_ids
dict <- read.csv('Mapping_dict.csv', stringsAsFactors=F)

# Long_id test
# If the output is 0 then long_ids are consistent
# hospital
test_unmatch_long_id(old_df=old_hospital, new_df=new_hospital)

# school
test_unmatch_long_id(old_df=old_school, new_df=new_school)


# update long_id in old_hospital
old_hospital <- long_id_update(old_hospital, dict=dict)

# Keep testing
test_unmatch_long_id(old_df=old_hospital, new_df=new_hospital)


#### Pt2

# Hospital
old_hosp_order <- subset(old_hospital, select=c("long_id" , "order"))
hospital <- merge(new_hospital, old_hosp_order, by='long_id', all=T)
hospital <- arrange(hospital, order, na.last=T)

# Schools
old_sch_order <- subset(old_school, select=c("long_id" , "order"))
school <- merge(new_school, old_sch_order, by='long_id', all=T)
school <- arrange(school, order, na.last=T)


# Test on hospital
test_unmatched_id_2(hospital)

# Test on school, No long_id un-consistency detected
# thus No need to do the re-freshing part downwards for school 
test_unmatched_id_2(school)


# Re-freshing function fill in NA's for those unmatched old data
hospital <- long_id_refresh_pt2(combined_df=hospital, original_copy=old_hospital_copy)

# Test order consistency
order_testing(hospital, old_hospital)
order_testing(school, old_school)


# Re-generate short_id to make sure the consistency
shortid_generate <- function(df, prefix) 

#education
school <- shortid_generate(school, 'F')

#health
hospital <- shortid_generate(hospital, 'F')


# Test shor_ids
short_id_test(school, old_school)
short_id_test(hospital, old_hospital)


# Take out the order column and output
school$order <- NULL
hospital$order <- NULL

write.csv(school, 'FACILITY_LIST_schools.csv', row.names=F)
write.csv(hospital, 'FACILITY_LIST_hospitals.csv', row.names=F)



