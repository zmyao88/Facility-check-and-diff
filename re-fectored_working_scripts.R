setwd('c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/')
source('C:/Users/zmyao/Documents/GitHub/Facility-check-and-diff/re-fectored_source_functions.R')

#^. To Use:
# 1. Change the directory of source function
# 2. Change the directory of old_hospital & old_school to latest snap shot
# 3. save the output into snap shot folder after being refreshed by Myf

##################
# Importing data #
##################

# Old data source from snap shot
old_hospital <- read.csv('Facility list snapshot/FACILITY_LIST_hospitals_full_may_28.csv', stringsAsFactors=F)
row.names(old_hospital) <- NULL
old_hospital$order <- as.numeric(row.names(old_hospital))
old_hospital_copy <- old_hospital

old_school <- read.csv('Facility list snapshot/FACILITY_LIST_schools_full_may_28.csv', stringsAsFactors=F)
row.names(old_school) <- NULL
old_school$order <- as.numeric(row.names(old_school))
old_school_copy <- old_school



# new data just come into formhub
new_hospital <- read.csv('facility_list_hospitals.csv', stringsAsFactors=F)
new_school <- read.csv('FACILITY_LIST_schools.csv', stringsAsFactors=F)

# mapping dictionary for long_ids
dict <- read.csv('Mapping_dict.csv', stringsAsFactors=F)

# Long_id test
# If the output is 0 then long_ids are consistent
# If there are negative value then there's duplicate value in long_id
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

school <- long_id_refresh_pt2(combined_df=school, original_copy=old_school_copy)

# Test order consistency
order_testing(hospital, old_hospital)
order_testing(school, old_school)


# Re-generate short_id to make sure the consistency
#education
school <- shortid_generate(school, 'F')

#health
hospital <- shortid_generate(hospital, 'F')


# Test shor_ids
short_id_test(school, old_school)
short_id_test(hospital, old_hospital)


#Take out the records with duplicated long_ids
school <- subset(school, !duplicated(long_id))
hospital <- subset(hospital, !duplicated(long_id))




# Take out the order column and output
school_diff <- subset(school, is.na(school$order), 
                      select=c("long_id", "lga_id", "zone", "state", "lga", "facility_name", 
                                "facility_type", "managed_by", "ward", "community", "short_id"))
hospital_diff <- subset(hospital, is.na(hospital$order), 
                      select=c("long_id", "lga_id", "zone", "state", "lga", "facility_name", 
                               "facility_type", "managed_by", "ward", "community", "short_id"))

school$order <- NULL
hospital$order <- NULL


write.csv(school_diff, './short_id_fixed/FACILITY_LIST_schools_diff.csv', row.names=F)
write.csv(hospital_diff, './short_id_fixed/FACILITY_LIST_hospitals_diff.csv', row.names=F)

write.csv(school, './short_id_fixed/FACILITY_LIST_schools_full.csv', row.names=F)
write.csv(hospital, './short_id_fixed/FACILITY_LIST_hospitals_full.csv', row.names=F)

