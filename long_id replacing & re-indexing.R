setwd('c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/')
library(plyr)
library(gdata)
library(digest)
library(RecordLinkage)
library(stringr)

"clean" = function(dt, cols, rows, value) {
    if (any(rows)) {
        set(dt, rows, cols, value) }}

old_hospital <- read.csv('Facility list snapshot/facility_list_hospitals_May_06.csv', stringsAsFactors=F)
row.names(old_hospital) <- NULL
old_hospital$order <- as.numeric(row.names(old_hospital))
old_hospital_copy <- old_hospital

old_school <- read.csv('Facility list snapshot/FACILITY_LIST_schools_May_06.csv', stringsAsFactors=F)
row.names(old_school) <- NULL
old_school$order <- as.numeric(row.names(old_school))






new_hospital <- read.csv('facility_list_hospitals.csv', stringsAsFactors=F)
new_school <- read.csv('FACILITY_LIST_schools.csv', stringsAsFactors=F)

dict <- read.csv('Mapping_dict.csv', stringsAsFactors=F)

# Long_id test
# If the output is 0 then long_ids are consistent
test_unmatch_long_id <- function(old_df, new_df, by_val = 'long_id')
{
    test <- match_df(new_df, old_df, on = by_val)
    print(nrow(old_df) - nrow(test))
}

# hospital
test_unmatch_long_id(old_df=old_hospital, new_df=new_hospital)

# school
test_unmatch_long_id(old_df=old_school, new_df=new_school)


# Function for replace long_id with 'new' long_id
long_id_update <- function(old_df, dict)
{
    for (i in 1:nrow(dict))
    {
        old_id <- dict$old_id[i]
        new_id <- dict$new_id[i]
        clean(dt=old_df, cols='long_id', 
              rows=which(old_df$long_id == old_id ), new_id)
        
    }
    return(old_df)
}

# update long_id in old_hospital
old_hospital <- long_id_update(old_hospital, dict=dict)

# Keep testing
test_unmatch_long_id(old_df=old_hospital, new_df=new_hospital)


####
test_unmatched_id_2 <- function(df)
{
    test <- length(which(is.na(df$lga_id)))
    if  (test == 0){
        print("No unmatched long ids in old data")
    }else{
        print(paste("There are", test, "old long IDs to be refreshed", sep=' '))
    }
} 

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


long_id_refresh_pt2 <- function(combined_df, original_copy)
{
    
    #Test result
    print("Before refreshing:")
    test_unmatched_id_2(combined_df)
    
    unmatch_old_long_IDs <- combined_df[is.na(combined_df$lga_id), 'long_id']
    length(unmatch_old_long_IDs)
    
    #Cut donw to #of unmatched old id records
    original_copy <- original_copy[original_copy$long_id %in% unmatch_old_long_IDs, ]
    
    for (l_id in unmatch_old_long_IDs)
    {
        combined_df[combined_df$long_id == l_id, c("lga_id", "zone", "state", "lga", "facility_name", 
                                                   "facility_type", "ward", "community", "managed_by", "short_id", 
                                                   "order")] <- original_copy[original_copy$long_id == l_id, c("lga_id", "zone", "state", "lga", "facility_name", 
                                                                                                                       "facility_type", "ward", "community", "managed_by", "short_id", 
                                                                                                                       "order")]
    }
    print("After refreshing:")
    test_unmatched_id_2(combined_df)
    return(combined_df)
}

hospital <- long_id_refresh_pt2(combined_df=hospital, original_copy=old_hospital_copy)

order_testing <- function(cmobined_df, old_df)
{
    cmobined_df$test <- 1:nrow(cmobined_df)
    test2 <- cmobined_df[!is.na(cmobined_df$order), ]
    
    n1 <- length(which(test2$order != test2$test))
    
    ifelse((n1 != 0), 
           print("The order of records is NOT right"), 
           print("The order of records is Correct"))
    
    
    ifelse(length(which(!is.na(cmobined_df$order))) != nrow(old_df), 
           print("# of records from old data source is not compatiable with orignal one"), 
           print("Number of records is correct"))
    
    n2 <- length(which(is.na(cmobined_df$lga_id)))
    
    ifelse( (n2 == 0), 
            print("All old_long_ids are refreshed"), 
            print( paste("There are still", n2, "long ids are wrong", sep=' ')))
    
}

order_testing(hospital, old_hospital)
order_testing(school, old_school)











