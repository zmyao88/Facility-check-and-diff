setwd('c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/')
library(plyr)
library(gdata)
library(digest)
library(RecordLinkage)

"clean" = function(dt, cols, rows, value) {
    if (any(rows)) {
        set(dt, rows, cols, value) }}

old_hospital <- read.csv('Facility list snapshot/facility_list_hospitals_May_06.csv', stringsAsFactors=F)
old_school <- read.csv('Facility list snapshot/FACILITY_LIST_schools_May_06.csv', stringsAsFactors=F)

new_hospital <- read.csv('facility_list_hospitals.csv', stringsAsFactors=F)
new_school <- read.csv('FACILITY_LIST_schools.csv', stringsAsFactors=F)

dict <- read.csv('Mapping_dict.csv', stringsAsFactors=F)

# Hospital
test <- merge(old_hospital, new_hospital, by = 'long_id')
nrow(old_hospital) - nrow(test)

# School
test <- merge(old_school, new_school, by = 'long_id')
nrow(old_school) - nrow(test)


for (i in 1:nrow(dict))
{
    old_id <- dict$old_id[i]
    new_id <- dict$new_id[i]
    clean(dt=old_hospital, cols='long_id', 
          rows=which(old_hospital$long_id == old_id ), new_id)
    
}

row.names(old_hospital) <- NULL
old_hospital$order <- row.names(old_hospital)




