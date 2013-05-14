library(plyr)
library(gdata)
library(digest)
library(RecordLinkage)
library(stringr)

"clean" = function(dt, cols, rows, value) {
    if (any(rows)) {
        set(dt, rows, cols, value) }}

# Long_id test
# If the output is 0 then long_ids are consistent
test_unmatch_long_id <- function(old_df, new_df, by_val = 'long_id')
{
    test <- match_df(new_df, old_df, on = by_val)
    print(nrow(old_df) - nrow(test))
}

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


# Further testing of unmatched data
test_unmatched_id_2 <- function(df)
{
    test <- length(which(is.na(df$lga_id)))
    if  (test == 0){
        print("No unmatched long ids in old data")
    }else{
        print(paste("There are", test, "old long IDs to be refreshed", sep=' '))
    }
} 

# Re-freshing function fill in NA's for those unmatched old data
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

# Test order consistency
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

# Re-generate short_id to make sure the consistency
shortid_generate <- function(df, prefix) 
{ 
    l <- letters
    set.seed(1)
    x <- sample(0:26^4-1, dim(df)[1], replace=F)
    
    digits <- vector(mode="list", length=4)
    tmp <- x
    for (i in 4:1)
    {
        digits[[i]] <- (tmp %% 26) + 1
        tmp <- tmp %/% 26
    }
    df$short_id <- paste0(prefix,':', l[digits[[4]]],l[digits[[3]]],l[digits[[2]]],l[digits[[1]]])
    
    # test that these are unique by lga before returning
    numberofshortids <- length(unique(df$short_id))
    numberoffacilities <- length(df$short_id)
    stopifnot(numberofshortids == numberoffacilities)
    
    return(df) 
}


# Test shor_ids
short_id_test <- function(df, old_df)
{
    n1 <- sum(!is.na(df$order))
    
    print("# of records from old data source in combined data:")
    print(n1)
    print("# of records in old data:") 
    print(nrow(old_df))
    
    n2 <- length(which(df[1:n1, "short_id"] != old_df[, "short_id"]))
    
    ifelse( n2 == 0, 
            print("all short_ids in new data are consistent with those in old data"),
            print(paste("short ids are still NOT consistent")))
    
    n3 <- length(which(duplicated(df$short_id)))
    n4 <- length(which(duplicated(df$long_id)))
    
    print( paste("# of NOT unique short_id:", n3, sep = ' '))
    print( paste("# of NOT unique long_id:", n4, sep = ' '))
}

