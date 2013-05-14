setwd('c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/')
library(plyr)
library(gdata)
library(digest)
library(RecordLinkage)

old_hospital <- read.csv('Facility list snapshot/facility_list_hospitals_May_06.csv')
old_school <- read.csv('Facility list snapshot/FACILITY_LIST_schools_May_06.csv')

new_hospital <- read.csv('facility_list_hospitals.csv')
new_school <- read.csv('FACILITY_LIST_schools.csv')

t1 <- data.frame(rowname = row.names(new_hospital), short_id = new_hospital$short_id, long_id = new_hospital$long_id)
t2 <- data.frame(rowname = row.names(old_hospital), short_id = old_hospital$short_id, long_id = old_hospital$long_id)


test <- merge(new_hospital, t2, by='long_id', all=T)

which(is.na(test$short_id.y))
which(is.na(test$short_id.x))


old_long <- as.character(test[which(is.na(test$short_id.x)), "long_id"])
new_long <- as.character(test[which(is.na(test$short_id.y)), "long_id"])

pt1 <- old_hospital[which(old_hospital$long_id %in% old_long),]
pt2 <- new_hospital[which(new_hospital$long_id %in% new_long),]

pairs_dist_2 <- function(dataset1, dataset2)
{
    #dataset1 <- pt1
    #dataset2 <- pt2
    
    pair_ids = merge(1:nrow(dataset1), 1:nrow(dataset2), 
                     all = TRUE)
    pair_ids = pair_ids[order(pair_ids[, 1], pair_ids[, 2]), ]
    
    left = dataset1[pair_ids[, 1], , drop = FALSE]
    right = dataset2[pair_ids[, 2], , drop = FALSE]
    patterns = matrix(0, ncol = ncol(left) , nrow = nrow(left))
    
    
    patterns[, 1] <- as.character(left[,1]) == as.character(right[,1])
    patterns[, 2] <- as.character(left[,2]) == as.character(right[,2])
    patterns[, 3] <- as.character(left[,3]) == as.character(right[,3])
    patterns[, 4] <- as.character(left[,4]) == as.character(right[,4])
    patterns[, 5] <- jarowinkler(as.character(left[,5]), as.character(right[,5]))
    patterns[, 6] <- as.character(left[,6]) == as.character(right[,6])
    patterns[, 7] <- jarowinkler(as.character(left[,7]), as.character(right[,7]))
    patterns[, 8] <- jarowinkler(as.character(left[,8]), as.character(right[,8]))
    patterns[, 9] <- as.character(left[,9]) == as.character(right[,9])
    
    pair_score <- cbind(pair_ids, patterns)
    names(pair_score) <- c("id1", "id2", "lga_id", "zone", "state", "lga", "facility_name", "facility_type", 
                           "ward", "community", "managed_by")
    pair_score$total <- rowSums(pair_score[3:11],dims=1,na.rm=T)
    #     rm(list=c("left", "right", "patterns"))
    pair_score <- subset(pair_score, pair_score$lga_id == 1)
    return(pair_score)
}







pair_score <- pairs_dist_2(pt1[,1:9], pt2[,1:9])


highest_score <- function(df)
{
    df <- arrange(df, desc(total))
    df <- df[1, ]
    return(df)
}


candidate <- ddply(pair_score, .(id1), function(x) highest_score(x))

result <- pt2[candidate$id2,]
result$id1 <- candidate$id1


write.csv(result, 'new_dic.csv', row.names=F)
write.csv(pt1, 'old_dic.csv', row.names=F)


row.names(result) <- NULL
result$row <- row.names(result)

result[duplicated(result[,1:11]) | duplicated(result[,1:11], fromLast=T),]


old_dic <- read.csv('./temp/old_dic.csv', stringsAsFactors=F)
new_dic <- read.csv('./temp/new_dic.csv', stringsAsFactors=F)

# test <- cbind(old_dic[,c("lga_id", "facility_name", "long_id")], new_dic[,c("lga_id", "facility_name", "long_id")])

dict <- data.frame(old_id = old_dic$long_id, new_id = new_dic$long_id )

write.csv(dict, "./Mapping_dict.csv", row.names=F)




