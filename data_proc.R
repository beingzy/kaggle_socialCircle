# ################################################# #
# Kaggle Compeititon for Knowlege, social cycle     #
# Analysis:                                         #
# Reformat data                                     #
#  from: egnonets, cirlces based on text            #
# description                                       #
#  To: JSON                                         #
#                                                   #
#                                                   #
# Author: Yi Zhang                                  #
# Date: Jun/30/2014                                 #
# ################################################# #
library(reshape)
library(plyr)
library(ggplot2)
library(stringr)
 
# ########################### #
# SETUP ENVIRONMENT           #
# ########################### #
repo        <- list()
repo$data   <- c("C:/A-PlayGround/socialCircle/data/")
repo$output <- c("C:/A-PlayGround/socialCircle/output/")
 
filesnm               <- list()
filesnm$egnonets      <- list.files(path="C:/A-PlayGround/socialCircle/data/egonets",  full.names = T)
filesnm$circles_train <- list.files(path="C:/A-PlayGround/socialCircle/data/training", full.names = T)
 
# ###################### #
# FUNCTION DEFINIITION   #
# ###################### #
getDataPath <- function(filename, dir = data.repo) {
  res <- paste(dir, filename, sep="")
  return(res)
}
 
# ####################################### #
# Convert the text-based (user&friends)   #
# Relationship data to JSON-ready text    #
# ####################################### #
temp_loop     <- list()
user_relation <- c()
meta_info     <- c()
for(temp.i in 1:length(filesnm$egnonets)){
  # looping through all files
  temp_loop$f <- readLines(con=filesnm$egnonets[temp.i])
  for(temp.j in 1:length(temp_loop$f)){
    # looping through all users within a files
    temp_loop$f_text         <- strsplit(x=temp_loop$f[temp.j], split=":")[[1]]
    temp_loop$user_id        <- temp_loop$f_text[1]
    temp_loop$friend_user_id <- gsub(pattern=" ", replacement=",", str_trim(temp_loop$f_text[2]))
    temp_loop$item_user_id   <- strsplit(str_trim(temp_loop$f_text[2]), split=" ")[[1]]
    temp_loop$user_json      <- paste("{userid:", temp_loop$user_id,",", 
                                       "connected_userid:[", temp_loop$friend_user_id, "]}", sep = "")
    user_relation <- c(user_relation, temp_loop$user_json)
    meta_info     <- cbind(meta_info , c(temp_loop$user_id, length(temp_loop$item_user_id)))
  }
}
rm(temp_loop)
## Format the meta data information (user_id, total of friends)
meta_info             <- t(meta_info)
meta_info             <- data.frame(meta_info)
colnames(meta_info)   <- c("user_id", "tot_friends")
meta_info$user_id     <- as.character(meta_info$user_id)
meta_info$tot_friends <- as.numeric(meta_info$tot_friends)
## Export the formated information
write.table(x=user_relation, file=getDataPath("user_relations_json.txt", dir=repo$output),      sep="\n", row.names=F, col.names=F)
write.table(x=meta_info,     file=getDataPath("user_relations_meta_info.csv", dir=repo$output), sep="\n", row.names=F, col.names=T)
 
# ####################################### #
# Convert circle data into a list of JSON #
# objects                                 #
# ####################################### #






