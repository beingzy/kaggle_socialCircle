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
library(reshape2)
library(plyr)
library(ggplot2)
library(stringr)
 
# ########################### #
# SETUP ENVIRONMENT           #
# ########################### #
repo        <- list()
repo$data   <- c("/Users/beingzy/Documents/kaggle/socialNetwork/data/")
repo$output <- c("/Users/beingzy/Documents/kaggle/socialNetwork/output/")
 
filesnm               <- list()
filesnm$egnonets      <- list.files(path=paste(repo$data, "/egonets/", sep=""),  full.names = T)
filesnm$circles_train <- list.files(path=paste(repo$data, "/Training/", sep=""), full.names = T)
 
# ###################### #
# FUNCTION DEFINIITION   #
# ###################### #
getDataPath <- function(filename, dir = data.repo) {
  res <- paste(dir, filename, sep="")
  return(res)
}

featureNormalization <- function(str, sep = ";"){
  # ################################# #
  # Standardize featuer name          #
  # ################################# #
  sep_pos      <- gregexpr(pattern=sep, text=str)[[1]]
  val_sep_pos  <- sep_pos[length(sep_pos)]
  feature_name <- gsub(pattern=";", replacement="_", x=substr(str, start=1, stop=val_sep_pos - 1))
  feature_val  <- substr(x=str, start=val_sep_pos+1, stop=nchar(str))
  res          <- paste(feature_name, ":", feature_val, sep="")
  return(res)
}

# ####################################### #
# Convert the text-based (user&friends)   #
# Relationship data to JSON-ready text    #
# ####################################### #
temp_loop     <- list()
user_relation <- c()
meta_info     <- c()
cat("# Loading egnonets files and turning them into JSON text\n")
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
rm(list = c("temp.i", "temp.j", "temp_loop"))
## Format the meta data information (user_id, total of friends)
meta_info             <- t(meta_info)
meta_info             <- data.frame(meta_info)
colnames(meta_info)   <- c("user_id", "tot_friends")
meta_info$user_id     <- as.character(meta_info$user_id)
meta_info$tot_friends <- as.numeric(meta_info$tot_friends)
## Export the formated information
cat("# Export user connections' JSON text and meta data!\n")
write.table(x=user_relation, file=getDataPath("user_relations_json.txt", dir=repo$output),      sep="\n", row.names=F, col.names=F)
write.table(x=meta_info,     file=getDataPath("user_relations_meta_info.csv", dir=repo$output), sep="\n", row.names=F, col.names=T)
 
# ####################################### #
# Convert circle data into a list of JSON #
# objects                                 #
# ####################################### #
temp_loop  <- list()
circles    <- c()
meta_info  <- c()
cat("# Loading circle files and turning them into JSON text\n")
for(temp.i in 1:length(filesnm$circles_train)) {
  temp_loop$f <- readLines(con=filesnm$circles_train[temp.i])
  for(temp.j in 1:length(temp_loop$f)){
    temp_loop$f_text              <- strsplit(x=temp_loop$f[temp.j], split=":")[[1]]
    temp_loop$circle_id           <- temp_loop$f_text[1]
    temp_loop$circled_user_id_str <- gsub(pattern=" ", replacement=",", str_trim(temp_loop$f_text[2] ))
    temp_loop$circled_user_id_vec <- strsplit(str_trim(temp_loop$f_text[2]), split=" ")[[1]]
    temp_loop$circle_json         <- paste("{circle_id:", temp_loop$circle_id, ",",
                                            "user_id:[", temp_loop$circled_user_id_str, "]}", sep = "")
    circles   <- c(circles, temp_loop$circle_json)
    meta_info <- cbind(meta_info , c(temp_loop$circle_id, length(temp_loop$circled_user_id_vec)))
  }
}
rm(list = c("temp.i", "temp.j", "temp_loop"))
## Format the meta data information (circle_id, number of circled users)
meta_info                 <- t(meta_info)
meta_info                 <- data.frame(meta_info)
colnames(meta_info)       <- c("circle_id", "circled_user_id")
meta_info$circle_id       <- as.character(meta_info$circle_id)
meta_info$circled_user_id <- as.numeric(meta_info$circled_user_id)
## Export the formated information
cat("# Exporting social circles' JSON text and meta data!\n")
write.table(x=circles,   file=getDataPath("socialCircles_json.txt", dir=repo$output),      sep="\n", row.names=F, col.names=F)
write.table(x=meta_info, file=getDataPath("socialCircles_meta_info.csv", dir=repo$output), sep="\n", row.names=F, col.names=T)

# ####################################### #
# Convert Features data into              #
# JSON text                               #
# ####################################### #
featuresDB <- c()
f          <- readLines(con=getDataPath("features.txt", dir=repo$data))
for(temp.i in 1:length(f)){
  row        <- strsplit(f[temp.i], split=" ")[[1]]
  row_idx    <- row[1]
  features   <- paste(sapply(X=row[-1], FUN=featureNormalization), collapse=",")
  res        <- paste("{row_index:", row_idx, ",", features, "}", sep = "")
  featuresDB <- c(featuresDB, res) 
}
write.table(x=as.data.frame(featuresDB), file=getDataPath(filename="featureDB.txt", dir=repo$output), sep="\n", row.names = F, col.names = F)