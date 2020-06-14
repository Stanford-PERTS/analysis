##########################################################
##
##	Reading and Writing PERTS De-Identified Data Files
##
##	DP December 2013
##
##########################################################

di_data_path	<- "~/Dropbox (PERTS)/PERTS shared data/deidentified_data/"
db_log_path 	<- "~/Dropbox (PERTS)/PERTS shared data/data_log.csv"
repo_log_path 	<- "~/Sites/perts_analyses/data_log.csv"
if(grepl("ericsmith",getwd())) { # Eric's path to dropbox is different...
  db_log_path <- gsub(" (PERTS)","",db_log_path, fixed=T)
  di_data_path  <- gsub(" (PERTS)","",di_data_path, fixed=T)
  repo_log_path <- gsub("Sites/","",repo_log_path, fixed=T)
}

if( grep( "/perts_analyses/",  getwd() ) != 1 ){
	warning("Path to base of perts_analysis should be set as wd")	
}


#	return the requested file using the data_log.csv as a lookup table
#	can return repo version (default) or dropbox version with override 
#	repo version is default because 
read_di_file <- function( study, file, version="repo" ){
	log  <- get_log(version)
	hash <- latest_hash( study, file, log ) 
	path <- paste0(di_data_path,hash,".csv")
	return( read.csv( path, stringsAsFactors=FALSE ) )
}

#	write a new de-identified file and update both log files
write_di_file <- function( df, study, file ){
	log  <- get_log("db")	# get dbox version--should be freshest
	file_hash <- digest( df , algo="md5" ) 
	date <- as.character(Sys.time())
	path <- paste0(di_data_path,file_hash,".csv")
	write.csv( df , file=path, row.names=FALSE)
	
	#	add log entry to dbox and repo
	new_row <- data.frame(study,file,file_hash,date)
	log <- rbind( log , new_row ) 
	write.csv( log , file=repo_log_path, row.names=FALSE)
	write.csv( log , file=db_log_path, row.names=FALSE)
}
		
get_log <- function(version){
	# check to see if we have the most up to date log; warn if not
	repo_log	<- read.csv(repo_log_path, stringsAsFactors=F)
	db_log 		<- read.csv(db_log_path, stringsAsFactors=F)
	if( digest(repo_log) != digest(db_log) ){
		warning("************************************************")
		warning("The repo and dropbox logs don't match!")
		warning("You need to pull, or someone else needs to push!")
	}else{
		message("Repo and Dropbox data logs match. Nice!")	
	}
	if( version %in% "db" ){ return(db_log) }
	else{ return(repo_log) }
}

latest_hash <- function( study, file, log ){
	rows <- log[ log$study %in% study & log$file %in% file, ]
	return( rows[ max(order(rows$date)) , "file_hash" ] )
}

find_old_hashes <- function(){
	log <- read.csv( repo_log_path, stringsAsFactors=FALSE )
	log$number <- 1:nrow(log)
	ordered_log <- log[ 
		order(log$study, log$file, log$number, decreasing=TRUE), 
	]
	old_hashes <- unique(ordered_log[ duplicated(ordered_log[,c("study","file")]) , "file_hash"])
	new_hashes <- unique(ordered_log[! duplicated(ordered_log[,c("study","file")]) , "file_hash"])
	cleaned_old_hashes <- old_hashes[ ! old_hashes %in% new_hashes ]
	return(cleaned_old_hashes)
}

remove_old_files <- function(){
	old_hashes <- find_old_hashes()
	for( old_hash in old_hashes ){
		path <- paste0(di_data_path, old_hash,".csv")
		if (file.exists(path)){
			file.remove(path)
			cat(paste("\nRemoved",path))
		}
	}
}






