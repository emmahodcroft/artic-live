args = commandArgs(trailingOnly=TRUE)

orig_meta <- args[1]
orig_seqs <- args[2]
new_meta <- args[3]
new_seqs <- args[4]


####
# This code processes the output from `augur parse` to convert dates to the format that `augur` can take
# (YYYY-MM-DD including XX for uncertainty) and de-duplicating (in a very simple way) strain names
# If you have more than pairs of duplicates (ex triplets) you'll need to modfiy the script to do better renaming :)

#library(lubridate)
library(seqinr)

#This is silly but ended up doing it manually due to not getting a particular
#R library to work on the cluster :)
replace_months <- function(date_str) {
  new_date <- gsub("Jan", "01", date_str)
  new_date <- gsub("Feb", "02", new_date)
  new_date <- gsub("Mar", "03", new_date)
  new_date <- gsub("Apr", "04", new_date)
  new_date <- gsub("May", "05", new_date)
  new_date <- gsub("Jun", "06", new_date)
  new_date <- gsub("Jul", "07", new_date)
  new_date <- gsub("Aug", "08", new_date)
  new_date <- gsub("Sep", "09", new_date)
  new_date <- gsub("Oct", "10", new_date)
  new_date <- gsub("Nov", "11", new_date)
  new_date <- gsub("Dec", "12", new_date)
}

meta <- read.csv(orig_meta, as.is=T)

#### Fix any duplicates
dups <- which(duplicated(meta$strain))
dup_names <- meta$strain[dups]
# add 2 to dedup (hopefully no more than 2....)
new_dup_names <- paste(dup_names, "2", sep="")
#replace in the metadata
meta$strain[dups] <- new_dup_names

#now replace in sequences
seqs <- read.fasta(orig_seqs, forceDNAtolower = FALSE)
names <- attr(seqs, "name")

seq_dups <- which(duplicated(names))
seq_dup_names <- names[seq_dups]
new_seq_dup_names <- paste(seq_dup_names, "2", sep="")
#replace in sequence names
names[seq_dups] <- new_seq_dup_names

write.fasta(seqs, names, file.out=new_seqs, nbchar=10000)

#### Now do dates

dates <- meta$raw_date

#find normal ones
dat <- which(grepl("^[[:digit:]]{4}[-][[:digit:]]{2}[-][[:digit:]]{2}$", dates))
if(length(dat)!=0){
  datt <- as.character(as.Date(dates[dat], format="%Y-%m-%d"))
  dates[dat] <- datt
}

#get those Jul-2005 or similar
dat <- which(grepl("^[[:alpha:]]+[-][[:digit:]]{4}$", dates))
if(length(dat)!=0){
  dates_to_change <- dates[dat]
  for(i in 1:length(dates_to_change)){
      dates_to_change[i] <- replace_months(dates_to_change[i])
  }
  #datt <- as.character(parse_date_time(dates[dat],"m-Y"))
  datt <- as.Date(paste("01-",dates_to_change,sep=""), format="%d-%m-%Y")
  datt <- gsub("-01$", "-XX", datt) 
  dates[dat] <- datt
}

#get those just year
dat <- which(grepl("^[[:digit:]]{4}$", dates))
if(length(dat)!=0){
  dates_to_change <- dates[dat]
  datt <- paste(dates_to_change,"-XX-XX", sep="")
  #datt <- as.character(parse_date_time(dates[dat],"Y"))
  #datt <- gsub("-01", "-XX", datt) 
  dates[dat] <- datt
}

#fill empty ones
dat <- which(dates == "")
dates[dat] <- "20XX-XX-XX"

#just to test
cbind(meta$raw_date, dates)

date <- dates

meta <- cbind(meta,date)

#find any columns that contain quotes
commas <- which(sapply(meta, function(y) any(grepl(",",y))))
#only put quotes on those
write.csv(meta, new_meta, quote=commas, row.names=F)
