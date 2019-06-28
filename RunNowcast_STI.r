#   
# EPA AirNow Ozone NowCast Calculator Beta
# 
# The beta version of the calculator requries the AirNow database for retrieving data
# The final version will be able to use a file as well.
#

list.of.packages <- c("plyr", "zoo", "pls", "RODBCext")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
 
library(RODBCext)
library(uuid) #used to track this worker node

# Generate a worker id for debugging purposes
worker_id <- UUIDgenerate();
print(worker_id);

# connect to AirNow database and retrieve data to process
dbserver <- "sqlstagean1";
dbname <- "AirNowDMC";
mycon <- odbcDriverConnect(paste0('driver={SQL Server};server=', dbserver, ';database=', dbname));
useDB <- TRUE;

# set timezone to GMT to prevent DST bug with converting date time strings
Sys.setenv(TZ = "GMT")

while(TRUE) {
  start_time <- Sys.time() # get start time
  
  if(useDB) {
    print("Retrieve task from the database.");
    data <- sqlExecute(mycon, 'EXEC import.AirIndexAggregateTasks_GetNextTask ?', worker_id, fetch=TRUE, errors=TRUE);
    
    elapsed_time <- Sys.time() - start_time; # calculate difference
    print(paste(elapsed_time, " seconds to retrieve the record."));
    
  } else {
    data <- import("test_nowcast_data.csv")
  }

  if(nrow(data) == 0) {
    print("no records to process. exiting.");
    return();
  }
  
  data$LST <- as.POSIXct(data$LST, tz = "UTC", format = "%m/%d/%Y %H:%M");
  
  allnowcast <- NULL
  
  dataStreamId = unique(data$DataStreamID);
  tdata <- subset(data, DataStreamID == dataStreamId);
 
  rowcount <- sum(!is.na(tdata$Value));
  
# now run decision tree using data and its properties computed above

out_df<-GetNowcast(tdata,rowcount,dataStreamId)

# Is it possible to store the message associated with the Nowcast as well to understand which calculation was used?
  
  print(paste('Saving NowCast value to the database.', dataStreamId, out_df$Date, out_df$NowCast, worker_id));
  sqlExecute(mycon, "EXEC import.DataAirIndex_Save ?, ?, ?, ?", list(out_df$DataStreamID, out_df$Date, out_df$NowCast, worker_id)); 
  
  
  # print elapsed time
  elapsed_time <- Sys.time() - start_time # calculate difference
  print(elapsed_time)


}

