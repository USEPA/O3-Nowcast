 
# define calculation functions, including PLS
source("NowcastFun_Upd.r")

# define decision function
source("GetNowcast.r")

# "local" version uses a local file for testing 
# and ends at the Nowcast calculation
# Uncomment the following for local testing of Nowcast calculations
source("RunNowcast_local_v2.r")

test.data<-as.data.frame(fread('ExampleOutput_importAirIndexAggregateTasks_GetNextTask.csv'))
test.data$Value<-as.numeric(test.data$Value)

test.data$Nowcast<-run.nowcast(test.data)
test.data$Nowcast_Message<-run.nowcast(test.data,'Message')

print(tail(test.data))

# STI's version connects to the database to get data
# then generates a Nowcast and sends it back to the database
# Uncomment the following for production use
# source(paste0(code.dir,"RunNowcast_STI.r"))

