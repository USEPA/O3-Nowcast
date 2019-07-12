 
# define calculation functions, including PLS
source("NowcastFun_Upd.r")

# define decision function
source("GetNowcast.r")

# "local" version uses a local file for testing 
source("RunNowcast_local_v2.r")

test.data<-read.table('ExampleData.csv',header=T,fill=T,sep=',',stringsAsFactors=F)

test.data$Value<-as.numeric(test.data$Value)

test.data$Nowcast<-run.nowcast(test.data)
test.data$Nowcast_Message<-run.nowcast(test.data,'Message')

# ExampleData.csv has 336 hours of data - enough for 1 Nowcast calculation.  See bottom of dataset after running for the prediction
print(tail(test.data))


