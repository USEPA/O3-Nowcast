
# Point to local file to perform the Nowcast prediction with 
data<-as.data.frame(fread('/home/areff/NowCast/Data/ExampleOutput_importAirIndexAggregateTasks_GetNextTask.csv'))
# example data above is from Rose Elementary site (040191032)

data$LST<-strptime(data$LST,format='%Y-%m-%d %H:%M:%S')
data$LST <- as.POSIXct(data$LST, tz = "UTC", format = "%m/%d/%Y %H:%M");
data$Value<-as.numeric(data$Value)

allnowcast <- NULL
dataStreamId <- unique(data$DataStreamID);
tdata <- subset(data, DataStreamID == dataStreamId);
rowcount <- sum(!is.na(tdata$Value));

# now run decision tree using data and its properties computed above

nowcast.res<-GetNowcast(tdata,rowcount,dataStreamId)



