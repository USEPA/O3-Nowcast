
## Redo RunNowcast_local as a function that a rolling stream of data can be fed too, not just a single example 2-week window

run.nowcast<-function(nowdata,what.2.get='NowCast'){
# data should be a data.frame with a least "Value" and "LST" columns
# LST should be some kind of POSIXlt format
#data$LST<-strptime(data$LST,format='%Y-%m-%d %H:%M:%S')

nowdata$LST <- as.POSIXct(nowdata$LST, tz = "UTC", format = "%m/%d/%Y %H:%M")

nowcast.res<-rollapply(1:nrow(nowdata),337,align='right',fill='NA',function(i){

tdata<-nowdata[i,c('Value','LST')]
rowcount <- sum(!is.na(tdata$Value));
# now run decision tree using data and its properties computed above
nowcast.res<-GetNowcast(tdata,rowcount,dataStreamId=1)
return(nowcast.res[[what.2.get]])
})

return(nowcast.res)
}



