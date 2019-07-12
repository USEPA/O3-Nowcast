
## Redo RunNowcast_local as a function that a rolling stream of data can be fed too, not just a single example 2-week window

run.nowcast<-function(nowdata,what.2.get='NowCast'){
# data should be a data.frame with a least "Value" and "LST" columns
# order nowdata by LST prior to applying this function

# LST column should be some kind of POSIXlt format
# Character in the form of %Y-%m-%d %H:%M:%S also works

nowdata$LST <- as.POSIXct(nowdata$LST, tz = "UTC", format = "%m/%d/%Y %H:%M")

nowcast.res<-rollapply(1:nrow(nowdata),336,align='right',fill='NA',function(i){

tdata<-nowdata[i,c('Value','LST')]
rowcount <- sum(!is.na(tdata$Value));
# now run decision tree using data and its properties computed above
nowcast.res<-GetNowcast(tdata,rowcount,dataStreamId=1)
return(nowcast.res[[what.2.get]])
})

return(nowcast.res)
}



