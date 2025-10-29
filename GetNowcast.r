# Decision tree to figure out which path to take for creating a Nowcast based on data completeness & # of 0's.  

GetNowcast<-function(tdata,rowcount,dataStreamId,rowCountToUseSurrogate=252,op.mode=T,pls.window=96){
# op.mode:  set to F for testing on local computeres; T for public reporting based on data from the AirNow database

#### 10/12/21:  moving 8-hour mean calculation and related completeness test to here
rolling.mean.data <- rollapply(tdata$Value, list(-4:3), fill = 'NA', align = 'center', FUN = function(x) {
          perc.missing <- length(grep(T, is.na(x))) / length(x)
          res <- NA
          if (perc.missing <= 0.25) {res <- mean(x, na.rm = T)}
          return(res)});

perc.miss.rollmean<-length(grep(T,is.na(rolling.mean.data[-c(1:(pls.window-1))])))/length(rolling.mean.data[-c(1:(pls.window-1))])
####
  
# if all values are null, then a NowCast value cannot be generated
if(rowcount == 0) {
    curr.message<-"all values are null. return null"
    out_df = data.frame(DataStreamID = NA, Date = NA, NowCast = NA, MethodID = 8)
} else if (rowcount < rowCountToUseSurrogate) {
	curr.message<-"does not meet 75% completeness criteria.  Using surrogate instead."
	surrogateData <- generateSurrogate(tdata)
	print(surrogateData)
  out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST),NowCast = surrogateData$surrogateValue, MethodID = surrogateData$methodID);
  } else if (get.max.na.stream(tdata$Value) > 7) { ## New condition from AR 4/24/19
   curr.message<-"too many consecutive missing values.  Using surrogate instead."
   surrogateData <- generateSurrogate(tdata)
   print(surrogateData)
   out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST),NowCast = surrogateData$surrogateValue, MethodID = surrogateData$methodID); 

### 10/12/21 adding the condition for the completeness of the 8-hour means
  } else if (perc.miss.rollmean>0.25) { 
   curr.message<-"too many consecutive missing 8-hour mean values.  Using surrogate instead."
   surrogateData <- generateSurrogate(tdata)
   print(surrogateData)
   out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST),NowCast = surrogateData$surrogateValue, MethodID = surrogateData$methodID); 
########   
   
  } else if(sum(as.numeric(tdata$Value), na.rm = TRUE) == 0) { 
    curr.message<-"all values are 0; returning 0 as nowcast value."
    # this usually indicates a bad instrument, but it can happen
    out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = 0, MethodID = 1);
  } else if(is.na(tdata[nrow(tdata)-0,]$Value)) { # current hour is null
      if(!is.na(tdata[nrow(tdata)-1,]$Value)){ #try to use previous hour
        previousHour = tdata[nrow(tdata)-1,]$LST
        curr.message<-paste("Meets 75% completeness, but missing current hour.  Use previous hour NowCast value instead; ", previousHour)
        # retrieve previous hour's data
        if (!op.mode){airIndexData<-data.frame()}
        if (op.mode){airIndexData <- GetEarlierHour(mycon,dataStreamId,previousHour)}; 
      airIndexValue = NA;
      if(nrow(airIndexData) > 0) {
        airIndexValue = airIndexData$AirIndexValue
      } else {
        curr.message<-"No Value returned."
      }
      out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = airIndexValue, MethodID = 4);
    }
    else if(!is.na(tdata[nrow(tdata)-2,]$Value)) {
      # use value from 2 hours previous
      dt = tdata[nrow(tdata)-2,]$LST
      curr.message<-paste("Meets 75% completeness, but missing current and previous hours.  Using value from 2 hours previous instead; ", dt)
      
      # retrieve previous hour's data
        if (!op.mode){airIndexData<-data.frame()}
        if (op.mode){airIndexData <- GetEarlierHour(mycon,dataStreamId,dt)}; 
      airIndexValue = NA;
      if(nrow(airIndexData) > 0) {
        airIndexValue = airIndexData$AirIndexValue
      } else {
        curr.message<-"No Value returned."
      }
      out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = airIndexValue, MethodID = 6);
    } 
    else {
      curr.message<-"No value from previous three hours.  Returning NULL"
      out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = NA, MethodID = 8);
    }
  } else {
    # Generate a NowCast value
    curr.message<-"Nowcast generated through the PLS method."
    tnowcast <- c()
    methodID <- 2
    for (i in 1:(nrow(tdata)-335)) {
      tpred <- pls.nowcast(tdata$Value[i:(i+335)],rolling.mean.data=rolling.mean.data,pls.window=pls.window)
      tnowcast <- c(tnowcast, tpred[!is.na(tpred)])
    }
    # Check that NowCast value was generated correctly.
    if(length(tnowcast) == 0) {
      curr.message<-"Failed to generate a NowCast value."
      tnowcast <- NA;
      methodID <- 8;
    } else if(tnowcast[length(tnowcast)] < 0) {
    curr.message<-"Nowcast was < 0, returning 0."
      #return 0 if generated NowCast value is less than 0
      tnowcast <- 0;
      methodID <- 1;
    } else {
      tnowcast <- tnowcast[length(tnowcast)];
    }
    
    out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = tnowcast, MethodID = methodID);
  }

print(curr.message)

## Add message to stored data if possible, comment out the line below if not.
out_df<-data.frame(out_df,Message=curr.message,stringsAsFactors=F)

return(out_df)
}
