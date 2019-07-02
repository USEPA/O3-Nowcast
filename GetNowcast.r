# Decision tree to figure out which path to take for creating a Nowcast based on data completeness & # of 0's.  

GetNowcast<-function(tdata,rowcount,dataStreamId,rowCountToUseSurrogate=252,op.mode=F){
# op.mode:  set to F for testing on local computeres; T for public reporting based on data from the AirNow database

#rowCountToUseSurrogate <- 252; # 75% completeness
  
#print(paste('DataStreamID =', dataStreamId, 'LST =', max(tdata$LST), 'TotalRowCount =', nrow(tdata), 'NumRowsWithData =', rowcount));
  
# if all values are null, then a NowCast value cannot be generated
if(rowcount == 0) {
    curr.message<-"all values are null. return null"
    out_df = data.frame(DataStreamID = NA, Date = NA, NowCast = NA)
} else if (rowcount < rowCountToUseSurrogate) {
   curr.message<-"does not meet 75% completeness criteria.  Using surrogate instead."
    out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST),NowCast = generateSurrogate(tdata));
  } else if (get.max.na.stream(tdata$Value) > 7) { ## New condition from AR 4/24/19
   curr.message<-"too many consecutive missing values.  Using surrogate instead."
   out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST),NowCast = generateSurrogate(tdata)); 
  } else if(sum(as.numeric(tdata$Value), na.rm = TRUE) == 0) { 
    curr.message<-"all values are 0; returning 0 as nowcast value."
    # this usually indicates a bad instrument, but it can happen
    out_df = data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = 0);
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
      out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = airIndexValue);
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
      out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = airIndexValue);
    } 
    else {
      curr.message<-"No value from previous three hours.  Returning NULL"
      out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = NA);
    }
  } else {
    # Generate a NowCast value
    curr.message<-"Nowcast generated through the PLS method."
    tnowcast <- c()
    for (i in 1:(nrow(tdata)-335)) {
      tpred <- pls.nowcast(tdata$Value[i:(i+335)])
      tnowcast <- c(tnowcast, tpred[!is.na(tpred)])
    }
    
    # Check that NowCast value was generated correctly.
    if(is.na(tnowcast[length(tnowcast)])) {
      curr.message<-"Failed to generate a NowCast value."
      tnowcast <- NA;
    } else if(tnowcast[length(tnowcast)] < 0) {
    curr.message<-"Nowcast was < 0, returning 0."
      #return 0 if generated NowCast value is less than 0
      tnowcast <- 0;
    } else {
      tnowcast <- tnowcast[length(tnowcast)];
    }
    
    out_df <- data.frame(DataStreamID = dataStreamId, Date = max(tdata$LST), NowCast = tnowcast);
  }

#print(curr.message)

## Add message to stored data if possible, comment out the line below if not.
out_df<-data.frame(out_df,Message=curr.message,stringsAsFactors=F)

return(out_df)
}
