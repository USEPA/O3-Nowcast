# import libraries
library(plyr)
library(zoo)
library(pls)

# Define calculation functions used to generate a Nowcast

pls.now.fun<-function(data, curr.mean.data, pls.window=96, what.2.get='pred') {

  pls.window=96;
    
    make.ts.matrix<-function(data,pls.window) {
      
      ts.matrix<-matrix(nrow=length(data),ncol=pls.window)
      
      for (curr.row in (pls.window):length(data)){
        curr.data<-data[(curr.row-pls.window+1):(curr.row)]
        ts.matrix[curr.row,]<-t(matrix(curr.data))
      }
      return(ts.matrix)
    }
    
    curr.reg.data <- data.frame(make.ts.matrix(data, pls.window), curr.mean.data);
    
    colnames(curr.reg.data) <-
      c(paste('Hr', 1:pls.window, sep = ''), 'Mean8Hr')
    curr.reg.data <-
      curr.reg.data[-c(1:(pls.window - 1)), ] # remove the start of the rolling window, so only full PLS windows are used
    curr.reg.data <-
      curr.reg.data[grep(F, is.na(curr.reg.data$Mean8Hr)), ] # only use rows that contain a non-NA rolling mean (y-variable)
    reg.data.rowmeans <-
      apply(curr.reg.data[-ncol(curr.reg.data)], 1, mean, na.rm = T)
    
    ## NRP 6/5/2019: get data for prediction, do not screen for non-NA rolling means
    curr.reg.data2 <- data.frame(make.ts.matrix(data, pls.window), curr.mean.data);   
    colnames(curr.reg.data2) <-
      c(paste('Hr', 1:pls.window, sep = ''), 'Mean8Hr')
    curr.reg.data2 <-
      curr.reg.data2[-c(1:(pls.window - 1)), ] # remove the start of the rolling window, so only full PLS windows are used
    
    curr.form <-
      as.formula(paste('Mean8Hr', paste(
        paste('Hr', 1:pls.window, sep = ''), collapse = '+'
      ), sep = '~'))
    curr.reg <- mvr(curr.form, data = curr.reg.data)
    curr.pls.preds <- predict(curr.reg, newdata = curr.reg.data2)
    curr.reg.data2$pred <- curr.pls.preds[, , dim(curr.pls.preds)[3]]
    curr.pred <- curr.reg.data2$pred[nrow(curr.reg.data2)]
    print(str(curr.pred))
    curr.r2 <-
      round(
        cor(curr.reg.data2$Mean8Hr, curr.reg.data2$pred, use = 'pairwise.complete.obs') **
          2,
        digits = 3
      )
    curr.rmse <-
      sqrt(mean((
        curr.reg.data2$Mean8Hr - curr.pred
      ) ** 2, na.rm = T))
    rm(curr.reg)
    gc()
    
    res<-get(paste('curr',what.2.get,sep='.'))
  
  return(res)
}

pls.nowcast <-
  function(data,
           rollwindow = 336,
           rolling.mean.data,
           pls.window = 96,
           what.2.get = 'pred',
           num.cores = 2,
           show.progress = F) {
    
    # New from AR 4/24/19: run an imputation function on data, 
    # the result of which is fed in to pls.now.fun() in the call below rather than "data"
    imputed.data<-imputeTS::na.ma(data)

    
    result <- do.call('c', llply(rollwindow:length(data),
                                 function(i) {
                                   if (show.progress) {
                                     print(paste(i, length(data), sep = '/'))
                                   }
                                   x <- (i - (rollwindow - 1)):i
                                   res <- pls.now.fun(imputed.data[x], rolling.mean.data[x], pls.window, what.2.get);
                                   return(res);
                                 },
                                 .parallel = F))
    
    result <- c(rep(as.numeric(NA), rollwindow - 1), result)
    return(result)
  }


generateSurrogate<-function(tdata) {
  # hard coded slope and intercept. Future versions may use dynamic values provided by the database
  slope <- 0.85;  
  intercept <- 4.5;
  surrogateValue = NA;
  methodID = 8;
  
  if(!is.na(tail(tdata,1)$Value[1])) { 
    # use current hour if not null
    surrogateValue = tail(tdata,1)$Value[1] * slope + intercept
	  methodID <- 3;
  } else if(!is.na(tail(tdata,2)$Value[1])) {
    # use previous hour if not null
    surrogateValue = tail(tdata,2)$Value[1] * slope + intercept
	  methodID <- 5
  } else if(!is.na(tail(tdata,3)$Value[1])) {
    surrogateValue = tail(tdata,3)$Value[1] * slope + intercept
	  methodID <- 7
  } 
  return(list(surrogateValue=surrogateValue, methodID=methodID));
}

# New from AR on 4/24/19
# Find longest string of missing values in current data stream
get.max.na.stream<-function(x){
  x<-ifelse(is.na(x),-9999,x)
  rle.out<-rle(x)
  missing.locs<-grep(-9999,rle.out$values)
  max.na.length<-max(rle.out$lengths[missing.locs],na.rm=T)
  return(max.na.length)
}


