zero_to_na<-function(x){
      if(is.numeric(x))x[x==0] <- NA 
      x}