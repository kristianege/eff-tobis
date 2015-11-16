
#ALL_DATA_larvae_id 
id<-(ALL_DATA_larvae_id$larvae_id)

OUT = array(0,dim = c(length(id),10))

for (i in 1:length(id)) {
  d<-subset(ALL_DATA_larvae_id, larvae_id==id[i])
  
  OUT[i,1] <- as.character(d$larvae_id)
  OUT[i,2] <- as.character(d$KorTobis_sample_id)

  if(NA %in% d$n_1_h && NA %in% d$n_1_v){
    OUT[i,3] <- "NA" 
    OUT[i,4] <- "NA"
    OUT[i,5] <- "NA"
    OUT[i,6] <- "NA"
    OUT[i,7] <- "NA"
    OUT[i,8] <- "NA"
    OUT[i,9] <- "NA"
    OUT[i,10] <- "NA"
    
  } else if(NA %in% d$n_1_h){
    
    OUT[i,3] <- "v" 
    OUT[i,4] <- d$int_1_v
    OUT[i,5] <- d$slope_1_v
    OUT[i,6] <- d$r2_1_v
    OUT[i,7] <- d$n_1_v
    OUT[i,8] <- d$radius_1_v
    OUT[i,9] <- d$age_1_v
    OUT[i,10] <- as.character(d$pic_id_1_v)
    
  }  else if (NA %in% d$n_1_v){
    
    
    OUT[i,3] <- "h" 
    OUT[i,4] <- d$int_1_h
    OUT[i,5] <- d$slope_1_h
    OUT[i,6] <- d$r2_1_h
    OUT[i,7] <- d$n_1_h
    OUT[i,8] <- d$radius_1_h
    OUT[i,9] <- d$age_1_h
    OUT[i,10] <- as.character(d$pic_id_1_h)
    
  } else if (d$n_1_v > d$n_1_h | d$n_1_v==d$n_1_h && d$age_1_v > d$age_1_h){  
    OUT[i,3] <- "v" 
    OUT[i,4] <- d$int_1_v
    OUT[i,5] <- d$slope_1_v
    OUT[i,6] <- d$r2_1_v
    OUT[i,7] <- d$n_1_v
    OUT[i,8] <- d$radius_1_v
    OUT[i,9] <- d$age_1_v
    OUT[i,10] <- as.character(d$pic_id_1_v)
  } else {
    OUT[i,3] <- "h" 
    OUT[i,4] <- d$int_1_h
    OUT[i,5] <- d$slope_1_h
    OUT[i,6] <- d$r2_1_h
    OUT[i,7] <- d$n_1_h
    OUT[i,8] <- d$radius_1_h
    OUT[i,9] <- d$age_1_h
    OUT[i,10] <- as.character(d$pic_id_1_h)
    
  }
  
  
}


OUTF1 <- as.data.frame(OUT)
#apply column names
colnames(OUTF1)<- c("larvae_id","KorTobis_sample_id","right_left","intercept","slope","r2","n","radius","age","pic_id")
OUTF1$measured_radius<-rep(1,dim(OUTF1)[1])
#columns to make numeric


################## Radius 2 ######################################

OUT = array(0,dim = c(length(id),10))

for (i in 1:length(id)) {
  d<-subset(ALL_DATA_larvae_id, larvae_id==id[i])
  
  OUT[i,1] <- as.character(d$larvae_id)
  OUT[i,2] <- as.character(d$KorTobis_sample_id)
  
  if(NA %in% d$n_2_h && NA %in% d$n_2_v){
    OUT[i,3] <- "NA" 
    OUT[i,4] <- "NA"
    OUT[i,5] <- "NA"
    OUT[i,6] <- "NA"
    OUT[i,7] <- "NA"
    OUT[i,8] <- "NA"
    OUT[i,9] <- "NA"
    OUT[i,10] <- "NA"
    
  } else if(NA %in% d$n_2_h){
    
    OUT[i,3] <- "v" 
    OUT[i,4] <- d$int_2_v
    OUT[i,5] <- d$slope_2_v
    OUT[i,6] <- d$r2_2_v
    OUT[i,7] <- d$n_2_v
    OUT[i,8] <- d$radius_2_v
    OUT[i,9] <- d$age_2_v
    OUT[i,10] <- as.character(d$pic_id_2_v)
    
  }  else if (NA %in% d$n_2_v){
    
    
    OUT[i,3] <- "h" 
    OUT[i,4] <- d$int_2_h
    OUT[i,5] <- d$slope_2_h
    OUT[i,6] <- d$r2_2_h
    OUT[i,7] <- d$n_2_h
    OUT[i,8] <- d$radius_2_h
    OUT[i,9] <- d$age_2_h
    OUT[i,10] <- as.character(d$pic_id_2_h)
    
  } else if (d$n_2_v > d$n_2_h | d$n_2_v==d$n_2_h && d$age_2_v > d$age_2_h){  
    OUT[i,3] <- "v" 
    OUT[i,4] <- d$int_2_v
    OUT[i,5] <- d$slope_2_v
    OUT[i,6] <- d$r2_2_v
    OUT[i,7] <- d$n_2_v
    OUT[i,8] <- d$radius_2_v
    OUT[i,9] <- d$age_2_v
    OUT[i,10] <- as.character(d$pic_id_2_v)
  } else {
    OUT[i,3] <- "h" 
    OUT[i,4] <- d$int_2_h
    OUT[i,5] <- d$slope_2_h
    OUT[i,6] <- d$r2_2_h
    OUT[i,7] <- d$n_2_h
    OUT[i,8] <- d$radius_2_h
    OUT[i,9] <- d$age_2_h
    OUT[i,10] <- as.character(d$pic_id_2_h)
    
  }
  
  
}


OUTF2 <- as.data.frame(OUT)
#apply column names
colnames(OUTF2)<- c("larvae_id","KorTobis_sample_id","right_left","intercept","slope","r2","n","radius","age","pic_id")
OUTF2$measured_radius<-rep(2,dim(OUTF2)[1])
#columns to make numeric

best_ages<-rbind(OUTF1,OUTF2)
num_columns<-c("intercept","slope","r2","n","radius","age")
#making columns numeric
best_ages[,num_columns] <- as.numeric(as.character(unlist(best_ages[,num_columns])))
#View(best_ages)
