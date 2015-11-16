#source("C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-scripts\\cleaned_workup_rad1_rad2.R")

oto_rad1 <- filter(ALL_DATA2,measured_radius==1)
oto_rad2 <- filter(ALL_DATA2, measured_radius==2)

#subset and select columns
oto1v<-subset(oto_rad1,right_left=="v",select=c("larvae_id","pic_id","int", "slope", "r2", "n", "radius", "age"))
colnames(oto1v)<-c("larvae_id","pic_id_1_v","int_1_v", "slope_1_v", "r2_1_v", "n_1_v", "radius_1_v", "age_1_v")

oto1h<-subset(oto_rad1,right_left=="h",select=c("larvae_id","pic_id","int", "slope", "r2", "n", "radius", "age"))
colnames(oto1h)<-c("larvae_id","pic_id_1_h","int_1_h", "slope_1_h", "r2_1_h", "n_1_h", "radius_1_h", "age_1_h")

oto2v<-subset(oto_rad2,right_left=="v",select=c("larvae_id","pic_id","int", "slope", "r2", "n", "radius", "age"))
colnames(oto2v)<-c("larvae_id","pic_id_2_v","int_2_v", "slope_2_v", "r2_2_v", "n_2_v", "radius_2_v", "age_2_v")

oto2h<-subset(oto_rad2,right_left=="h",select=c("larvae_id","pic_id","int", "slope", "r2", "n", "radius", "age"))
colnames(oto2h)<-c("larvae_id","pic_id_2_h","int_2_h", "slope_2_h", "r2_2_h", "n_2_h", "radius_2_h", "age_2_h")

#list of larvae names

larvae<-unique(oto_raw$larvae_id)
larvae<-as.data.frame(larvae)
colnames(larvae)<-c("larvae_id")

temp1<-merge(larvae,oto1v,by="larvae_id",all.x=TRUE)
#dim(temp1)

temp2<-merge(temp1,oto1h,by="larvae_id",all.x=TRUE)
#dim(temp2)

temp3<-merge(temp2,oto2v,by="larvae_id",all.x=TRUE)
#dim(temp3)

temp4<-merge(temp3,oto2h,by="larvae_id",all.x=TRUE)

temp4$KorTobis_sample_id<-substr(temp4$larvae_id,1,14)


ALL_DATA_larvae_id<-temp4

remove(temp1,temp2,temp3,temp4,oto_rad1,oto_rad2,larvae,oto2h,oto2v,oto1h,oto1v)
#View(ALL_DATA_larvae_id)
# 
# meta<-subset(meta, select=c("KorTobis_sample_id","year","monthh","dayy","posixct_time","stlondec","stlatdec"))
# colnames(meta)<-c("KorTobis_sample_id","styear","stmonthh","stdayy","stposixct_time","stlondec","stlatdec")
# meta$st_day_of_year<-(as.POSIXlt(as.character(meta$stposixct_time),format="%Y%m%d")$yday)+1
# meta$st_R_dates<-as.Date(as.character(meta$stposixct_time),format='%Y%m%d')
# meta<-subset(meta,select=c("KorTobis_sample_id","styear","stmonthh","stdayy","st_day_of_year","st_R_dates","stlondec","stlatdec"))
# 
# ALL_DATA_larvae_id<-merge(temp4,meta,all.x=TRUE)
# ALL_DATA_larvae_id<-merge(ALL_AGE_DATA,larvaelength2,all.x=TRUE)
# 
# 
# #factorices the year column, to avoid cleanup...
# ALL_AGE_DATA$styear<-as.factor(ALL_AGE_DATA$styear)
# 
# clean<-function(x){
#   if(is.numeric(x))x[(x>1000)|(x==-999)] <- NA
#   x
# }
# ALL_AGE_DATA<-as.data.frame(lapply(ALL_AGE_DATA,clean))
# 
# 
# #write.csv(ALL_AGE_DATA,file="C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\ALL_AGE_DATA.csv",row.names=FALSE)
# ALL_AGE_DATA<-read.csv("C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\ALL_AGE_DATA.csv")
# names(ALL_AGE_DATA)
# #OptimalAgeDATA<-subset(ALL_AGE_DATA,select=c("larvae_id","KorTobis_sample_id","styear","stmonthh","stdayy","st_day_of_year","st_R_dates","stlondec","stlatdec","larvae_tlgd", "larvaer_sd_lgd") )
# 
# 
# qplot(slope_1_v,slope_2_v,data=ALL_AGE_DATA)
# fit <- lm(slope_1_v~slope_2_v, data = ALL_AGE_DATA)
# summary(fit)
# 
# qplot(age_1_v,age_2_v,data=ALL_AGE_DATA)
# fit <- lm(age_1_v~age_2_v, data = ALL_AGE_DATA)
# summary(fit)
# 
# qplot(age_1_v,age_1_h,data=ALL_AGE_DATA)
# fit<-lm(age_1_h~age_1_v-1,data=ALL_AGE_DATA)
# summary(fit)
# 
# qplot(radius_1_v,slope_1_v,data=ALL_AGE_DATA)
# names(ALL_AGE_DATA)
# qplot(hatch_day_of_year_1_h,geom="density",data=ALL_AGE_DATA,colour=factor(styear))+theme_classic()
# qplot(hatch_day_of_year_1_v,geom="density",data=ALL_AGE_DATA,colour=factor(styear))+theme_classic()
# qplot(age_1_v,larvae_tlgd,data=ALL_AGE_DATA,colour=factor(styear))+theme_classic()
# qplot(age_1_h,larvaer_sd_lgd,data=ALL_AGE_DATA,colour=factor(styear))+theme_classic()
# qplot(larvaer_sd_lgd,larvae_tlgd,data=ALL_AGE_DATA)
# qplot(larvaer_sd_lgd,slope_1_v,data=ALL_AGE_DATA,colour=factor(styear))
# qplot(larvaer_sd_lgd,slope_1_h,data=subset(ALL_AGE_DATA,styear==2006),colour=factor(styear))
# qplot(larvaer_sd_lgd,slope_1_h,data=subset(ALL_AGE_DATA,styear==2006),colour=factor(styear))
# 
