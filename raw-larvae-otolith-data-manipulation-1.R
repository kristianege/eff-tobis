#Workup otolith data
#This script takes the raw larval otolith measurements
# Loading data ------------------------------------------------------------
library(dplyr)

#Load otolith measurements
oto_raw <- read.csv("raw_larvae_otolith_measurements.csv",header=TRUE,sep=',',na.strings=c("NA"),)
# Loading metadata

# meta<-read.csv("C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\KorTobis_metadata.csv",header=TRUE,sep=',',na.strings=c("NA"))
# #Loading larvae lengths
# larvaelength2 <- read.csv("C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\KorTobis_larvelgd2_sdl2.csv",header=TRUE,sep=',',na.strings=c("NA"))

#loading otolith marks to be deleted. The marks were determined from picture/measurement reviews in Matlab
mark_to_delete_r1 <- read.csv("mark_to_delete_r1.csv",header=TRUE,sep=',',na.strings=c("NA"))
mark_to_delete_r2 <- read.csv("mark_to_delete_r2.csv",header=TRUE,sep=',',na.strings=c("NA"))

oto <- subset(oto_raw,pic_id!="DA_04_18-04-08_14_i2_hX40" & pic_id!="DA_05_20-04-08_05_i2_hX20" & pic_id!="PK_01_15-04-08_05_i1_hX40" & pic_id!="PK_09_21-04-08_11_i1_vX40"& pic_id!="PK_12_22-04-08_20_i1_hX60"& pic_id!="PK_03_24-04-08_02_i2_vX40"& pic_id!="PK_09_21-04-08_11_i1_vX40"& pic_id!="PK_09_21-04-08_19_i1_vX40"& pic_id!="CY_02_20-04-06_15_i1_vX60"& pic_id!="CY_02_20-04-06_16_i1_hX40"& pic_id!="CY_02_20-04-06_20_i1_hX40")
oto<-arrange(oto,pic_id,measured_radius,mark_no)
oto<-tbl_df(oto) 

#removes faulty points 
otor1 <- oto%>%filter(measured_radius==1)%>%anti_join(.,mark_to_delete_r1,by=c('pic_id','mark_no'))
otor2 <- oto%>%filter(measured_radius==2)%>%anti_join(.,mark_to_delete_r2,by=c('pic_id','mark_no'))
oto<-rbind(otor1,otor2)
remove(mark_to_delete_r1,mark_to_delete_r2,otor1,otor2)
#calcualte new distances between marks, after points have been removed
oto<-oto%>%arrange(pic_id,measured_radius,mark_no)%>%
  group_by(pic_id,measured_radius)%>%
  mutate(dist_btw_markers = sqrt((x_position-lead(x_position))^2+(y_position-lead(y_position))^2))


###Treatment of oto input data- micron calibration 
# Treating otolith measurements -------------------------------------------

#shifts ring measurements one down and places mark 1 (the center) at 0
#Convert pixel measures to microns, data is split in magnification, converted and reassembled
    
oto40<-subset(oto,oto$magnification=="X40")
oto40$dist_btw_markers<-oto40$dist_btw_markers/oto40$X40_pix_pr_micron
oto40$dist_sec_growth_center<-oto40$dist_sec_growth_center/oto40$X40_pix_pr_micron
oto40$radius_1_length<-oto40$radius_1_length/oto40$X40_pix_pr_micron
oto40$radius_2_length<-oto40$radius_2_length/oto40$X40_pix_pr_micron


oto20<-subset(oto,oto$magnification=="X20")
oto20$dist_btw_markers<-oto20$dist_btw_markers/oto20$X20_pix_pr_micron
oto20$dist_sec_growth_center<-oto20$dist_sec_growth_center/oto20$X20_pix_pr_micron
oto20$radius_1_length<-oto20$radius_1_length/oto20$X20_pix_pr_micron
oto20$radius_2_length<-oto20$radius_2_length/oto20$X20_pix_pr_micron


oto63<-subset(oto,oto$magnification=="X60")
oto63$dist_btw_markers<-oto63$dist_btw_markers/oto63$X63_pix_pr_micron
oto63$dist_sec_growth_center<-oto63$dist_sec_growth_center/oto63$X63_pix_pr_micron
oto63$radius_1_length<-oto63$radius_1_length/oto63$X63_pix_pr_micron
oto63$radius_2_length<-oto63$radius_2_length/oto63$X63_pix_pr_micron

oto<-rbind(oto40,oto63,oto20)
remove(oto40,oto63,oto20)

otor1 <- oto%>%ungroup(.)%>%filter(measured_radius==1)%>%mutate(radius=radius_1_length)
otor2 <- oto%>%ungroup(.)%>%filter(measured_radius==2)%>%mutate(radius=radius_2_length)

oto<-rbind(otor1,otor2)
remove(otor1,otor2)

source("zero_to_na.R")

oto<-oto%>%ungroup(.)%>%filter(dist_btw_markers>0.5)
#Distance from center to dayring (marker) is calculated from distance between markers, relevant columns selected
oto1 <-  oto %>%
            group_by(pic_id,measured_radius)%>%
            arrange(pic_id,measured_radius,mark_no)%>%
            mutate(
              dist_to_center = cumsum(dist_btw_markers),
              dist_to_center = lag(dist_to_center, default = NA), 
              dist_btw_markers = zero_to_na(dist_btw_markers)) %>%
            filter(!is.na(dist_btw_markers),!is.na(dist_to_center))%>%  #removes center to 2nd mark dist and last mark with dist = 0
            select(larvae_id,pic_id,KorTobis_sample_id,measured_radius,mark_no,dist_to_center,dist_btw_markers,dist_sec_growth_center,radius,radius_1_length,radius_2_length,x_position,y_position,magnification)
            


######removes distances more than 1.8 times the next value, in theory this should only be run 1 time, after removal of faulty points#
oto2<-oto1%>%
      group_by(pic_id,measured_radius)%>%
      filter(dist_btw_markers  < 1.8 * lead(dist_btw_markers, default = Inf))%>%
      filter(dist_btw_markers<9,dist_to_center<160)


#########################
#the filter does not do anything to the last masurement, if the point is far from the edge, it will create at large distanse, for removal use:
#oto2<-mutate(oto2, bla=lead(dist_btw_markers, default = NA))
#oto2<-filter(oto2,!is.na(bla))
#it is also possible to just remove all distances bigger than 9, this way smaller measures at the rim is maintained.  

oto2<-oto2%>%filter(dist_btw_markers<9,dist_to_center<160)
oto2<-tbl_df(oto2) #makes it at local data.frame

#For matlab review of filtering results, the following files are used to review results in matlab
#otonames<-unique(oto2%>%select(pic_id))
#write.csv(oto_raw,file="C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\oto.csv",row.names=FALSE)
#write.csv(oto2,file="C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\oto2.csv",row.names=FALSE)
#write.csv(otonames,file="C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\pictures.csv",row.names=FALSE)
#write.csv(oto1,file="C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\oto1.csv",row.names=FALSE)


####merge on metadata and larvaer length
# meta<-select(meta,KorTobis_sample_id,year,monthh,dayy,st_hour,st_min,stlondec,stlatdec)
# oto2<-merge(oto2,meta, by="KorTobis_sample_id",all.x=TRUE)
# oto2<-merge(oto2,larvaelength2, by="larvae_id",all.x=TRUE)

