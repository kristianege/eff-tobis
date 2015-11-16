
###
# load the function "all_model_info.R" used to extract all model information from 
# the linear models between distance to centre of otolith and the ringwidth
# this supplies among a lot of other model information handy for evaluation the 
# slope and intercept for further use and selection. The function extracts the 
# model values from the lm list object and puts them into a more handy dataframe
# format. 
###
source("all_model_info.R")

library(broom)

# If you want to filter edge of otholith ----------------------------------
temp1<-oto2%>%filter(measured_radius==1, dist_to_center <= 0.90*radius_1_length)
temp2<-oto2%>%filter(measured_radius==2, dist_to_center <= 0.90*radius_2_length)
oto_no_edge <- rbind(temp1,temp2)
remove(temp1,temp2)

# Filter by intercept window at 20mu distance to center--------------------
winmax <- max(oto2%>%ungroup()%>%filter(dist_to_center<21,dist_to_center>19)%>%select(dist_btw_markers))
winmin <- min(oto2%>%ungroup()%>%filter(dist_to_center<21,dist_to_center>19)%>%select(dist_btw_markers))

oto_dist_minus_20<-oto_no_edge%>%mutate(dist_to_center=dist_to_center-20)
minus20<-all_model_info(oto_dist_minus_20,c("pic_id","measured_radius"),"dist_btw_markers","dist_to_center")
inc_fit<-minus20%>%filter(inc_estimate< winmax,inc_estimate> winmin)%>%
      select(pic_id,measured_radius)

oto_inc_fit<-semi_join(oto_no_edge,inc_fit,by=c("pic_id","measured_radius"))

# Apply lm on filtered data -----------------------------------------------
#first the summarized version
YS<-20
YSdays<-12
oto_filtered_model<-all_model_info(oto_inc_fit,c("pic_id","measured_radius"),"dist_btw_markers","dist_to_center")

temp<-oto_inc_fit%>%ungroup()%>%
      group_by(pic_id,measured_radius)%>%
      summarize(radius=mean(radius))%>%
      select(pic_id,measured_radius,radius)

oto_filtered_model<-left_join(oto_filtered_model,temp,by=c("pic_id","measured_radius"))%>%
      mutate(mean_increment = abs(slope_estimate) * (((radius-YS)/2)+YS) + inc_estimate,
             age = ((radius-YS)/mean_increment) + YSdays,
            larvae_id = substr(pic_id,1,17),
            KorTobis_sample_id = substr(pic_id,1,14),
            right_left = substr(pic_id,22,22),
            YS = rep(YS,dim(oto_filtered_model)[1]),
            YSdays = rep(YSdays,dim(oto_filtered_model)[1]))

#and model info for each point 
oto_augment<-oto_inc_fit%>% ungroup()%>%
      group_by(pic_id,measured_radius) %>% 
      do(augment(lm(dist_btw_markers~dist_to_center, data = .)))%>%
      select(-dist_btw_markers,-dist_to_center)

oto_filtered_all <-cbind(oto_inc_fit,oto_augment%>%ungroup()%>%select(-pic_id,-measured_radius))

# Selection for next script -----------------------------------------------
ALL_DATA2<-oto_filtered_model%>%
      select(YS,YSdays,pic_id,int=inc_estimate,slope=slope_estimate,r2=model_r.squared,n,radius,age,measured_radius,larvae_id,KorTobis_sample_id,right_left)



#Rearange data in ALL_DATA2 so each radius and right/left otolith gets own columns
source("ALL_DATA_larvae_id.R")

#Selects best age estimates on basis of highest number of increments in estimate, hereafter higher age
#both for first and second radius, output is "best_ages"
source("best_ages.R")

#Assembles metadata with best_ages and adds meta output is best_ages_inc, adds meta to oto2 and selects its rows
#based on best_ages_inc with mach by pic_id and measured radius, output is oto_best_inc. 
source("Assembly_length_meta_area.R")

best_ages<-best_ages%>%group_by(pic_id,measured_radius)
oto_filtered_all<-oto_filtered_all%>%group_by(pic_id,measured_radius)

#Makes congruence between larvae in best_ages_inc and the version with all marks etc included
oto4<-inner_join(oto_filtered_all,best_ages_inc %>%select(-KorTobis_sample_id,-radius,-larvae_id,-age,-slope,-magnification),by=c("pic_id", "measured_radius"))%>%
      mutate(hatch_year=year)



oto5<-inner_join(oto_filtered_model,best_ages_inc %>% 
                 select(-KorTobis_sample_id,-radius,-larvae_id,-age,-magnification,-n),
           by=c("pic_id", "measured_radius", "right_left"))%>%
      select(-slope_estimate,-inc_estimate,-model_r.squared,-YS,-YSdays)

#write.csv(oto5,file=("C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\otolith_filtered.csv"),row.names=FALSE)
#write.csv(oto4,file=("C:\\EFF_korttidsprog_tobis\\Larve_otolitter\\R-csv_files\\otolith_filtered_all_marks.csv"),row.names=FALSE)


rm(larvae,num_columns,winmax,winmin,oto_augment,temp,temp1,temp2,temp3,temp4,OUT,OUTF1,OUTF2,YS,YSdays,d,i,id,minus20,oto1h,oto1v,oto2h,oto2v,oto_rad1,oto_rad2,oto_inc_fit,oto_dist_minus_20,oto_best_inc,ALL_DATA2,ALL_DATA_larvae_id,inc_fit,best_ages,best_ages_inc,oto_no_edge)
# Use Mikaels method (almost!! this utilizes the point distance to regression line)---------------
# plus_fit<-oto2 %>% ungroup(.)%>%
#       arrange(pic_id,measured_radius,mark_no)%>%
#       group_by(pic_id,measured_radius) %>%
#       do(augment (lm(dist_btw_markers ~ dist_to_center, data = .)))%>%
#       ungroup(.)%>%
#       select(.fitted,.se.fit,.resid,.hat,.sigma,.cooksd,.std.resid)
# oto_plus_fitA<-cbind(oto2,plus_fit)
# 
# oto_plus_fitB <- all_model_info(oto2,c("pic_id","measured_radius"),"dist_btw_markers","dist_to_center")
# oto_plus_fit <- left_join(oto_plus_fitA,oto_plus_fitB,by=c("pic_id","measured_radius"))
# oto_plus_fit<-oto_plus_fit %>% mutate(dist_to_line = abs(slope_estimate*dist_to_center+inc_estimate-dist_btw_markers)/sqrt(slope_estimate^2+1))
# oto_plus_fit<-oto_plus_fit %>% filter(dist_to_line < 1)
# remove(plus_fit,oto_plus_fitA,oto_plus_fitB)
#  ------------------------------------------------------------------------

#remove(inc_fit,minus20,oto_dist_minus_20,winmax,winmin)


