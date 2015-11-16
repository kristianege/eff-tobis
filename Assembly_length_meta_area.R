#Loading metadata
meta<-read.csv("KorTobis_metadata.csv",header=TRUE,sep=',',na.strings=c("NA"))
#Loading larvae lengths
larvaelength2 <- read.csv("KorTobis_larvelgd2_sdl2.csv",header=TRUE,sep=',',na.strings=c("NA"))

areafun<-function(x) {if (x < 2.7){"DB"
                              } else if (x>5){"FB"
                                    }else{"ES"
                                          }
                        }

best_ages_inc <- best_ages %>%
      left_join (.,meta,by="KorTobis_sample_id") %>%
      left_join (.,larvaelength2,by="larvae_id") %>%
      mutate (sd_lgd=(floor(sd_lgd*2))/2,
            age=round(age),
            st_date = as.Date(as.character(posixct_time),format='%Y%m%d'),
            st_julian_day = as.POSIXlt(as.character(posixct_time),format="%Y%m%d")$yday,
            hatch_date = st_date-age,             
            hatch_julian = st_julian_day - age,
            area = unlist(lapply(stlondec,areafun)),
            magnification = substr(pic_id,23,25),
            right_left = right_left)%>%
      select(year,pic_id,larvae_id,KorTobis_sample_id,measured_radius,
             right_left,radius,sd_lgd,age,slope,intercept,n,
             r2,st_date,st_julian_day,hatch_date,hatch_julian,
             area,stlondec,stlatdec,magnification)

#merges meta and length data oto2 file, add area 
oto_best_inc<-oto2%>%
      inner_join ( . , select(best_ages_inc,pic_id,measured_radius,sd_lgd,age,slope,intercept,n,
                              r2,st_date,st_julian_day,hatch_date,hatch_julian,
                              area,stlondec,stlatdec),
                  by=c("pic_id","measured_radius"))


