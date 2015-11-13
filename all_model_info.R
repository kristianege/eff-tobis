all_model_info<-function(data,group_vector,lm_y.var,lm_x.var){
      library(dplyr)
      library(broom)
      dots <- lapply(group_vector, as.symbol)
      regression<-data %>% ungroup(.)%>%
            group_by_(.dots=dots) %>% 
            do(fit=lm(as.formula(paste(lm_y.var,lm_x.var,sep="~")), data = .))
      test2<-data%>%ungroup(.)%>%
            group_by_(.dots=dots)%>%
            summarize(n=n())%>%
            ungroup(.)%>%
            select(n) 
      test3<-regression%>%tidy(fit)
      test4<-test3%>%filter(term=="(Intercept)")%>%select(-term)
      test5<-test3%>%filter(term==lm_x.var)%>%select(-term)
      test6<-regression%>% glance(fit)
      colnames(test4)<-c(names(test4)[1:length(group_vector)],paste("inc",names(test4)[(length(group_vector)+1):dim(test4)[2]],sep="_"))
      colnames(test5)<-c(names(test5)[1:length(group_vector)],paste("slope",names(test5)[(length(group_vector)+1):dim(test5)[2]],sep="_"))
      colnames(test6)<-c(names(test6)[1:length(group_vector)],paste("model",names(test6)[(length(group_vector)+1):dim(test6)[2]],sep="_"))
      test7<-left_join(test4,test5,by=group_vector)
      test8<-left_join(test7,test6,by=group_vector)
      test9<-cbind(test8,test2)
      test9
}


