deg_decmin_2decdeg <- function(string){
      deg<-as.numeric(substr(as.character(string),1,2))
      min<-as.numeric(substr(as.character(string),4,10))
      min_in_deg<-min/60
      dec_deg<-deg+min_in_deg
      dec_deg
}
