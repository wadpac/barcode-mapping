barcoding_main_last<- function(file_list){
  #lists<- filelist_generation()
  #file_list<- lists$file_list
  # meta_data<- lists$meta_data
  
  n=length(file_list)
  barcodes=NULL
  all_length=NULL
  file_names=NULL
  barcodes_length<- NULL
  
  barcodes_all<-NULL
  days=NULL
  short_barcoding=NULL
  long_barcoding=NULL
  short_barcoding_length=NULL
  long_barcoding_length=NULL
  vd=NULL
  ll1=NULL
  ll2=NULL
  for(i in 1:n){
    print(i)
    
    file_name=gsub(".csv", "", file_list[i], ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
    file_name=gsub("CSV", "", file_name, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, use= FALSE)
    print("ready to read:")
    print(file_name)
    temp<-  readdata(file_list[i], path_input, fileid, tz = "Europe/London",sparse = FALSE, fault = 32767)
    print("ready to aggregate data:")
    #HIER KAN EEN IF STATEMENT: if rescale.epoch %% epoch == 0 then aggAccFile, else aggAccFile_self?
    #test<- aggAccFile_self(temp, by =epoch, keep.error = FALSE,which="y")
    test<- aggAccFile(temp, by =epoch, keep.error = FALSE,which="y")
    print("ready to apply boutlength filter")
    calculation<-bouts_length_filter(test$outcome,test$ts_agg,file_list[i],epoch,validdays, mimwear)
    ll1<- c(ll1, calculation$l1)
    ll2<- c(ll2, calculation$l2)
    short_barcoding=rbind(  short_barcoding,  calculation$short_barcoding)
    long_barcoding= long.barcode(long_barcoding,    calculation$long_barcoding)
    short_barcoding_length=rbind(  short_barcoding_length,    calculation$short_barcoding_length)
    long_barcoding_length=long.barcode(long_barcoding_length,    calculation$long_barcoding_length)
    days=c(days,calculation$days)
    vd<- rbind(vd,calculation$vd)
  }
  results=list(short_sequence=short_barcoding,long_sequence=long_barcoding)
  #save.image("below11052017_15_teatske.Rdata")
  #save.image("barcoding_2101.Rdata")
  return(results)
}
