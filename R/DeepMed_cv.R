DeepMed_cv=function(y,d,m,x,hyper_grid,epochs=500, filename=NA){
  mbin=1*(length(unique(m))==2 & min(m)==0 & max(m)==1)
  if (mbin==0) hyper=DeepMed_cont_cv(y=y,d=d,m=m,x=x, hyper_grid,epochs=epochs)
  if (mbin!=0) hyper=DeepMed_bin_cv(y=y,d=d,m=m,x=x, hyper_grid,epochs=epochs)
  if(is.na(filename)==0){
    write.table(hyper,file = paste0(filename,".txt"),append = FALSE)
  }
  return(hyper)
}


