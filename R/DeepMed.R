
# function for estimation, se, and p-values
DeepMed=function(y,d,m,x,hyper_grid=NA,epochs=500, hyper=NA,cv=TRUE,trim=0.05){

  if(cv==TRUE){
    hyper=DeepMed_cv(y,d,m,x,hyper_grid,epochs=epochs)
  }

  mbin=1*(length(unique(m))==2 & min(m)==0 & max(m)==1)
  if (mbin==0) temp=DeepMed_cont(y=y,d=d,m=m,x=x, hyper=hyper,trim=0.05)
  if (mbin!=0) temp=DeepMed_bin(y=y,d=d,m=m,x=x, hyper=hyper,trim=0.05)
  ATE = temp$ATE

  eff=ATE[1:5]
  se=sqrt( (ATE[6:10])/ATE[11])
  results=rbind(eff,se, 2*pnorm(-abs(eff/se)))
  colnames(results)=c("total", "dir.treat", "dir.control", "indir.treat", "indir.control")
  rownames(results)=c("effect","se","p-val")
  ntrimmed=length(d)-ATE[11]

  # round(results,3)
  list("results"=results, "ntrimmed"=ntrimmed)
}


