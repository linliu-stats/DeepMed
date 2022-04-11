
DeepMed_cont=function(y,d,m,x,hyper=hyper,trim=0.05){

  y=as.vector(y);d=as.vector(d);m=as.vector(m)
  if(is.null(nrow(x))){x=as.matrix(x,length(x),1)
  }else{x=as.matrix(x,nrow(x),ncol(x))}
  xm=as.matrix(cbind(x,m),nrow(xm),ncol(xm))

  stepsize=ceiling((1/3)*length(d))
  nobs = min(3*stepsize,length(d)); set.seed(1); idx = sample(nobs);
  sample1 = idx[1:stepsize]; sample2 = idx[(stepsize+1):(2*stepsize)];
  sample3 = idx[(2*stepsize+1):nobs];
  y1m0=c();y1m1=c();y0m0=c(); y0m1=c(); selall=c()


  for (k in 1:3){
    if (k==1) {tesample=sample1; musample=sample2; deltasample=sample3}
    if (k==2) {tesample=sample3; musample=sample1; deltasample=sample2}
    if (k==3) {tesample=sample2; musample=sample3; deltasample=sample1}
    trsample=c(musample,deltasample); dte=d[tesample]; yte=y[tesample]
    dtrte=d[deltasample]; xtrte=x[deltasample,]


    l1_1 =hyper[1,1]; l1_2 =hyper[1,2]; l1_3 =hyper[1,3]; l1_4 =hyper[1,4]; l1_5 =hyper[1,5]; l1_6 =hyper[1,6]; l1_7 = hyper[1,7];l1_8 = hyper[1,8];
    layer_1 =hyper[2,1]; layer_2 =hyper[2,2]; layer_3 =hyper[2,3]; layer_4 =hyper[2,4]; layer_5 =hyper[2,5]; layer_6 =hyper[2,6]; layer_7 = hyper[2,7];layer_8 = hyper[2,8];
    units_1 =hyper[3,1]; units_2 =hyper[3,2]; units_3 =hyper[3,3]; units_4 =hyper[3,4]; units_5 =hyper[3,5]; units_6 =hyper[3,6]; units_7 = hyper[3,7];units_8 = hyper[3,8];
    epochs_1 =hyper[4,1]; epochs_2 =hyper[4,2]; epochs_3 =hyper[4,3]; epochs_4 =hyper[4,4]; epochs_5 =hyper[4,5]; epochs_6 =hyper[4,6]; epochs_7 = hyper[4,7];epochs_8 = hyper[4,8];
    hyper=hyper[,-c(1:8)]



    pmxte = ann (d[trsample],xm[trsample,],d[tesample],xm[tesample,], l1_1,layer_1,units_1,epochs_1)$ypred
    pxte  = ann (d[trsample],x[trsample,],d[tesample],x[tesample,], l1_2,layer_2,units_2,epochs_2)$ypred


    eymx1te_all  = ann (y[musample[d[musample]==1]],xm[musample[d[musample]==1],],
                            y[c(tesample,deltasample)],xm[c(tesample,deltasample),],l1_3,layer_3,units_3,epochs_3)$ypred
    eymx1te= eymx1te_all[1:length(tesample)] # ypredict E(Y|M,X,D=1) in test data
    eymx1trte= eymx1te_all[-(1:length(tesample))]  # ypredict E(Y|M,X,D=1) in delta sample
    regweymx1te  = ann (eymx1trte[dtrte==0],xtrte[dtrte==0,],eymx1te, x[tesample,], l1_4,layer_4,units_4,epochs_4)$ypred
    eyx1te  = ann (y[trsample[d[trsample]==1]],x[trsample[d[trsample]==1],],y[tesample], x[tesample,], l1_5,layer_5,units_5,epochs_5)$ypred


    eymx0te_all  = ann (y[musample[d[musample]==0]],xm[musample[d[musample]==0],],
                            y[c(tesample,deltasample)],xm[c(tesample,deltasample),],l1_6,layer_6,units_6,epochs_6)$ypred
    eymx0te= eymx0te_all[1:length(tesample)] # ypredict E(Y|M,X,D=0) in test data
    eymx0trte= eymx0te_all[-(1:length(tesample))]  # ypredict E(Y|M,X,D=0) in delta sample
    regweymx0te  = ann (eymx0trte[dtrte==1],xtrte[dtrte==1,],eymx0te, x[tesample,], l1_7,layer_7,units_7,epochs_7)$ypred
    eyx0te  = ann (y[trsample[d[trsample]==0]],x[trsample[d[trsample]==0],],y[tesample], x[tesample,], l1_8,layer_8,units_8,epochs_8)$ypred



    # select observations satisfying trimming restriction
    sel= 1*((((1-pmxte)*pxte)>=trim) & ((1-pxte)>=trim)  & (pxte>=trim) &  (((pmxte*(1-pxte)))>=trim)   )
    # ypredict E(Y0,M(1)) in the test data
    temp=((1-dte)*pmxte/((1-pmxte)*pxte)*(yte-eymx0te)+dte/pxte*(eymx0te-regweymx0te)+regweymx0te)
    y0m1=c(y0m1,temp[sel==1])
    # ypredict E(Y0,M(0)) in the test data
    temp=(eyx0te + (1-dte)*(yte-eyx0te)/(1-pxte))
    y0m0=c(y0m0,temp[sel==1])
    # ypredict E(Y1,M(0)) in the test data
    temp=(dte*(1-pmxte)/(pmxte*(1-pxte))*(yte-eymx1te)+(1-dte)/(1-pxte)*(eymx1te-regweymx1te)+regweymx1te)
    y1m0=c(y1m0,temp[sel==1])
    # ypredict E(Y1,M(1)) in the test data
    temp=(eyx1te + dte*(yte-eyx1te)/pxte)
    y1m1=c(y1m1,temp[sel==1])
    # collect selection dummies
    selall=c(selall,sel)

  }
  # average over the crossfitting steps
  my1m1=mean(y1m1); my0m1=mean(y0m1); my1m0=mean(y1m0); my0m0=mean(y0m0)
  # compute effects
  tot=my1m1-my0m0; dir1=my1m1-my0m1; dir0=my1m0-my0m0; indir1=my1m1-my1m0; indir0=my0m1-my0m0;
  #compute variances
  vtot=mean((y1m1-y0m0-tot)^2); vdir1=mean((y1m1-y0m1-dir1)^2); vdir0=mean((y1m0-y0m0-dir0)^2);
  vindir1=mean((y1m1-y1m0-indir1)^2); vindir0=mean((y0m1-y0m0-indir0)^2);

  ATE = c(tot, dir1, dir0, indir1, indir0,  vtot, vdir1, vdir0, vindir1, vindir0, sum(selall))
  list("ATE"=ATE)

}

















































