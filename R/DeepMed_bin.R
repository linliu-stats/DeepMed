DeepMed_bin=function(y,d,m,x,hyper=hyper, trim=0.05){

  y=as.vector(y);d=as.vector(d);m=as.vector(m)
  if(is.null(nrow(x))){x=as.matrix(x,length(x),1)
  }else{x=as.matrix(x,nrow(x),ncol(x))}

  stepsize=ceiling((1/k)*length(d))
  set.seed(1); idx= sample(length(d), replace=FALSE)
  y1m0=c();y1m1=c();y0m0=c(); y0m1=c(); selall=c()
  # crossfitting procedure that splits sample in training an testing data
  for (k in 1:3){
    tesample=idx[((k-1)*stepsize+1):(min((k)*stepsize,length(d)))]
    dtr=d[-tesample]; dte=d[tesample]; ytr=y[-tesample]; yte=y[tesample]; ytr1= ytr[dtr==1]; ytr0= ytr[dtr==0];
    mtr=m[-tesample]; mte=m[tesample]; mtr1=mtr[dtr==1]; mtr0=mtr[dtr==0];
    if (is.null(ncol(x)) | ncol(x)==1) {
      xtr=x[-tesample]; xte=x[tesample]; xtr1=xtr[dtr==1]; xtr0=xtr[dtr==0]; xtr11=xtr[dtr==1 & mtr==1]; xtr10=xtr[dtr==1 & mtr==0]; xtr01=xtr[dtr==0 & mtr==1]; xtr00=xtr[dtr==0 & mtr==0]
    }
    if (is.null(ncol(x))==0 & ncol(x)>1) {
      xtr=x[-tesample,]; xte=x[tesample,]; xtr1=xtr[dtr==1,]; xtr0=xtr[dtr==0,]; xtr11=xtr[dtr==1 & mtr==1,]; xtr10=xtr[dtr==1 & mtr==0,]; xtr01=xtr[dtr==0 & mtr==1,]; xtr00=xtr[dtr==0 & mtr==0,]
    }
    ytr11=ytr[dtr==1 & mtr==1]; ytr10=ytr[dtr==1 & mtr==0]; ytr01=ytr[dtr==0 & mtr==1]; ytr00=ytr[dtr==0 & mtr==0];
    # tr stands for first training data, te for test data, "1" and "0" for subsamples with treated and nontreated


    l1_1 =hyper_nn[1,1]; l1_2 =hyper_nn[1,2]; l1_3 =hyper_nn[1,3]; l1_4 =hyper_nn[1,4]; l1_5 =hyper_nn[1,5]; l1_6 =hyper_nn[1,6]; l1_7 = hyper_nn[1,7];l1_8 = hyper_nn[1,8];l1_9 = hyper_nn[1,9]
    layer_1 =hyper_nn[2,1]; layer_2 =hyper_nn[2,2]; layer_3 =hyper_nn[2,3]; layer_4 =hyper_nn[2,4]; layer_5 =hyper_nn[2,5]; layer_6 =hyper_nn[2,6]; layer_7 = hyper_nn[2,7];layer_8 = hyper_nn[2,8];layer_9 = hyper_nn[2,9]
    units_1 =hyper_nn[3,1]; units_2 =hyper_nn[3,2]; units_3 =hyper_nn[3,3]; units_4 =hyper_nn[3,4]; units_5 =hyper_nn[3,5]; units_6 =hyper_nn[3,6]; units_7 = hyper_nn[3,7];units_8 = hyper_nn[3,8];units_9 = hyper_nn[3,9]
    epochs_1 =hyper_nn[4,1]; epochs_2 =hyper_nn[4,2]; epochs_3 =hyper_nn[4,3]; epochs_4 =hyper_nn[4,4]; epochs_5 =hyper_nn[4,5]; epochs_6 =hyper_nn[4,6]; epochs_7 = hyper_nn[4,7];epochs_8 = hyper_nn[4,8];epochs_9 = hyper_nn[4,9]
    hyper_nn=hyper_nn[,-c(1:9)]

    # predict Pr(M=1|D=1,X) in test data
    # predict Pr(M=1|D=0,X) in test data
    # predict Pr(D=1|X) in test data
    # predict E(Y| D=1, M=1, X) in test data
    # predict E(Y| D=0, M=1, X) in test data
    # predict E(Y| D=1, M=0, X) in test data
    # predict E(Y| D=0, M=0, X) in test data
    # predict E(Y|D=1, X) in test data
    # predict E(Y|D=0, X) in test data
    pm1te = ann (mtr1,xtr1,mte,xte, l1_1,layer_1,units_1,epochs_1)$ypred
    pm0te = ann (mtr0,xtr0,mte,xte, l1_2,layer_2,units_2,epochs_2)$ypred
    pdte  = ann (dtr, xtr, dte,xte, l1_3,layer_3,units_3,epochs_3)$ypred
    eymx11te = ann (ytr11, xtr11, yte,xte, l1_4,layer_4,units_4,epochs_4)$ypred
    eymx01te = ann (ytr01, xtr01, yte,xte, l1_5,layer_5,units_5,epochs_5)$ypred
    eymx10te = ann (ytr10, xtr10, yte,xte, l1_6,layer_6,units_6,epochs_6)$ypred
    eymx00te = ann (ytr00, xtr00, yte,xte, l1_7,layer_7,units_7,epochs_7)$ypred
    eyx1te = ann (ytr1, xtr1, yte,xte, l1_8,layer_8,units_8,epochs_8)$ypred
    eyx0te = ann (ytr0, xtr0, yte,xte, l1_9,layer_9,units_9,epochs_9)$ypred


    # predict E(Y| D=0, M, X) in test data
    eymx0te=mte*eymx01te+(1-mte)*eymx00te
    # predict E(Y| D=1, M, X) in test data
    eymx1te=mte*eymx11te+(1-mte)*eymx10te

    # predict score functions for E(Y(1,M(0))) in the test data
    eta10=(eymx11te*pm0te+eymx10te*(1-pm0te))
    sel= 1*(((pdte*pm1te)>=trim) & ((1-pdte)>=trim)  & (pdte>=trim) &  (((1-pdte)*pm0te)>=trim)   )
    temp=dte*pm0te/(pdte*pm1te)*(yte-eymx1te)+(1-dte)/(1-pdte)*(eymx1te- eta10 )+eta10
    y1m0=c(y1m0, temp[sel==1])
    # predict score functions for E(Y(1,M(1))) in the test data
    temp=eyx1te + dte*(yte-eyx1te)/pdte
    y1m1=c(y1m1,temp[sel==1])
    # predict score functions for E(Y(0,M(1))) in the test data
    eta01=(eymx01te*pm1te+eymx00te*(1-pm1te))
    temp=(1-dte)*pm1te/((1-pdte)*pm0te)*(yte-eymx0te)+dte/pdte*(eymx0te- eta01 )+eta01
    y0m1=c(y0m1, temp[sel==1])
    # predict score functions for E(Y0,M(0)) in the test data
    temp=eyx0te + (1-dte)*(yte-eyx0te)/(1-pdte)
    y0m0=c(y0m0, temp[sel==1])
    selall=c(selall,sel)
  }
  # average over the crossfitting steps
  my1m1=mean(y1m1); my0m1=mean(y0m1); my1m0=mean(y1m0); my0m0=mean(y0m0)
  # compute effects
  tot=my1m1-my0m0; dir1=my1m1-my0m1; dir0=my1m0-my0m0; indir1=my1m1-my1m0; indir0=my0m1-my0m0;
  #compute variances
  vtot=mean((y1m1-y0m0-tot)^2); vdir1=mean((y1m1-y0m1-dir1)^2); vdir0=mean((y1m0-y0m0-dir0)^2);
  vindir1=mean((y1m1-y1m0-indir1)^2); vindir0=mean((y0m1-y0m0-indir0)^2);
  c(tot, dir1, dir0, indir1, indir0, vtot, vdir1, vdir0, vindir1, vindir0, sum(selall))
}
