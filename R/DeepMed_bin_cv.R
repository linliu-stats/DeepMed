DeepMed_bin_cv=function(y,d,m,x, hyper_grid,epochs=500){

  y=as.vector(y);d=as.vector(d);m=as.vector(m)
  if(is.null(nrow(x))){x=as.matrix(x,length(x),1)
  }else{x=as.matrix(x,nrow(x),ncol(x))}


  stepsize=ceiling((1/k)*length(d))
  set.seed(1); idx= sample(length(d), replace=FALSE)
  y1m0=c();y1m1=c();y0m0=c(); y0m1=c(); selall=c()
  # crossfitting procedure that splits sample in training an testing data
  hyper=numeric()
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

    hyper_k=numeric()
    # predict Pr(M=1|D=1,X) in test data
    # predict Pr(M=1|D=0,X) in test data
    # predict Pr(D=1|X) in test data
    # predict E(Y| D=1, M=1, X) in test data
    # predict E(Y| D=0, M=1, X) in test data
    # predict E(Y| D=1, M=0, X) in test data
    # predict E(Y| D=0, M=0, X) in test data
    # predict E(Y|D=1, X) in test data
    # predict E(Y|D=0, X) in test data
    out <- foreach(t=1:nrow(hyper_grid), .combine=rbind, .packages=c("keras")) %dopar% {
      out1 = cv_nn(mtr1,xtr1, hyper_grid, epochs, t)
      out2 = cv_nn(mtr0,xtr0, hyper_grid, epochs, t)
      out3 = cv_nn(dtr, xtr, hyper_grid, epochs, t)
      out4 = cv_nn(ytr11, xtr11, hyper_grid, epochs, t)
      out5 = cv_nn(ytr01, xtr01, hyper_grid, epochs, t)
      out6 = cv_nn(ytr10, xtr10, hyper_grid, epochs, t)
      out7 = cv_nn(ytr00, xtr00, hyper_grid, epochs, t)
      out8 = cv_nn(ytr1, xtr1, hyper_grid, epochs, t)
      out9 = cv_nn(ytr0, xtr0, hyper_grid, epochs, t)

      out=cbind(out1,out2,out3,out4,out5,out6,out7,out8,out9)
    }

    for(i in 1:9){
      outi=out[,1:5]
      out=out[,-c(1:5)]
      loc = which.min(outi[,5])
      hyper_k = cbind(hyper_k, outi[loc,]  )
    }
    colnames(hyper_k)=1:9


    if(k==1){hyper=hyper_k
    }else{hyper = cbind(hyper,hyper_k)}
  }

  return(hyper)
}
