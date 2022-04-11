
cv_nn=function(ytrain, xtrain, hyper_grid, epochs=500,t){
  units=hyper_grid[t,1]
  layers=hyper_grid[t,2]
  l1=hyper_grid[t,3]

  stepsize=ceiling((1/3)*length(ytrain))
  nobs = min(3*stepsize,length(ytrain)); set.seed(1); idx = sample(nobs);
  sample1 = idx[1:stepsize]; sample2 = idx[(stepsize+1):(2*stepsize)];
  sample3 = idx[(2*stepsize+1):nobs];

  loss_t=epoch_t=numeric()
  for(i in 1:3){
    if (i==1) {tesample=sample1; trsample=c(sample2,sample3)}
    if (i==2) {tesample=sample3; trsample=c(sample1,sample2)}
    if (i==3) {tesample=sample2; trsample=c(sample1,sample3)}

    temp=ann(ytrain[trsample],xtrain[trsample,], ytrain[tesample],xtrain[tesample,],
           l1,layers,units, epochs=epochs)

    loss_t = c(loss_t, temp$loss)
    epoch_t = c(epoch_t,temp$epochs)
  }

  loss_t=mean(loss_t); epoch_t=mean(epoch_t)
  cbind(l1,layers,units,epoch_t,loss_t)
}






















