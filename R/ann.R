# built up NN

nn_cont <- function(y,x,ytest,xtest,l1=0,layers=1,units=3,epochs=500,batch_size=100,verbose=0){
  if(is.vector(x)){x = as.matrix(x)}

  model <- keras_model_sequential()
  i = 0
  while(i<layers){
    if (i==0){ n_input = ncol(x); lambda=l1; units_i=  units
    }else{ n_input = units; lambda=0; units_i=units  }

    model %>% layer_dense(units = units_i, activation = 'relu', input_shape = n_input,
                          kernel_regularizer = regularizer_l1(lambda),
                          kernel_initializer = initializer_glorot_uniform(1))
    model %>% layer_batch_normalization()
    i = i+1
  }

  model %>% layer_dense(units = 1,kernel_initializer = initializer_glorot_uniform(1) )

  model %>% compile(
    loss = "mean_squared_error",
    optimizer =  optimizer_adam(),
    metrics = "mean_squared_error"
  )
  NNfit <- model %>% fit(x, y, epochs = epochs, batch_size=batch_size, verbose = verbose,
                         validation_data = list (xtest, ytest))
  w=get_weights(model)[[1]]
  w=rowMeans(abs(w))

  y_pred = model %>% predict(xtest)
  val_loss_all = NNfit[["metrics"]][["val_loss"]]

  return (list("y_pred"=y_pred,"val_loss_all"=val_loss_all, "w"=w))
}



nn_bin <- function(y,x,ytest,xtest,l1=0,layers=1,units=3,epochs=500,batch_size=100,verbose=0){
  if(is.vector(x)){x = as.matrix(x)}

  model <- keras_model_sequential()
  i = 0
  while(i<layers){
    if (i==0){ n_input = ncol(x); lambda=l1; units_i=  units
    }else{ n_input = units; lambda=0;  units_i=units }

    model %>% layer_dense(units = units_i, activation = 'relu', input_shape = n_input,
                          kernel_regularizer = regularizer_l1(lambda),
                          kernel_initializer = initializer_glorot_uniform(1)
    )
    model %>% layer_batch_normalization()
    i = i+1
  }

  model %>% layer_dense(units=1, activation="sigmoid",kernel_initializer = initializer_glorot_uniform(1))
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer =  optimizer_adam(),
    metrics = "binary_crossentropy"
  )
  NNfit <- model %>% fit(x, y, epochs = epochs, batch_size=batch_size, verbose = verbose,
                         validation_data = list (xtest, ytest))
  w=get_weights(model)[[1]]
  w=rowMeans(abs(w))

  y_pred = predict_proba (model, xtest)
  val_loss_all = NNfit[["metrics"]][["val_loss"]]

  return (list("y_pred"=y_pred,"val_loss_all"=val_loss_all, "w"=w))
}


ann = function(y, x, ytest, xtest, l1,layers,units, epochs=500,batch_size=100){
  train.ybin=1*(length(unique(y))==2 & min(y)==0 & max(y)==1)
  if(train.ybin==1){nn_func = nn_bin}
  if(train.ybin!=1){nn_func = nn_cont}

  temp = nn_func(y,x, ytest,xtest, units= units, layers=layers, l1= l1,epochs=epochs, batch_size=batch_size)
  ypred = temp$y_pred
  loss = min(temp$val_loss_all)
  epoch_best = which.min(temp$val_loss_all)

  list("loss"=loss,"epochs"=epoch_best,"loss_epoch"=cbind(epoch_best,loss),"ypred"=ypred)
}













































































