# DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning
DeepMed is an approach for semi-parametric causal mediation analysis to estimate the natrual (in)direct effects of a binary treatment on an outcome of interet. DeepMed adopts the deep neural networks to estimate the nuisance parameters involved in the influence functions of the potential outcomes.
## Setup
The DeepMed package will use the R package "keras" to establish the neural networks. It also depends on the R packages "foreach" and "doMC" for parallel computation. Therefore, make sure that these packages have been installed.

Use the following command in R to install the package:
```
install.packages(pkgs="keras")    # install the "keras" package
install.packages(pkgs="foreach")  # install the "foreach" package
install.packages(pkgs="doMC")     # install the "doMC" package

library(devtools)
install_github("siqixu/DeepMed",ref="main") # install the "DeepMed" package
```
## Usage
```
DeepMed(y,d,m,x,hyper_grid,epochs=500,hyper,cv=TRUE,trim=0.05)
```
`y`: A numeric vector for the outcome variable in causal mediation analysis.

`d`: A numeric vector for the binary treatment variable in causal mediation analysis, which is coded as 0 or 1.

`m`: A numeric vector for the mediator variable in causal mediation analysis.

`x`: A numeric vector or matrix for the covariates in causal mediation analysis.

`hyper_grid`: A grid of candidate hyperparameters for deep neural networks. It is a numeric matrix with three columns for the number of hidden units, the number of hidden layers, and L1 regularizition parameter in the input layer, respecively. "hyper_grid" should be provided if cv==TRUE, then a 3-fold cross-validation will be used to select the hyperparameters.

`epochs`: The maximum number of candidate epochs. By default, epochs=500.

`hyper`: The hyperparameters of neural networks, i.e., L1 regularizition parameter, the number of hidden layers, the number of hidden units and epochs. The neural networks with hyperparameters specified by "hyper" will be used in a 3-fold cross-fitting to estiamte the nuisance parameters in the influence functions for the mean potential outcomes. "hyper" should be provided if cv==FALSE.
  
`cv`: If cv==TRUE, then "hyper_grid" should be provided and a 3-fold cross-validation will be used to select hyperparameters among "hyper_grid". Otherwise, hyperparameters should be provided in the argument "hyper". By default, cv=TRUE.
  
`trim`: The trimming rate for preventing conditional treatment or mediator probabilities from being zero. Observations with any denominators in the potential outcomes smaller than the trimming rate will be excluded from the analysis. By default, trim=0.05.

## Value
`results`: The estimates (effect), standard errors (se) and P values (p-val) of the total treatment effect (total), (in)direct treatment effect in treated ((in)dir.treat), and (in)direct treatment effect in control group ((in)indir.control).
 
`ntrimmed`: The number of observations being excluded due to the denominators in the potential outcomes smaller than the trimming rate. 


## Details
All binary variables in the data should be coded as 0 or 1.

## References
Xu S, Liu L and Liu Z. DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning. NeurIPS 2022.

## Acknowledgement
Some functions in " DeepMed" package are built upon the framework of the "medDML" function from the R package "causalweight". We thank the authors, Hugo Bodory and Martin Huber, of the "causalweight" package.

## Examples
```
library(DeepMed)
# use parallel computation with 30 cores for a grid search of hyperparameters
library(doMC); registerDoMC(30)

l1 = c(0,0.05,0.1)  # the L1 regularizition parameter of the input layer
layer = c(1:3)      # the number of hidden layers
unit = c(10,20,50)  # the number of hidden units
hyper_grid=expand.grid(unit,layer,l1) # create a grid of candidate hyperparameters

# run DeepMed on the example data with 6150 observations and two covariates. The computation time is around 1 hour. 
DeepMed(y,d,m,x,hyper_grid,cv=TRUE)  
```

  
  
  
