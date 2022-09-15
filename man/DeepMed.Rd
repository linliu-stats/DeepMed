\name{DeepMed}
\alias{DeepMed}
\title{DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning}
\usage{
DeepMed(y,d,m,x,hyper_grid,epochs=500,hyper,cv=TRUE,trim=0.05)
}
\description{De-biased Deep Learning for Semi-parametric Causal Mediation Analysis
}

\arguments{
  \item{y}{A numeric vector for the outcome variable in causal mediation analysis.}
  \item{d}{A numeric vector for the exposure variable in causal mediation analysis, which should be a binary variable with values being 0 or 1.}
  \item{m}{A numeric vector for the mediator variable in causal mediation analysis.}
  \item{x}{A numeric vector or matrix for the covariates in causal mediation analysis.}
  \item{hyper_grid}{A grid of candidate hyperparameters for deep neural networks. It is a numeric matrix with three columns for the number of hidden units, the number of hidden layers, and L1 regularizition parameter in the input layer, respecively. "hyper_grid" should be provided if cv==TRUE, then a 3-fold cross-validation will be used to select the hyperparameters.}
  \item{epochs}{The maximum number of candidate epochs.}
  \item{hyper}{The hyperparameters of neural networks, i.e., L1 regularizition parameter, the number of hidden layers, the number of hidden units and epochs. The neural networks with hyperparameters specified by "hyper" will be used in a 3-fold cross-fitting to estiamte the nuisance parameters in the influence functions for the mean potential outcomes. "hyper" should be provided if cv==FALSE}
  \item{cv}{If cv==TRUE, then "hyper_grid" should be provided and a 3-fold cross-validation will be used to select hyperparameters among "hyper_grid". Otherwise, hyperparameters should be provided in the argument "hyper".}
  \item{trim}{The trimming rate for preventing conditional treatment or mediator probabilities from being zero. Observations with any denominators in the potential outcomes smaller than the trimming rate will be excluded from the analysis.}
}

\value{
  \item{results}{The estimates (effect), standard errors (se) and P values (p-val) of the total treatment effect (total), (in)direct treatment effect in treated ((in)dir.treat), and (in)direct treatment effect in control group ((in)indir.control).}
  \item{ntrimmed}{The number of observations being excluded due to the denominators in the potential outcomes smaller than the trimming rate.}
}
\details{All binary variables in the data should be coded as 0 or 1.}

\references{
Xu S, Liu L and Liu Z. DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning. NeurIPS 2022.
}

\examples{
library(DeepMed)
# use parallel computation with 30 cores for a grid search of hyperparameters
library(doMC); registerDoMC(30)

l1 = c(0,0.05,0.1)  # the L1 regularizition parameter of the input layer
layer = c(1:3)      # the number of hidden layers
unit = c(10,20,50)  # the number of hidden units
hyper_grid=expand.grid(unit,layer,l1) # create a grid of candidate hyperparameters
DeepMed(y,d,m,x,hyper_grid,cv=TRUE)
}
