#' @import mmppr
#' @import BreakoutDetection
#' @import bcp
#' @importFrom forecast auto.arima
#' @import AnomalyDetection

#' Recommend a particular time series analysis
#'
#' @param df A dataframe, with a column of time series values and a column of counts
#'
capaldi <- function(df) {

}

#' Attempt to analyze a time series with a Markov Modulated Poisson Process
#' Return a verbose description of success and/or failure
#'
#' @param df A dataframe with two columns, one of which contains time series data.
mmppr <- function(df) {

  library(mmppr)

  # Is the data appropriate?
  # elaborate...

  # Run the time series analysis

  # Analyze the results.
}

#' Attempt to analyze a time series with the E-Divisive with Medians (EDM) Algorithm,
#' as implemented at Twitter.
#' Return a verbose description of success and/or failure
#'
#' @param df A dataframe with two columns, one of which contains time series data.
edm_algorithm <- function(df) {

  library(BreakoutDetection)

  # Is the data appropriate?
  # elaborate...

  # Run the time series analysis

  # Analyze the results.
}

#' Attempt to analyze a time series with the E-Divisive with Medians (EDM) Algorithm,
#' as implemented at Twitter.
#' Return a verbose description of success and/or failure
#'
#' @param df A dataframe with two columns, one of which contains time series data.
edm_algorithm <- function(df) {

  library(BreakoutDetection)

  # Is the data appropriate?
  # elaborate...

  # Run the time series analysis

  # Analyze the results.
}


#' Attempt to analyze a time series with a variant of Barry and Hartigan's product partition model.
#' Return a verbose description of success and/or failure
#'
#' @param df A dataframe with two columns, one of which contains time series data.
bcp_mcmc <- function(df) {

  library(bcp)

  # Is the data appropriate?
  # elaborate...

  # Run the time series analysis

  # Analyze the results.
}


#' Attempt to analyze a time series with an ARIMA model.
#' Return a verbose description of success and/or failure
#'
#' @param df A dataframe with two columns, one of which contains time series data.
arima <- function(df) {

  library(forecast)

  # Is the data appropriate?
  # elaborate...

  # Run the time series analysis
  fit<-auto.arima(x)
  bt<-Box.test(fit$residuals,
               lag=5,
               fitdf=length(fit$model$phi) + length(fit$model$theta))
  return(list("p.value"=bt$p.value,"ub"=as.vector(fit$resid+sd(fit$resid)),"lb"=as.vector(fit$resid-sd(fit$resid)),"resid"=as.vector(fit$resid),"phi"=as.vector(fit$model$phi),"theta"=as.vector(fit$model$theta),"D"=as.vector(fit$model$D)))


  # Analyze the results.
}


#' Attempt to analyze a time series with a Seasonal-Hybrid-ESD (S-H-ESD) model.
#' Return a verbose description of success and/or failure.
#'
#' @param df A dataframe with two columns, one of which contains time series data.
s.h.esd_alg <- function(df) {

  library(AnomalyDetection)

  # Is the data appropriate?
  # elaborate...

  # Run the time series analysis
  fit<-auto.arima(x)
  bt<-Box.test(fit$residuals,
               lag=5,
               fitdf=length(fit$model$phi) + length(fit$model$theta))
  return(list("p.value"=bt$p.value,"ub"=as.vector(fit$resid+sd(fit$resid)),"lb"=as.vector(fit$resid-sd(fit$resid)),"resid"=as.vector(fit$resid),"phi"=as.vector(fit$model$phi),"theta"=as.vector(fit$model$theta),"D"=as.vector(fit$model$D)))


  # Analyze the results.
}
