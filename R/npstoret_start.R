# ------------------------------------
#' Running npstoret2R
# ------------------------------------
#'
#' The following example shows how to prepare a run.R script for pulling in 
#' your NPSTORET data for analysis.\cr
#' \cr
#' Remember that this script will enable you to pull in data from the NPSTORET
#' database of your choice, but if you want to save the results of your
#' analysis, you'll want to save your Environment and its *.RData file for later use.\cr
#' \cr
#' ----------------------\cr
#' Initialize Environment & Connect to Db\cr
#' ----------------------\cr
#' environment for variables to avoid collisions with .GlobalEnv\cr
#' \code{ pkgEnv <- new.env(parent = emptyenv()) }
#'
#' \code{ initializeEnvironment() }
#' \cr
#' ----------------------\cr
#' Connect to Db\cr
#' ----------------------\cr
#' note app must be in both assignment as argument of dbfilepathname otherwise it doesn't work\cr \cr
#' \code{ app <- dbfilepathname(app, "C:/NPSTORET_DATA/NPSTORET_BE.MDB") }
#' \cr
#' \code{ app$connect <- odbcConnectAccess2007(app[["dbfilepathname"]]) }
#'
#' \code{ assign("npstoret",app,envir=pkgEnv) }
#' \cr \cr
#' create data frames\cr
#' \code{ initializeData() }
#'
#' @docType package
#' @name Starting with npstoret2R

NULL