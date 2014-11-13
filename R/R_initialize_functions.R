############################################################
# R initialize Functions
############################################################
# Purpose:    Single file access to miscellaneous NCPN app initialization functions
#
# Notes:      N/A
#
# Sources:  See individual functions
#              
# Revisions:  0.1  2014-09-29  B. Campbell  initial version
# ==========================================================

# ----------------------------------------------------------------------
#' @title initializeEnvironment
#' @description Load packages, functions and NPSTORET data into global (working) environment
#'
#' @section Requirements:
#'   none
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-09-30 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-30  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-08  \tab\tab BLC   \tab\tab Documentation update, removed unused siny, reshape2, & stringr package calls \cr
#'   \tab 0.3   \tab\tab 2014-11-12  \tab\tab BLC   \tab\tab Revise SQL paths to sqlPath \cr
#'   }
#'
#' @usage
#' initializeEnvironment() 
#'         
#' @section Notes:
#' This function accomplishes the following tasks: 
#' \itemize{
#' \item Increases memory limit to 4095
#' \item Calls necessary libraries - tools, RODBC, sqldf
#' \item Loads app class and packages functions into the environment
#' }
#'   
#' @family app initialization functions
#' @export
# ----------------------------------------------------------------------
initializeEnvironment <- function(){

  setWindowTitle("NCPN NPSTORET WQ App")

  # increase memory limit
  memory.limit(size = 4095)
  
  # ----------------------
  # --- set SQL path   --- 
  # ----------------------
  sqlPath <- paste(path.package("npstoret2R"),"/SQL/",sep="")
  
  # ----------------------
  # --- load packages  --- 
  # ----------------------
  # ensure required packages are installed, if not install them
  pkgTest("tools")
  pkgTest("RODBC")
  pkgTest("sqldf")
  
  # reference packages
  library(tools)
  library(RODBC)
  library(sqldf)
  
  # ----------------------
  # --- load classes --- 
  # ----------------------
  
  # App Class
  filePath = paste(getwd(),"/R/R_app_class.R",sep="")
  source(filePath)
    
  # ----------------------
  # --- load functions --- 
  # ----------------------
  
  # helpers
  filePath = paste(getwd(),"/R/R_helper_functions.R",sep="")
  source(filePath)
  # NPSTORET
  filePath = paste(getwd(),"/R/R_NPSTORET_functions.R",sep="")
  source(filePath)
  # WQ
  filePath = paste(getwd(),"/R/R_WQstds_functions.R",sep="")
  source(filePath)
  # WQ Results
  filePath = paste(getwd(),"/R/R_WQ_results_functions.R",sep="")
  source(filePath)  
}

# ----------------------------------------------------------------------
#' @title initializeData
#' @description Load NPSTORET data into global (working) environment
#'              The following data frames are generated:
#'              \itemize{
#'              \item dfResults - contains ALL NPSTORET results
#'              \item dfRAWStds - unmodified NPSTORET standard characteristics
#'              \item dfIndepChars - unmodified NPSTORET independent characteristics
#'              \item dfRAWDepChars - unmodified NPSTORET dependent characteristics
#'              \item dfDepCharResults - sample results for pH, hardness (Ca+Mg), temperature (dependencies)
#'              \item dfRAWChars - all unmodified NPSTORET characteristics
#'              \item depStds - dependent standards
#'              \item indepStds - independent standards
#'              \item dStds - dependent standards
#'              \item iStds -  independent standards
#'              \item pH - all pH sample results
#'              \item Hardness - all Hardness (Ca + Mg) sample results
#'              \item Temp - all water Temperature sample results
#'              }
#'              * Dependent characteristics include standard values which depend on other sample results (like pH, temp, hardness (Ca + Mg), time).
#'                Independent characteristics include standard values that do not rely on other sample results.      
#'
#' @section Requirements:
#' R libraries:
#'   \itemize{
#'   \item RODBC
#'   \item sqldf
#'   }
#' Valid NPSTORET database app dbfilepathname with required tables \cr
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-10-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-10-05  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-08  \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family app initialization functions
#' @export
# ----------------------------------------------------------------------
initializeData <- function(){
    
  # ------------------------
  #  Fetch NPSTORET Results
  # ------------------------
  # Start the clock!
  #ptm <- proc.time()
  
  dfResults <- loadNPSTORETWQData("Results")
  assign("dfResults", dfResults, envir=.GlobalEnv)
  
  # ------------------------
  #  Subset Results
  # ------------------------
  
  # prepare subsets for identifying if results have indep std / dep std / no std
  dfRAWStds <- getWQRawStdCharacteristics()
  assign("dfRAWStds", dfRAWStds, envir=.GlobalEnv)
  
  # -------------------------------------------
  #  Get Independent Characteristics
  # -------------------------------------------
  dfIndepChars <- getIndepWQStdChars() #characteristics
  assign("dfIndepChars", dfIndepChars, envir=.GlobalEnv)
  
  # -------------------------------------------
  #  Get Dependent Characteristics
  # -------------------------------------------
  dfRAWDepChars <- getRAWDepWQStdChars()
  assign("dfRAWDepChars", dfRAWDepChars, envir=.GlobalEnv)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # DATA CHECKS: 
  # dfDeps <- sqldf("SELECT DISTINCT CharDependent FROM dfRAWDepChars WHERE IsDependent = 1;")
  # RESULT ==> 5 values: 261, time, 481n, 481x, 1e
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # -------------------------------------------
  #  Get Dependent Characteristic Results (for std calculations)
  # -------------------------------------------
  dfDepCharResults <- getDepCharResults(dfResults)
  assign("dfDepCharResults", dfDepCharResults, envir=.GlobalEnv)
  
  # ------------------------
  #  Get Characteristics
  # ------------------------
  dfRAWChars <- getWQRawStdCharacteristics()
  assign("dfRAWChars", dfRAWChars, envir=.GlobalEnv)
  
  # ------------------------
  #  Subset Stds
  # ------------------------
  # subsets for quick match to subset results into indep/dep/no std results
  indepStds <- dfIndepChars[,c("DISPLAY_NAME", "LocCHDEF_IS_NUMBER")] # indep std   41586 / 2
  depStds <- dfRAWDepChars[,c("DISPLAY_NAME", "LocCHDEF_IS_NUMBER")] # dep std    9626 / 2
  
  # working dfs
  iStds <- dfIndepChars
  dStds <- dfRAWDepChars
  
  # convert SMPL_FRAC_TYPE_NM & MEDIUM to lower case for matching
  iStds$SMPL_FRAC_TYPE_NM <- tolower(iStds$SMPL_FRAC_TYPE_NM)
  dStds$SMPL_FRAC_TYPE_NM <- tolower(dStds$SMPL_FRAC_TYPE_NM)
  
  assign("indepStds", indepStds, envir=.GlobalEnv)
  assign("depStds", depStds, envir=.GlobalEnv)
  assign("iStds", iStds, envir=.GlobalEnv)
  assign("dStds", dStds, envir=.GlobalEnv)
  
  # unique(dfRAWDepChars$DISPLAY_NAME) # 11 stds: Cd, Cu, Pb, Ni, Ag, U, Mn, N, Zn, PCP, Temp
  # unique(depStds$DISPLAY_NAME) # matches dfRAWDepChars
  # unique(depStds$LocCHDEF_IS_NUMBER)
    
  # ------------------------
  #  Prep Results (add cols)
  # ------------------------
  # add columns to results sets
  allres <- AddResultColumns(dfResults)
  
  # convert SMPL_FRAC_TYPE_NM & MEDIUM to lower case
  allres$SMPL_FRAC_TYPE_NM <- tolower(allres$SMPL_FRAC_TYPE_NM)
  allres$MEDIUM <- tolower(allres$MEDIUM)
  
  # set Std_Code value
  allres$Std_Code <- "R"
  
  # ------------------------
  #  Add DepChar Result Values
  # ------------------------
  
  # set DepCharLookup
  allres$DepCharLookup <- "pH"
  
  assign("allres", allres, envir=.GlobalEnv)
    
  # subset dfDepCharResults to reduce memory issues
  pH <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "pH",] # 14827 / 65
  Temp <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "Temperature, water",] # 12111 / 65
  Hardness <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "Hardness, Ca + Mg",] # 11233 / 65
  
  # make dfs available to .GlobalEnv
  assign("pH", pH, envir=.GlobalEnv)
  assign("Temp", Temp, envir=.GlobalEnv)
  assign("Hardness", Hardness, envir=.GlobalEnv)

  # Stop clock
  #proc.time() - ptm
  #print(proc.time()-ptm)
}