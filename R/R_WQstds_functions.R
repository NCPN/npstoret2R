############################################################
# R NCPN WQ Standard Criteria Functions
############################################################
# Purpose:    Single file access to WQ Std Related NCPN functions
#
# Notes:      N/A
#
#             Place this file in the same directory as .Rhistory
#
# Sources:  See individual functions
#              
# Revisions:  0.1  2014-06-13  B. Campbell  initial version
#             0.2  2014-09-08  B. Campbell  use droplevels to remove unused levels from dfs
#             0.3  2014-11-11  B. Campbell  update documentation
# ==========================================================
#
# Process:
#   1) Retrieve ALL WQ criteria for ALL standards
#
#   2) Prepare subsets of WQ criteria (all sets include visit date & stationID)
#       a) Independent Criteria - IsDependent = 0, No DependentCharacteristic
#       b) Dependent Criteria - IsDependent = 1, DependentCharacteristic present
#          Add columns to subsets for:
#             "CalcStdValue" (calculated std value)
#             "LookupCriteria" (dependency values needed for calculations)
#          Look up & populate LookupCriteria values (per visitDate & stationID)
#          Run rapply to each row (@ stationID & visitDate) & populate CalcStdValue
#       c) Dependency Criteria - pH, Hardness, Temp (1, 261, 481)
#          Subset these by specific values:
#             i) pH
#             ii) Hardness
#             iii) Temp
#          Add columns to subsets for:
#             "CalcStdValue" (calculated std value)
#             "LookupCriteria" (dependency values needed for calculations)
#             "CalcFormula" (formula used for calculation, validity check)
#          Look up & populate LookupCriteria values (per visitDate & stationID)
#          Run rapply to each row (@ stationID & visitDate) & populate CalcStdValue
#
#       Use subset (c) to prepare subset (b) calculated standard values
#
#   3) Prepare final WQ criteria (per stationID & visitDate)
#         Create df by UNION of (a) & (b) subsets above ((c) is already included)
#
#   4) Compare results against final WQ criteria (per stationID & visitDate)
#        a) Run LoadNPSTORETData to fetch results
#        b) Add columns to results df
#            i) "Compliance" (result compared to std, Hi = 1, Std = 0, Lo = -1)
#                             default to NULL to avoid false compliancy
#            ii) "StdComparison" (std value result was compared to, validity check)
#            iii) "CalcFormula" (formula used for calculation, validity check)
#
#   NOTE:
#       Time dependent characteristics WILL NOT be included @ this time
#       due to their complexity
#
# ======================================================================

# ----------------------------------------------------------------------
#' @title getWQRawStdCharacteristics: 
#' @description Fetch ALL NPSTORET WQ Standard Characteristics for Dependent (Calculated) & 
#'              Independent (Uncalculated) Standards
#'
#' @note Standards are 'raw' pulled directly from NPSTORET - NO CALCULATED VALUES!
#'       DO NOT USE for dependent characteristic standard values
#'
#' @section Requirements:
#' R libraries:
#' \itemize{
#'    \item RODBC
#'   }
#'
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-06-13 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-13 \tab\tab   BLC \tab\tab  Initial version \cr
#'   \tab 0.2   \tab\tab 2014-06-24 \tab\tab   BLC  \tab\tab Added StnID column & converted to non-scientific notation \cr
#'   \tab 0.3   \tab\tab 2014-09-08 \tab\tab   BLC  \tab\tab Drop unused levels \cr
#'   \tab 0.4   \tab\tab 2014-10-05 \tab\tab   BLC  \tab\tab Add check to ensure dfRAWStds is a data.frame \cr
#'   \tab 0.5   \tab\tab 2014-11-11 \tab\tab   BLC  \tab\tab Documentation update \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getWQRawStdCharacteristics <- function(){  
  dfRAWstds <- loadNPSTORETWQData("WQProjStnStdCritAlt")
  
  # dataframe check w/ lazy (&&) evaluation (2nd condition evaluates only if 1st is true)
  if (exists("dfRAWStds") && is.data.frame(get("dfRAWStds"))){  
    # convert StationID to non-scientific notation
    # add nonsci column
    dfRAWstds$Station_ID <- format(dfRAWstds$StationID, scientific = FALSE)
        
    # remove spaces in column names
    names(dfRAWstds) <- sub(" ", "_", names(dfRAWstds))
    
    # reorder columns (places Station_ID after StationID)
    dfRAWstds <- dfRAWstds[,c(1:16,43,17:42)]
    
    # drop unused levels
    dfRAWstds <- droplevels(dfRAWstds)
  }
  
  return(dfRAWstds)
}

# ----------------------------------------------------------------------
#' @title getIndepWQStdChars
#' @description Fetch ALL NPSTORET WQ Standard Characteristics for Independent (Uncalculated) Standards
#'
#' @section Requirements:
#'  R libraries:
#'   \itemize{
#'    \item RODBC
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-06-13 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-13    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-18    \tab\tab BLC   \tab\tab Added stringsAsFactors = FALSE to prevent
#'                             factorization of df strings & subsequent
#'                             limitation of df values & fix df names & station \cr
#'   \tab 0.3   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update, add check for dfRAWstds & remove sqldf depedency \cr
#'  }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getIndepWQStdChars <- function(){

  # check for dfRAWstds
  if (!(dfExists(dfRAWstds, "dfRAWstds"))) {
    dfRAWstds <- getWQRawStdCharacteristics()
  }
  
  # subset only chars
  #       a) Independent Criteria - IsDependent = 0, No DependentCharacteristic
  dfIndepStds <- dfRAWstds[, IsDependent = 0]

  # remove unused levels
  dfIndepStds <- droplevels(dfIndepStds)
  
  # cleanup
  cleanUp(c('dfRawstds'), FALSE)
  
  return(dfIndepStds)
}

# ----------------------------------------------------------------------
#' @title getRAWDepWQStdChars
#' @description fetch ALL NPSTORET WQ Standard Characteristics for Dependent (Uncalculated) Standards
#'
#' @section Requirements:
#'  R libraries:
#'   \itemize{
#'    \item RODBC
#'   }
#' 
#' @section Sources:
#' \tabular{llllllll}{
#'    \tab 2014-06-25 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'    }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-25    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-08    \tab\tab BLC   \tab\tab Drop unused levels \cr
#'   \tab 0.3   \tab\tab 2014-09-10    \tab\tab BLC   \tab\tab Remove all extra columns except CalcFormula
#'                             since these are time/date dependent & require lookups
#'                             Hardness, pH, & Temp will be added to RESULTS vs chars dataframes \cr
#'   \tab 0.5   \tab\tab 2014-09-18    \tab\tab BLC   \tab\tab Added stringsAsFactors = FALSE to prevent
#'                             factorization of df strings & subsequent
#'                             limitation of df values, fix spaces & reordering df \cr
#'   \tab 0.6   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update, add check for dfRAWStds & remove sqldf depedency \cr
#'  }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getRAWDepWQStdChars <- function(){  

  # check for dfRAWstds
  if (!(dfExists(dfRAWstds, "dfRAWstds")) ){
    dfRAWstds <- getWQRawStdCharacteristics()
  }
  
  # subset only chars
  #       b) Dependent Criteria - IsDependent = 1, DependentCharacteristic present
  dfRAWDepWQStdChars <- dfRAWstds[, IsDependent = 1]
    
  # remove unused levels
  dfRAWDepWQStdChars <- droplevels(dfRAWDepWQStdChars)
  
  # cleanup
  cleanUp(c('dfRawstds'), FALSE)
  
  return(dfRAWDepWQStdChars)
}

# ----------------------------------------------------------------------
#' @title getDepCharResults
#' @description Fetch ALL NPSTORET WQ Results for Dependent Characteristics
#'
#' @param dfResults - WQ Results dataframe (optional, if already generated)
#' 
#' @return
#'   dataframe - contains dependent characteristic results (pH, temp, hardness(Ca+Mg))
#'               for all stations & visits (filters results by dependent chars)
#'               
#' @section Requirements:
#'  R libraries:
#'   \itemize{
#'    \item RODBC
#'    \item sqldf
#'   }
#'
#' @section Sources:
#'   Sven Hohenstein, December 13, 2013
#'   \url{http://stackoverflow.com/questions/20573119/check-if-data-frame-exists}
#' @section Adapted:
#' \tabular{llllllll}{
#'   \tab 2014-06-25 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#' }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-25    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-08    \tab\tab BLC   \tab\tab Drop unused levels \cr
#'   \tab 0.3   \tab\tab 2014-09-18    \tab\tab BLC   \tab\tab Include StringsAsFactors = FALSE to ensure
#'                             characters are not limited to factor values \cr
#'   \tab 0.4   \tab\tab 2014-10-05    \tab\tab BLC   \tab\tab Change results check to use loadNPSTORETWQData() vs loadNPSTORETData() \cr
#'   \tab 0.5   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getDepCharResults <- function(dfResults){  
  
  if (!(dfExists(dfResults, "dfResults"))) {
    # fetch NPSTORET results
    dfResults <- loadNPSTORETWQData("Results")   
  }
  
  #fetch characteristics list
  dfChars <- loadNPSTORETWQData("Characteristics")
  
  # subset of dependent chars - pH, temp, hardness
  dfCharDeps <- sqldf("SELECT * FROM dfChars WHERE TSRCHAR_IS_NUMBER IN (1,261,481)", stringsAsFactors=FALSE);
  
  # subset of dependent chars results
  dfCharDepResults = sqldf("SELECT * FROM dfResults WHERE LocChDef_IS_NUMBER IN (SELECT LocCHDEF_IS_NUMBER FROM dfCharDeps)", stringsAsFactors=FALSE)

  # drop unused levels
  dfCharDepResults = droplevels(dfCharDepResults)
  
  # cleanup
  cleanUp(c('df','dfChars','dfCharDeps'), FALSE)
  
  return(dfCharDepResults)
}

# ----------------------------------------------------------------------
#' @title getDepCharDfs
#' @description Generate dataframes for dependent characteristics
#'
#' @param results - WQ results dataframe
#'
#' @section Returns:
#'   N/A - nothing
#'
#' @section Requirements:
#'  none
#'  
#' @section Sources:
#'  \tabular{llllllll}{
#'   \tab 2014-09-09 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-09    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getDepCharDfs <- function(results){
  
  # prepare 2-d matrix
  depChars <- c("pH", "Hardness", "Temp")
  depDisplayNames <- c("pH", "Hardness, Ca + Mg", "Temperature, water")
  depCharMatrix <- cbind(depChars, depDisplayNames)
  
  # default list
  dfs <- list()
  
  for (i in 1:nrow(depCharMatrix)){
  
    # create the dynamically named data frames & return as a list    
    dfs[[depCharMatrix[i,1]]] <- droplevels(results[which(results$DISPLAY_NAME == depCharMatrix[i,2]), ])
  
  }

  return(dfs)
}

# ----------------------------------------------------------------------
#' @title getStdDepCharDisplayName
#' @description Fetch the display name for a characteristic's dependent characteristic
#'
#' @param depCharacteristic - CharDependent value from NPSTORET (dfRAWDepChars)
#'
#' @return display_name - string value for the display name of the characteristic used to 
#'          determine the WQ standard
#'          This includes variables like pH, lnVal (hardness, Ca+Mg), temp
#'          The values for these variables are in separate columns of the result dataframe.
#'
#' @section Requirements:
#'  none
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-09-17 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-17    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getStdDepCharDisplayName <- function(depCharacteristic){
  # --------------------------------------------------
  #  StandardValue Dependent Characteristic Formulae
  # --------------------------------------------------
  #   
  #  Dependent characteristics: 
  #       261 (Hardness, Ca + Mg)
  #       1e (pH exponential)
  #       1x (pH power of 10)
  #       481n & 481x (pH power of 10 & temp (min & max))
  #       time --- NOT HANDLED ---
  #
  # --------------------------------------------------
  # default
  display_name = "no dependency"
  switch(as.character(depCharacteristic),
         "1e"= {display_name = "pH"},
         "1p"= {display_name = "pH"},
         "261"= {display_name = "Hardness, Ca + Mg"},
         "481n"= {display_name = "Temperature, water"},
         "481x"= {display_name = "Temperature, water"},
         "time"= {display_name = "time"}
  )
  return(display_name)
}

# ----------------------------------------------------------------------
#' @title getStdFormula
#' @description Fetch the formula for calculating dependent characteristic standards
#'
#' @param depCharacteristic - CharDependent value from NPSTORET (dfRAWDepChars)
#'
#' @return calc - string value for the calculation used to determine the WQ standard
#'          This includes variables like pH, lnVal (hardness, Ca+Mg), temp, & coefficients 1-8.
#'          The values for these variables are in separate columns of the result dataframe.
#'
#' @section Requirements:
#'  none
#' 
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-06-30 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-30    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getStdFormula <- function(depCharacteristic){
  # --------------------------------------------------
  #  StandardValue Dependent Characteristic Formulae
  # --------------------------------------------------
  #   
  #  Dependent characteristics: 
  #       261 (Hardness, Ca + Mg)
  #       1e (pH exponential)
  #       1x (pH power of 10)
  #       481n & 481x (pH power of 10 & temp (min & max))
  #       time --- NOT HANDLED ---
  #
  # --------------------------------------------------
  # default
  calc = "no formula"
  switch(as.character(depCharacteristic),
         "1e"= {calc = "exp((coef1*pH)+coef2)"},
         "1p"= {calc = "(coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4))) "},
         "261"= {calc = "(exp((coef1*lnVal) + coef2)*(coef3 + (coef4*lnVal)))"},
         "481n"= {calc = "((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * min(coef5,coef6*10^(coef7*(coef8-T)))"},
         "481x"= {calc = "((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * coef5*10^(coef6(coef7-max(T,coef8)))"},
         "time"= {calc = "time dependent formula required"}
  )
  return(calc)
}

# ----------------------------------------------------------------------
#' @title getHardnessLnVal 
#' @description Fetch the hardness (Ca+Mg) lnVal for calculating hardness dependent characteristic standards
#'
#' @param hardness - hardness (Ca+Mg) result value for a given stationID & visit (dfResults)
#'
#' @return lnVal - numeric value for the log 10 of hardness OR -99999999 if hardness = 0
#'
#' @section Requirements:
#'  R libraries:
#'   \itemize{
#'    \item RODBC
#' }
#'
#' @section Sources:
#'\tabular{llllllll}{
#'   \tab 2014-06-30 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-30    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-08    \tab\tab BLC   \tab\tab fixed to be natural log (ln) in R vs log10
#'                             log(hardness) vs. log(10, hardness) where 10 identifies base 10
#'                             & added check for hardness values = "*Non-detect"
#'                             added NULL check & as.numeric for handling hardness log evaluations \cr
#'   \tab 0.3   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'  }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getHardnessLnVal <- function(hardness){
  
  # handle NULLs
  if(is.null(hardness)){
    # if no value exists for hardness give -99999999
    lnVal = -99999999
    return(lnVal)
  }
  
  #handle non-detects
  switch( as.character(hardness),
    "*Non-detect" = {lnVal = -88888888},
    
    "0" = {
      # if no value exists for hardness give -99999999
      lnVal = -99999999},
  
    {
      # default
      # R equivalent of ln(hardness) 
      
      lnVal = log(as.numeric(hardness))}
  )
    
  return(lnVal)  
}

# ----------------------------------------------------------------------
#' @title lookupDependentCharResult
#' @description Lookup dependent characteristic result value (pH, H20 temp, hardness)
#'
#' @param depChar       - NPSTORET dependent characteristic value (CharDependent)
#' @param stationID     - NPSTORET StationID
#' @param startDate     - NPSTORET visit StartDate
#' @param startTimeZone - NPSTORET visit StartTimeZone
#' @param uom           - NPSTORET unit of measure (UOM)
#' @param medium        - NPSTORET measurement medium (MEDIUM)
#' @param field_lab     - NPSTORET field or lab mesurement (FIELD_LAB)
#'
#' @return resultVal - results value for the matched characteristic
#'
#' @section Requirements:
#' none
#'
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-07-02 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#'   Richie Cotton, April 24, 2012
#'   \url{http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r}
#'   Dason, Jun 2, 2012
#'   \url{http://stackoverflow.com/questions/10865095/why-do-i-get-warning-longer-object-length-is-not-a-multiple-of-shorter-object-l}
#'   
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-07-02    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-11    \tab\tab BLC   \tab\tab Fixed match to return proper result value \cr
#'   \tab\tab\tab\tab\tab\tab\tab Replaced length with nrow to get proper # of rows (vs. df width/cols) \cr
#'   \tab\tab\tab\tab\tab\tab\tab Replaced == with \%in\% for comparisons \cr
#'   \tab 0.3   \tab\tab 2014-11-1    \tab\tab BLC   \tab\tab Documentation update \cr
#'    }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
lookupDependentCharResult <- function(depChar, stationID, startDate, startTimeZone, uom, medium, field_lab){

  # default value
  depCharValue = ""

  # --------------------------------------------------
  #  StandardValue Dependent Characteristic Formulae
  # --------------------------------------------------
  #   
  #  Dependent characteristics: 
  #       261 (Hardness, Ca + Mg)
  #       1e (pH exponential)
  #       1x (pH power of 10)
  #       481n & 481x (pH power of 10 & temp (min & max))
  #       time
  #
  # --------------------------------------------------
  
  # determine string to match
  switch(as.character(depChar),
         "1e" = {charName = "pH"},
         "1n" = {charName = "pH"},
         "261" = {charName = "Hardness, Ca + Mg"},
         "481n" = {
           # both temp & pH are required, handle pH separately
           charName = "Temperature, water" 
         },
         "481x" = {
           # both temp & pH are required, handle pH separately
           charName = "Temperature, water" 
         },
         {
          # no dependent characters
           return("NA")
         }
  )
  
  # match ALL values
#  dfMatch <- dfDepCharLookup[with(dfDepCharLookup,which(DISPLAY_NAME==charName & 
#                           StationID == stationID & 
#                            as.Date(as.character(START_DATE,"%Y-%m-%d"))==as.Date(startDate) & 
#                            START_TIME_ZONE == startTimeZone, UOM==uom & 
#                            MEDIUM==medium & FIELD_LAB==field_lab)),]

# ================================================
#  UOM values present in dfResults (sqldf("SELECT DISTINCT UOM FROM dfResults"))
# ================================================
# 1  deg C     2  cfs       3  uS/cm     4  mg/l      5  None      6  mg/l CaCO3
# 7  %         8  m3/sec    9  tons/day  10 tons/ac f 11 ug/l      12       <NA>
# 13 cfu/100ml 14 #/ml      15 NTU       16 g/m2      17 mm/Hg     18 ug/g      
# 19 g/min     20 ft/sec    21 mgd       22 ppth      23 #/100ml   24 ppt       
# 25 pCi/L     26 m         27 mg/m2     28 count     29 ft        30 ug/kg     
# 31 g/kg      32 mg/kg     33 deg F     34 mi2       35 PCU       36 JTU       
# 37 Sec       38 umho/cm   39 gal/min   40 T.U.      41 units/cm  42 g/ml      
# 43 FNU       44 MPN/100ml 45 nu        46 0/00      47 ml 
# ================================================

# replace problematic values with appropriate equivalent words
# str_replace_all(x, "/", " per ")
# str_replace_all(x, "%", " percent ")
# str_replace_all(x, "#/", " number per ")
# str_replace_all(x, "<NA>", " NA ")
# ALL wrapped together:
#  str_replace_all(str_replace_all(str_replace_all(str_replace_all(x, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per ")
#  str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(dfResults[1,"UOM"], "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per "))

#  dfMatch <- dfDepCharLookup[with(dfDepCharLookup,which(as.Date(as.character(START_DATE,"%Y-%m-%d"))==as.Date(startDate) 
#                                          & DISPLAY_NAME==charName
#                                          & StationID==stationID
#                                          & MEDIUM==medium
#                                          & FIELD_LAB==field_lab
#                                          & str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(UOM, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per "))
#                                          ==str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(uom, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per "))
#                            )),]

# "== does elementwise checking for equality"
# "find which of the elements of X that are in the vector Y" <<<<< use %in% instead!!

dfMatch <- dfDepCharLookup[with(dfDepCharLookup,
                                which(
                                  as.Date(as.character(START_DATE,"%Y-%m-%d"))==as.Date(as.character(startDate)) 
                                  & as.character(DISPLAY_NAME)==as.character(charName)
                                  & as.character(StationID)==as.character(stationID)
                                  & as.character(MEDIUM)==as.character(medium)
                                  & as.character(FIELD_LAB)==as.character(field_lab)
                                  & as.character(str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(UOM, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per ")))==as.character(str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(uom, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per ")))
                                )),]

  # assumes 1 result per match
  # use nrow(df) vs. length(df) as the latter is the width (#cols) of the df vs the #rows
  if(nrow(dfMatch)>1){
    # give only the first result
    depCharResultValue = as.character(dfMatch$RESULT_TEXT)
    depCharResultValue = depCharResultValue[1]
  }else{
    depCharResultValue = as.character(dfMatch$RESULT_TEXT)
  }

  print(depCharResultValue)

  return(depCharResultValue)
}

# ----------------------------------------------------------------------
#' @title lookupIndependentCharStd
#' @description Lookup independent characteristic std value
#'
#' @param LocChDefNum   - WQ result local independent characteristic ID number (LocChDef_IS_NUMBER)
#' @param projectID     - WQ result ProjectID
#' @param stationID     - WQ result StationID
#' @param uom           - WQ result unit of measure (UOM)
#' @param medium        - WQ result measurement medium (MEDIUM)
#' @param field_lab     - WQ result field or lab mesurement (FIELD_LAB)
#'
#' @return indepCharStdValue - standard value for the matched independent characteristic 
#'                       at the station/project identified
#'
#' @section Requirements:
#' none
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-07-02 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#'   \tabular{l}{
#'   Richie Cotton, April 24, 2012 \cr
#'   \url{http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r} \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-07-02    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-07-08    \tab\tab BLC   \tab\tab Removed with & identified dfIndepChars for df variables \cr
#'   \tab 0.3   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
lookupIndependentCharStd <- function(LocChDefNum, projectID, stationID, uom, medium, field_lab){
  
  # ensure stringr package is available
  pkgTest("stringr")
  library("stringr")
  
  # default value
  indepCharStdValue = ""
  
# match ALL values

# ================================================
#  UOM values present in dfResults (sqldf("SELECT DISTINCT UOM FROM dfResults"))
# ================================================
# 1  deg C     2  cfs       3  uS/cm     4  mg/l      5  None      6  mg/l CaCO3
# 7  %         8  m3/sec    9  tons/day  10 tons/ac f 11 ug/l      12       <NA>
# 13 cfu/100ml 14 #/ml      15 NTU       16 g/m2      17 mm/Hg     18 ug/g      
# 19 g/min     20 ft/sec    21 mgd       22 ppth      23 #/100ml   24 ppt       
# 25 pCi/L     26 m         27 mg/m2     28 count     29 ft        30 ug/kg     
# 31 g/kg      32 mg/kg     33 deg F     34 mi2       35 PCU       36 JTU       
# 37 Sec       38 umho/cm   39 gal/min   40 T.U.      41 units/cm  42 g/ml      
# 43 FNU       44 MPN/100ml 45 nu        46 0/00      47 ml 
# ================================================

# replace problematic values with appropriate equivalent words
# str_replace_all(x, "/", " per ")
# str_replace_all(x, "%", " percent ")
# str_replace_all(x, "#/", " number per ")
# str_replace_all(x, "<NA>", " NA ")
# ALL wrapped together:
#  str_replace_all(str_replace_all(str_replace_all(str_replace_all(x, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per ")
#  str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(dfResults[1,"UOM"], "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per "))

dfMatch <- dfIndepChars[which( 
          dfIndepChars$LocCHDEF_IS_NUMBER==LocChDefNum
          & dfIndepChars$StationID==stationID
          & dfIndepChars$ProjectID==projectID
          & dfIndepChars$Medium==medium
          & dfIndepChars$FIELD_LAB==field_lab
          & str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(dfIndepChars$UOM, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per "))
          ==str_trim(str_replace_all(str_replace_all(str_replace_all(str_replace_all(uom, "<NA>", " NA "), "#/", " number per "), "%", " percent "), "/", " per "))
),]

# assumes 1 result per match
if(length(dfMatch)>1){
  # give only the first result
  indepCharStdValue = as.character(dfMatch$RESULT_TEXT)
  indepCharStdValue = indepCharStdValue[1]
}else{
  indepCharStdValue = as.character(dfMatch$RESULT_TEXT)
}

print(indepCharStdValue)

return(indepCharStdValue)
}

# ----------------------------------------------------------------------
#' @title calculateStdValues
#' @description Add calc std values to dfResults
#'
#' @param depChar       - NPSTORET dependent characteristic value (CharDependent)
#' @param depCharValue  - result value of first dependent characteristic (pH, hardness, Ca+Mg)
#' @param depCharValue2 - result value of second dependent characteristic (temp, water) if present
#' @param coef1         - NPSTORET calculation coefficients 1-8
#' @param coef2         - NPSTORET calculation coefficients 1-8
#' @param coef3         - NPSTORET calculation coefficients 1-8
#' @param coef4         - NPSTORET calculation coefficients 1-8
#' @param coef5         - NPSTORET calculation coefficients 1-8
#' @param coef6         - NPSTORET calculation coefficients 1-8
#' @param coef7         - NPSTORET calculation coefficients 1-8
#' @param coef8         - NPSTORET calculation coefficients 1-8
#' 
#' @return resultVal - results value for the matched characteristic
#'
#' @section Requirements:
#' none
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-07-02 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-07-02    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation udpate \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
calculateStdValues <- function(depChar, depCharValue, depCharValue2, coef1, coef2, coef3, coef4, coef5, coef6, coef7, coef8){
  #default
  calcStd = ""
  
  calc = "no formula"
  switch(as.character(depChar),
         "1e"= {  pH = depCharValue
                  calc = exp((coef1*pH)+coef2)},
         "1p"= {  pH = depCharValue
                  calc = (coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))},
         "261"= { lnVal = getHardnessLnVal(depCharValue)
                  calc = (exp((coef1*lnVal) + coef2)*(coef3 + (coef4*lnVal)))},
         "481n"= {T = depCharValue
                  pH = depCharValue2
                  calc = ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * min(coef5,coef6*10^(coef7*(coef8-T)))},
         "481x"= {T = depCharValue
                  pH = depCharValue2
                  calc = ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * coef5*10^(coef6(coef7-max(T,coef8)))},
         "time"= {calc = "time dependent formula required"}
  )
  
  return(calc)
}

# ----------------------------------------------------------------------
#' @title compareToStandards
#' @description Compare results to standard values
#'
#' @param resultValue - WQ results
#' @param stdValue    - WQ standard value (either dependent or independent)
#' @param loHi        - indicator if WQ standard is a low or high standard
#'                 low = results values should not be below the standard
#'                 high = results values should not be above the standard
#'
#' @return compliance - indicator for whether WQ results were in/out of compliance
#'                 -1 = result below WQ standard
#'                  0 = result in acceptable range
#'                 +1 = result above WQ standard
#'
#' @section Requirements:
#'  \tabular{l}{
#'    RODBC library package loaded \cr
#'    Working directory set for app \cr
#'    LoadNPSTORET function \cr
#' }
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-07-03 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-07-03    \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
compareToStandards <- function(resultValue, stdValue, loHi){
  # default
  compliance = NULL
  
  switch(as.character(loHi),
         # std = lower limit (threshold)
         "1" = {
           if(resultValue < stdValue){
             compliance = -1
           }else if(resultValue == stdValue){
             compliance = 0  
           }
         },
         # std = upper limit (exceedence)
         "0" = {
           if(resultValue > stdValue){
             compliance = 1
           }else if(resultValue == stdValue){
             compliance = 0  
           }
         }
  )
  
  return(compliance)
}

# ----------------------------------------------------------------------
#' @title depCharResultsLookup
#' @description Fetch specific Dependent Characteristic NPSTORET WQ Result
#'
#' @param dfDepCharResults  - WQ Results dataframe for pH, temp, hardness (Ca+Mg)
#'                       (optional, if already generated)
#' @param characteristic    - dependent characteristic to lookup
#' @param startVisitDate    - target visit date (start of range)
#' @param endVisitDate      - target visit date (end of range), use startVisitDate if looking
#'                       for characteristics for one visit date
#' @param stationID         - specific stationID, default = 0 for ALL stations
#'
#' @return dataframe - includes dependent characteristic values & info for given
#'                       characteristic (pH, temp, hardness(Ca+Mg)) 
#'                       w/in the visit range (startVisitDate, endVisitDate)
#'                       for the station identified (or ALL stations if 0)
#'                       may include:  0 rows (no results found)
#'                                     1 row (usable for calculations)
#'                                     many rows (multiple visits or stations)
#' @section Requirements:
#' R libraries
#' \itemize{
#'  \item \code{\link[sqldf]{sqldf}}
#' }
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-06-26 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-26    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-18    \tab\tab BLC   \tab\tab Added stringsAsFactors=FALSE to avoid
#'                             factorization of df character strings &
#'                             subsequent limitation of df to those values \cr
#'   \tab 0.3   \tab\tab 2014-11-11    \tab\tab BLC   \tab\tab Documentation update \cr
#'  }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
depCharResultsLookup <- function(dfDepCharResults, characteristic, startVisitDate, endVisitDate, stationID=0){  
  
  if (!(dfExists(dfDepCharResults, "dfDepCharResults"))) {
    # fetch NPSTORET dependent characteristics results for pH, temp, hardness(Ca+Mg)
    dfDepCharResults <- getDepCharResults()   
  }
  
  # subset dependent chars - pH, temp, hardness(Ca+Mg)
  dfSpecificDepCharResult <- dfDepCharResults[with(dfDepCharResults, DISPLAY_NAME == characteristic, )]
  
  # prepare date range statment
  if(startVisitDate)
  sqlDateRange = ""
  
  # requires sqldf
  pkgTest("sqldf")
  library(sqldf)
    
  # subset of dependent chars - pH, temp, hardness
  dfCharDeps <- sqldf("SELECT * FROM dfChars WHERE TSRCHAR_IS_NUMBER IN (1,261,481)", stringsAsFactors=FALSE);
    
  # cleanup
  cleanUp(c('sqlDateRange','startVisitDate','endVisitDate','stationID'), FALSE)
  
  return(dfCharDepResults)
}

# ----------------------------------------------------------------------
#' @title getExtendedDepCharResults
#' @description Fetch dependent char results with lookup values for calculating dependent standards, formulae, & calculated std
#'
#' @section Assumptions:
#'   NPSTORET results are available as dfResults
#'
#' @return df - dfDepCharResults plus pH, Hardness, Temp, Hardness_UOM, Temp_UOM, CalcFormula,
#'        Values for these variables are in separate columns of the result dataframe.
#'        CalcStdValue is also provided and is based on CalcFormula and the applicable
#'        standard characteristics
#'
#' @section Requirements:
#'  none
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-09-10 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-10    \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getExtendedDepCharResults <- function(){
  
  # -------------------------------------------
  #  Get Dependent Characteristics Results
  # -------------------------------------------
  dfDepCharResults <- getDepCharResults(dfResults)
  
  # Add column for numeric dependent characteristics only (not the 1e, 481x, etc lookups)
  
  dfDepCharResults$CharDepNum <- as.numeric(as.character(gsub("[[:alpha:]]","",dfDepCharResults$CharDependent)))
  
  # Add columns to subsets for:
  #         "CalcStdValue" (calculated std value)
  #          Lookup criteria - pH, Hardness (Ca + Mg), Temp (dependency values needed for calculations)
  #          Look up & populate Lookup Criteria values (per visitDate & stationID)
  #          Run rapply to each row (@ stationID & visitDate) & populate CalcStdValue
  
  # add columns Hardness (Ca+Mg), Hardness_UOM, pH, Temp, Temp_UOM for
  # handling lookups of hardness, pH, and temperature & their units of measure (UOM)
  # (these WILL be NULL, but are needed for merging dataframes)
  dfDepCharResults$Hardness <- format(NULL, scientific = FALSE)
  dfDepCharResults$Hardness_UOM <- format(NULL, scientific = FALSE)
  dfDepCharResults$pH <- format(NULL, scientific = FALSE)
  dfDepCharResults$Temp<- format(NULL, scientific = FALSE)
  dfDepCharResults$Temp_UOM <- format(NULL, scientific = FALSE)
  
  # add columns CalcStdValue & Calculation Formula Used
  # (these WILL be NULL, but are needed for merging dataframes)
  dfDepCharResults$CalcFormula <- format(NULL, scientific = FALSE)
  dfDepCharResults$CalcStdValue <- format(NULL, scientific = FALSE)
  
  # suppressWarnings to prevent coercion messages due to NULLs
  # Warning messages:
  #  1: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
  #  2: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
  #  3: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
  
  #set numeric columns (66-hardness, 68-pH, 69-temp)
  dfDepCharResults[,c(66,68,69)] <- suppressWarnings(sapply(dfDepCharResults[,c(66,68,69)], as.numeric))
  
  # -------------------------------------------
  #  Populate Columns for Dep Chars
  # -------------------------------------------
  # fetch formulae
  dfDepCharResults$CalcFormula <- getStdFormula(dfDepCharResults$x)
  
  # fetch pH, Temp, Hardness, Temp & Hardness UOMs
  # lookupDependentCharResult(depChar, stationID, startDate, startTimeZone, uom, medium, field_lab)
  
  # fetch pH - 1
  # using "1e" as both "1e" and "1x" result in a pH value lookup 
  # (both get the same result for the pH measurement for the sample date/site)
  dfDepCharResults$pH <- lookupDependentCharResult("1e",
                                                   dfDepCharResults$StationID,
                                                   dfDepCharResults$START_DATE,
                                                   dfDepCharResults$START_TIME_ZONE,
                                                   dfDepCharResults$UOM,
                                                   dfDepCharResults$MEDIUM,
                                                   dfDepCharResults$FIELD_LAB)
  # fetch hardness (Ca + Mg) - 261
  dfDepCharResults$Hardness <- lookupDependentCharResult("261",
                                                   dfDepCharResults$StationID,
                                                   dfDepCharResults$START_DATE,
                                                   dfDepCharResults$START_TIME_ZONE,
                                                   dfDepCharResults$UOM,
                                                   dfDepCharResults$MEDIUM,
                                                   dfDepCharResults$FIELD_LAB)  
  # fetch temperature - 481
  # using "481n" as both "481n" and "481x" result in a temp value lookup 
  # (both get the same result for the temp measurement for the sample date/site)
  dfDepCharResults$Hardness <- lookupDependentCharResult("481n",
                                                         dfDepCharResults$StationID,
                                                         dfDepCharResults$START_DATE,
                                                         dfDepCharResults$START_TIME_ZONE,
                                                         dfDepCharResults$UOM,
                                                         dfDepCharResults$MEDIUM,
                                                         dfDepCharResults$FIELD_LAB)  
  
  
  
  return(dfExtDepChars)
}

# ----------------------------------------------------------------------
#' @title getFormulaUsed
#' @description Fetch the formula used to calculate dependent NPSTORET WQ Standard Values
#'
#' @param dependency - values for NPSTORET characteristics upon which the desired characteristic is based
#' @param params     - coefficients 1-8 as key value pairs from NPSTORET
#'                list("coef1" = Coefficient1, "coef2" = Coefficient2, ...)
#'                
#' @return string - formula used to calculate the dependent standard value
#'
#' @section Requirements:
#'   RODBC library package loaded
#'   Working directory set for app
#'
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-06-26 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'  \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-06-26    \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getFormulaUsed <- function(dependency,params){  
  
  # ----------Characteristic Value to Return---------------
  
  # -------------------------------------------
  #  Dependent Value
  # -------------------------------------------
  # prepare for switch statement
  DependentChar = paste0('dc',dependency)
  
  #  Params of the form
  #  params <- list(coef1 = XX, coef2 = Y,  ...)
  
    # ----------------------
    # set parameter values
    # ----------------------
    if((is.list(params)) & (length(params) == 8)){
       coef1 = list("coef1")
       coef2 = list("coef2")
       coef3 = list("coef3")
       coef4 = list("coef4")
       coef5 = list("coef5")
       coef6 = list("coef6")
       coef7 = list("coef7")
       coef8 = list("coef8")
    }else{
      return(FALSE)
    }
    
    # --------------------------------------------------
    #  StandardValue Dependent Characteristic Formulae
    # --------------------------------------------------
    #   
    #  Dependent characteristics: 
    #       261 (Hardness, Ca + Mg)
    #       1e (pH exponential)
    #       1x (pH power of 10)
    #       481n & 481x (pH power of 10 & temp (min & max))
    #       time
    #
    # --------------------------------------------------

  # need to set lnVal for Hardness outside main switch statement
  if(DependentChar == "dc261"){
    
    # -------------------------------------------
    #  Hardness Dependent 
    #   (exp(coef1*ln(Hardness) + coef2) * CF where CF = coef3 + (coef4*ln(Hardness))
    #
    #   (exp(coef1*ln(Hardness) + coef2) * (coef3 + (coef4*ln(Hardness)))
    #   
    # -------------------------------------------
    
    # if no value exists for hardness give -99999999
    if(Hardness==0){
      lnVal = -99999999
    }else{
      lnVal = Log(Hardness)
    }
    
    calc = (exp((coef1*lnVal) + coef2)*(coef3 + (coef4*lnVal)))
    
  
  }
  
  print(DependentChar)
  print(calc)
  
    switch(DependentChar,
           
           "dc261" = {
             # -------------------------------------------
             #  Hardness Dependent 
             #   (exp(coef1*ln(Hardness) + coef2) * CF where CF = coef3 + (coef4*ln(Hardness))
             #
             #   (exp(coef1*ln(Hardness) + coef2) * (coef3 + (coef4*ln(Hardness)))
             #   
             # -------------------------------------------
             
             # if no value exists for hardness give -99999999
             if(Hardness==0){
               lnVal = -99999999
             }else{
               lnVal = Log(Hardness)
             }
             
             calc = (exp((coef1*lnVal) + coef2)*(coef3 + (coef4*lnVal)))
           },
           
           "dc1e" = {
             # -------------------------------------------
             #  pH Dependent - exponential
             #
             #     exp((coef1*pH)+coef2)
             #   
             # -------------------------------------------
             
             calc = exp((coef1*pH)+coef2)
           },
           
           "dc1p" = {
             # -------------------------------------------
             #  pH Dependent - power of 10
             #
             #     (coef1/(1+10^(coef3-pH)+coef2/(1+10^(pH-coef4))))
             #   
             # -------------------------------------------
             
             calc = (coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4))) 
           },      
           
           "dc481n" = {
             # -------------------------------------------
             #  pH, Temperature (min) Dependent - power of 10
             #
             #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * f(T) where f(T) = min(coef5,coef6*10^(coef7*(coef8-T)))
             #
             #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * min(coef5,coef6*10^(coef7*(coef8-T)))
             #   
             # -------------------------------------------
             
             calc = ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * min(coef5,coef6*10^(coef7*(coef8-T)))
           },
           
           "dc481x" = {
             # -------------------------------------------
             #  pH, Temperature (max) Dependent - power of 10
             #
             #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * f(T) where f(T) = coef5*10^(coef6*(coef7-iif(T>coef8, T, coef8)))
             #
             #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * coef5*10^(coef6(coef7-max(T,coef8)))
             #   
             # -------------------------------------------
             
             calc = ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * coef5*10^(coef6(coef7-max(T,coef8)))
           },
           
           # ========================================================================
           # CHECK ON TIME VALUES SINCE THIS IS HANDLED DIFFERENTLY IN NPSTORET
           # ========================================================================
           "dctime" = {
             # -------------------------------------------
             #  Temperature(T) - 6/1-9/30 = 17, 10/1-5/31 = 9
             #                  rivers tier 1 (trout)
             # -------------------------------------------
             
             calc = if(6 <= month(visitdate) && month(visitdate) <= 9 ){17}else{9}
           },
           # ======================================================================== 
           # -------------------------------------------
           #  Other non-calculated values (should be none of these)
           # -------------------------------------------
           "dc" = {
             calc = ""
           }
    )
          # ========================================================================
  return(calc)

}
  
# ----------------------------------------------------------------------
#' @title getWQStdValues
#' @description Fetch NPSTORET WQ Standard Values for Calculated & Uncalculated Characteristics
#'
#' @param dfWQstds - NPSTORET water quality standards data frame
#'
#' @section Requirements:
#' \itemize{
#'  \item \code{\link[RODBC]}
#'  \item \code{\link[sqldf]{sqldf}}
#' }
#'
#' @section Sources:
#'   Reference NPSTORET reportUtilities module, CalcDepStandard function
#'
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 2014-05-21 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   \tab 2014-06-05 \tab\tab B. Campbell \tab\tab 0.2 \tab\tab Adjusted to mirror NPSTORET calculations \cr
#'   }
#' @family WQ Standards functions
#' @export
# ----------------------------------------------------------------------
getWQStdValues <- function(dfWQstds){  
  
  # ----------Characteristic Value to Return---------------
  
  # -------------------------------------------
  #  StandardValue
  # -------------------------------------------
  # TODO:
  #  Check if value is numeric
  #  
  #  Dependent characteristics: 
  #       261 (Hardness, Ca + Mg)
  #       1e (pH exponential)
  #       1x (pH power of 10)
  #       481n & 481x (pH power of 10 & temp (min & max))
  #       time
  #
  # -------------------------------------------
  
  if(is.not.null(dfWQStds[StandardValue])){
    print(dfQWStds[StandardValue])
    
  }else{
    
    if(dfWQStds[IsDependent]){
      
      print("dependent")
      
      switch(DependentChar,
             
             dc261 = {
               # -------------------------------------------
               #  Hardness Dependent 
               #   (exp(coef1*ln(Hardness) + coef2) * CF where CF = coef3 + (coef4*ln(Hardness))
               #
               #   (exp(coef1*ln(Hardness) + coef2) * (coef3 + (coef4*ln(Hardness)))
               #   
               # -------------------------------------------
               
               # if no value exists for hardness give -99999999
               if(Hardness==0){
                 lnVal = -99999999
               }else{
                 lnVal = Log(Hardness)
               }
               
               calc = (exp((coef1*lnVal) + coef2)*(coef3 + (coef4*lnVal)))
             },
             
             dc1e = {
               # -------------------------------------------
               #  pH Dependent - exponential
               #
               #     exp((coef1*pH)+coef2)
               #   
               # -------------------------------------------
               
               calc = exp((coef1*pH)+coef2)
             },
             
             dc1p = {
               # -------------------------------------------
               #  pH Dependent - power of 10
               #
               #     (coef1/(1+10^(coef3-pH)+coef2/(1+10^(pH-coef4))))
               #   
               # -------------------------------------------
               
               calc = (coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4))) 
             },      
             
             dc481n = {
               # -------------------------------------------
               #  pH, Temperature (min) Dependent - power of 10
               #
               #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * f(T) where f(T) = min(coef5,coef6*10^(coef7*(coef8-T)))
               #
               #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * min(coef5,coef6*10^(coef7*(coef8-T)))
               #   
               # -------------------------------------------
               
               calc = ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * min(coef5,coef6*10^(coef7*(coef8-T)))
             },
             
             dc481x = {
               # -------------------------------------------
               #  pH, Temperature (max) Dependent - power of 10
               #
               #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * f(T) where f(T) = coef5*10^(coef6*(coef7-iif(T>coef8, T, coef8)))
               #
               #     ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * coef5*10^(coef6(coef7-max(T,coef8)))
               #   
               # -------------------------------------------
               
               calc = ((coef1/(1+10^(coef3-pH)))+(coef2/(1+10^(pH-coef4)))) * coef5*10^(coef6(coef7-max(T,coef8)))
             },
             
             # ========================================================================
             # CHECK ON TIME VALUES SINCE THIS IS HANDLED DIFFERENTLY IN NPSTORET
             # ========================================================================
             dctime = {
               # -------------------------------------------
               #  Temperature(T) - 6/1-9/30 = 17, 10/1-5/31 = 9
               #                  rivers tier 1 (trout)
               # -------------------------------------------
               
               calc = if(6 <= month(visitdate) && month(visitdate) <= 9 ){17}else{9}
             },
             
             # -------------------------------------------
             #  Other non-calculated values
             # -------------------------------------------
             t = {
               calc = value
             }
      )
      # ========================================================================
      return(df)
    }
  }
}
