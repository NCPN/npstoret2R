############################################################
# R NCPN Functions
############################################################
# Purpose:    Single file access to miscellaneous NCPN functions
#
# Notes:      N/A
#
#             Place this file in the same directory as .Rhistory
#
# Sources:  See individual functions
#              
# Revisions:  0.1  2014-05-07  B. Campbell  initial version
#             0.2  2014-05-14  B. Campbell  add WQ std functions
#             0.3  2014-06-05  B. Campbell  move WQ std functions to R_NCPN_WQ_functions.R
#             0.4  2014-06-27  B. Campbell  renamed R_NCPN_NPSTORET_functions.R
# ==========================================================

# ----------------------------------------------------------------------
#' loadNPSTORETData
#' Load NPSTORET data & populate data frame (dfName)
#'
#' @section Requirements:
#' R libraries:
#' \itemize{
#'   \item RODBC
#'}
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-05-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#'   
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-05-05  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-06-24  \tab\tab BLC   \tab\tab Updated from SQLquery_NPSTORET_qryResultsAllOrganizations_altered.txt to SQLquery_NPSTORET.txt
#'                              (changes filename only, not SQL) \cr
#'   \tab 0.3   \tab\tab 2014-06-26  \tab\tab BLC   \tab\tab Added cleanup \cr
#'   \tab 0.4   \tab\tab 2014-09-18  \tab\tab BLC   \tab\tab Added stringsAsFactors = FALSE to prevent factorization
#'                             of character strings & subsequent limitation of resulting dfs
#'                             to values in original column's factor list
#'                             Drop levels to avoid retaining unused levels in df \cr
#'   \tab 0.5   \tab\tab 2014-09-19  \tab\tab BLC  \tab\tab Added odbcClose for connection cleanup \cr
#'   \tab 0.6   \tab\tab 2014-10-09  \tab\tab BLC  \tab\tab Documentation update \cr
#'   \tab 0.7   \tab\tab 2014-10-13  \tab\tab BLC  \tab\tab Revised connection to app[["connection"]] vs internal connection \cr
#'   \tab 0.8   \tab\tab 2014-11-11  \tab\tab BLC  \tab\tab Updated documentation & removed odbcClose \cr
#'   \tab 0.9   \tab\tab 2014-11-12  \tab\tab BLC   \tab\tab Revise SQL paths to sqlPath \cr
#'   }
#' @family NPSTORET functions
#' @export
# ----------------------------------------------------------------------
loadNPSTORETData <- function(){  
  
  # ----------SQL---------------
  # read in SQL query
  filePath = paste(sqlPath,"SQLquery_NPSTORET.txt",sep="")
  fileSQL<-file(filePath,"rt")            
  sqlString<-readLines(fileSQL)           
  sqlString<-paste(sqlString,collapse="","") 
  
  # -------------------------------------------
  #  NPSTORET - altered qryResultsAllOrganizations 
  #             w/o PersonName: FillResponsiblePersonList([tblActivities].[LocFdAct_IS_NUMBER],[tblActivities].[LocFdAct_ORG_ID],False) field
  # -------------------------------------------

  # ----------Connect----------
  # connect to the current NPSTORET front end connection via npstoret pkgEnv connection
  assign("dbConn", npstoret, envir=pkgEnv)
  
  #REMEMBER! sqlFetch from a table, sqlQuery a SQL statement!!
  # NOTE: SQLString can have brackets ex: tblLocations.[Station Name] 
  #       To reference these columns use "Station Name"
  #       Brackets enable column name to have a space
  df <- sqlQuery(dbConn, sqlString, errors = TRUE, rows_at_time=100, stringsAsFactors = FALSE)
  
  # double check that there are no missing tables/fields (SQL errors)
  if (class(df) == "character"){
    print(df)
    stop ("Check your NPSTORET database, it may be missing a table. Look at the df value returned and the information printed in the console.")
    return(df)
  }
  # drop unused levels (if factors exist)
  df <- droplevels(df)

  # cleanup
  cleanUp(c('sqlString','filePath','fileSQL'), FALSE)
  
  return(df)
}

# ----------------------------------------------------------------------
#' @title getFilteredNPSTORETData
#' @description Filter NPSTORET data by standard columns & populate data frame
#' 
#' @param df - data frame holding park data
#' 
#' @return data frame - subset of park data with the following data
#' \itemize{
#'  \item Park
#'  \item ParkName
#'  \item ProjectID
#'  \item ProjectName
#'  \item StationID
#'  \item Station Name
#'  \item START_DATE
#'  \item QAQC_SAMPLE
#'  \item LocCharNameCode
#'  \item DISPLAY_NAME
#'  \item SMPL_FRAC_TYPE_NM
#'  \item MEDIUM
#'  \item DETECTION_CONDITION
#'  \item RESULT_TEXT
#'  \item UOM
#'  \item VALUE_STATUS
#'  \item VALUE_TYPE
#'  \item DETECTION_LIMIT
#' }
#'
#' @section Requirements:
#'   Assumes the following columns exist in park dataframe
#'     Park, ParkName, ProjectID, ProjectName, StationID, Station Name,
#'     START_DATE, QAQC_SAMPLE, LocCharNameCode, DISPLAY_NAME, 
#'     SMPL_FRAC_TYPE_NM, MEDIUM, DETECTION_CONDITION, RESULT_TEXT, UOM, 
#'     VALUE_STATUS, VALUE_TYPE, DETECTION_LIMIT
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-05-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-05-05  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11  \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family NPSTORET functions
#' @export
# ----------------------------------------------------------------------
getFilteredNPSTORETData <- function(df){  
  results <- df[, c("Park", "ParkName", "ProjectID", "ProjectName", "StationID", "Station Name","START_DATE", "QAQC_SAMPLE", "LocCharNameCode","DISPLAY_NAME", "SMPL_FRAC_TYPE_NM", "MEDIUM","DETECTION_CONDITION", "RESULT_TEXT", "UOM", "VALUE_STATUS", "VALUE_TYPE", "DETECTION_LIMIT")]

  return(results)
}

# ----------------------------------------------------------------------
#' @title getParksList 
#' @description Return a list of Park abbreviations from dataframe
#'
#' @param df - data frame holding park data
#'
#' @section Requirements:
#'   assumes df column name for Park abbreviation is "Park"
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-05-08 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-05-08  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family NPSTORET functions
#' @export
# ----------------------------------------------------------------------
getParksList <- function(df){
    
  unique(df$Park,incomparables=FALSE)
}

# ----------------------------------------------------------------------
#' @title getParkData 
#' @description Pull park data from dataframe
#'
#' @param df - data frame holding park data
#' @param parkAbbrev - park name abrev (ARCH, CANY, etc.)
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-05-07 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-05-07  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11  \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family NPSTORET functions
#' @export
# ----------------------------------------------------------------------
getParkData <- function(df, parkAbbrev){
  
  subset(df,Park == parkAbbrev)

}

# ----------------------------------------------------------------------
#' @title loadNPSTORETWQData
#' @description Load NPSTORET data & populate data frame
#'
#' @param datatype - String (enclosed in quotes) that identifies what data should be retrieved.
#'                 This identifies which SQL query should be run to retrieve data from NPSTORET.
#'                 Possible values, what they return, & the related SQL query include:
#'                 \itemize{
#'                  \item \code{Result} - all NPSTORET results (\href{/doc/SQLquery_NPSTORET_WQResults.txt}{SQLquery_NPSTORET_WQResults})
#'                  \item \code{Characteristics} - all WQ characteristics (SQLquery_NPSTORET_Characteristics.txt)
#'                  \item \code{WQProjStnStds} - all WQ locations tied to assigned projects & standards (SQLquery_NPSTORET_WQSP.txt)
#'                  \item \code{WQProjStnStdCrit} - all WQ locations tied to assigned projects, standards, & criteria (SQLquery_NPSTORET_WQSPC.txt)
#'                  \item \code{WQProjStnStdCritAlt} - all WQ locations tied to assigned projects, standards, & criteria (SQLquery_NPSTORET_WQProjStnCritStds.txt)
#'                  \item \code{WQLocales} - all WQ locations tied to assigned projects & standards (SQLquery_NPSTORET_WQLocales.txt)
#'                  \item \code{WQDepChars} - all WQ dependent characteristics (SQLquery_NPSTORET_WQResults_DepChars.txt)
#'                  \item \code{WQDepCharsPivot} - all WQ dependent characteristics pivoted to wide format (vs. long) (SQLquery_NPSTORET_WQResults_DepChars_Pivot.txt)
#'                  \item \code{WQIndepChars} - all WQ standard criteria tied to characteristics (SQLquery_NPSTORET_WQCriteria.txt)
#'                  \item \code{WQstds} - all WQ standards (SQLquery_NPSTORET_WQStandards.txt)
#'                  \item \code{WQstdsFiltered} - all WQ standard criteria (filtered by StandardID) (SQLquery_NPSTORET_WQStandardsFiltered.txt)
#'                  \item \code{WQCriteria} - all WQ standard criteria (SQLquery_NPSTORET_WQCriteria.txt)
#'                 }
#'                 
#'                 \emph{Time required to retrieve a data set depends on the size of the data sets being retrieved
#'                 and the computer's CPU. 
#'                 
#'                 A result set of ~455,000 with 66 fields takes approximately 1 minute to load the data frame.
#'                 
#'                 Good time to take a break from the computer screen if you have a very large data set!
#'                 }
#'                 
#' @param filter - the WQ standard to filter by (see examples below)
#'                 
#' @return df - dataframe with the desired NPSTORET results, characteristics, criteria (as selected by "dataType")
#' 
#' @section Requirements:
#' r Libraries:
#' \itemize{
#'  \item \code{\link[RODBC]{RODBC}}
#' }
#'   
#' @examples
#' ## Typically calls are made to retrieve all results, then WQ characteristics.
#'   
#'     \dontrun{
#'        # get results
#'        dfResults <- loadNPSTORETData("Results")
#'        
#'        # get characteristics related to locations, projects, & criteria
#'        dfChars <- loadNPSTORETData("WQProjStnStdCritAlt")
#'     }  
#'     
#' ## Limited filtering is available for water quality standards.
#'   \dontrun{
#'        # get a filtered list of standards for Colorado Cold Water
#'        dfCOCWStds <- loadNPSTORETData("WQstdsFiltered","CO_ALCW1")
#'     }  
#'       
#' @section Notes:
#'   SQL queries for NPSTORET data are contained within the npstoret2R package
#'
#'  ------------------------------------------------------------------------------------------------------
#'       \code{default.stringsAsFactors()} is typically TRUE and limits any character strings to the factors in the 
#'       original df, so different values not in the factor cannot be entered.
#'       \code{stringsAsFactors} can ONLY be set at the BEGINNING of the df when it is created
#'       if a df is created from another df, that df must have been set with \code{stringsAsFactors = FALSE} 
#'       to avoid the issue.
#'       So, the first df must set \code{stringsAsFactors = FALSE} when its created
#'       otherwise character strings for a data frame field cannot be changed to values other than 
#'       those that initially exist within the data frame.
#'  ------------------------------------------------------------------------------------------------------
#'
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-05-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-05-05  \tab\tab BLC  \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-05-20  \tab\tab BLC  \tab\tab Modified loadNPSTORETData() to include
#'                          datatype to ID SQL query to load &
#'                          filter to identify the subset of records to load \cr
#'   \tab 0.3   \tab\tab 2014-06-02  \tab\tab BLC  \tab\tab Revised Results to limit results fields to speed up query
#'                          replaced "SQLquery_NPSTORET.txt" with "SQLquery_NPSTORET_WQResults.txt"
#'                          Added Characteristics to pull the full tblCharacteristics from NPSTORET
#'                           for tying tblResults to tblWQStandardCriteria \cr
#'   \tab 0.4   \tab\tab 2014-06-04  \tab\tab BLC  \tab\tab Added locales to bring in WQ sampling stations & their related
#'                          projects, parks, and WQ assignments \cr
#'   \tab 0.5   \tab\tab 2014-06-26  \tab\tab BLC  \tab\tab Added cleanup \cr
#'   \tab 0.6   \tab\tab 2014-09-10  \tab\tab BLC  \tab\tab Added stringsAsFactors = FALSE to enable dfs to *not* automatically 
#'                          convert strings -> factors (default.stringsAsFactors() is TRUE)
#'                          which can cause later manipulation problems \cr
#'   \tab 0.7   \tab\tab 2014-09-19  \tab\tab BLC  \tab\tab Added odbcClose for connection cleanup \cr
#'   \tab 0.8   \tab\tab 2014-10-05  \tab\tab BLC  \tab\tab Updated directory location for package \cr
#'   \tab 0.9   \tab\tab 2014-10-13  \tab\tab BLC  \tab\tab Revised connection to app[["connection"]] vs internal connection \cr
#'   \tab 0.10   \tab\tab 2014-11-11  \tab\tab BLC  \tab\tab Documentation updated & removed odbcClose to leave connection available \cr
#'   \tab 0.11   \tab\tab 2014-11-12  \tab\tab BLC   \tab\tab Revise SQL paths to sqlPath \cr
#'   }
#' @family NPSTORET functions
#' @export
# ----------------------------------------------------------------------
loadNPSTORETWQData <- function(datatype="Results", filter=""){  
 
  # ----------Type to Return---------------
  switch(datatype,
    # -------------------------------------------
    #  NPSTORET - altered qryResultsAllOrganizations 
    #             w/o PersonName: FillResponsiblePersonList([tblActivities].[LocFdAct_IS_NUMBER],[tblActivities].[LocFdAct_ORG_ID],False) field
    # -------------------------------------------
    Results = {
      # WQ Results (all)
      fileName = "SQLquery_NPSTORET_WQResults.txt"
    },
    Characteristics = {
      # WQ characteristics (all)
      fileName = "SQLquery_NPSTORET_Characteristics.txt"
    },
    WQProjStnStds = {
      # WQ locations tied to assigned projects & standards (all)
      fileName ="SQLquery_NPSTORET_WQSP.txt"
    },
    WQProjStnStdCrit = {
      # WQ locations tied to assigned projects, standards, & criteria (all)
      fileName = "SQLquery_NPSTORET_WQSPC.txt"
    },
    WQProjStnStdCritAlt = {
      # WQ locations tied to assigned projects, standards, & criteria (all)
      fileName = "SQLquery_NPSTORET_WQProjStnCritStds.txt"
    },    
    WQLocales = {
      # WQ locations tied to assigned projects & standards (all)
      fileName = "SQLquery_NPSTORET_WQLocales.txt"
    },
    WQDepChars = {
      # WQ dependent characteristics (all)
      fileName = "SQLquery_NPSTORET_WQResults_DepChars.txt"
    },
    WQDepCharsPivot = {
      # WQ dependent characteristics pivoted to wide format (vs. long)
      fileName = "SQLquery_NPSTORET_WQResults_DepChars_Pivot.txt"
    },
    WQIndepChars = {
      # WQ standard criteria tied to characteristics
      fileName = "SQLquery_NPSTORET_WQCriteria.txt"
    },
    WQstds = {
      # WQ standards (filtered by StandardID)
      fileName = "SQLquery_NPSTORET_WQStandards.txt"
    },
    WQstdsFiltered = {
      # WQ standard criteria (filtered by StandardID)
      fileName = "SQLquery_NPSTORET_WQStandardsFiltered.txt"
    },
    WQCriteria = {
      # WQ standard criteria (all)
      fileName = "SQLquery_NPSTORET_WQCriteria.txt"
    },
  )
    
  # ----------SQL---------------
  # read in SQL query
  filePath = paste(sqlPath,fileName,sep="")
  fileSQL<-file(filePath,"rt")            
  sqlString<-readLines(fileSQL)
  
  # handle filtered substitutions
  if(nchar(filter) > 0){
    sqlString<-gsub("%s",filter,sqlString)    
  }
  
  # cleanup \t (tab) & \n (newline) in SQL
  sqlString<-gsub("\t","",sqlString)
  sqlString<-gsub("\n","",sqlString)
  sqlString<-gsub("\"","",sqlString)
  
  sqlString<-paste(sqlString,collapse="","") 
  
  # ----------Connect----------
  # connect to the current NPSTORET front end connection
  assign("dbConn", pkgEnv$npstoret$connect, envir=.GlobalEnv)


  #REMEMBER! sqlFetch from a table, sqlQuery a SQL statement!!
  # NOTE: SQLString can have brackets ex: tblLocations.[Station Name] 
  #       To reference these columns use "Station Name"
  #       Brackets enable column name to have a space
  df <- sqlQuery(dbConn, sqlString, errors = TRUE, rows_at_time=100, stringsAsFactors = FALSE)
    
  # cleanup
  cleanUp(c('sqlString','datatype','filter', 'filePath','fileSQL'), FALSE)
  
  # close connection
  # odbcClose(dbConn)
  
  return(df)
}
