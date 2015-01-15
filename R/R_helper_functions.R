############################################################
# R Helper Functions
############################################################
# Purpose:    Single file access to miscellaneous helper functions
#
# Notes:      N/A
#
# Sources:  See individual functions, B. Campbell, May 2014
#
# Revisions:  0.1  2014-05-05  BLC  initial version
#             0.2  2014-06-25  BLC  added insertColumn, revised cleanup
#             0.3  2014-06-27  BLC  renamed R_NCPN_helper_functions.R
#             0.4  2015-01-15  BLC  documentation changes
# ==========================================================

# ----------------------------------------------------------------------
#' @title pkgTest
#' @description Ensure required packages are installed, if not install them
#'
#' @param x - String name of the desired package.
#' 
#' @examples
#' # Test for the dplyr package and install it if necessary.
#' # pkgTest("dplyr")
#'
#' @section Sources:
#'  \tabular{ll}{
#'  \tab \url{http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages} \cr
#'  }
#' @section Adapted:
#'   \tabular{llllllll}{
#'   2014-05-05 \tab \tab B. Campbell \tab \tab 0.1 \tab \tab Initial version \cr
#'   }
#' @section Revisions:
#'  \tabular{llllllll}{
#'    0.1   \tab \tab 2014-05-05 \tab \tab BLC  \tab \tab Initial version \cr
#'    0.2   \tab \tab 2014-06-26 \tab \tab BLC  \tab \tab Added cleanup \cr
#'    0.3   \tab \tab 2015-01-15 \tab \tab BLC  \tab \tab Documentation update \cr
#'  }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
pkgTest <- function(x){
  if(x %in% rownames(installed.packages()) == FALSE) {
    if(x == "shinyIncubator"){
      # install devtools first via pkgTest(devtools)
      # install unsupported shinyIncubator package via GitHub
      devtools::install_github("shiny-incubator", "rstudio")  
    }
    install.packages(x)
  }
  
  # cleanup
  cleanUp(c('x'), FALSE)
}

# ----------------------------------------------------------------------
#' @title iterateDataLoad
#' @description Iterate through an ODBC load from tables or query and populates a data frame (dfName)
#'                  displays a progress bar while data frame is loaded
#'
#' @param dfName = new data frame name
#' @param tableName = database name of table or query (no function columns in data unless R code re-creates that function)
#' @param maxResults = maximum number of results to return, 0 = ALL results
#' @param numRowsAtTime = number of results records to return at a time
#' 
#' @return progress bar until the data is loaded
#' 
#' @section Sources:
#' \tabular{ll}{
#'  \tab \url{http://www.inside-r.org/r-doc/utils/winProgressBar} \cr
#'  \tab \url{http://rgm3.lab.nig.ac.jp/RGM/R_rdfile?f=utils/man/windows/winProgressBar.Rd&d=R_Rel} \cr
#'  }
#' @section Adapted:
#' \tabular{llllllll}{
#'   \tab 2014-05-05 \tab \tab B. Campbell \tab \tab 0.1 \tab \tab Initial version \cr
#'   }
#' @section Revisions:
#'  \tabular{llllllll}{ 
#'   \tab 0.1   \tab \tab 2014-05-05 \tab \tab BLC  \tab \tab Initial version \cr
#'   \tab 0.2   \tab \tab 2014-06-26 \tab \tab BLC  \tab \tab Added cleanup \cr
#'   }
#'   
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
iterateDataLoad <- function(dfName, tableName, maxResults, numRowsAtTime){
  
  dfName <- sqlFetch(mdb,tableName, max = maxResults, rows_at_time = numRowsAtTime)
  
  pb <- winProgressBar("Data Loading...", "Successfully loaded data (%)",
                       min=0, max=maxResults, width=50)#max=100, width=50)
  Sys.sleep(0.5)
  
  #u <- c(0, sort(runif(n=20, min=0 ,max=100)), 100)
  u <- c(0, sort(runif(n=20, min=0 ,max=maxResults)), 100)
  for(i in u) {
  Sys.sleep(0.1)
    info <- sprintf("%d%% complete", round(i))
    setWinProgressBar(pb, i, sprintf("Data Loaded (%s)", info), info)
  }
  Sys.sleep(5)
  close(pb)
  
  # cleanup
  cleanUp(c('pb','i','u','dfName','tableName','maxResults', 'numRowsAtTime'), FALSE)
}

# ----------------------------------------------------------------------
#' @title readSQL
#' @description Read SQL queries from separate SQL file
#'
#' @note Long SQL queries become unreadable from R so it's best to keep
#'        them separate, this function reads & returns the SQL from the query file.
#'        To bind parameters into the queries, use /%s where the parameter will go 
#'        in the .sql file, then add in the parameters in R using sprintf.       
#'        Include the .sql file by using source(result of readSQL)
#'
#' @param path = SQL file location
#' 
#' @return sql - Returns the SQL string from the SQL file.
#' 
#' @section Sources:
#' \tabular{ll}{
#'  \tab \url{http://stackoverflow.com/questions/13384555/string-continuation-across-multiple-lines-no-newline-characters/13384777#'13384777} \cr
#'  \tab \url{http://stackoverflow.com/questions/8726882/how-is-it-possible-in-r-to-include-external-files-with-source-code} \cr
#'  }
#' @section Adapted:
#'   \tabular{llllllll}{
#'   \tab 2014-05-05 \tab \tab B. Campbell \tab \tab 0.1 \tab \tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1 \tab \tab 2014-05-05 \tab \tab BLC  \tab \tab Initial version \cr
#'   \tab 0.2 \tab \tab 2014-11-10 \tab \tab BLC  \tab \tab Updated documentation \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
readSQL <- function(path){
  stopifnot(file.exists(path))
  sql <- readChar(path,nchar = file.info(path)$size)
  sql
}

# ----------------------------------------------------------------------
#' @title cleanUp
#' @description Release resources
#'
#' @note
#'   This function is specific to the NPSTORET_import functions
#'   in the future it should be made more generic.
#' 
#' @param varList - list of vars to remove   list = c('temp1','temp2')
#'             Make the list a character vector (not a vector of names)
#'             To remove ALL items set   list = ls()
#' @param blnDisconnect - disconnect the db connection? (True/False)
#'
#' @note
#'  For gc()
#'  verbose   logical; TRUE = garbage collection prints statistics about cons cells 
#'                            & space allocated for vectors.
#'  reset	   logical; TRUE = values for maximum space used are reset to the current 
#'                            values.
#' @section Requirements:
#'   N/A
#' @section Sources:
#'   \tabular{ll}{
#'   \tab M. Nel, July 24, 2012 \cr
#'   \tab \url{http://stackoverflow.com/questions/11624885/remove-multiple-objects-with-rm} \cr
#'   \tab David Robinson, Jan 11, 2012 \cr
#'   \tab \url{http://stackoverflow.com/questions/8813753/what-is-the-difference-between-gc-and-rm} \cr
#'   \tab Chad Birch, March 16, 2009 \cr
#'   \tab \url{http://stackoverflow.com/questions/652136/how-can-i-remove-an-element-from-a-list} \cr
#'   \tab Paul Teetor \cr
#'   \tab \url{https://www.inkling.com/read/r-cookbook-paul-teetor-1st/chapter-5/recipe-5-12} \cr
#'   \tab Evans, July 12, 2010 \cr
#'   \tab \url{http://economics.wordpress.com/2010/07/12/remove-null-elements-from-a-list-in-r/} \cr
#'   }
#' @section Adapted:
#'   \tabular{llll}{
#'   \tab 2014-05-08  \tab \tab B. Campbell \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1    \tab\tab 2014-05-08  \tab\tab BLC  \tab \tab Initial version \cr
#'   \tab 0.2    \tab\tab 2014-06-25  \tab\tab BLC  \tab \tab Extended to include variable input & disconnect option \cr
#'   \tab 0.3    \tab\tab 2014-07-01  \tab\tab BLC   \tab\tab Added existence checks \cr
#'   \tab 0.4    \tab\tab 2014-11-10  \tab\tab BLC   \tab\tab Updated documentation & removed plyr references since Filter isn't used \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
cleanUp <- function(varList, blnDisconnect = TRUE){
  
  if(blnDisconnect){
    # close db connections
    odbcCloseAll()
    if(exists("mdb")){
      on.exit(dbDisconnect(mdb))
    }
  }
  
  # check for existence, then remove
  if(length(varList)>0){
    
    # remove variables & functions
    suppressWarnings(rm(list=varList))
    
  }
  
  # trigger garbage collection
  suppressWarnings(gc(reset=TRUE))
}

# ----------------------------------------------------------------------
#' @title is.not.null
#' @description Determine if a value is or is not null
#' 
#' @param x - Value to check
#' 
#' @return TRUE - if value is not NULL
#' @return FALSE - if value is NULL
#' 
#' @section Sources:
#' \tabular{ll}{
#'   \tab \url{http://stackoverflow.com/questions/2175809/alternative-to-is-null-in-r Etiennebr, 2/2/2010} \cr
#'   }
#' @section Adapted:
#'  \tabular{llllllll}{
#'   \tab 2014-05-08  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1    \tab\tab 2014-05-08  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2    \tab\tab 2014-11-10  \tab\tab BLC   \tab\tab Updated documentation \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
is.not.null <- function(x) ! is.null(x)  

# ----------------------------------------------------------------------
#' @title as.is
#' @description Leave the value unchanged
#'
#' @note
#'   This function is used to pass into aggregation functions and similar to allow for
#'   unchanged values to be passed through.
#' 
#' @param x - any value
#' 
#' @return x - Returns the value unchanged.
#' 
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-05-30  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab \tab 2014-05-30 \tab \tab BLC  \tab \tab Initial version \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
as.is <- function(x) x

# ----------------------------------------------------------------------
#' @title daysBetween
#' @description Returns number of days between two dates
#'
#' @param firstDate   - first date to compare
#' @param secondDate  - second date to compare
#'
#' @return integer - # of days between the 2 dates, 0 if first & second dates are the same
#'                   or -1 if values aren't dates
#'
#' @section Sources:
#'   N/A
#'   
#' @section Adapted:
#'   \tabular{llllllll}{
#'   \tab 2014-06-26  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'    \tab 0.1    \tab\tab 2014-06-26  \tab\tab BLC   \tab\tab Initial version \cr
#'    \tab 0.2    \tab\tab 2014-11-10  \tab\tab BLC   \tab\tab Updated documentation  \cr
#'    }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
daysBetween <- function(firstDate, secondDate){

  # ensure firstDate > secondDate
  if(firstDate > secondDate){
    # swap them
    oldFirstDate = firstDate
    firstDate = as.Date(secondDate,format="%Y-%m-%d")
    secondDate = as.Date(oldFirstDate,format="%Y-%m-%d")
  }
  
  # days between
  nDays = difftime(secondDate, firstDate, units='days')
    
  cleanUp(c('oldFirstDate', 'firstDate', 'secondDate'), FALSE)
    
  return(nDays)

}

# ----------------------------------------------------------------------
#' @title isDateBetween
#' @description Returns TRUE or FALSE depending upon if the Date lies between start & end dates
#'
#' @param firstDate     - first date in YYYY-mm-dd format
#' @param secondDate     - second date in YYYY-mm-dd format
#' @param evalDate  - date to evaluate in YYYY-mm-dd format
#' @param blnInclusive - boolean, include first & second dates in the span 
#'                  (i.e. evalDate is OK if it equals either date)
#'
#' @return boolean - TRUE if evalDate is 'between' (or equal, if inclusive=TRUE)
#'             date1 and date2, FALSE if evalDate outside the date range
#'
#' @section Sources:
#' N/A
#' @section Adapted:
#' \tabular{llllllll}{
#'   \tab 2014-06-26  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1    \tab\tab 2014-06-26  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2    \tab\tab 2014-11-10  \tab\tab BLC   \tab\tab Update documentation \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
isDateBetween <- function(firstDate, secondDate, evalDate, blnInclusive = TRUE){
  
  # ensure dates
  firstDate = as.Date(firstDate,format="%Y-%m-%d")
  secondDate = as.Date(secondDate,format="%Y-%m-%d")
  
  # ensure firstDate > secondDate
  if(firstDate > secondDate){
    # swap them
    oldFirstDate = firstDate
    firstDate = as.Date(secondDate,format="%Y-%m-%d")
    secondDate = as.Date(oldFirstDate,format="%Y-%m-%d")
  }

  evalDate = as.Date(evalDate, format="%Y-%m-%d")
  
  # days between
  nDays = difftime(secondDate, firstDate, units='days')
  
  # days between evalDate & secondDate
  evalDays = difftime(secondDate, evalDate, units='days')
 
  if((evalDays < nDays)|((blnInclusive==TRUE) & (evalDays == nDays))){
    
    return(TRUE)
  
  }
  
  return(FALSE)

}

# ----------------------------------------------------------------------
#' @title dfExists
#' @description Object exists and is a data frame object
#'
#' @param df     - dataframe object
#' @param dfName - name of the dataframe as a string
#' 
#' @return boolean - TRUE if the name represents a dataframe object, FALSE if not
#'
#' @section Sources:
#' \tabular{ll}{
#'   \tab Sven Hohenstein, December 13, 2013 \cr
#'   \tab \url{http://stackoverflow.com/questions/20573119/check-if-data-frame-exists} \cr
#'   }
#' @section Adapted:
#' \tabular{llllllll}{
#'   \tab 2014-05-05  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1    \tab\tab 2014-06-25  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
dfExists <- function(df, dfName = ""){
  # does the df exist?  if so, lazily evaluate if it's a dataframe
  # lazy evaluation --> (is.data.frame only evaluates if exists = true)

  if(exists(dfName) && is.data.frame(df)){
    return(TRUE)
  }
  return(FALSE)
}

# ----------------------------------------------------------------------
#' @title dfDatesBetween
#' @description Returns subset of a dataframe with values between dates for a specific column
#'
#' @param df      - dataframe object
#' @param col     - name of the column as a string
#' @param firstDate   - first date in YYYY-mm-dd format
#' @param secondDate   - second date in YYYY-mm-dd format
#'
#' @return df - dataframe with column dates between the 2 dates or -1 if df isn't a dataframe
#'
#' @section Sources:
#' \tabular{ll}{
#' \tab beginneR, March 13, 2014 \cr
#' \tab \url{http://stackoverflow.com/questions/23622338/subset-a-dataframe-between-2-dates-in-r-better-way} \cr
#' }
#' @section Adapted:
#'  \tabular{llllllll}{
#'    \tab 2014-06-26  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'    }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab \tab 2014-06-26 \tab \tab BLC  \tab \tab Initial version \cr
#'   \tab 0.2   \tab \tab 2014-11-10 \tab \tab BLC  \tab \tab Update documentation \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
dfDatesBetween <- function(df, col = "", firstDate, secondDate){
  # does the df exist?  if so, lazily evaluate if it's a dataframe
  # lazy evaluation --> (is.data.frame only evaluates if exists = true)
  
  # ensure firstDate > secondDate
  if(firstDate > secondDate){
    # swap them
    oldFirstDate = firstDate
    firstDate = as.Date(secondDate,format="%Y-%m-%d")
    secondDate = as.Date(oldFirstDate,format="%Y-%m-%d")
  }
  
  if(dfExists(df, "df")){
    
    dfBetweenDates <- df[with(df, df[,col] >= firstDate & df[,col] <= secondDate), ]

    cleanUp(c('oldFirstDate', 'firstDate', 'secondDate', 'col'), FALSE)
    
    return(dfBetweenDates)
    
  }
  return(-1)
}

# ----------------------------------------------------------------------
#' @title insert.column
#' @description Insert column into dataframe
#'
#' @note
#'   Used to insert a column into the middle of a dataframe
#' 
#' @param df             - dataframe to insert column into
#' @param col            - name of column to insert
#' @param afterCol       - column name before where the inserted column will be placed
#' @param copyCol        - copied column name (default = '' meaning no column is copied)
#' @param blnSciNotation - whether column should be in scientific notiation or not (default = FALSE)
#'
#' @return dataframe w/ new column
#'
#' @section Sources:
#'   \tabular{ll}{
#'   \tab Peter McMahan, August 2, 2009 \cr
#'   \tab \url{http://stackoverflow.com/questions/1177919/does-column-exist-and-how-to-rearrange-columns-in-r-data-frame} \cr
#'   }
#' @section Adapted:
#'   \tabular{llllllll}{
#'   \tab 2014-06-25  \tab\tab B. Campbell  \tab\tab 0.1  \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab \tab 2014-06-25 \tab \tab BLC  \tab \tab Initial version \cr
#'   \tab 0.2    \tab\tab 2014-06-26  \tab\tab BLC   \tab\tab Added cleanup \cr
#'   \tab 0.3    \tab\tab 2014-11-11  \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
insert.column <- function(df, col, afterCol, copyCol, blnSciNotation = FALSE){
  # default
  pos = 0
  # find column position
  if(nchar(afterCol) > 0){
    if((afterCol %in% names(df)) & (nchar(afterCol) > 0)){
      pos = as.numeric(grep(afterCol, colnames(df)))
    }    
  }

  # check if column already exists
  if(!(col %in% names(df))){
      if((nchar(copyCol)>0) & (copyCol %in% names(df))){
        #add column
        df[,col] <- format(df[,copyCol] , scientific = blnSciNotation)      
      }else{
        df[,col] <- format(df[,col] , scientific = blnSciNotation)
      }
    }
    
  # cleanup
  cleanup(c('col','afterCol','copyCol','blnSciNotation'), FALSE)
  
  return(df)
}

# ----------------------------------------------------------------------
#' @title convert.magic
#' @description Change data frame column types
#'
#' @note
#'   Used in npstoret2R to change from scientific notation to standard for dataframes
#' 
#' @param obj   - Data frame
#' @param type  - String ("character", "numeric", "factor", "nonsci") of desired column type to convert to 
#' @param cols  - Column to change
#' 
#' @return Data frame with column(s) changed to the desired type
#' 
#' @section Sources:
#'   \tabular{ll}{
#'   \tab \url{http://stackoverflow.com/questions/11261399/function-for-converting-dataframe-column-type} \cr
#'   }
#' @section Created:  
#'   \tabular{ll}{
#'   \tab Mikko June 29, 2012 \cr
#'   }
#' @section Adapted:  
#'   \tabular{llll}{
#'   \tab 2014-05-30  \tab\tab B. Campbell \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-05-30 \tab \tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-06-26  \tab\tab BLC  \tab \tab Added cleanup \cr
#'   \tab 0.3   \tab\tab 2014-11-10  \tab\tab BLC  \tab \tab Updated documentation \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
convert.magic <- function(obj, type, cols){   
  FUN1 <- switch(type,                  
                 character = as.character,                  
                 numeric = as.numeric,                  
                 factor = as.factor,
                 nonsci = as.numeric #don't use FUN1 though
                 )
  if(type == nonsci){
    obj[,cols] <- format(obj[,cols],scientific=FALSE)
  }else{
    obj[,cols] <- lapply(obj[,cols], FUN1)
  }
  as.data.frame(obj)
  
  # cleanup
  cleanup(c('type','cols'), FALSE)
}

# ----------------------------------------------------------------------
#' @title mgsub
#' @description gsub function for multiple replacements
#'
#' @param pattern     - string to search for
#' @param replacement - string to replace search pattern with
#' @param x           - string to inspect
#' @param ...         - additional gsub parameters
#'
#' @return result - the original x string with pattern replaced by replacement
#'
#' @section Sources:
#'   \tabular{ll}{
#'   \tab Theodore Lytras, March 6, 2013 \cr
#'   \tab \url{http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub} \cr
#'   }
#' @section Adapted:
#'   \tabular{llllllll}{
#'   \tab 2014-09-30 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-30 \tab\tab BLC  \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-11 \tab\tab BLC  \tab\tab Updated documentation \cr
#'   }
#' @family helper functions
#' @export
# ----------------------------------------------------------------------
mgsub <- function(pattern, replacement, x, ...) {
    if (length(pattern)!=length(replacement)) {
      stop("pattern and replacement do not have the same length.")
    }
    result <- x
    for (i in 1:length(pattern)) {
      result <- gsub(pattern[i], replacement[i], result, ...)
    }
    return(result)
}