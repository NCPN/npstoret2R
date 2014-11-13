############################################################
# R App Settings
############################################################
# Purpose:    Single file access to application settings class
#
# Notes:      N/A
#
# Sources:  See individual functions
#              
# Revisions:  0.1  2014-10-05  B. Campbell  initial version
# ==========================================================

# ----------------------------------------------------------------------
#' @title app Class
#' @details Creates a database object whose connection parameters are contained within the object.
#'
#' @param dbfilepathname - database name & directory path
#' @param username       - database username (for authentication)
#' @param pwd            - database password (for authentication)
#'
#' @examples
#' ## Create the application object (app) and assign the NPSTORET backend filename.
#' ## Paths should be written UNIX style with / vs. \ .
#' ## So a windows file path like "C:\db\dbname.mdb" should be written "C:/db/dbname.mdb".
#' 
#' \dontrun{
#' app <- dbfilepathname(app, "C:/my_database/NPSTORET_BE.MDB")
#' 
#' Connect to your database via a DSN-less connection.
#' 
#' app$connect <- odbcConnectAccess2007(app[["dbfilepathname"]])
#'}
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item RODBC
#'  \item tools
#' }
#'
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-10-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-10-05  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.1   \tab\tab 2014-11-07  \tab\tab BLC   \tab\tab Documentation update & removed sqlQUeriesFilepath since SQL queries are integrated \cr
#'   }
#'    
#' @family Application settings
#' @export
# ----------------------------------------------------------------------

app <- list(dbfilepathname="LOCATION OF DATABASE FILE",
                   dbusername="",
                   dbpwd = "")

class(app) <- 'app'

# ----------------------------
# NPSTORET Database Attributes
# ----------------------------
dbpassword.app <- function(app, new.value = NULL)
{
  if (is.null(new.value))
  {
    return(app[['dbpassword']])
  }
  else
  {
    app[['dbpassword']] <- new.value
    return(app)
  }
}

dbpassword <- function(app, new_value = NULL)
{
  # pass only the method & class, UseMethod silently passes other params to function
  UseMethod('dbpassword', app)
}

dbuser.app <- function(app, new.value = NULL)
{
  if (is.null(new.value))
  {
    return(app[['dbusername']])
  }
  else
  {
    app[['dbusername']] <- new.value
    return(app)
  }
}

dbusername <- function(app, new_value = NULL)
{
  # pass only the method & class, UseMethod silently passes other params to function
  UseMethod('dbusername', app)
}


dbfilepathname.app <- function(app, new.value = NULL)
{
  if (is.null(new.value))
  {
    return(app[['dbfilepathname']])
  }
  else
  {
    #check that the new.value is valid
    if(file.exists(new.value)==TRUE){
    
      app[['dbfilepathname']] <- new.value
      return(app)
    }else{
      stop ("Please check your database file location. Also ensure all \\ are replaced by //
            since R doesn't translate the \\ in the same way Windows does.")
    }
    }
}

dbfilepathname <- function(app, new_value = NULL)
{
  # pass only the method & class, UseMethod silently passes other params to function
  UseMethod('dbfilepathname', app)
}

# ----------------------------
# NPSTORET Database Connection
# ----------------------------
connect.app <- function(app)
{
  # check for filepathname
  if (is.null(app[['dbfilepathname']])){
    return(app[['connect']])
  }else{
    library("tools")  #required for file_ext()
    # check file is access app (do outside of connect.app): 
    #          if((file_ext(app[['filepathname']]) = 'mapp') | (file_ext(app[['filepathname']]) = 'accb')){
    
    # check file is readable
    if(file.access(app[['dbfilepathname']],mode=4) == 0){
      library("RODBC")
      app[['connect']] <- odbcConnectAccess2007(app[['dbfilepathname']])
      return(app[['connect']])
    } else{
      return(app[['connect']])
    }    
  }
}

connect <- function(app)
{
  # pass only the method & class, UseMethod silently passes other params to function
  UseMethod('connect', app)
}