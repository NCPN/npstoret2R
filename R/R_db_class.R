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
#             0.2  2014-11-17  B. Campbell  updated to S4 class
# ==========================================================

# ----------------------------------------------------------------------
#' @title db Class
#' @details Creates a database object whose connection parameters are contained within the object.
#'
#' @field dbfilepath     - database name & directory path
#' @field user           - database username (for authentication)
#' @field pwd            - database password (for authentication)
#'
#' @examples
#' ## Create the database object (db) and assign the NPSTORET backend filename.
#' ## Paths should be written UNIX style with / vs. \ .
#' ## So a windows file path like "C:\db\dbname.mdb" should be written "C:/db/dbname.mdb".
#' 
#' \dontrun{
#' npstoret <- db("C:/my_database/NPSTORET_BE.MDB","","")
#' 
#' Connect to your database via a DSN-less connection.
#' 
#' npstoret <- connect(npstoret, dbfilepath, user, pwd)
#'}
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#'  \item \link[tools]{tools}
#' }
#'
#' @section Sources:
#'   \tabular{lll}{
#'   \tab 2004 \tab Friedrich Leisch \cr
#'   \tab\tab http://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf \cr
#'   \tab May 6, 2014 \tab Rappster \cr
#'   \tab\tab http://stackoverflow.com/questions/23495627/roxygen2-s4-generic-functions-are-not-exported-unless-a-method-is-also-defined \cr
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-10-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-10-05  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-11-07  \tab\tab BLC   \tab\tab Documentation update & removed sqlQUeriesFilepath since SQL queries are integrated \cr
#'   \tab 0.3   \tab\tab 2014-11-13  \tab\tab BLC   \tab\tab Documentation update \cr
#'   \tab 0.4   \tab\tab 2014-11-17  \tab\tab BLC   \tab\tab Converted to S4 class & renamed to db vs. app \cr
#'   }
#'    
#' @family Application settings
#' @exportClass db
# ----------------------------------------------------------------------

# ----------------------------
# NPSTORET Database Attributes
# ----------------------------
#user, pwd, dbpath
db <- setClass(
    # set db file path
    "db",
    
    # define the slots
    slots = c(
        dbfile = "character",
        user = "character",
        pwd = "character"
      ),
    
    # validate if data is consistent
    # not called if an initialize function is defined!
    validity=function(object){
      if(!(file.exists(object@dbfile))){
        return("Database file doesn't exist.")
#        stop ("Please check your database file location. Also ensure all \\ are replaced by //
#            since R doesn't translate the \\ in the same way Windows does.")
        
      }
      return(TRUE)
    }
  )
# setDbFile generic & method
setGeneric(name="setDbFile",
           def=function(theObject,dbfile){
             standardGeneric("setDbFile")
           }
)
setMethod(f="setDbFile",
          signature="db",
          definition = function(theObject, dbfile){
            theObject@dbfile <- dbfile            
            return(theObject)
          }
)

# setDbUser generic & method
setGeneric(name="setDbUser",
           def=function(theObject,user){
             standardGeneric("setDbUser")
           }
)
setMethod(f="setDbUser",
          signature="db",
          definition = function(theObject, user){
            theObject@user <- user            
            return(theObject)
          }
)

# setDbPwd generic & method
setGeneric(name="setDbPwd",
           def=function(theObject,user){
             standardGeneric("setDbPwd")
           }
)
setMethod(f="setDbPwd",
          signature="db",
          definition = function(theObject, user){
            theObject@user <- user            
            return(theObject)
          }
)

# ----------------------------
# NPSTORET Database Connection
# ----------------------------
# connection generic & method
setGeneric(name="connect",
           def=function(theObject,dbfile,user,pwd){
             standardGeneric("connect")
           }
)
setMethod(f="connect",
          signature="db",
          definition = function(theObject, dbfile, user, pwd){
            # check file is access app (do outside of connect.app): 
            #          if((file_ext(dbfile) = 'mapp') | (file_ext(dbfile) = 'accb')){}
            
            # check file is readable
            if(file.access(dbfile,mode=4) == 0){  
              theObject@connect <- odbcConnectAccess2007(dbfile)
              
              return(theObject)
            }
          })

