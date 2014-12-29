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
#             0.3  2014-11-24  B. Campbell  fixed connect slot
#             0.4  2014-11-29  B. Campbell  updated connect documentation
# ==========================================================

# ----------------------------
#  Database S3 Class
# ----------------------------

# ----------------------------------------------------------------------
#' @title RODBC
#' @name RODBC-class
#' @details Handles ODBC database connections to R.
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#' }
#'
#' @section References:
#'   \tabular{lll}{
#'   \tab Jan. 30, 2012 \tab Kyle Brandt \cr
#'   \tab\tab http://stackoverflow.com/questions/9067492/example-of-using-an-s3-class-in-a-s4-object \cr
#'   \tab Oct. 14, 2011 \tab Triad Sou. \cr
#'   \tab\tab http://stackoverflow.com/questions/7758748/documenting-setas-and-setoldclass-with-roxygen \cr
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-11-24 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-11-24  \tab\tab BLC   \tab\tab Added to resolve S4 connect slot error \cr
#'   }
#'
#' @family RODBC
#' @exportClass RODBC
# ----------------------------------------------------------------------
setOldClass("RODBC")

# ----------------------------------------------------------------------
#' @title db Class
#' @details Creates a database object whose connection parameters are contained within the object.
#'
#' @slot dbfile         - database name & directory path
#' @slot user           - database username (for authentication)
#' @slot pwd            - database password (for authentication)
#' @slot connect        - database connection
#' 
#' @examples
#' ## Create the database object (db) and assign the NPSTORET backend filename.
#' ## Paths should be written UNIX style with / vs. \ .
#' ## So a windows file path like "C:\db\dbname.mdb" should be written "C:/db/dbname.mdb".
#' 
#' \dontrun{
#' 
#' Connect to your database via a DSN-less connection.
#' 
#'connect(mynpstoret,dbfile="C:/NPSTORET/NPSTORET_BE.MDB", user="myusername", pwd="mypwd")
#'}
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#'  \item \link[tools]{tools}
#' }
#'
#' @section References:
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
#'   \tab 0.5   \tab\tab 2014-11-23  \tab\tab BLC   \tab\tab Changed to Ref class vs S4 to resolve connect slot error \cr
#'   \tab 0.6   \tab\tab 2014-12-29  \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#'    
#' @family Application settings
#' @exportClass db
# ----------------------------------------------------------------------

# ----------------------------
# NPSTORET Database Attributes
# ----------------------------
#user, pwd, dbpath
setClass(
    # set db file path
    Class="db",
        
    # define the slots
    representation=representation(
      dbfile="character",
      user = "character",
      pwd = "character",
      connect = "RODBC"
      ),
    
    # ensure connect is valid
    prototype = prototype(
      connect = structure(list(), class="RODBC")
      )
    
    # validate if data is consistent
    # not called if an initialize function is defined!
#    validity=function(object){
#      if(!(file.exists(object@dbfile))){
#        return("Database file doesn't exist.")
#        stop ("Please check your database file location. Also ensure all \\ are replaced by //
#            since R doesn't translate the \\ in the same way Windows does.")
        
#      }
#      return(TRUE)
#    }
  )

# ----------------------------
#  Database Class Methods
# ----------------------------
# ----------------------------------------------------------------------
#' @title setDbFile
#' @details Set the database file name and path.
#'
#' @param dbObject - database object
#' @param dbfile   - database name & directory path
#'
#' @examples
#' \dontrun{
#'   #set database file & path
#'   setDbFile(db,"C:/mydatabase/dbfile.accb")
#' }
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#'  \item \link[tools]{tools}
#' }
#'
#' @section References:
#'   \tabular{lll}{
#'   \tab 2004 \tab Friedrich Leisch \cr
#'   \tab\tab http://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf \cr
#'   \tab May 6, 2014 \tab Rappster \cr
#'   \tab\tab http://stackoverflow.com/questions/23495627/roxygen2-s4-generic-functions-are-not-exported-unless-a-method-is-also-defined \cr
#'   \tab March 11, 2014 \tab Hadley Wickham \cr
#'   \tab\tab https://groups.google.com/forum/#!topic/rdevtools/sq3DG0oj058 \cr
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-11-18 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-11-18  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#'    
#' @family Application settings
#' @exportMethod setDbFile
# ----------------------------------------------------------------------
# setDbFile generic & method
setGeneric(name="setDbFile",
           def=function(dbObject,dbfile){
             standardGeneric("setDbFile")
           }
)
setMethod(f="setDbFile",
          signature="db",
          definition = function(dbObject, dbfile){
            dbObject@dbfile <- dbfile            
            return(dbObject)
          }
)

# ----------------------------------------------------------------------
#' @title setDbUser
#' @details Sets database username
#'
#' @param dbObject - database object
#' @param user     - database username (for authentication)
#'
#' @examples
#' \dontrun{
#'  setDbUser(db,"myusername")
#'}
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#'  \item \link[tools]{tools}
#' }
#'
#' @section References:
#'   \tabular{lll}{
#'   \tab 2004 \tab Friedrich Leisch \cr
#'   \tab\tab http://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf \cr
#'   \tab May 6, 2014 \tab Rappster \cr
#'   \tab\tab http://stackoverflow.com/questions/23495627/roxygen2-s4-generic-functions-are-not-exported-unless-a-method-is-also-defined \cr
#'   \tab March 11, 2014 \tab Hadley Wickham \cr
#'   \tab\tab https://groups.google.com/forum/#!topic/rdevtools/sq3DG0oj058 \cr
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-11-18 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-11-18  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#'    
#' @family Application settings
#' @exportMethod setDbUser
# ----------------------------------------------------------------------
# setDbUser generic & method
setGeneric(name="setDbUser",
           def=function(dbObject,user){
             standardGeneric("setDbUser")
           }
)
setMethod(f="setDbUser",
          signature="db",
          definition = function(dbObject, user){
            dbObject@user <- user            
            return(dbObject)
          }
)

# ----------------------------------------------------------------------
#' @title setDbPwd
#' @details Sets database user password
#'
#' @param dbObject - database object
#' @param pwd      - database password (for authentication)
#'
#' @examples
#' 
#' \dontrun{
#'  setDbPwd(db,"mypassword")
#'}
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#'  \item \link[tools]{tools}
#' }
#'
#' @section References:
#'   \tabular{lll}{
#'   \tab 2004 \tab Friedrich Leisch \cr
#'   \tab\tab http://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf \cr
#'   \tab May 6, 2014 \tab Rappster \cr
#'   \tab\tab http://stackoverflow.com/questions/23495627/roxygen2-s4-generic-functions-are-not-exported-unless-a-method-is-also-defined \cr
#'   \tab March 11, 2014 \tab Hadley Wickham \cr
#'   \tab\tab https://groups.google.com/forum/#!topic/rdevtools/sq3DG0oj058 \cr
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-11-18 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-11-18  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#'    
#' @family Application settings
#' @exportMethod setDbPwd
# ----------------------------------------------------------------------
# setDbPwd generic & method
setGeneric(name="setDbPwd",
           def=function(dbObject,pwd){
             standardGeneric("setDbPwd")
           }
)
setMethod(f="setDbPwd",
          signature="db",
          definition = function(dbObject, pwd){
            dbObject@pwd <- pwd            
            return(dbObject)
          }
)
# ----------------------------
# NPSTORET Database Connection
# ----------------------------
# ----------------------------------------------------------------------
#' @title connect
#' @details Creates DSN-less database RODBC connection
#'
#' @param dbObject - database object
#' @param dbfile   - database name & directory path
#' @param user     - database username (for authentication)
#' @param pwd      - database password (for authentication)
#' 
#' @examples
#' 
#' \dontrun{
#'  connect(mydb, dbfilename="C:/database/npstoret.accb", user="myusername", pwd="mypassword") 
#'}
#'
#' @section Requirements:
#' R Libraries:
#' \itemize{
#'  \item \link[RODBC]{RODBC}
#'  \item \link[tools]{tools}
#' }
#'
#' @section References:
#'   \tabular{lll}{
#'   \tab 2004 \tab Friedrich Leisch \cr
#'   \tab\tab http://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf \cr
#'   \tab May 6, 2014 \tab Rappster \cr
#'   \tab\tab http://stackoverflow.com/questions/23495627/roxygen2-s4-generic-functions-are-not-exported-unless-a-method-is-also-defined \cr
#'   \tab March 11, 2014 \tab Hadley Wickham \cr
#'   \tab\tab https://groups.google.com/forum/#!topic/rdevtools/sq3DG0oj058 \cr
#'   \tab Dec. 12, 2012 \tab Martin Morgan \cr
#'   \tab\tab http://stackoverflow.com/questions/13841400/use-s3-virtual-class-as-slot-of-an-s4-class-got-error-got-class-s4-should-b \cr
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-11-18 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-11-18  \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-12-01  \tab\tab BLC   \tab\tab Fixed check file bug (use == not =) \cr
#'   \tab 0.3   \tab\tab 2014-12-29  \tab\tab BLC   \tab\tab Updated documentation \cr
#'   }
#'    
#' @family Application settings
#' @exportMethod connect
# ----------------------------------------------------------------------
# connection generic & method
setGeneric(name="connect",
           def=function(dbObject,dbfile,user,pwd){
             standardGeneric("connect")
           }
)
setMethod(f="connect",
          signature="db",
          definition = function(dbObject, dbfile, user, pwd){
            #library("tools")
            
            # check file is access app (do outside of connect.app): 
            if((file_ext(dbfile) == 'mapp') | (file_ext(dbfile) == 'accb')){

              # set values
              setDbFile(dbfile)
              setUser(user)
              setPwd(pwd)
              
              # check file is readable
              if(file.access(dbfile,mode=4) == 0){  
                dbObject@connect <- odbcConnectAccess2007(dbfile)   
                return(dbObject)
              }
            }        
          }
)