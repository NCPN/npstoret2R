# ------------------------------------
#' NPSTORET DATABASE REQUIREMENTS
# ------------------------------------
#'
#'
#'@description
#' When connecting an NPSTORET local database to npstoret2R, the following tables MUST be present.
#' If they are missing, npstoret2R will be unable to run the queries necessary to connect results
#' with characteristic water quality standards.
#' 
#' \bold{\emph{REQUIRED NPSTORET TABLES}}
#' \itemize{
#'  \item tblActivities
#'  \item tblAnalyticalProcedures
#'  \item tblCharacteristics
#'  \item tblFieldProcedures
#'  \item tblLocations
#'  \item tblLocationProjectAssignment
#'  \item tblLocationStationGroups
#'  \item tblLocationStationGroupAssignment
#'  \item tblLocationWQStandardAssignment
#'  \item tblOrganizations
#'  \item tblProjects
#'  \item tblResults
#'  \item tblVisits
#'  \item tblWQStandardCriteria
#'  \item tblDef_ActivityTypes
#'  \item tblDef_TSRUOM
#'  \item qryTaxon *
#'  \item tblDef_BIOPT *
#' }
#' * Required only for \code{\link{loadNPSTORETData}} ( \code{\link{loadNPSTORETWQData}} does not use these tables).
#' 
#' Because of the sometimes large data sets, it is highly recommended that a local copy of your
#' NPSTORET Access database be used. 
#' 
#' \bold{\emph{IMPORTING FROM NETWORK DATABASES}}
#' 
#' Importing from network databases has not been tested and 
#' is likely to fail without a fast network connection and sufficient computer memory.
#' If you have been successful connecting to a network database, please email the author (\email{Bonnie_Campbell@@nps.gov})
#' with specifics so this success can be documented.
#'
#' @docType package
#' @name NPSTORET requirements

NULL