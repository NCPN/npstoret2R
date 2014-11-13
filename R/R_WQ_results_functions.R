# ----------------------------------------------
#' @title AddResultColumns
#' @description Adds standards column to result dataframe
#'               including dependent characteristics result values
#'               (pH, Hardness, Temp), their units of measure (UOM),
#'               calculation coefficients, formulae as well
#'               as standards values and compliance information
#'               Values are initially set as NA until populated
#'               by sample (pH, Hardness, Temp) & standard values
#'
#' @param results - result dataframe 
#'  
#' @return df - dataframe with columns added
#'  
#' @section Requirements:
#'   \tabular{l}{
#'   RODBC library package loaded \cr
#'   Working directory set for app \cr
#'   LoadNPSTORET function \cr
#'   }
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-09-25 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-25    \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family WQ Results functions
#' @export
# ----------------------------------------------
AddResultColumns <- function (results){
  # ----------------------------------------------
  #  Dependent Characteristic Std Results
  # ----------------------------------------------
  # pH, Hardness, Temp, Hardness_UOM, Temp_UOM
  # coeffs 1-8
  # Std_CalcFormula
  # ----------------------------------------------
  depChars <- list("pH", "Hardness", "Temp", "Hardness_UOM", "Temp_UOM",
                   "Coeff1", "Coeff2", "Coeff3", "Coeff4", 
                   "Coeff5", "Coeff6", "Coeff7", "Coeff8",
                   "Std_CalcFormula")
  
  # ----------------------------------------------
  #  Independent Charactersitic Std Results
  # ----------------------------------------------
  # - no additional columns -
  
  # ----------------------------------------------
  #  ALL
  # ----------------------------------------------
  # Std_Value
  # Std_LoHi  +1/0/-1
  # Std_UOM
  # Std_Medium
  # Std_SMPL_FRAC_TYPE_NM
  # Std_Compliance +1/0/-1# prepare list
  # ----------------------------------------------
  allChars <- list("Std_Value", "Std_LoHi", "Std_UOM", "Std_Medium", 
                   "Std_SMPL_FRAC_TYPE_NM", "Std_Compliance", "Std_Code", "DepCharLookup"
  )
  
  newColumns <- append(depChars, allChars)
  
  # ensure df is present
  if (is.data.frame(results)){
    df <- results
  }else{
    # set default data frame
    df <- data.frame()
  }
  
  for (i in 1:length(newColumns)){
    
    # create the dynamically named columns
    colname <- as.character(newColumns[[i]])
    df[,colname] <- NA
    
  }
  
  # cleanup
  rm(depChars)
  rm(allChars)
  rm(newColumns)
  
  return(df)
}


# ----------------------------------------------------------------------
#' @title lookupDependentCharResult2
#' @description Lookup dependent characteristic result value (pH, H20 temp, hardness)
#'
#' @param depChar       - NPSTORET dependent characteristic value (pH, Temp, Hardness)
#' @param park          - NPSTORET Park
#' @param stationID     - NPSTORET StationID
#' @param startDate     - NPSTORET visit StartDate
#' @param startTimeZone - NPSTORET visit StartTimeZone
#' @param smpl_frac     - NPSTORET sample fraction type name
#' @param medium        - NPSTORET measurement medium (MEDIUM)
#' @param field_lab     - NPSTORET field or lab mesurement (FIELD_LAB)
#' @param uom           - NPSTORET unit of measure (UOM)#'
#'
#' @return resultVal - results value for the matched characteristic
#'
#' @section Requirements:
#'   \itemize{
#'   \item \link[RODBC]{RODBC} library package loaded
#'   \item Working directory set for app
#'   \item LoadNPSTORET function
#'   \item \[stringr]{stringr} library package loaded
#'   }
#' @section Sources:
#'   \tabular{llllllll}{
#'   \tab 2014-07-02 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#'   \tabular{ll}{
#'   \tab Richie Cotton, April 24, 2012 \cr
#'   \tab \url{http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r} \cr
#'   \tab Dason, Jun 2, 2012 \cr
#'   \tab \url{http://stackoverflow.com/questions/10865095/why-do-i-get-warning-longer-object-length-is-not-a-multiple-of-shorter-object-l} \cr
#'   }
#' @section Adapted:
#'   -
#' @section Revisions:
#'   \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-07-02    \tab\tab BLC   \tab\tab Initial version \cr
#'   \tab 0.2   \tab\tab 2014-09-11    \tab\tab BLC   \tab\tab Fixed match to return proper result value
#'                             Replaced length with nrow to get proper # of rows (vs. df width/cols)
#'                             Replaced == with \%in\% for comparisons \cr
#'   \tab 0.3   \tab\tab 2014-09-25    \tab\tab BLC   \tab\tab Park, sample fraction type name added to parameters \cr
#'   \tab 0.4   \tab\tab 2014-09-25    \tab\tab BLC   \tab\tab Documentation update \cr
#'   }
#' @family WQ Results functions
#' @export
# ----------------------------------------------------------------------
lookupDependentCharResult2 <- function(depChar, park, stationID, startDate, startTimeZone, smpl_frac, medium, field_lab, uom){
  
  # default value
  #depCharResult = ""
  
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

# "== does elementwise checking for equality"

# determine string to match
switch(as.character(depChar),
       "pH" = {
         if (!dfExists(pH, "pH")){
           if (!dfExists(dfDepCharResults,"dfDepCharResults")){
             # subset dfDepCharResults to reduce memory issues
             pH <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "pH",]
           }
         }
         
#         print ( class(as.Date(as.character(pH$START_DATE,"%Y-%m-%d"))))
#         print ( class(as.Date(as.character(startDate,"%Y-%m-%d"))))
          dfMatch <- filter(pH, 
                            pH$Park == toupper(park) &
                            pH$StationID == stationID & 
                            as.character(pH$START_DATE,"%Y-%m-%d") == as.character(startDate,"%Y-%m-%d") &
                            tolower(pH$MEDIUM) == tolower(medium) &
                            tolower(pH$SMPL_FRAC_TYPE_NM) == tolower(smpl_frac) & 
                            tolower(pH$FIELD_LAB) == tolower(field_lab)                  
          )

         },
       "Hardness" = {
         if (!dfExists(Hardness, "Hardness")){
           if (!dfExists(dfDepCharResults, "dfDepCharResults")){
             # subset dfDepCharResults to reduce memory issues
             Hardness <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME %in% "Hardness, Ca + Mg",]             
           }
         }
         
         dfMatch = filter(Hardness,
              as.Date(Hardness$START_DATE) == as.Date(startDate) &
              Hardness$Park == toupper(park) & 
              Hardness$StationID == stationID & 
              tolower(Hardness$SMPL_FRAC_TYPE_NM) == tolower(smpl_frac) & 
              tolower(Hardness$MEDIUM) == tolower(medium) & 
              tolower(Hardness$FIELD_LAB) == tolower(field_lab)
         )
       },
       "Temp" = {
         if (!dfExists(Temp, "Temp")){
           if (!dfExists(dfDepCharResults, "dfDepCharResults")){
             # subset dfDepCharResults to reduce memory issues
             Temp <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME %in% "Temperature, water",]
           }
         }
         
         dfMatch = filter(Temp,
              as.Date(Temp$START_DATE) == as.Date(startDate) &
              Temp$Park == toupper(park) & 
              Temp$StationID == stationID & 
              tolower(Temp$MEDIUM) == tolower(medium) & 
              tolower(Temp$FIELD_LAB) == tolower(field_lab)
         )
        }
)

# assumes 1 result per match
# use nrow(df) vs. length(df) as the latter is the width (#cols) of the df vs the #rows
if(nrow(dfMatch)>1){
  # give only the first result
  depCharResult = dfMatch[1,]
}else{
  # set dfMatch to default NO VALUE entry
  depCharResult = -999
}

#print(depCharResult$RESULT_TEXT)

return(depCharResult)
}