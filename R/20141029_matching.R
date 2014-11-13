library(dplyr)

splitResults <- split(allres, (seq(nrow(allres))-1) %/% 10000)
df1 <- splitResults[[1]] # dataframe of first set

unique(iStds$CharDep) # [1] NA
unique(iStds$Coefficient1) # [1] NA
unique(iStds$UOM_TYPE) # [1] "TAX"   "SUBST" "NONE"  "TEMP" 
unique(iStds$UOM_DESCRIPTION_TEXT) # [1] "Colony Forming Units per 100 Milliliters" "milligrams per liter"                     "micrograms per liter"                    
# [4] "None"                                     "Most Probable Number per 100 milliliters" "pico-Curies per Liter"                   
# [7] "Degrees Celsius (Centigrade)"    
unique(dStds$CharDep) # [1] "261"  "481n" "481x" "time" "1e" 
unique(dStds$Coefficient1) #  [1]  0.7998  0.8545  1.2730  0.3331  0.8460  0.0577  1.7200 27.5000  1.1021  0.9094  2.1400 17.0000  0.9049 18.3000  0.7409  1.0050  0.8473
                           # [18] 28.7000

# ---------------------------------
# fill values
# ---------------------------------
# lookup dependent characteristics -- pH, Hardness, Temp & the Hardness_UOM & Temp_UOM
# lookup Coeff1-8 for characteristic
# lookup Std_CalcFormula
# lookup Std_Value
# lookup Std_UOM
# lookup Std_Medium
# lookup Std_SMPL_FRAC_TYPE_NM
# determine Std_Compliance
# ---------------------------------

# lookup iStd values

X <- tail(head(df1,5000), 1)
std <- X$TSRCHAR_IS_NUMBER
park <- X$PARK_ID
stationID <- X$StationID
medium <- X$MEDIUM
smpl_frac <- X$SMPL_FRAC_TYPE_NM

dfMatch = filter(iStds,
                   iStds$TSRCHAR_IS_NUMBER == std &
                   iStds$PARK_ID == toupper(park) & 
                   iStds$StationID == stationID & 
                   tolower(iStds$MEDIUM) == tolower(medium) & 
                   tolower(iStds$SMPL_FRAC_TYPE_NM) == tolower(smpl_frac)
)
# Error: incorrect length (0), expecting: 41586

pkgTest("data.table")
library(data.table)
rkeys <- c("TSRCHAR_IS_NUMBER", "Park", "StationID", "MEDIUM", "SMPL_FRAC_TYPE_NM")
tRes <- data.table(allres, key=rkeys)
skeys <- c("TSRCHAR_IS_NUMBER", "PARK_ID", "StationID", "Medium", "SMPL_FRAC_TYPE_NM")
tIStd <- data.table(iStds, key=skeys)
tRes[tIStd, Matched := 1L]

# 
