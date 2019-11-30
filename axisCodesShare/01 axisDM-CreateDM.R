library(readxl)
library(dplyr)
library(lubridate)
library(reshape2)

source("00 axisDM-GetFunctions.R")

setwd("C:/work/worxogo")

#axisFileD <- "01 inputDataAxis-20170627/Axis_till June 05/Axis_till June 05/Axis CarpeDiem Updated New Worksheets till  June 05"
axisFileD <- "01 inputDataAxis-20170627/20170904/AxisAugust/Axis CarpeDiem Updated New Worksheets till August 31"
fl1 <- "5. Axis_Point Economics Worksheet_May_New.xlsx"
fl2 <- "6. Axis_Point Economics Worksheet_June_New.xlsx"

fl3 <- "7. Axis_Point Economics Worksheet_July_New.xlsx"
fl4 <- "8. Axis_Point Economics Worksheet_August_New.xlsx"


profileFile <- "01 inputDataAxisTitan/axis/User Profile_Axis.csv"
gaFile <- "01 inputDataAxis-20170627/Axis Bank GA May-June.csv"
# All
gaFile <- "01 inputDataAxis-20170627/GA-20170711/AxisBankFull.csv"


###
# func to get data
###

funGetData <- function(axisDir,fl,yr,mon,profileFile,rankMon,emplCnt){
  user <- funProcUser(axisDir,fl,mon,profileFile)
  kpiT <- funProcKPIt(axisDir,fl,yr,mon)
  kpiA <- funProcKPIa(axisDir,fl,yr,mon)
  lb <- funcGetScoreRank(axisDir,fl,yr,rankMon,emplCnt)
  
  kpiT$kpi_Date <- as.character(kpiT$kpi_Date)
  kpiA$kpi_Date <- as.character(kpiA$kpi_Date)
  
  kpi <- merge(kpiT, kpiA, by = c("EmployeeID","kpi_Date"),all.x = TRUE,all.y = TRUE)
  kpi <- merge(kpi, lb, by = "EmployeeID",all.x = TRUE,all.y = TRUE)
  d <- merge(kpi,user,by = "EmployeeID",all.x = TRUE,all.y=TRUE)
  return(d)

}

###
# Get data for months
###

#"May" "Jun"

mayD <- funGetData(axisFileD,fl1,"2017","May",profileFile,"May",65)
junD <- funGetData(axisFileD,fl2,"2017","Jun",profileFile,"June",63)

julD <- funGetData(axisFileD,fl3,"2017","Jul",profileFile,"July",65)
augD <- funGetData(axisFileD,fl4,"2017","Aug",profileFile,"August",63)

dim(mayD)
View(mayD)

dim(junD)
View(junD)

dim(julD)
View(julD)

dim(augD)
View(augD)

###
# Get ga file
# Comment the hour processing if Hour variable is in gaf file or add the processing for hour
###

gaf <- funcGetGA(gaFile)
dim(gaf)

View(gaf)

###
# Merge - 01
###

d <- rbind(mayD,junD)

d <- rbind(d,julD)
d <- rbind(d,augD)

identical(names(mayD), names(junD) )
identical(names(mayD), names(julD) )
identical(names(augD), names(julD) )
names(junD)
names(augD)

################################################################################
# July Aug...not getting the total score and rank
# columns have moved ...check the excel
# Aug is columns M and N 
# May has K and L ..explore more - what new has been  added in july and august
# Ignore july and aug for now
# t <- funcGetScoreRank(axisFileD,fl4,"2017","August",63)
# View(t)
################################################################################

d <- rbind(mayD,junD)
d <- d[c(1:34)]

# Remove Dates which are NA - becuase of 31 ( invalid date )
d1 <- d[!is.na(d$kpi_Date),]
names(d1)

d2 <- bind_rows(d1,gaf)
nrow(d1) + nrow(gaf)
dim(d2)


write.csv(d2,"C:/work/worxogo/02 output/axis/axisDM-20170918.csv",row.names = FALSE)
