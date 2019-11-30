setwd("C:/work/worxogo")

library(dplyr)
###
# read DM
###
dm <- read.csv("02 output/axis/axisDM-20170918.csv",stringsAsFactors = FALSE,na.strings = "NA")

dim(dm)
names(dm)

###
# Get GA data
###
gaf <- dm[c(35:39)]
dim(gaf)
names(gaf)

# create new dataset without missing data
gaf <- na.omit(gaf) 
dim(gaf)


# Remove 0 values
gaf <- subset(gaf,Pageviews > 0 & UniquePageviews > 0)

###
# Sum by id, date and hr
###

dim(gaf)

#gafMay <- subset(gaf,gaf_Date >= "2017-05-01" & gaf_Date <= "2017-05-31") 
gafJun <- subset(gaf,gaf_Date >= "2017-06-01" & gaf_Date <= "2017-06-30") 

#View(gafJun)
# gafMay1 <- gafMay %>% group_by(emp_code,gaf_Date, Hour) %>%
#   summarise(TotUniquePageviews = sum(Unique.Pageviews),
#             TotPageviews = sum(Pageviews))


###
# Number of views by User
###

gafMay2 <- gafMay %>% group_by(emp_code) %>%
  summarise(TotUniquePageviews = sum(UniquePageviews),
            TotPageviews = sum(Pageviews),
            TotVisits = n())

gafJun2 <- gafJun %>% group_by(emp_code) %>%
  summarise(TotUniquePageviews = sum(UniquePageviews),
            TotPageviews = sum(Pageviews),
            TotVisits = n())


###
# Get KPI and score data
###
names(dm)
kpi <- dm[c(1:24)]

# Remove rows with NA vals in kpi_date
# For NA's by gaf
kpi <- kpi[!is.na(kpi$kpi_Date),]

dim(kpi)

###
# Get May data
###
kpiMay <- subset(kpi,kpi_Date >= "2017-05-01" & kpi_Date <= "2017-05-31") 
dim(kpiMay)

###
# Get June data
###
kpiJun <- subset(kpi,kpi_Date >= "2017-06-01" & kpi_Date <= "2017-06-30") 
dim(kpiJun)

#kpiMaySum <- kpiMay %>% group_by(EmployeeID) %>%
kpiJunSum <- kpiJun %>% group_by(EmployeeID) %>%
  summarise(RevPointsTarget = mean(RevPointsTarget),
            LILoginTarget = mean(LILoginTarget),
            GILoginTarget = mean(GILoginTarget),
            CIMonthlyTarget = mean(CIMonthlyTarget),
            CIDailyTarget = sum(CIDailyTarget),
            GILoginAchTillDate = mean(GILoginAchTillDate),
            GILoginAch = sum(GILoginAch),
            CIAchTillDate = mean(CIAchTillDate),
            CIAch = sum(CIAch),
            LIMtngsAchTillDate = mean(LIMtngsAchTillDate),
            LIMtngsAch = sum(LIMtngsAch),
            LILoginAchTillDate = mean(LILoginAchTillDate),
            LILoginAch = sum(LILoginAch),
            RevPointsAchTillDate = mean(RevPointsAchTillDate),
            RevPointsAch = sum(RevPointsAch),
            CASAAchTillDate = mean(CASAAchTillDate),
            CASAAch = sum(CASAAch),
            TotalScore = mean(TotalScore),
            Rank = mean(Rank))
            


###
# get user info
###
names(dm)
user <- dm[c(1,25:34)]
dim(user)

user <- na.omit(user)
names(user)
user <- user[!duplicated(user$EmployeeID),]

###
# Merge all , for May
###
names(gafMay2)[1] <- "EmployeeID"
names(gafJun2)[1] <- "EmployeeID"

# # merge for may
# may <- merge(may,gafMay2,by = "EmployeeID",all.x = TRUE,all.y = TRUE)
# dim(may)

# # For NA's by kpi - NO KPI , user info
# may <- may[!is.na(may$DRMName),]
# # For NA's by gaf - No gaf info
# may[is.na(may)] <- 0
# 

###
# merge for June
###
# KPI
june <- merge(user,kpiJunSum,by = "EmployeeID",all.x = TRUE,all.y = TRUE)
dim(june)
nrow(june[!complete.cases(june),])
# GAF
june <- merge(june,gafJun2,by = "EmployeeID",all.x = TRUE,all.y = TRUE)
dim(june)
#View(june[!complete.cases(june),])

###
# Clean June
###

# For NA's by kpi - NO KPI , user info
june <- june[!is.na(june$DRMName),]
# For NA's by gaf - No gaf info
june[is.na(june)] <- 0

dim(june)
names(june)
#View(june)

# Remove Test
#may <- subset(may,may$Designation != "Test")
#dim(may)

june <- subset(june,june$Designation != "Test")
dim(june)

View(june)
# There are Rank 0 present....
