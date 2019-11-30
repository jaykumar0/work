setwd("C:/work/worxogo")
library(readxl)
library(dplyr)

###
# New file - 04/09/2017
###
d <- read_excel("01 inputDataAxis-20170627/20170904/FOS- Nudges(June - July).xlsx",sheet="June(FOS)",skip=0)

View(d)
names(d)
dim(d)

nm_str <- c("Phone Number","SCE Name","EmpID","1-Jun-17","2-Jun-17","3-Jun-17","4-Jun-17","5-Jun-17",
            "6-Jun-17",	"6-Jun-17",	"7-Jun-17",	"8-Jun-17",	"9-Jun-17",	"10-Jun-17",
            "11-Jun-17","12-Jun-17","13-Jun-17","14-Jun-17","15-Jun-17","15-Jun-17",	"16-Jun-17",
            "17-Jun-17",	"18-Jun-17",	"19-Jun-17",	"20-Jun-17",	"21-Jun-17",	"22-Jun-17",	
            "22-Jun-17",	"23-Jun-17",	"24-Jun-17",	"25-Jun-17",	"26-Jun-17",	"27-Jun-17",
            "27-Jun-17",	"27-Jun-17",	"28-Jun-17",	"28-Jun-17",	"28-Jun-17",	"28-Jun-17",
            "29-Jun-17",	"29-Jun-17",	"30-Jun-17")


names(d) <- nm_str
names(d)

dim(d)
View(d)
nudge <- d[,c(3,4:41)]
nudge <- nudge[rowSums(is.na(nudge)) != ncol(nudge),]

dim(nudge)
names(nudge)
View(nudge)

# Melt and process data


library(reshape2)

# m <- dcast(melt(dfClean,id.vars=c("CandidateEmail","Attribute")),
#           CandidateEmail ~ Attribute + variable, fun.aggregate = sum)

m <- melt(nudge,id.vars=c("EmpID"))
View(m)

head(m)

# update with correct date value
m$NudgeDt <- as.Date(m$variable,"%d-%b-%y")

# So many are no msg ...OK - as in excel...
View(m[!complete.cases(m),])

### 
# Categorize the nudges
###

m$msg_cat5 <- ifelse(is.na(m$value),"NoNudge",
                     ifelse(grepl("last day of the month",m$value, fixed=TRUE),"Generic",
                            ifelse(grepl( "closed last month at rank",m$value, fixed=TRUE),"Generic",
                                   ifelse(grepl( "for every LI meeting completed",m$value, fixed=TRUE),"LIMeeting",
                                          ifelse(grepl( "for Customer Interactions in last week",m$value, fixed=TRUE),"CI",
                                                 ifelse(grepl("Green Zone (>=24 Customer Interactions) for",m$value, fixed=TRUE),"CI",
                                                        ifelse(grepl("Focus more complete your LI Meeting target this month",m$value, fixed=TRUE),"LIMeeting",
                                                               ifelse(grepl("Great Job",m$value, fixed=TRUE) & grepl("LI Meetings",m$value, fixed=TRUE),"LIMeeting",
                                                                      ifelse(grepl("Champion's Trophy winning Team",m$value, fixed=TRUE),"Generic",
                                                                             ifelse(grepl("Completing Customer Interactions towards the end of week",m$value, fixed=TRUE),"CI",
                                                                                    ifelse(grepl("LI meeting",m$value, fixed=TRUE),"LIMeeting", # 21st LI M ..get back....
                                                                                           ifelse(grepl("Do you know how your team is performing in Champions Trophy",m$value, fixed=TRUE),"NotEngaged",
                                                                                                  ifelse(grepl("Good to see that you are regularly monitoring your performance on the app",m$value, fixed=TRUE),"Engaged",
                                                                                                         ifelse(grepl("Runs in Champions Trophy Contest",m$value, fixed=TRUE),"Generic",
                                                                                                                ifelse(grepl("you are leading the Champions Trophy Contest",m$value, fixed=TRUE),"Generic",
                                                                                                                       ifelse(grepl("Customer Interaction",m$value, fixed=TRUE) & grepl("performing way below your potential",m$value, fixed=TRUE),"CI",
                                                                                                                              ifelse(grepl("Customer Interaction",m$value, fixed=TRUE) & grepl("doing excellent job",m$value, fixed=TRUE),"CI",
                                                                                                                                     ifelse(grepl("customer interactions",m$value, fixed=TRUE) & grepl("Badge",m$value, fixed=TRUE),"CI",
                                                                                                                                            ifelse(grepl("Customer Interaction",m$value, fixed=TRUE) & grepl("green zone",m$value, fixed=TRUE),"CI",
                                                                                                                                                   ifelse(grepl("LI meetings",m$value, fixed=TRUE) & grepl("You are missing your target by big margin", m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                          #ifelse((grepl("Great Job",m$value, fixed=TRUE) & grepl("LI Meetings",m$value, fixed=TRUE)),"LIMeeting",
                                                                                                                                                          ifelse(grepl("few who has not done any LI meetings this month",m$value, fixed=TRUE),"LIMeetin",
                                                                                                                                                                 ifelse(grepl("LI meetings",m$value, fixed=TRUE) & grepl("Congratulations",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                        ifelse(grepl("LI meetings",m$value, fixed=TRUE) & grepl("you can achieve the target",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                               ifelse(grepl("Carpediem feedback survey",m$value, fixed=TRUE),"Generic",
                                                                                                                                                                                      ifelse(grepl("survey on your experience with Carpediem program",m$value, fixed=TRUE),"Generic",
                                                                                                                                                                                             ifelse(grepl("CASA score is Zero for this month",m$value, fixed=TRUE),"CASA", 
                                                                                                                                                                                                    ifelse(grepl("Completing Customer Interactions towards the end",m$value, fixed=TRUE),"CI",
                                                                                                                                                                                                           ifelse(grepl("Customer Interaction performance for this month is very poor",m$value, fixed=TRUE),"CI",
                                                                                                                                                                                                                  ifelse(grepl("missing your Customer Interaction target by small margin for this month",m$value, fixed=TRUE),"CI",
                                                                                                                                                                                                                         ifelse(grepl("You are doing good in Customer Interaction for this month",m$value, fixed=TRUE),"CI",
                                                                                                                                                                                                                                
                                                                                                                                                                                                                                ifelse(grepl("LI meeting",m$value, fixed=TRUE) & grepl("performance is way below your potential",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                                                                                       ifelse(grepl("LI meetings",m$value, fixed=TRUE) & grepl("not performing upto your potential",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                                                                                              ifelse(grepl("LI meetings",m$value, fixed=TRUE) & grepl("You can do much better",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                                                                                                     ifelse(grepl("LI meetings",m$value, fixed=TRUE) & grepl("opportunity slip",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                            ifelse(grepl("LI Meetings",m$value, fixed=TRUE) & grepl("away from achieving 100%",m$value, fixed=TRUE),"LIMeeting",
                                                                                                                                                                                                                                                                   ifelse(grepl("Customer Interaction",m$value, fixed=TRUE) & grepl("On average you are doing",m$value, fixed=TRUE),"CI",
                                                                                                                                                                                                                                                                          "Z-Misc"))))))))))))))))))))))))))))))))))))



table(m$msg_cat5)
table(m$NudgeDt,m$msg_cat5)

m$msg_cat6 <- ifelse(m$msg_cat5 == "Generic","Generic",
                     ifelse(m$msg_cat5 == "NoNudge","NoNudge","Specific"))

table(m$msg_cat6)

#View(subset(m,msg_cat3 == "Z-Misc"))

names(m)

m$Freq <- 1

###
# Process nydge and all data Day wise
###

# Get Frequencies

md1 <- dcast(m,EmpID + NudgeDt ~ msg_cat5, value.var = "Freq",fun.aggregate = sum)

head(md1)

names(md1)
md1$TotNudge <- rowSums(md1[c(3:7,9)])
#md1$TotNudge <- md1$CASA + md1$CI + md1$Engaged + md1$Generic + md1$LIMeeting + md1$NotEngaged
View(md1)

md1$Specific <- md1$CASA + md1$CI + md1$Engaged + md1$LIMeeting + md1$NotEngaged

table(m$msg_cat6)
sum(md1$Generic)
sum(md1$Specific)


names(md1)

# Keep required Columns

md <- md1[c(1:4,7,5,9,6,11,8,10)]
names(md)
dim(md)
dim(md1)

###
# Merge all day wise data
###
names(md)
names(kpiJun)

md$NudgeDt <- as.character(md$NudgeDt)
df <- merge(kpiJun,md,by.x = c("EmployeeID","kpi_Date"),by.y = c("EmpID","NudgeDt"),all.x = TRUE)

dim(df)
dim(kpiJun)
dim(md)


View(df)

View(df[!complete.cases(df),])
# Mark all to 0
# No Nudge data ..then no nudge and total becoms wrong...better remove them
df[is.na(df)] <- 0

dim(user)
dim(gafJun)

gafNudge <- gafJun %>% group_by(emp_code,gaf_Date) %>%
  summarise(TotUniquePageviews = sum(UniquePageviews),
            TotPageviews = sum(Pageviews),
            TotVisit = n())


# Merge kpi and others with gaf data

df1 <- merge(df,gafNudge,by.x = c("EmployeeID","kpi_Date"),by.y = c("emp_code","gaf_Date"),all.x = TRUE)
dim(df1)
View(df1)


nrow(df1[!complete.cases(df1),])

#Only gaf cols, mark to 0
df1[is.na(df1)] <- 0


# Merge user data
df2 <- merge(df1,user,by = "EmployeeID",all.x = TRUE)
dim(df2)


# Clean  df2
df3 <- subset(df2,df2$ReportingManager != "Test")
dim(df2)
dim(df3)

names(df3)
nrow(subset(df3,df3$Rank == 0))
df3 <- subset(df3,df3$Rank != 0)

# write.csv(df3,"C:/work/worxogo/02 output/axis/axisJune-DayWise-NudgeAndKPI-20170918.csv",row.names = FALSE)

###
# Get cummulative sum
###

t1 <- df3
names(df3)
t1 <- t1 %>% group_by(EmployeeID) %>%
  mutate(CIAchCS = cumsum(CIAch),
         CIAchTot = sum(CIAch),
         
         CASAAchCS = cumsum(CASAAch),
         CASAAchTot = sum(CASAAch),
         
         LIMtngsAchCS = cumsum(LIMtngsAch),
         LIMtngsAchTot = sum(LIMtngsAch),
         
         LILoginAchCS = cumsum(LILoginAch),
         LILoginAchTot = sum(LILoginAch),
         
         GILoginAchCS = cumsum(GILoginAch),
         GILoginAchTot = sum(GILoginAch),
         
         RevPointsAchCS = cumsum(RevPointsAch),
         RevPointsAchTot = sum(RevPointsAch))


names(t1)

# Get the Ach  Percent - day wise
t1$CIAchPercent <- round((t1$CIAchCS/t1$CIMonthlyTarget) * 100,2)
t1$GILoginAchPercent <- round((t1$GILoginAchCS/40) * 100,2)
t1$LILoginAchPercent <- round((t1$LILoginAchCS/200) * 100,2)
t1$RevPointsAchPercent <- round((t1$RevPointsAchCS/350) * 100,2)

# No Targets for these
t1$CASAAchPercent <- round((t1$CASAAchCS/t1$CASAAchTot) * 100,2)
t1$LIMtngsAchPercent <- round((t1$LIMtngsAchCS/t1$LIMtngsAchTot) * 100,2)

View(t1[!complete.cases(t1),])
# mark NA to 0
t1[is.na(t1)] <- 0

###
# Group into quartiles
##

dSum <- t1 %>% group_by(EmployeeID) %>%
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

View(dSum)
dim(dSum)

dSum <- dSum %>% mutate(CIAchQutle = ntile(CIAch, 4),
                        LIMtngsAchQutle = ntile(LIMtngsAch, 4),
                        LILoginAchQutle = ntile(LILoginAch, 4),
                        RevPointsAchQutle = ntile(RevPointsAch, 4),
                        GILoginAchQutle = ntile(GILoginAch, 4),
                        CASAAchQutle = ntile(CASAAch, 4))







View(dSum)

s1 <- dSum[c(1,21:26)]
d1 <- merge(t1,s1)

dim(t1)
dim(d1)
table(d1$CIAchQutle)

write.csv(d1,"C:/work/worxogo/02 output/axis/axisJune-DayWise-NudgeAndKPI-20170918.csv",row.names = FALSE)
