###
# Get Files
###

library(ggplot2)
library(dplyr)
library(reshape2)

# The nudge tagging is not oK..ci  has a 's ..need to cater...

d <- read.csv("C:/work/worxogo/02 output/axis/axisJune-DayWise-NudgeAndKPI-20170918.csv",stringsAsFactors = FALSE)

dim(d)

names(d)
View(d)


d$kpi_Date <- as.Date(d$kpi_Date)

###
# Nudge Summary
###

nSum <- d %>% group_by(DRMName) %>%
  summarise(CASANudge = sum(CASA),
            CINudge = sum(CI),
            LIMeetingNudge = sum(LIMeeting),
            EngagedNudge = sum(Engaged),
            NotEngagedNudge = sum(NotEngaged),
            GenericNudge = sum(Generic),
            SpecificNudge = sum(Specific),
            NoNudge = sum(NoNudge),
            TotNudge = sum(TotNudge),
            Rank = mean(Rank),
            
            CIAchPercent = max(CIAchPercent),
            RevPointsAchPercent = max(RevPointsAchPercent),
            GILoginAchPercent = max(GILoginAchPercent),
            CASAAchPercent = max(CASAAchPercent),
            LILoginAchPercent = max(LILoginAchPercent),
            LIMtngsAchPercent = max(LIMtngsAchPercent))


View(nSum)
dim(nSum)

###
# Why is 0 or 100 for CASA and LIMtngs... ???
# No Targets for these
# CASA
# LIMtngs

# Divided by Total Achieved ..hence 100%
# 100% has no meaning for these two 
###


###
# 01
# Stacked bar of all  Ach
###

names(nSum)

kpiAll <- nSum[c(1,12:17,11)]
names(kpiAll)

names(kpiAll) <- c("DRMName","CI","RevPoints","GILogin","CASA","LILogin","LIMtngs","Rank")
library(reshape2)

# Can NULL and plot without these
#kpiAll$RevPoints <- NULL
#kpiAll$CI <- NULL

kpiM <- melt(kpiAll,id.vars = c("DRMName","Rank"))
head(kpiM)

p <- ggplot(kpiM, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')


p + coord_flip() + theme_bw()

# By Rank
r1 <- subset(kpiM,Rank <= 15)
r2 <- subset(kpiM,Rank > 15 & Rank <= 30)
r3 <- subset(kpiM,Rank > 30 & Rank <= 45)
r4 <- subset(kpiM,Rank > 45)

p <- ggplot(r1, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')

p <- ggplot(r2, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')

p <- ggplot(r3, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')

p <- ggplot(r4, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')

# Can plot above one by one
p + coord_flip() + theme_bw()

###
# 02
# Nudge
### 

View(nSum)
names(nSum)
nudges <- nSum[c(1,2,3,4,7,9)]
#nudges <- nSum[c(1,2,3,4,7)] # No NoNudge
nudgeM <- melt(nudges,id.vars = "DRMName")
head(nudgeM)

p <- ggplot(nudgeM, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')

p + coord_flip()

###
# 03 
# Achieved Graph - Group By Quartile
# CI
##

ci1 <- subset(d,CIAchQutle == 1)
ci2 <- subset(d,CIAchQutle == 2)
ci3 <- subset(d,CIAchQutle == 3)
ci4 <- subset(d,CIAchQutle == 4)

myplt <- function(df) {
  p <- ggplot(df,aes(y = CIAchPercent, x = kpi_Date, colour = DRMName)) + 
    geom_line(size=.5,alpha=0.9) + 
    #geom_point(size=1) + 
    expand_limits(y = c(0,100)) 
  
  return(p)
}


myVlines <- function(p,lnClr){
  p <- p + geom_vline(aes(xintercept = as.integer(as.Date("2017/06/05"))), col = lnClr)
  p <- p + geom_vline(aes(xintercept = as.integer(as.Date("2017/06/13"))), col = lnClr)
  p <- p + geom_vline(aes(xintercept = as.integer(as.Date("2017/06/23"))), col = lnClr)
  p <- p + geom_vline(aes(xintercept = as.integer(as.Date("2017/06/28"))), col = lnClr)
  p <- p + geom_vline(aes(xintercept = as.integer(as.Date("2017/06/29"))), col = lnClr)
  
  p + scale_x_date(date_labels="%d",date_breaks  ="1 day")
  
  return(p)
}

# Get the plots
myVlines(myplt(ci1),"green")
myVlines(myplt(ci2),"orange")
myVlines(myplt(ci3),"blue")
myVlines(myplt(ci4),"purple")

# H lines
myHlines <- function(p,ci1,ci2,ci3,ci4,lnClr1,lnClr2,lnClr3,lnClr4){
  p <- p + geom_hline(yintercept=mean(ci1$CIAchPercent),size=1,col = lnClr1)
  p <- p + geom_hline(yintercept=max(ci1$CIAchPercent),size=1,linetype="dashed",col = lnClr1)
  
  p <- p + geom_hline(yintercept=mean(ci2$CIAchPercent),size=1,col = lnClr2)
  p <- p + geom_hline(yintercept=max(ci2$CIAchPercent),size=1,linetype="dashed",col = lnClr2)
  
  p <- p + geom_hline(yintercept=mean(ci3$CIAchPercent),size=1,col = lnClr3)
  p <- p + geom_hline(yintercept=max(ci3$CIAchPercent),size=1,linetype="dashed",col = lnClr3)
  
  p <- p + geom_hline(yintercept=mean(ci4$CIAchPercent),size=1,col = lnClr4)
  #p <- p + geom_hline(yintercept=max(ci4$CIAchPercent),size=2,linetype="dotted",col = lnClr4)
  p <- p + geom_hline(yintercept=max(ci4$CIAchPercent),size=1,linetype="dashed",col = lnClr4)
  
  p + theme_bw()
  return(p)
}

# Plot the graph
p <- myVlines(myplt(ci1),"pink")
myHlines(p,ci1,ci2,ci3,ci4,"red","Orange","blue","green") + theme_bw()

p <- myVlines(myplt(ci2),"orange")
myHlines(p,ci1,ci2,ci3,ci4,"red","Orange","blue","green")  + theme_bw()

p <- myVlines(myplt(ci3),"blue")
myHlines(p,ci1,ci2,ci3,ci4,"red","Orange","blue","green") + theme_bw()

p <- myVlines(myplt(ci4),"purple")
myHlines(p,ci1,ci2,ci3,ci4,"red","Orange","blue","green") + theme_bw()


###
# 04 Q1 and Q4
###

ci13 <- subset(d,CIAchQutle == 1 | CIAchQutle == 3)
ci14 <- subset(d,CIAchQutle == 1 | CIAchQutle == 4)

myVlines(myplt(ci13),"red")
myVlines(myplt(ci14),"pink")

p <- myVlines(myplt(ci14),"orange")
myHlines(p,ci1,ci2,ci3,ci4,"red","Orange","blue","green")  + theme_bw()



##
# 05 Get Qunatile wise mean and max
###

ciQ <- d %>% group_by(kpi_Date,CIAchQutle) %>%
  summarise(CIAchQMean = mean(CIAchPercent),
            CIAchQMax = max(CIAchPercent))

View(ciQ)
names(ciQ)

ciQm <- melt(ciQ,id.vars = c("kpi_Date","CIAchQutle"))
View(ciQm)

ciQm$DRMName <- ifelse(ciQm$variable == "CIAchQMean",paste("CIAchMeanQ",ciQm$CIAchQutle,sep=""),
                       paste("CIAchMaxQ",ciQm$CIAchQutle,sep=""))
table(ciQm$DRMName)                      
names(ciQm)
ciQm <- ciQm[c(5,1,4,2)]
colnames(ciQm)[3] <- "CIAchPercent"

# Plot the max and mean
###
names(ciQm)

ggplot(ciQm, aes(x=kpi_Date, y=CIAchPercent, color=DRMName)) + geom_line() + geom_point(size=1)


###
# Quartile 1 with max and mean
###
ci1Q <- ci1[c("DRMName","kpi_Date","CIAchPercent","CIAchQutle")]
ci1Q <- rbind(ci1Q,subset(ciQm,CIAchQutle == 1))

ggplot(ci1Q, aes(x=kpi_Date, y=CIAchPercent, color=DRMName)) + geom_line()
myplt(ci1Q)
myVlines(myplt(ci1Q),"blue")


# Quartile 2 with min and max
ci2Q <- ci2[c("DRMName","kpi_Date","CIAchPercent","CIAchQutle")]
ci2Q <- rbind(ci2Q,subset(ciQm,CIAchQutle == 2))

ggplot(ci2Q, aes(x=kpi_Date, y=CIAchPercent, color=DRMName)) + geom_line()


###
# 06 Other KPI's by Quartile
###


mypltGeneric <- function(df,kpiComp,yMax=100) {
  p <- ggplot(df,aes(y = df[[kpiComp]], x = kpi_Date, colour = DRMName)) + 
    geom_line(size=.8) + expand_limits(y = c(0,yMax)) 
  
  return(p)
}

###
# CASA
###

q11 <- subset(d,CASAAchQutle == 1)
q21 <- subset(d,CASAAchQutle == 2)
q31 <- subset(d,CASAAchQutle == 3)
q41 <- subset(d,CASAAchQutle == 4)


mypltGeneric(q11,"CASAAchCS",max(d$CASAAchCS))
mypltGeneric(q21,"CASAAchCS",max(d$CASAAchCS))
mypltGeneric(q31,"CASAAchCS",max(d$CASAAchCS))
mypltGeneric(q41,"CASAAchCS",max(d$CASAAchCS))

# most are zeroes...remove and plot
casa <- subset(d,CASAAchTillDate != 0)
length(unique(casa$EmployeeID))

casaNo <- subset(d,CASAAchTillDate == 0)
length(unique(casaNo$EmployeeID))

# Too many
mypltGeneric(casa,"CASAAchCS",max(casa$CASAAchCS)) 

# Group them into 2
casa50 <- unique(casa$EmployeeID)
casa50

casa50[1:10]
casa50[11:19]

casa51 <- subset(casa,EmployeeID %in% casa50[1:10])
unique(casa51$EmployeeID)
casa52 <- subset(casa,EmployeeID %in% casa50[11:19])

mypltGeneric(casa51,"CASAAchCS",max(casa51$CASAAchCS)) 
mypltGeneric(casa52,"CASAAchCS",max(casa52$CASAAchCS)) 

###
# GILogin
###

gil1 <- subset(d,GILoginAchQutle == 1)
gil2 <- subset(d,GILoginAchQutle == 2)
gil3 <- subset(d,GILoginAchQutle == 3)
gil4 <- subset(d,GILoginAchQutle == 4)

mypltGeneric(gil1,"GILoginAchCS",max(d$GILoginAchCS))
mypltGeneric(gil2,"GILoginAchCS",max(d$GILoginAchCS))
mypltGeneric(gil3,"GILoginAchCS",max(d$GILoginAchCS))
mypltGeneric(gil4,"GILoginAchCS",max(d$GILoginAchCS))

# Too many zeroes
# Remove 0's

gil <- subset(d,d$GILoginAchTillDate != 0)
length(unique(gil$EmployeeID))

gilNo <- subset(d,d$GILoginAchTillDate == 0)
length(unique(gilNo$EmployeeID))

gil50 <- unique(gil$EmployeeID)
gil50

gil51 <- subset(gil,gil$EmployeeID %in% gil50[1:8])
unique(gil51$EmployeeID)
gil52 <- subset(gil,gil$EmployeeID %in% gil50[9:16])

mypltGeneric(gil51,"GILoginAchCS",max(gil51$GILoginAchCS)) 
mypltGeneric(gil52,"GILoginAchCS",max(gil52$GILoginAchCS)) 

mypltGeneric(gil51,"GILoginAchCS",40)
mypltGeneric(gil52,"GILoginAchCS",40) 

# mypltGeneric(r1,"LIMtngsAchCS",max(d$LIMtngsAchCS))
# mypltGeneric(r1,"LILoginAchCS",max(d$LILoginAchCS))
# mypltGeneric(r1,"GILoginAchCS",max(d$GILoginAchCS))

###
# LILogin
###

li1 <- subset(d,LILoginAchQutle == 1)
li2 <- subset(d,LILoginAchQutle == 2)
li3 <- subset(d,LILoginAchQutle == 3)
li4 <- subset(d,LILoginAchQutle == 4)

mypltGeneric(li1,"LILoginAchCS",max(d$LILoginAchCS))
mypltGeneric(li2,"LILoginAchCS",max(d$LILoginAchCS))
mypltGeneric(li3,"LILoginAchCS",max(d$LILoginAchCS))
mypltGeneric(li4,"LILoginAchCS",max(d$LILoginAchCS))

# Too many 0's, remove them
li <- subset(d,d$LILoginAchTillDate != 0)
length(unique(li$EmployeeID))

liNo <- subset(d,d$LILoginAchTillDate == 0)
length(unique(liNo$EmployeeID))

li50 <- unique(li$EmployeeID)
li50

li51 <- subset(li,li$EmployeeID %in% li50[1:8])
unique(li51$EmployeeID)
li52 <- subset(li,li$EmployeeID %in% li50[9:17])

mypltGeneric(li51,"LILoginAchCS",max(li51$LILoginAchCS)) 
mypltGeneric(li52,"LILoginAchCS",max(li52$LILoginAchCS)) 

mypltGeneric(gil51,"GILoginAchCS",40)
mypltGeneric(gil52,"GILoginAchCS",40) 

###
# Scatter plot
###

sSum <- d %>% group_by(DRMName) %>%
  summarise(CIAch = sum(CIAch),
            GILoginAch = sum(GILoginAch),
            LIMtngsAch = sum(LIMtngsAch),
            LILoginAch = sum(LILoginAch),
            CASAAch = sum(CASAAch),
            RevPoints = sum(RevPointsAch),
            Score = mean(TotalScore),
            Rank = mean(Rank),
            CIAchPercent = max(CIAchPercent),
            GILoginAchPercent = max(GILoginAchPercent),
            LILoginAchPercent = max(LILoginAchPercent),
            RevPointsAchPercent = max(RevPointsAchPercent),
            CASAAchPercent = max(CASAAchPercent),
            LIMtngsAchPercent = max(LIMtngsAchPercent))

View(sSum)  
names(sSum)
ggplot(sSum, aes(x = CIAch, y = RevPoints)) + geom_point()

dim(sSum)
t1 <- d[c("DRMName","ReportingManager","Personality")]
dim(t1)
t1 <- t1[!duplicated(t1$DRMName),]

s <- merge(t1,sSum)
dim(s)
View(s)
names(s)


g1 <- ggplot(s, aes(CIAch, RevPoints)) + geom_point(aes(color = ReportingManager,shape=Personality),size=6) + 
  xlab('CI Ach') + ylab('Rev Points') + ggtitle('Axis - May')

g1 + scale_color_brewer(type = 'div', palette = 'Spectral', direction = 1)



###
# Plot Distribution
###

# My manager
ggplot(s, aes(x = CIAch)) + geom_density(fill = "green", colour = "red",alpha = 0.4) + 
  facet_grid(. ~ ReportingManager)


# All together
ggplot(s, aes(x = CIAch, fill = ReportingManager)) +
  geom_density(position = "stack", alpha = 0.6) +
  scale_x_continuous(name = "CI Achieved",
                     breaks = seq(0, 750, 20),
                     limits=c(0, 750)) +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of CI Achieved by Manager") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma")) +
  scale_fill_brewer(palette="Accent")


###
# Nudge and KPI
###

View(nSum)
names(nSum)
nudges <- nSum[c(1,2,3,4,7,9)]
#nudges <- nSum[c(1,2,3,4,7)] # No NoNudge
nudgeM <- melt(nudges,id.vars = "DRMName")
head(nudgeM)

p <- ggplot(nudgeM, aes(x = DRMName, y = value,fill=variable)) +
  geom_bar(stat='identity')

p + coord_flip()

p <- ggplot(nudgeM, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity')

p + coord_flip() # Final

###
# Not engaged Nudge and gaf behaviour
# Engaged/Not Engaged messgae sent to  22nd...seperate the data
###

bef <- subset(d,kpi_Date >= "2017-06-01" & kpi_Date < "2017-06-22") 
aft <- subset(d,kpi_Date >= "2017-06-22") 
dim(bef)
dim(aft)

befSum <- bef %>% group_by(DRMName) %>%
  summarise(TotUniquePageviews = sum(TotUniquePageviews),
            TotPageviews = sum(TotPageviews),
            TotVisit = sum(TotVisit),
            
            MeanUniquePageviews = mean(TotUniquePageviews),
            MeanPageviews = mean(TotPageviews),
            MeanVisit = mean(TotVisit),
            
            Engaged = sum(Engaged),
            NotEngaged = sum(NotEngaged))

befSum <- bef %>% group_by(DRMName) %>%
  summarise(
    MeanUniquePageviews = mean(TotUniquePageviews),
    MeanPageviews = mean(TotPageviews),
    MeanVisit = mean(TotVisit),
    
    TotUniquePageviews = sum(TotUniquePageviews),
    TotPageviews = sum(TotPageviews),
    TotVisit = sum(TotVisit),
    
    
    Engaged = sum(Engaged),
    NotEngaged = sum(NotEngaged)
  )


aftSum <- aft %>% group_by(DRMName) %>%
  summarise(
    MeanUniquePageviews = mean(TotUniquePageviews),
    MeanPageviews = mean(TotPageviews),
    MeanVisit = mean(TotVisit),
    
    TotUniquePageviews = sum(TotUniquePageviews),
    TotPageviews = sum(TotPageviews),
    TotVisit = sum(TotVisit),
    
    
    Engaged = sum(Engaged),
    NotEngaged = sum(NotEngaged)
  )


names(befSum)
# These are 0 as values are in afer(22nd)...remove them...
befSum$Engaged <- NULL
befSum$NotEngaged <- NULL

colnames(befSum) <- paste("Before", colnames(befSum), sep = "_")
names(befSum)[1] <- "DRMName"

colnames(aftSum) <- paste("After", colnames(aftSum), sep = "_")
names(aftSum)[1] <- "DRMName"

## Check for outliers

boxplot(befSum$Before_TotPageviews)
boxplot(befSum$Before_TotUniquePageviews)

boxplot(aftSum$After_TotPageviews)
boxplot(aftSum$After_TotUniquePageviews)


### merge
eNotEgaf <- merge(befSum,aftSum) 
dim(befSum)
dim(aftSum)
dim(eNotEgaf)

View(eNotEgaf)
names(eNotEgaf)

#eNotEgaf <- eNotEgaf[c(1:4,10:12,5:9,13:17)]
eng <- subset(eNotEgaf,eNotEgaf$After_Engaged == 1)
notE <- subset(eNotEgaf,eNotEgaf$After_NotEngaged == 1)

dim(eng)
dim(notE)

names(eng)
eng1 <- eng[c(1,3,9)]
head(eng1)
eng1m <- melt(eng1)

# Engaged

p1 <- ggplot(eng1m, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity', position='dodge')
p1 + coord_flip() + scale_fill_brewer(palette = "Set1")

notE1 <- notE[c(1,3,9)]
notE1m <- melt(notE1)

### Not Engaged

p1 <- ggplot(notE1m, aes(x=reorder(DRMName,value), y = value,fill=variable)) +
  geom_bar(stat='identity', position='dodge')

p1 + coord_flip() + scale_fill_brewer(palette = "Dark2")



###
# Diff in tot KPI ach...share data 
###
dm <- read.csv("C:/work/worxogo/02 output/axis/axisDM-20170904.csv",stringsAsFactors = FALSE,na.strings = "NA")

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
# Get June data
###

kpiJun <- subset(kpi,kpi_Date >= "2017-06-01" & kpi_Date <= "2017-06-30") 
dim(kpiJun)

kpiSum <- kpiJun %>% group_by(EmployeeID) %>%
  summarise(GILoginAchTillDate = mean(GILoginAchTillDate),
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
            CASAAch = sum(CASAAch))

View(kpiSum) 
getwd()

# Write and share this...only CI,RevPoints.. has difference....
write.csv(kpiSum,"c:/work/worxogo/09 Process/45 axisNudgeAndKPI/CIAchNotMatchingForMay-20170912.csv",row.names=FALSE)
