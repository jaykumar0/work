
###
# 01 Read user master data
###

funProcUser <- function(dir,fl,mon,profileFile) {
  user <- read_excel(paste(dir,fl,sep="/"),na = "NA",sheet="1_Master",skip=0)
  #user <- user[rowSums(is.na(user)) != ncol(user),]
  #user$UsrMon <- mon
  #user <- na.omit(user)
  user <- user[,c(2:11)]
  nmStr <- make.names(names(user), unique = TRUE, allow_ = TRUE)
  nmStr <- gsub("\\.", "", nmStr)
  names(user) <- nmStr
  user <- user[rowSums(is.na(user)) != ncol(user),]
  user$Email <- NULL
  user$UsrMon <- mon
  #
  prof <- read.csv(profileFile,stringsAsFactors = FALSE)
  #prof$ï..Name <- NULL
  prof <- prof[c(2,3)]
  names(prof) <- c("EmpId","Personality")
  #
  user <- merge(user,prof,by = "EmpId",all.x = TRUE,all.y = FALSE)
  user$Personality[is.na(user$Personality)] <- "Missing"
  names(user)[1] <- "EmployeeID"
  return(user)
}


###
# Fun to add - Y, M W
###

funAddDtComp <- function(df,dtCol) {
  df[[dtCol]] <- as.Date(df[[dtCol]],"%Y-%b-%d")
  df$Year <- format(df[[dtCol]],"%Y")
  df$Month <- format(df[[dtCol]],"%b")
  df$Week <- paste("W",ceiling(day(df[[dtCol]]) / 7),sep = "")
  #df$SumKey <- paste(df$Year,df$Month,df$Week,sep = "-")
  return (df)
}

###
# 02 Read KPI Target Data
###

funProcKPIt <- function(dir,fl,yr,mon) {
  kpi <- read_excel(paste(dir,fl,sep="/"),sheet="1_Target (Axis)",skip=1)
  #kpi <- na.omit(kpi)
  kpi <- kpi[,c(3,5:39)]
  kpi <- kpi[rowSums(is.na(kpi)) != ncol(kpi),]
  nmStr <- make.names(names(kpi), unique = TRUE, allow_ = TRUE)
  nmStr <- gsub("\\.", "", nmStr)
  names(kpi) <- nmStr
  
  # impute the daily date
  kpi <- melt(kpi, id.vars=c("EmployeeID","RevenuePoints","LILoginValue","GILoginValue","CustomerInteractions"))
  kpi$kpi_Date <- paste(yr,mon,gsub("\\D","",kpi$variable),sep='-')
  kpi$variable <- NULL
  names(kpi)[2] <- "RevPointsTarget"
  names(kpi)[3] <- "LILoginTarget"
  names(kpi)[4] <- "GILoginTarget"
  names(kpi)[5] <- "CIMonthlyTarget"
  names(kpi)[6] <- "CIDailyTarget"
  
  kpi <- funAddDtComp(kpi,"kpi_Date")
  
  return(kpi)
  }

###
# fun to add to ach
###

funAddTgt <- function(df,v1,v2,nm,yr,mon){
  #df <- melt(df, id.vars=c(df[[v1]],df[[v2]]))
  names(df)[1] <- v1
  names(df)[2] <- v2
  df <- melt(df, id.vars=c(v1,v2))
  df$kpi_Date <- paste(yr,mon,gsub("\\D","",df$variable),sep='-')
  #df <- funAddDtComp(df,"kpi_Date")
  df$variable <- NULL
  names(df)[3] <- nm
  return(df)
}

###
# 02 Read KPI Achieved Data
###

funProcKPIa <- function(dir,fl,yr,mon) {
  kpi <- read_excel(paste(dir,fl,sep="/"),sheet="1_Daily Transaction",skip=1)
  # GI login value
  # Customer Interactions
  # LI Meetings
  # LI Login Value
  # Revenue Points
  # CASA
  # Ignore IPG Points
  gilogIn <- kpi[,c(3,5:36)]
  ci <- kpi[,c(39,41:72)]
  liMtngs <- kpi[,c(75,77:108)]
  lilogIn <- kpi[,c(111,113:144)]
  revPoints <- kpi[,c(147,149:180)]
  casa <- kpi[,c(183,185:216)]
  #
  gilogIn <- gilogIn[rowSums(is.na(gilogIn)) != ncol(gilogIn),]
  ci <- ci[rowSums(is.na(ci)) != ncol(ci),]
  liMtngs <- liMtngs[rowSums(is.na(liMtngs)) != ncol(liMtngs),]
  lilogIn <- lilogIn[rowSums(is.na(lilogIn)) != ncol(lilogIn),]
  revPoints <- revPoints[rowSums(is.na(revPoints)) != ncol(revPoints),]
  casa <- casa[rowSums(is.na(casa)) != ncol(casa),]
  #
  gilogIn <- funAddTgt(gilogIn,"EmployeeID","GILoginAchTillDate","GILoginAch",yr,mon)
  ci <- funAddTgt(ci,"EmployeeID","CIAchTillDate","CIAch",yr,mon)
  liMtngs <- funAddTgt(liMtngs,"EmployeeID","LIMtngsAchTillDate","LIMtngsAch",yr,mon)
  lilogIn <- funAddTgt(lilogIn,"EmployeeID","LILoginAchTillDate","LILoginAch",yr,mon)
  revPoints <- funAddTgt(revPoints,"EmployeeID","RevPointsAchTillDate","RevPointsAch",yr,mon)
  casa <- funAddTgt(casa,"EmployeeID","CASAAchTillDate","CASAAch",yr,mon)
  # Merge all achieved
  t <- merge(gilogIn,ci,by = c("EmployeeID","kpi_Date")) 
  t <- merge(t,liMtngs,by = c("EmployeeID","kpi_Date")) 
  t <- merge(t,lilogIn,by = c("EmployeeID","kpi_Date")) 
  t <- merge(t,revPoints,by = c("EmployeeID","kpi_Date")) 
  t <- merge(t,casa,by = c("EmployeeID","kpi_Date")) 
  t$kpi_Date <- as.Date(t$kpi_Date,"%Y-%b-%d")

  return(t)
}

funcGetScoreRank <- function(dir,fl,yr,rankMon,emplCnt) {
  lb <- read_excel(paste(dir,fl,sep="/"),sheet=rankMon,skip=6)
  # Get id, score and rank
  lb <- lb[1:emplCnt,c(3,11,12)]
  #lb$Month <- mon
  names(lb)[1] <- "EmployeeID"
  nmStr <- make.names(names(lb), unique = TRUE, allow_ = TRUE)
  nmStr <- gsub("\\.", "", nmStr)
  names(lb) <- nmStr
  return(lb)
}

### 
# Read ga
###
funcGetGA <- function(gaFilePath){
  gaf <- read.csv(gaFilePath,skip = 6,stringsAsFactors = FALSE)
  gaf$gaf_Date <- paste(substring(gaf$Date,1,4),substring(gaf$Date,5,6),substring(gaf$Date,7,8),sep = "-")
  gaf$gaf_Date <- as.Date(gaf$gaf_Date)
  # For Cryptic Hour
  gaf$Hour <- substring(gaf$Hour.of.Day,9,10)
  #gaf <- filter(gaf,gaf_Date >= "2017-05-01" & gaf_Date <= "2017-05-31") 
  gaf <- gaf %>% group_by(userId, gaf_Date, Hour) %>%
    summarise(UniquePageviews = sum(Unique.Pageviews),
              Pageviews = sum(Pageviews))#,
  #gaf_TimeonPage = sum(substr(Time.on.Page,4,5)))
  
  names(gaf)[1] <- "gaf_emp_code"
  t <- data.frame(gaf$gaf_emp_code,gaf$gaf_Date)
  names(t) <- c("emp_code","Date")
  t$Key <- paste(t$emp_code,t$Date,sep = "-")
  t1 <- t[!duplicated(t$Key),]
  dim(t1)
  t1$Key <- NULL
  t1 <- t1[rep(1:nrow(t1),1,each=24),]
  t1$hr <- c(0:23)
  t1$Key <- paste(t1$emp_code,t1$Date,t1$hr,sep="-")
  gaf$Key <- paste(gaf$gaf_emp_code,gaf$gaf_Date,gaf$Hour,sep="-")
  gaf <- merge(t1,gaf,by = "Key",all.x = TRUE,all.y = FALSE)
  gaf$Key <- NULL
  gaf$Hour <- NULL
  gaf$gaf_Date <- NULL
  gaf$gaf_emp_code <- NULL
  gaf[is.na(gaf)] <- 0
  names(gaf)[1] <- "emp_code"
  names(gaf)[2] <- "gaf_Date"
  names(gaf)[3] <- "Hour"
  rm(t1)
  rm(t)
  #t <- filter(gaf,gaf_Date >= "2017-05-01" & gaf_Date <= "2017-05-31") 
  return(gaf)
}

###
# for column wise
###

funColWiseData <- function(gaFilePath,d) {
  t <- dcast(d,emp_code + gaf_Date ~ Hour, value.var="UniquePageviews", fun=sum)
  t1 <- dcast(d,emp_code + gaf_Date ~ Hour, value.var="Pageviews", fun=sum)
  # make names ok
  colnames(t) <- paste("UniquePage_viewhour", names(t), sep = "_")
  names(t)[1] <- "emp_code"
  names(t)[2] <- "gaf_Date"
  colnames(t1) <- paste("TotalPage_viewhour", names(t1), sep = "_")
  names(t1)[1] <- "emp_code"
  names(t1)[2] <- "gaf_Date"
  t12 <- merge(t,t1,Key=c("emp_code","gaf_Date"))
  
  
  t <- read.csv(gaFilePath,skip = 6,stringsAsFactors = FALSE)
  t$gaf_Date <- paste(substring(t$Date,1,4),substring(t$Date,5,6),substring(t$Date,7,8),sep = "-")
  t$gaf_Date <- as.Date(t$gaf_Date)
  t1 <- t %>% group_by(userId, gaf_Date) %>%
    summarise(UniquePageviews = sum(Unique.Pageviews),
              Pageviews = sum(Pageviews))#,
              #gaf_TimeonPage = sum(substr(Time.on.Page,4,5)))
  names(t1)[1] <- "emp_code"
  t12 <- merge(t1,t12,by = c("emp_code","gaf_Date"))
  rm(list = c("t","t1"))
  return(t12)
}
