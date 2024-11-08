loc_to_numeric <- function(x) {
  # assertions on arguments
  assert_that(is.character(x) || is.numeric(x))
  x <- toupper(gsub('[[:space:]]', '', x))
  x <- ifelse(valid_loc(x), x, NA)
  for (i in 1:26) {
    x <- sub(LETTERS[i], i, x, fixed = TRUE)
  }
  round(as.numeric(x), 1)
}

valid_loc <- function(x, alpha = TRUE, reflo = FALSE) {
  if (is.numeric(x)) {
    # if a numeric vector convert to character to check
    x <- as.character(x)
  } else if (!is.character(x)) {
    # if neither numeric nor character, return FALSE
    return(rep(FALSE, length(x)))
  }
  # define regular expression
  if (alpha) {
    if (reflo) {
      regex <- "^([A-Z]|(-?[0-9]{1,2}))([.][05])?$"
    } else {
      regex <- "^([A-Z]|(-?[0-9]{1,2}))([.][0-9])?$"
    }
  } else {
    if (reflo) {
      regex <- "^-?[0-9]{1,2}([.][05])?$"
    } else {
      regex <- "^-?[0-9]{1,2}([.][0-9])?$"
    }
  }
  # perform check
  grepl(regex, x, ignore.case = TRUE)
}
library(assertthat)
library(krsp)
library(tidyverse)
library(krsp)
con<-krsp_connect(dbname="krsp", host="krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com", port=3306, user="estudd", password="krsp")
krsp_tables(con)
krsp_censusmap(con, "JO", 2017, "august")
behav=tbl(con, "dbabehaviour", behaviour=1) %>% collect()
squirrel=tbl(con, "trapping") %>% collect()
census=tbl(con, "census") %>% collect()
census2=tbl(con, "dbamidden") %>% collect()
census=filter(census, gr=="JO"|gr=="KL"|gr=="SU"|gr=="AG"|gr=="LL")
census$locx=loc_to_numeric(census$locx)
census<-census %>% separate(census_date, c("census_year", "census_month", "census_day"), "-")
census<-census %>% mutate(census_year=as.numeric(census_year), census_month=as.numeric(census_month), locy=as.numeric(locy))
#convert september census to august
census$census_month=ifelse(census$census_month==9, 8, census$census_month)
#### calculating local density for each squirrel ----

setwd("~/Dropbox/phd/Analysis/SquirrelEnabling")
key<-read.csv("SQRaxy_key_id.csv")
key$locx=loc_to_numeric(as.character(key$locx))
#from erin
distance <- 130
key2<-filter(key, is.na(locx)==FALSE)
key2$density=99
n <- length(key2$row)
for (i in 1:n) {
  print(i)
  neighbours <- subset(census, census$gr==key2$Grid[i] & census$census_year==key2$census_year[i] & census$census_month == key2$census_month[i] & (30*key2$locx[i]-30*census$locx)^2+(30*key2$locy[i]-30*census$locy)^2<=(distance)^2) #This selects neighbours in the same grid, year, month and within 130 m of your focal individual.
  neighbours$Focal_ID <- key2$Squirrel_ID[i] #this creates a column in your new 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for.
  neighbours <- subset(neighbours, !neighbours$Focal_ID==neighbours$squirrel_id) #this makes sure your focal squirrel is not included in your neighbours.
  
  #Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
  num.indiv <-  length(neighbours$gr)
  key2$density[i] <- num.indiv/((pi*distance^2)/10000)
  
}


write.csv(key2, "SQRaxy_key_mid_den.csv")

### adding squirrel ids to trapping data----
setwd("~/Dropbox/phd/Data/2017 Data")
mytrap<-read.table("trapping.csv", sep=",", header=T)
setwd("~/Downloads")
squirrel<-read.table("squirrel.csv", sep=",", header=T)
s1<-squirrel %>% select(id, taglft, tagrt)
trap<-left_join(mytrap, s1, by=c("tagLft"="taglft", "tagRt"="tagrt"))
write.csv(trap, "trapping_id.csv")



### calculating mortality rate of territory owners for each census period----

#first run the code at top to get a census file

#calculate a month/year variabe for census
census$census_month_year=as.numeric(paste(census$census_year, census$census_month, sep="."))

# separate census around 2016 as different fates were used. Select only those squirrels that owned a territory
census1=filter(census, census_year<=2016, census_month_year!=2016.8, sq_fate==1|sq_fate==2|sq_fate==10|sq_fate==13) %>% group_by(squirrel_id) %>% summarise(num=n())
census2=filter(census, census_month_year>=2016.8, sq_fate==1|sq_fate==2|sq_fate==15|sq_fate==16|sq_fate==18) %>% group_by(squirrel_id) %>% summarise(num=n())

#create a list of all sqrs that owned a territory for a least one census period
ter_own=full_join(census1, census2, by="squirrel_id") %>% select(squirrel_id)

# calculate the total number of sqrs that owned territories each census
t_sqrs1<-filter(census, census_year<=2016, census_month_year!=2016.8, sq_fate==1|sq_fate==2|sq_fate==10|sq_fate==13) %>% group_by(gr, census_month_year) %>% summarise(totalsqrs=n())
t_sqrs2<-filter(census, census_month_year>=2016.8, sq_fate==1|sq_fate==2|sq_fate==15|sq_fate==16|sq_fate==18) %>% group_by(gr, census_month_year) %>% summarise(totalsqrs=n())
t_sqrs=rbind(t_sqrs1, t_sqrs2)

# Determine when each squirrel was last seen in a census regardless of whether or not it owned a territory
census1<-census %>% group_by(squirrel_id, gr, sex) %>% summarise(num=n(), lastcensus=max(census_month_year))

# Combine this with the list of squirrels that owned to territories at some point to determine when they died.
ter_own=left_join(ter_own, census1)

#Calculate the number of sqrs that disappeared each census
mortalities=ter_own %>% group_by(gr, lastcensus) %>% summarise(morts=n())

# Combine this with the total number of territory owners each census and calcualte a mortality rate. 
totalsqrs<-left_join(t_sqrs, mortalities, by=c("gr", "census_month_year"="lastcensus")) 
totalsqrs=na.omit(totalsqrs)
totalsqrs$mort_rate=(totalsqrs$morts/totalsqrs$totalsqrs)*100

#save this file
write.csv(totalsqrs, "mort_rate.csv")


morts=read.csv("mort_rate.csv")
morts$census=substr(morts$census_month_year, 6,6)
morts$rate_month=ifelse(morts$census==5, morts$mort_rate/9,morts$mort_rate/3)
#plot
ggplot(morts, aes(census_month_year, rate_month, color=gr))+geom_line() +ylim(0,10)
ggplot(totalsqrs, aes(census_month_year, totalsqrs, color=gr))+geom_line()

### calculating mortality rate of territory owners for each census period.  Take 2 - using full census and flastall. ----
census=tbl(con, "census") %>% collect()
census2=tbl(con, "dbamidden") %>% collect()
census=select(census, census_date, gr, sq_fate, squirrel_id)
census2=select(census2, census_date=date, gr=grid, sq_fate=def, squirrel_id)
census2=filter(census2, sq_fate==4)

## converting date into separate columns
census<-census %>% separate(census_date, c("census_year", "census_month", "census_day"), "-")
census<-census %>% mutate(census_year=as.numeric(census_year), census_month=as.numeric(census_month))
census2<-census2 %>% separate(census_date, c("census_year", "census_month", "census_day"), "-")
census2<-census2 %>% mutate(census_year=as.numeric(census_year), census_month=as.numeric(census_month))
#convert september census to august
census$census_month=ifelse(census$census_month==9, 8, census$census_month)
# separate census around 2016 as different fates were used. Select only those squirrels that owned a territory
census$census_month_year=as.numeric(paste(census$census_year, census$census_month, sep="."))
census2$census_month_year=as.numeric(paste(census2$census_year, census2$census_month, sep="."))
censusx=filter(census, census_year<=2016, census_month_year!=2016.8, sq_fate==1|sq_fate==2|sq_fate==10|sq_fate==13) 
censusy=filter(census, census_month_year>=2016.8, sq_fate==1|sq_fate==2|sq_fate==15|sq_fate==16|sq_fate==18) 
census=rbind(censusx, censusy)

census_all=rbind(census, census2)

### get a list of all squirrels that had territories
squirrel_resident=census_all %>% group_by(squirrel_id, gr) %>% summarise(num=n())

## get date last seen
flastall=tbl(con, "flastall") %>% collect()
flastall=select(flastall, squirrel_id, datee)
squirrel_resident=left_join(squirrel_resident, flastall)
squirrel_resident<-squirrel_resident %>% separate(datee, c("yearee", "monthee", "dayee"), "-")
squirrel_resident$monthee<-as.numeric(squirrel_resident$monthee)
squirrel_resident$last_census<-ifelse(squirrel_resident$monthee>=5 &squirrel_resident$monthee<8, paste(squirrel_resident$yearee, 5, sep="."), paste(squirrel_resident$yearee, 8, sep="."))
sqr_exit=squirrel_resident %>% group_by(last_census, gr) %>% summarise(morts=n())
sqr_exit$last_census=as.numeric(sqr_exit$last_census)

### calculate # of squirrels per grid and census
census_all=filter(census_all, census_month==5 | census_month==8, census_day==15)
tot_sqrs=census_all %>% group_by(census_month_year, gr) %>%  summarise(tot_sqrs=n())

morts=left_join(tot_sqrs, sqr_exit, by=c("census_month_year"="last_census", "gr"))
morts=filter(morts,gr=="KL"|gr=="SU"|gr=="AG")
morts$mort_rate=morts$morts/morts$tot_sqrs*100
morts$census=substr(morts$census_month_year, 6,6)
morts$rate_month=ifelse(morts$census==5, morts$mort_rate/3,morts$mort_rate/9)

write.csv(morts, "mort_rate_sqr.csv")
#plot
ggplot(data=subset(morts, census==8), aes(census_month_year, rate_month, color=gr))+geom_line() +geom_point() +ylim(0,5)+theme_bw()+ scale_x_continuous(breaks = c(1992,1996, 2000,2004, 2008,2012, 2016))
ggplot(data=subset(morts, census==5), aes(census_month_year, rate_month, color=gr))+geom_line()+geom_point()+ylim(0,20)+theme_bw()+ scale_x_continuous(breaks = c(1992,1996, 2000,2004, 2008,2012, 2016))
ggplot(data=morts, aes(census_month_year, rate_month, color=census))+stat_summary()+ylim(0,20)+theme_bw()+ scale_x_continuous(breaks = c(1992,1996, 2000,2004, 2008,2012, 2016))

##


##### local density from ERS on slack -----
distance <- 130 #establish your radius for collecting neighbourhood data
neighbours.all <- data.frame() #create an empty data frame to store the iterations of your loop in

n <- length(data$Squirrel_ID) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute Squirrel_ID for any column. This is the data the code below will loop through.

for (i in 1:n) {
  print(i)
  neighbours <- subset(census, census$grid==data$grid[i] & census$year==data$year[i] & census$month == data$month[i] & (30*data$locx[i]-30*census$locx)^2+(30*data$locy[i]-30*census$locy)^2<=(distance)^2) #This selects neighbours in the same grid, year, month and within 130 m of your focal individual.
  neighbours$Focal_ID <- data$Squirrel_ID[i] #this creates a column in your new 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for.
  neighbours <- subset(neighbours, !neighbours$Focal_ID==neighbours$Squirrel_ID) #this makes sure your focal squirrel is not included in your neighbours.
  
  #Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
  num.indiv <-  length(neighbours$Nbor.ID)
  neighbours$density <- num.indiv/((pi*distance^2)/10000)
  
  #Put it all together
  neighbours.all <- rbind(neighbours.all,neighbours)
  
}

#### Determining the dates of the breeding season for each grid and year----
library(lubridate)
library(zoo)
litter<-tbl(con, "litter") %>% collect()
litter$fieldBDate<-as.Date(litter$fieldBDate)
breeding<-litter %>% group_by(yr, grid) %>% summarise(early=summary(fieldBDate)[2], late=summary(fieldBDate)[5])
breeding$late1=as.numeric(round(breeding$late, 0))
breeding$early1=as.numeric(round(breeding$early, 0))
breeding=breeding %>% mutate(late=as.Date(late1), early=as.Date(early1))
breeding=breeding %>% mutate(Start=early-35, End=late-35)
breeding=select(breeding, yr, grid, Start, End, early, late)
write.csv(breeding, "breedingSeason.csv")


test=filter(litter, yr==2016, grid=="KL")
breeding$duration=breeding$End-breeding$Start



###

## getting cone count data for each year -----
cones<-tbl(con, "cones") %>% collect()
cones[cones==""]  <- NA
cones$NumNew<-as.numeric(cones$NumNew)
cones_grid=cones %>% group_by(Year) %>% summarise(cones_tree=mean(NumNew, na.rm=TRUE))
cones_grid$Year<-as.numeric(cones_grid$Year)
key<-left_join(key, cones_grid)
cones_grid=cones %>% group_by(Year) %>% summarise(cones_tree_x1=mean(NumNew, na.rm=TRUE))
cones_grid$Year<-as.numeric(cones_grid$Year)+1
key<-left_join(key, cones_grid)
write.csv(key, "SQRaxy_key_mid_den.csv")

