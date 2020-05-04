# Data on policies from Raifman and collaborators
library(gsheet)
policies  <- data.frame(gsheet2tbl("https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ"))
# remove footnoes
policiesNotes <- policies[-(1:51),]
policies <- policies[1:51,]

datecols <- c("State.of.emergency",
              "Date.closed.K.12.schools",
              "Closed.day.cares",
              "Date.banned.visitors.to.nursing.homes",
              "Stay.at.home..shelter.in.place",
              "Closed.non.essential.businesses",
              "Closed.restaurants.except.take.out",
              "Closed.gyms",
              "Closed.movie.theaters",
              "Stop.enforcement.of.evictions.overall.or.due.to.COVID.related.issues",
              "Stop.Initiation.of.Evictions.overall.or.due.to.COVID.related.issues" ,
              "Order.freezing.utility.shut.offs",
              "Froze.mortgage.payments",
              "Waived.one.week.waiting.period.for.unemployment.insurance",
              "End.relax.stay.at.home.shelter.in.place",
              "Reopen.businesses",
              "Mandate.face.mask.use.by.all.individuals.in.public.spaces",
              "Mandate.face.mask.use.by.employees.in.essential.businesses",
              "Reopen.restaurants",
              "Repened.gyms",
              "Reopened.movie.theaters",
              "Suspended.elective.medical.dental.procedures",
              "Resumed.elective.medical.procedures",
              "Modify.Medicaid.requirements.with.1135.waivers..date.of.CMS.approval.",
              "Renter.grace.period.or.use.of.security.deposit.to.pay.rent"
              )
for(col in datecols) {
  policies[,col]  <-  as.Date(policies[,col], format="%m/%d/%Y")
}
policies$Population.2018 <- as.numeric(gsub(",","", policies$Population.2018))


# Data on state cases, deaths, etc from JHU
jhucoviddata <- function() {
  confirmed = read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true', stringsAsFactors=FALSE)
  deaths = read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv?raw=true', stringsAsFactors=FALSE)
  makelong <- function(df, varname) {
    names(df) <- gsub("\\.","-",names(df))
    names(df) <- gsub("^X","date.",names(df))
    day0 <- as.Date("2020-01-01")
    for (c in grep("date\\..+",names(df))) {
      d <- as.Date(gsub("date\\.","",names(df)[c]), format="%m-%d-%y") - day0
      names(df)[c] <- sprintf("count.%03d",d)
    }
    df <- reshape(df, varying=grep("count\\..+",names(df)), direction="long")
    df$date <- day0 + df$time
    names(df)[names(df)=="count"] = varname
    return(df)
  }
  deaths <- makelong(deaths, "deaths")
  confirmed <- makelong(confirmed, "cases")
  jhu <- merge(deaths, confirmed)
  return(jhu)
}
jhu.all <- jhucoviddata()
# Aggregate to state level
jhu <- aggregate(cases ~ Province_State*date, data=jhu.all, FUN=sum)
jhu <- merge(jhu, aggregate(deaths ~ Province_State*date, data=jhu.all, FUN=sum))
fips <- unique(jhu.all[,c("Province_State","FIPS")])
fips$fips <- fips$FIPS %/% 1000
fips <- fips[fips$fips<=70,]
fips <- fips[!is.na(fips$fips),]
fips <- unique(fips[,c("Province_State","fips")])
jhu <- merge(jhu, fips)
names(jhu)[names(jhu)=="Province_State"]  <- "state"

# Data on state cases, deaths, etc from NYTimes
nyt  <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", stringsAsFactors=FALSE)
nyt$date <- as.Date(nyt$date)


## Check for differences
df <- merge(jhu,nyt, by=c("state","fips","date"), all=TRUE)
# There are some differences in the counts. NYT has no data while states weren't reporting numbers, JHU puts zeros. After numbers appear, they are still occassionally different.
# Deaths differ on 0.25 portion of days with mean absolute difference of 1.9
# Cases differ on 0.59 portion of days wiht mean absolute difference of 36.9
mean(abs(df$deaths.x - df$deaths.y), na.rm=T)
# We will use JHU data for now, but ...

## Covid Tracking project data on tests and hospitalizations
ctp  <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors=FALSE)
# Convert date to  correct format
year <- ctp$date %/% 1e4
month <- (ctp$date %% 1e4)  %/% 100
day  <-  (ctp$date %% 1e4) %% 100
date <- as.Date(sprintf("%d-%d-%d", year, month, day))
ctp$date <- date
names(ctp)[names(ctp)=="death"] <- "deaths.ctp"
names(ctp)[names(ctp)=="recovered"] <- "recovered.ctp"
names(ctp)[names(ctp)=="state"] <- "ST"



## Homebase data on businesses with hourly employees
library(rvest)
loadhomebase <- function(url) {
  page <- read_html(url)
  rows <- html_nodes(page,"tr")
  first <- TRUE
  for (row in rows) {
    datecells <- html_nodes(row,"td.s5")
    if (length(datecells)>0) {
      dates <- html_text(datecells)
      dates <- paste("2020/",dates,sep="")
      dates <- as.Date(dates,"%Y/%m/%d")
    } else {
      cells <- html_text(html_nodes(row,"td"))
      if (length(cells)==0 || all(cells=="")) next
      else if (sum(cells!="")==1) header=cells[cells!=""]
      else {
        i <- which(cells!="")
        region <- cells[i[1]]
        values <- cells[i[-1]]
        if (length(values)!=length(dates)) stop()
        values <- as.numeric(gsub("%","",values))
        ndf <- data.frame(region=region, date=dates, vals=values, stringsAsFactors=FALSE)
        names(ndf)[names(ndf)=="vals"] <- header
        if (first) {
          hb <- ndf
          first=FALSE
        } else hb <- merge(hb, ndf, on=c("region","date"), all=T)
      }
    }
  }
  return(hb)
}

urls <- c("https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pubhtml/sheet?headers=false&gid=1930671010", "https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pubhtml/sheet?headers=false&gid=1102464531")
varname <- c("percentchangehours","percentchangebusinesses")
hb <- data.frame(state=c(),date=c())
for (i in 1:length(urls)) {
  url <- urls[i]
  ndf <- loadhomebase(url)
  ndf <- ndf[,c("region","date","Break-down by state")]
  names(ndf) <- c("state","date",varname[i])
  ndf <- ndf[!is.na(ndf[,varname[i]]),]
  summary(ndf)
  if (i==1) hb <- ndf
  else hb <- merge(hb, ndf, on=c("state","date"),all=T)
}

## Google mobility reports
gmr <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", stringsAsFactors=FALSE)
gmr <- subset(gmr, country_region_code=="US")
gmr$date <- as.Date(gmr$date, "%Y-%m-%d")
gmr <- subset(gmr, sub_region_1 != "" & sub_region_2=="") # get state level data
names(gmr)[names(gmr)=="sub_region_1"] <- "state"
drops <- c("country_region_code","country_region","sub_region_2")
gmr <- gmr[, !(names(gmr) %in% drops)]

## Merge data together
df <- merge(jhu, policies, by.x="state",by.y="State")
df <- merge(df, ctp, by=c("fips","date"), all=TRUE)
df <- merge(df, gmr, by=c("state","date"), all=TRUE)
df <- merge(df, hb, by=c("state","date"), all=TRUE)
names(nyt)[names(nyt)=="cases"]  <- "cases.nyt"
names(nyt)[names(nyt)=="deaths"]  <- "deaths.nyt"
df$fips[df$state=="Puerto Rico"] <- 72
# Make sure state and fips never missing
tmp <- unique(df[!is.na(df$ST),c("fips","state","ST")])
tmp <- merge(aggregate( ST  ~ fips, FUN=function(x) unique(x[!is.na(x)])[1],tmp),
             aggregate( state  ~ fips, FUN=function(x) unique(x[!is.na(x)])[1],tmp), by="fips", all=T)
tmp$state[tmp$ST=="AS"]="America Somoa"
tmp$state[tmp$ST=="GU"]="Guam"
tmp$state[tmp$ST=="MP"]="North Marianas"
tmp$state[tmp$ST=="PR"]="Puerto Rico"
tmp$state[tmp$ST=="VI"]="Virgin Islands"
covidstates <- merge(df[,!(names(df) %in% c("state","ST"))], tmp, by="fips", all.x=TRUE)
covidstates <- merge(covidstates, nyt, by=c("date","fips","state"))

write.csv(covidstates, "../data/covidstates.csv", row.names=FALSE)
