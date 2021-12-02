## Prep dataset
#Cleaning
# * Remove "Statewide" summary rows
# * Remove Territories (but leave DC)
# * X Make County name consistent
# * X Split COUNTY into County and State
# * X Make LFO (Name of industry) consistent
# * coerce factor variables to be factors (NOT DONE YET)

# Add a date type field for the year. Maybe 3/15/YEAR

# Deal with inflation (Use Consumer Price Index)
# * merge in inflation factors and convert to 2019 dollars
# * Annual Payroll adjusted for inflation
# * Quarter 1 payroll adjusted for inflation

# Add Average wage rates 
# * Average wage rate for all industries by county
# * Average wage rate for a given industry by county
# * Average wage rate for all industries by state
# * Average wage rate for a given industry by state


library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)




# Create a not in operator:
`%notin%` <- Negate(`%in%`)

# Read in Census Data
currPath <- paste(dirname(rstudioapi::getSourceEditorContext()$path),"/FinalProjectDataset.csv",sep="")
df <- read.csv(currPath)

# Remove summary "Statewide" rows
df<-df %>% filter(substr(df$COUNTYSTATE,1,9)!="Statewide")
# Removes 4,683 rows that are statewide aggregate numbers


## Remove Territories
# * 60 = American Samoa
# * 66 = Guam
# * 69 = Commonwealth of the Northern Mariana Islands
# * 72 = Puerto Rico
# * 78 = United States Virgin Islands

## But Keep Washington DC
# * 11 = District of Columbia

excludeList <- list(60,66,69,72,78) # If we want to get rid of DC add 11 to this list
df<-df %>% filter(!STATENUM %in% excludeList)
# Removes 21,604 rows that are records on the territories

## Make County Name consistent

# add a field to df that concatenates state and county nums (STCTYNUM)
#df<-mutate(df,STCTYNUM=paste(str_pad(df$STATENUM,2,pad="0"),str_pad(df$COUNTYNUM,3,pad="0"),sep="_"))
df<-mutate(df,STCTYNUM=paste(str_pad(df$STATENUM,2,pad="0"),str_pad(df$COUNTYNUM,3,pad="0"),sep=""))

# create df of 2019 county names, state names, and STCTYNUM
countiesNames <- df %>% filter(YEAR == 2019) %>% 
  group_by(COUNTYSTATE) %>%
  summarise(
    COUNTY19 = unique(COUNTYSTATE),
    COUNTYNUM = unique(COUNTYNUM),
    STATENUM = unique(STATENUM)
  ) %>% 
  #mutate(STCTYNUM=paste(str_pad(STATENUM,2,pad="0"),str_pad(COUNTYNUM,3,pad="0"),sep="_")) %>% # Add concat of state and county nums (with underscore)
  mutate(STCTYNUM=paste(str_pad(STATENUM,2,pad="0"),str_pad(COUNTYNUM,3,pad="0"),sep="")) %>% # Add concat of state and county nums
  separate(COUNTY19,c("COUNTY","STATE"),sep=", ") %>% # split county and state names
  select(COUNTY,STATE,STCTYNUM) %>%
  arrange(STCTYNUM)

# Add to countiesNames the 14 that are missing from 2019
Missing2019 <- data.frame(COUNTY = c("Prince of Wales Outer Ketchikan Census Area",
                                     "Skagway-Hoonah-Angoon Census Area",
                                     "Kusilvak Census Area", 
                                     "Wrangell-Petersburg Census Area",
                                     "Kalawao County", "Petroleum County",
                                     "Banner County", "McPherson County",
                                     "Esmeralda County", "Oglala Lakota County",
                                     "Borden County", "King County", 
                                     "Loving County", "Bedford City"),
                          STATE = c("Alaska", "Alaska", "Alaska", "Alaska", 
                                    "Hawaii", "Montana", "Nebraska", "Nebraska",
                                    "Nevada", "South Dakota", "Texas", "Texas",
                                    "Texas", "Virginia"),
                          #                          STCTYNUM = c("02_201", "02_232", "02_270", "02_280",
                          #                                       "15_005", "30_069", "31_007", "31_117",
                          #                                       "32_009", "46_113", "48_033", "48_269", 
                          #                                       "48_301","51_515"))
                          STCTYNUM = c("02201", "02232", "02270", "02280",
                                       "15005", "30069", "31007", "31117",
                                       "32009", "46113", "48033", "48269", 
                                       "48301","51515"))

countiesNames <- rbind(countiesNames, Missing2019)

# Add COUNTY and STATE to df
#df<-merge(df,counties2019, by="STCTYNUM", all=TRUE) # use this one to debug the missing records
df <- merge(df, countiesNames, by="STCTYNUM")

## Make LFO consistent
LFO2019 <- df %>% filter(YEAR == 2019) %>% 
  group_by(NAIC) %>%
  summarise(
    INDUSTRY = unique(LFO),
  ) %>% 
  arrange(INDUSTRY)

# Need to trim the whitespace off NAIC.
df$NAIC <- str_trim(df$NAIC, side="both")

#df<-merge(df, LFO2019, by="NAIC")
df<-merge(df, LFO2019, by="NAIC",all=TRUE)

## Add a date type field for the year. Maybe 3/15/YEAR

df$DATE <- mdy(paste("03", "15", df$YEAR, sep="/"))

##Deal with inflation (Use Consumer Price Index)
# * merge in inflation factors and convert to 2019 dollars
# * Annual Payroll adjusted for inflation
# * Quarter 1 payroll adjusted for inflation

# Add inflation multiplier to convert to 2019 dollars to df
currPath <- paste(dirname(rstudioapi::getSourceEditorContext()$path),"/CPI2019.csv",sep="")
CPI <- read.csv(currPath)
CPI<-CPI[,c("Year","CPI_2019")] # removed Annual column before merge
df<-merge(df, CPI, by.x="YEAR", by.y="Year")

# Add fields for Annual Payroll and Quarterly Payroll in 2019 dollars
df$PAYANN_19DOL = df$PAYANN * df$CPI_2019
df$PAYQTR1_19DOL = df$PAYQTR1 * df$CPI_2019

## Average wage rates by year
# * Average wage rate for a given industry by county
# * Average wage rate by county, regardless of industry
# * Average wage rate for each state, regardless of industry
# * Average wage rate for an industry by state

# Round these new fields to 2 decimal places

# Average wage rate for counties, regardless of industry
df<-df %>% group_by(STCTYNUM) %>%
  mutate(CTYPAYRATE = round(sum(PAYANN)/sum(EMP), digits=2)) %>%  ## not adj for inflation
  mutate(CTYPAYRATE_19DOL = round(sum(PAYANN_19DOL)/sum(EMP), digits=2)) %>% ## adjusted to 2019 dollars
  ungroup()

#Average wage rate by Year-County-Industry, add to df
df$CTYPAYRATE_NAIC = round(df$PAYANN/df$EMP, digits=2)  ## not adj for inflation
df$CTYPAYRATE_NAIC_19DOL = round(df$PAYANN_19DOL/df$EMP, digits=2) ## adjusted to 2019 dollars

# Average wage rate for each state, regardless of industry
df <- df %>% group_by(STATENUM) %>%
  mutate(STATEPAYRATE = round(sum(PAYANN)/sum(EMP), digits=2))  %>%  ## not adj for inflation
  mutate(STATEPAYRATE_19DOL = round(sum(PAYANN_19DOL)/sum(EMP), digits=2)) %>% ## adjusted to 2019 dollars
  ungroup()

# Average wage rate for an industry by state
df<-df %>% group_by(STATENUM,NAIC) %>%
  mutate(STATEPAYRATE_NAIC = round(sum(PAYANN)/sum(EMP), digits=2)) %>% ## not adj for inflation
  mutate(STATEPAYRATE_NAIC_19DOL = round(sum(PAYANN_19DOL)/sum(EMP), digits=2)) %>% ## adjusted to 2019 dollars
  ungroup()

##Coerce to factors
# * NAIC
# * STCTYNUM
# * STATENUM
# * COUNTYNUM
# * COUNTY
# * STATE

df$NAIC <- as.factor(df$NAIC)
# Not factoring these because usmap wants them as characters
#df$STCTYNUM <- as.factor(df$STCTYNUM)
df$STATENUM <- as.factor(df$STATENUM)
df$COUNTYNUM <- as.factor(df$COUNTYNUM)
df$COUNTY <- as.factor(df$COUNTY)
df$STATE <- as.factor(df$STATE)

# for usmap
df$FIPS_ST <-str_sub(df$STCTYNUM,1,2)
df$FIPS_CTY <-as.character(df$STCTYNUM)

# Remove fields no longer needed
df<- df %>% select(-c("COUNTYSTATE","LFO"))
