## Script to process US VAERS system
## SEE: https://vaers.hhs.gov/data.html
## AND: https://vaers.hhs.gov/data/datasets.html
library(dplyr)

## Clear the system variables
rm(list=ls()) # clears environment variables

## Set the working directory so we can load our functions
wandir <- paste(path.expand("~"), "/Dropbox/R", sep = "")
wandir <- sub("/Documents", "", wandir)
curdir <- getwd()
if(curdir!=wandir){
  setwd(wandir)
}

## ---------------------------------
## Wrangle VAERS Age Groups so that they come out in order in ggplot
wrangleVaersAgeGroup <- function(df)
{
  vaers <- df
  vaers$AgeGroup <- as.integer(vaers$AGE_YRS / 10)
  
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "0", "A (0-9)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "1", "B (10-19)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "2", "C (20-29)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "3", "D (30-39)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "4", "E (40-49)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "5", "F (50-59)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "6", "G (60-69)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "7", "H (70-79)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "8", "I (80-89)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "9", "J (90-99)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "10","K (100-109)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(vaers$AgeGroup == "11","L (110-119)", vaers$AgeGroup)
  vaers$AgeGroup <- ifelse(is.na(vaers$AgeGroup),"U (Unknown)", vaers$AgeGroup)
  
  return(vaers)
}

## ---------------------------------
## Wrangle VAERS Age Groups so that they come out in order in ggplot
wrangleVaersAges <- function(df)
{
  vaers <- df
  vaers$Age <- ifelse(vaers$Age == "< 6 months",  "A (< 6 months)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "6-11 months", "B (6-11 months)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "1-2 years",   "C (1-2 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "3-5 years",   "D (3-5 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "6-17 years",  "E (6-17 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "18-29 years", "F (18-29 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "30-39 years", "G (30-39 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "40-49 years", "H (40-49 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "50-59 years", "I (50-59 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "60-64 years", "J (60-64 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "65-79 years", "K (65-79 years)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "80+ years",   "L (80+)", vaers$Age)
  vaers$Age <- ifelse(vaers$Age == "Unknown",     "U (Unknown)", vaers$Age)
  
  return(vaers)
}

  
#########################################################################
## PROGRAM ENTRY 
#########################################################################
# Read in the CSV
vaers.data <- read.csv("../VAERS/domestic/2021VAERSDATA.csv") 
vaers.symptoms <- read.csv("../VAERS/domestic/2021VAERSSYMPTOMS.csv") 
vaers.vax <- read.csv("../VAERS/domestic/2021VAERSVAX.csv") 

vaers.vax.grp <- vaers.vax %>% group_by(VAX_TYPE) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) # 
head(vaers.vax.grp)

nrow(vaers.data)
nrow(vaers.symptoms)
nrow(vaers.vax)


## ----------------------------------------------------------
## ---------------- CLEAN 
## ----------------------------------------------------------
# CLEANING - Filter by date as some VAX dates are 1970
vaers.data$VAX_DATE_R <- as.Date(vaers.data$VAX_DATE, "%m/%d/%Y")
vaers.data <- filter(vaers.data,VAX_DATE_R>"2020-12-31")

# CLEANING - Add rownum by VAERS_ID to identify duplicates
vaers.data.df <- vaers.data %>% group_by(VAERS_ID) %>% mutate(data_rowNum = row_number()) # View(vaers.data.df)
vaers.symptoms.df <- vaers.symptoms %>% group_by(VAERS_ID) %>% mutate(sym_rowNum = row_number()) # View(vaers.symptoms.df)
vaers.vax.df <- vaers.vax %>% group_by(VAERS_ID) %>% mutate(vax_rowNum = row_number()) # View(vaers.vax.df)
vaers.vax.df <- filter(vaers.vax.df, vax_rowNum==1)
vaers.vax.df <- filter(vaers.vax.df, VAX_TYPE=="COVID19")

# Add an Age Group
vaers.data.df$AGE_YRS <- as.integer(vaers.data.df$AGE_YRS)
vaers.data.df <- wrangleVaersAgeGroup(vaers.data.df)

nrow(vaers.data.df)       # View(vaers.data.df)
nrow(vaers.symptoms.df)   # View(vaers.symptoms.df)
nrow(vaers.vax.df)        # View(vaers.vax.df)


## ----------------------------------------------------------
## ---------------- WRANGLE
## ----------------------------------------------------------
# WRANGLE - Join the data into one data frame
vaers.corona.df <- left_join(vaers.data.df, vaers.vax.df, by = "VAERS_ID")
vaers.corona.df <- left_join(vaers.corona.df, vaers.symptoms.df, by = "VAERS_ID")
nrow(vaers.corona.df)        # View(vaers.corona.df)

# Filter out UNKNOWN (1) gender; (2) age; (3) manufacturer
vaers.corona.df <- filter(vaers.corona.df, sym_rowNum==1 & (SEX=="F" | SEX=="M") & (VAX_MANU!="UNKNOWN MANUFACTURER") & (AgeGroup!="U (Unknown)"))
nrow(vaers.corona.df)        # View(vaers.corona.df)

#Concatenate symptoms
vaers.corona.df$SYMPTOMS <- paste(vaers.corona.df$SYMPTOM1, vaers.corona.df$SYMPTOM2, vaers.corona.df$SYMPTOM3, vaers.corona.df$SYMPTOM4, vaers.corona.df$SYMPTOM5, sep="~")

# Arrange by VAERS ID and choose the top 500,000 records
vaers.corona.df <- vaers.corona.df %>% 
  arrange(desc(VAERS_ID))

# Wrangle the Lot Numbers to Upper Case
vaers.corona.df$VAX_LOT <- toupper(vaers.corona.df$VAX_LOT)

###############################################################
# Write out the full data
#write.csv(vaers.corona.df, file = "C:/Users/User/cave-full-12-31-2021-dev.csv", row.names = FALSE, fileEncoding ="UTF-8")
###############################################################


# 500K MAX
vaers.corona.sub.df <- head(vaers.corona.df, 500000)

nrow(vaers.data.df)       
nrow(vaers.corona.df)       
nrow(vaers.corona.sub.df) #View(vaers.corona.sub.df)


## ----------------------------------------------------------
# OUTPUT SUB (DEATH) 
sub.died <- filter(vaers.corona.sub.df, DIED=='Y') %>% 
  select("VAERS_ID",
         "VAX_LOT",
         "VAX_MANU",
         "AGE_YRS",
         "SEX",
         "DIED",
         "DATEDIED",
         "AgeGroup",
         "VAX_DATE",
         "VAX_DOSE_SERIES",
         "SYMPTOM_TEXT") 
nrow(sub.died)       

###############################################################
# Write out the died data
#write.csv(sub.died, file = "C:/Users/User/cave-died-12-31-2021-dev.csv", row.names = FALSE, fileEncoding ="UTF-8")
###############################################################


## ----------------------------------------------------------
# OUTPUT SUB (FULL)
vaers.corona.sub.df <- vaers.corona.sub.df %>% 
  select("VAERS_ID",
         "AGE_YRS",
         "SEX",
         "DIED",
         "OTHER_MEDS",
         "HISTORY",
         "ALLERGIES",
         "AgeGroup",
         "VAX_DATE",
         "VAX_MANU",
         "SYMPTOMS") 

###############################################################
# Write out the subset data
#write.csv(vaers.corona.sub.df, file = "C:/Users/User/cave-sub-12-31-2021-dev.csv", row.names = FALSE, fileEncoding ="UTF-8")
###############################################################

## ----------------------------------------------------------
# OUTPUT GRP 
# Arrange by VAERS ID and choose the top 500,000 records
vaers.corona.grp.df <- vaers.corona.df %>% 
  arrange(desc(VAERS_ID))
vaers.corona.grp.df <- head(vaers.corona.grp.df, 500000)

vaers.corona.grp.df <- vaers.corona.grp.df %>%
  group_by(AgeGroup, SEX, VAX_MANU, DIED, L_THREAT, ER_ED_VISIT, HOSPITAL) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) # 

sum(vaers.corona.grp.df$n) #View(vaers.corona.grp.df)

###############################################################
# Write out the grouped data
#write.csv(vaers.corona.grp.df, file = "C:/Users/User/cave-grp-12-31-2021-dev.csv", row.names = FALSE, fileEncoding ="UTF-8")
###############################################################

## ----------------------------------------------------------


#----------- HACKING
