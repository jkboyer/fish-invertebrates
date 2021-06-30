#Loads fish data for grand canyon and lees ferry from:
#  AGFD Lees Ferry (SAMPLE_TYPE 96, 97) and Grand Canyon Monitoring (99, 115)
#  USFWS mainstem sampling (aggregation trip) ()
#  Grand Canyon Youth 2019 (121)
#  ALL the recapture data in the big boy (subset to fish that stayed within 5 miles)
#Standardizes columns included and format between trips
#Output:  "fish_condition.csv" all fish with condition or growth data
#                           different projects have different selection/sampling
#                           but consistent fish processing protocols
#         "fish_growth.csv"
#         "fish_biomass.csv" AGFD monitoring data (non-supplemental) only
#                            random site selection/standard sampling
#                            can use for CPUE based measurements
#         ALL output saved in project data folder
#Notes: MUST USE 32-bit R for RODBC database connection to work

#TODO
# 1. add sample reach to s - may be easier to display reach aggregated biomass,
#    rather than CPUE for each net/site


require(RODBC) # database interface
require(tidyverse)
theme_set(theme_minimal()) #no ugly default ggplot theme!

# load data #####
# AGFD monitoring data - site and specimen data
LF.filepath <- "\\\\flag-server/Office/Lees Ferry/lees_ferry_analysis_R_project/lees_ferry_analysis_2018/data/clean/"
GC.filepath <- "\\\\flag-server/Office/Grand Canyon Downstream/R_code_annual_report_analysis/GC_annual_report_analysis_2019/data/clean/"

f.gc <- read.csv(paste0(GC.filepath, "fish.csv"), stringsAsFactors = FALSE)
s.gc <- read.csv(paste0(GC.filepath, "site.csv"), stringsAsFactors = FALSE)
f.lf <- read.csv(paste0(LF.filepath, "fish.csv"), stringsAsFactors = FALSE)
s.lf <- read.csv(paste0(LF.filepath, "site.csv"), stringsAsFactors = FALSE)
s.lf$GEAR_CODE <- "EL"

#Cooperator data - Use only when fish (not site) is unit of measurement
#cannot use this sample data for biomass, non-random reach/site selection methods

#grand canyon youth trip - fish weights measured in July 2019
f.gcy <- read.csv("./data/raw/PIS20190711.csv", stringsAsFactors = FALSE)
f.gcy$SAMPLE_TYPE <- 121

#USFWS aggregation trip

# define database file name as most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20191028_1636.mdb"

# specify file location of GCMRC database
gcmrc.file.path <- "\\\\flag-server/Office/GCMRC_master_database/"

# define database query
# define columns wanted from database
specimen.columns <- paste(
  "RIVER_CODE, FISH_T_SAMPLE.ACCESS_SAMPLE_ID, DATASHEET_SAMPLE_ID, FISH_T_SAMPLE.TRIP_ID,",
  "SAMPLE_TYPE, FISH_T_SAMPLE.START_DATETIME, END_DATETIME, START_RM, END_RM,",
  "SIDE_CODE,  CLIPBOARD, GEAR_CODE, ACCESS_FISH_ID,",
  "FISH_T_SAMPLE.SAMPLE_ID, SPECIES_CODE, TOTAL_LENGTH, FORK_LENGTH, WEIGHT, SEX_CODE,",
  "SEX_COND_CODE, SEX_CHAR_CODE, PITTAG_RECAP, PITTAG, PITTAG2_RECAP,",
  "PITTAG2, DISPOSITION_CODE, SPECIMEN_NOTES")

# Load Data from GCMRC Database
#connect to database
db <- odbcConnectAccess(paste(gcmrc.file.path, db.GCMRC, sep = ""))

# query desired data from specimen table
f.fws <- sqlQuery(db, paste("SELECT", specimen.columns,
                        "FROM", "SAMPLE_SPECIMEN_ALL",
                        "WHERE",
                        "SAMPLE_TYPE = 128 AND RIVER_CODE = 'COR'"),
                  stringsAsFactors = FALSE)

odbcClose(db) #close database connection

#Recapture data for growth
#I already formatted and cleaned up this data in our annual report project
#data has already been subset to moved less than 5 km and at large >=90days
f.recap <- read_csv("\\\\flag-server/Office/Grand Canyon Downstream/R_code_annual_report_analysis/GC_annual_report_analysis_2019/data/clean/recaptures.csv")

#supporting data
#weight-length coefficients to calculate predicted weights and condition
wt.equations <- read.csv(paste0(GC.filepath, "length_wt_coefficients.csv"),
                         stringsAsFactors = FALSE)

reaches <- read.csv(paste0(GC.filepath, "sample_reaches.csv"),
                   stringsAsFactors = FALSE)


# format/standardize columns from different trips and merge trips #####
#agfd data - for biomass ####
s <- bind_rows(s.gc, s.lf) %>%
  filter(GEAR_CODE %in% c("EL", "MHB") &
         START_RM <= 281.7)
rm(s.gc, s.lf)

f.agfd <- bind_rows(f.lf, f.gc)
rm(f.lf, f.gc)

#all trips: for condition

#select and order only needed columns
f.gcy <- f.gcy %>%
  transmute(START_DATETIME = as.POSIXct(S_START_DATE_TIME), #sample data I'm using
            START_RM = S_RIVER_MILE,
            SPECIES_CODE = SPECIES,             #specimen data I'm using
            TOTAL_LENGTH = TOTAL_LENGTH,
            FORK_LENGTH = FORK_LENGTH,
            WEIGHT = WEIGHT,
            PITTAG_RECAP = RECAP_YES_NO,
            PITTAG = toupper(FISH_TAG),
            SEX_CODE = SEX,                     #specimen data I might use
            SEX_COND_CODE = SEX_COND,
            SPECIMEN_NOTES = SPECIMEN_NOTES,
            ACCESS_SAMPLE_ID = ID,              #sample data i might need
            GEAR_CODE = S_GEAR,
            TRIP_ID = S_TRIP_ID,
            SAMPLE_TYPE = SAMPLE_TYPE)

glimpse(f.gcy) #check that datetimes are datetime format

#format dates in others
glimpse(f.agfd)
f.agfd <- f.agfd %>%
  mutate(START_DATETIME = as.POSIXct(START_DATETIME),
         ACCESS_SAMPLE_ID = as.character(ACCESS_SAMPLE_ID)) %>%
  select(-END_DATETIME)

glimpse(f.fws)
f.fws <- f.fws %>%
  mutate(START_DATETIME = as.POSIXct(START_DATETIME),
         END_DATETIME = as.POSIXct(END_DATETIME),
         ACCESS_SAMPLE_ID = as.character(ACCESS_SAMPLE_ID)) %>%
  select(-END_DATETIME)

glimpse(f.pit)
f.pit <- f.pit %>%
  mutate(START_DATETIME = as.POSIXct(START_DATETIME),
         END_DATETIME = as.POSIXct(END_DATETIME),
         ACCESS_SAMPLE_ID = as.character(ACCESS_SAMPLE_ID)) %>%
  select(-END_DATETIME)

f.cond <- bind_rows(f.agfd, f.gcy, f.fws) %>%
  mutate(DATE = as.Date(format(START_DATETIME, "%Y-%m-%d"))) %>%
  select(DATE, START_RM,
        SPECIES_CODE, TOTAL_LENGTH, FORK_LENGTH, WEIGHT,
        SEX_CODE,SEX_COND_CODE, SPECIMEN_NOTES,
        ACCESS_SAMPLE_ID, GEAR_CODE, TRIP_ID, SAMPLE_TYPE) %>%
  filter(!is.na(WEIGHT))


f.growth <- bind_rows(f.agfd, f.gcy, f.fws, f.pit) %>%
  mutate(DATE = as.Date(format(START_DATETIME, "%Y-%m-%d"))) %>%
            select(PITTAG, PITTAG_RECAP,
                   START_DATETIME, START_RM,
                   SPECIES_CODE, TOTAL_LENGTH, FORK_LENGTH, WEIGHT,
                   SEX_CODE,SEX_COND_CODE, SPECIMEN_NOTES,
                   ACCESS_SAMPLE_ID, GEAR_CODE, TRIP_ID, SAMPLE_TYPE) %>%
  filter(!is.na(PITTAG))

rm(f.fws, f.gcy, f.pit)

#calculate biomass #####

#calculate effort for each gear type and add sample reach to LF reaches
s <- s %>%
  mutate(TOTAL_HOURS = case_when(
           GEAR_CODE == "EL" ~ EF_TOTAL_SECONDS/(60*60),
           GEAR_CODE %in% c("HB", "MHB") ~
             as.numeric(difftime(END_DATETIME, START_DATETIME, units = "hours"))))


#filter to data that is appropriate to analyze
s <- s %>%
  filter(SAMPLE_TYPE %in% c(96, 99, NA) & !is.na(TOTAL_HOURS) & #remove supplemental
           year >= 2012) %>% #match time scale to bug data
  select(TRIP_ID, START_DATETIME, START_DATE, year, month,
         START_RM, sample_reach_start,
         SAMPLE_TYPE, GEAR_CODE, TOTAL_HOURS, unique_sample_id)

# Calculate predicted weights to get weight estimate for unweighed fish
glimpse(f.agfd)
f.agfd <- f.agfd %>%
  filter(!is.na(SPECIES_CODE) & SPECIES_CODE != "NFC" &
           SPECIES_CODE != "" &
           START_DATETIME >= as.POSIXct("2012-01-01 01:01:01")) %>%
  select(unique_sample_id, SPECIES_CODE,
         TOTAL_LENGTH, TL, FORK_LENGTH, WEIGHT)

#merge coefficients to calculate weight onto fish data
f.agfd <- merge(f.agfd, wt.equations, by = "SPECIES_CODE", all.x = TRUE)

#calculate predicted weights
f.agfd <- f.agfd %>%
  mutate(wt = case_when(
    !is.na(WEIGHT) ~ WEIGHT, #use actual weight if possible
    #if weight not measured, calculate from total length
    is.na(WEIGHT) & !is.na(TOTAL_LENGTH) ~
      round(10^(TL.intercept + TL.slope*log10(TOTAL_LENGTH)), 1),
    #or from fork length if no total length
    is.na(WEIGHT) & is.na(TOTAL_LENGTH) ~
      round(10^(FL.intercept + FL.slope*log10(FORK_LENGTH)), 1))) %>%
  select(-TL.intercept, -FL.intercept, -TL.slope, - FL.slope) %>%
  mutate(wt = wt/1000) #convert weight from g to kg

#calculate biomass/hr and add to sample dataframe
b <- f.agfd %>%
  group_by(unique_sample_id) %>%
  summarize(biomass = sum(wt, na.rm = TRUE))#calculate biomass/site

#biomass/site by species for 3 common species
b.common <- f.agfd %>%
  group_by(SPECIES_CODE,unique_sample_id) %>%
  summarize(biomass = sum(wt, na.rm = TRUE)) %>%
  filter(SPECIES_CODE %in% c("FMS", "HBC", "RBT"))

b.common$SPECIES_CODE <- paste(b.common$SPECIES_CODE, "kg", sep = ".")

b.common <- spread(b.common, SPECIES_CODE, biomass,
                   fill = 0)

s <- merge(s, b, by = "unique_sample_id", all.x = TRUE)
s <- merge(s, b.common, by = "unique_sample_id", all.x = TRUE)

#replace NAs with zeros
NaToZero <- function (x) { #function to replace zeros with NAs
  x[is.na(x)] <- 0
  return(x)
}

s[,c("biomass", "FMS.kg", "HBC.kg", "RBT.kg")] <- #replace NAs with zeros
  NaToZero(s[,c("biomass","FMS.kg", "HBC.kg", "RBT.kg")])

s <- s %>%
  mutate(biomass.hr = biomass/TOTAL_HOURS, #divide by hours to get rel. biomass
         FMS.kg.hr = FMS.kg/TOTAL_HOURS,
         HBC.kg.hr = HBC.kg/TOTAL_HOURS,
         RBT.kg.hr = RBT.kg/TOTAL_HOURS,
         rkm = round(1.60934*(START_RM + 15.8), 2)) #add river kilometers

rm(b, b.common) #no longer needed, remove

write.csv(s, "./data/fish_sample.csv", row.names = FALSE)


# calculate condition ######
glimpse(f.cond)

f.cond <- f.cond %>%
  filter(DATE >= as.Date("2012-01-01"))  #match to timescale of bug data

#check sample size by species
f.cond %>%
  group_by(SPECIES_CODE) %>%
  summarise(n = n()) %>%
  arrange(-n)

f.cond <- f.cond %>%
  filter(SPECIES_CODE %in% c("FMS", "BHS", "HBC", "RBT", "BNT", "CRP"))

#examine geographic distr. of species
f.cond %>%
  ggplot(aes(x = START_RM, y = SPECIES_CODE)) +
  geom_jitter(alpha= 0.2)

#definitely look at FMS, HBC, RBT - largest sample sizes
#CRP, BNT, BHS - maybe - lower n, but broad geographic distr.

#calculate predicted weights
f.cond <- merge(f.cond, wt.equations, by = "SPECIES_CODE", all.x = TRUE)

#calculate predicted weights
f.cond <- f.cond %>%
  mutate(predicted.wt = case_when(
    #calculate from total lenght if TL was measured
    !is.na(TOTAL_LENGTH) ~
      round(10^(TL.intercept + TL.slope*log10(TOTAL_LENGTH)), 1),
    #or from fork length if no total length
    is.na(TOTAL_LENGTH) ~
      round(10^(FL.intercept + FL.slope*log10(FORK_LENGTH)), 1)),
    kn = round(WEIGHT/predicted.wt, 2)) %>%
 select(-TL.intercept, -FL.intercept, -TL.slope, - FL.slope) #remove, no longer needed

#define outlier levels for condition
#these fish will be removed - most likely are weight measurement errors
high.bigfish.cutoff <- 1.70
high.littlefish.cutoff <- 2.00
low.cutoff <- 0.40

f.cond <- f.cond %>%
  mutate(outlier = case_when(((TOTAL_LENGTH < 150 | FORK_LENGTH < 150) &
                              (kn < low.cutoff | kn > high.littlefish.cutoff)) |
                              ((TOTAL_LENGTH >= 150 | FORK_LENGTH >= 150) &
                                   (kn < low.cutoff | kn > high.bigfish.cutoff)) ~
                                "outlier",
                              TRUE ~ "normal"))
#check comments to make sure fish marked in comments as "skinny", "snake", or
#or "fat" are marked normal, not outlier
#i.e., check that I am removing only measurement or recording errors, not
#unusually fat or skinny fish.


#loop to plot for each species
#check to make sure only likely measurement errors marked as outlier
species.list <- unique(f.cond$SPECIES_CODE) #list of species w weight data
for (i in seq_along(species.list)) {

  #plot to check fit of model
  print(f.cond %>%
          filter(SPECIES_CODE == species.list[i]) %>%
          ggplot(aes(x = TOTAL_LENGTH, y = WEIGHT, color = outlier)) +
          geom_point(alpha = 0.5) +
          scale_color_manual(values = c("green", "red", "transparent")) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]))

}


f.cond <- f.cond %>%
  filter(outlier == "normal") %>%
  select(-outlier) %>%
  mutate(rkm = round(1.60934*(START_RM + 15.8), 2)) #add river kilometer

#save
write.csv(f.cond, "./data/fish_condition.csv", row.names = FALSE)

# growth rates - subset to usable data ######

#add a few columns I need
f.recap <- f.recap %>%
  mutate(distance.moved = START_RM_0 - START_RM_1,
         TL_mean = (TL_1 + TL_0)/2,
         mm.week = mm.day*7,
         mm.month = mm.day*30.44,
         mm.year = mm.day*365,
         rkm_0 = round(1.60934*(START_RM_0 + 15.8), 2),
         rkm_1 = round(1.60934*(START_RM_1 + 15.8), 2))

#some exploratory plots to see distribution
#sample size by project
n <- f.recap %>%
  group_by(SAMPLE_TYPE_0, SAMPLE_TYPE_1) %>%
  summarise(n = n())

#geographic dist by project
ggplot(f.recap, aes(x = START_RM_0, y = factor(SAMPLE_TYPE_0))) +
  geom_point()
ggplot(f.recap, aes(x = START_RM_0, y = factor(SAMPLE_TYPE_0))) +
  geom_bin2d()

#size distr.
ggplot(f.recap, aes(x = TL_0)) +
  geom_histogram(binwidth = 10) +
  facet_grid(SPECIES_CODE~., scales = "free")

#spatial distr.
ggplot(f.recap, aes(x = START_RM_0)) +
  geom_histogram(binwidth = 5) +
  facet_grid(SPECIES_CODE~., scales = "free")

#distribution of days at large
ggplot(f.recap, aes(x = days)) +
  geom_histogram(binwidth = 30) +
  coord_cartesian(xlim = c(0, 1000))

#Filter out data I don't want
#LCR based projects (NSE, removals) and Mike Yard's projects are so much effort
#concentrated in one area, skewing data - remove if both mark and recapture
# are from these projects (but keep if at least one capture was from a project
# with geographically broad sampling (AGFD monitoring, USFWS aggregation)
f.recap <- f.recap %>%
  filter(SAMPLE_TYPE_0 %in% c(107:113, 129, 137:140) == FALSE |
           SAMPLE_TYPE_1 %in% c(107:113, 129, 137:140) == FALSE) %>%
  filter(SPECIES_CODE %in% c("FMS", "HBC", "RBT")) %>% #most common species
  filter(days >= 100 & #more variation (summer vs. winter rates) in fish at
           #large during only one season
           days <= 365*2) #at large < 2 years - so can meaningfylly bin by size

write.csv(f.recap, "./data/fish_growth.csv", row.names = FALSE)

