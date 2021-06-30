#Reads bug data, saves in project folder
#inputs: data available on github/jmuehlbauer-usgs
#        LightTrapSample.csv      sample data for light traps
#        LightTrapSpecimen.csv      specimen data for light traps
#outputs: ./data/bug_sample.csv
#         Light Trap Sample data
#         subset to Colorado River in Grand Canyon, Apr to Sept, pre bug flows,
#         added columns for midge abundance, month/day(same year)

require(data.table)
require(ggplot2)

#data from USGS-GCMRC foodbase group
#light trap samples (emergent insects) collected by science trips and citizen
#scientists in the Colorado River basin
bug.sample.url <- "https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/LightTrapSample.csv"
bug.specimen.url <- "https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/LightTrapSpecimen.csv"
bug.sample <- fread(bug.sample.url)
bug.specimen <- fread(bug.specimen.url)

#set keys: sorts by this column, and this column is now default for joins
setkey(bug.sample, SampleID)
setkey(bug.specimen, SampleID)


#create a month/day field (no year) to plot sample distribution
bug.sample[, `:=` (Date = as.Date(Date, format = "%m/%d/%Y"),
                   TimeBegin = as.POSIXct(TimeBegin, format = "%m/%d/%Y %H:%M:%S"),
                   TimeEnd = as.POSIXct(TimeEnd, format = "%m/%d/%Y %H:%M:%S"))]
str(bug.sample)
bug.sample[, DateSameYear := as.Date(paste0("2000-",
                                            strftime(Date, format = "%m-%d")))]

#subset to glen canyon to lake mead parts of colorado river
bug.sample <- bug.sample[Reach %in% c("CRGrandCanyon", "CRLeesFerry"),]

#subset to before bug flows started
bug.sample <- bug.sample[Date < as.Date("2018-04-30"),]

#subset to APRIL TO SEPTEMBER
#April to September are most consistently sampled over the years, and years with
#year round sampling show that most production occurs april to september anyway
bug.sample <- bug.sample[DateSameYear >= as.Date("2000-04-01") &
                           DateSameYear <= as.Date("2000-09-30"),]


#subset to midges only: CHIA = chironomidae adult
midge.specimen <- bug.specimen[SpeciesID == "CHIA",]
midge.specimen <- midge.specimen[, .(SampleID, Count)]
setnames(midge.specimen, old = "Count", new = "MidgeCount")

#join midge counts to sample dataframe
#data.table joins: keep all rows from DT in brackets, only matching rows
#from DT outside of brackets
bug.sample <- midge.specimen[bug.sample]

#calculate midges per hour
bug.sample[, `:=` (TimeElapsed =
                     as.numeric(difftime(TimeEnd, TimeBegin, units = "hours")))]
bug.sample[, `:=` (MidgeHr = MidgeCount/TimeElapsed)]

#add river kilometers
bug.sample[, `:=` (rkm = 1.60934*(RiverMile + 15.8))]

ggplot(bug.sample, aes(x = TimeElapsed)) +
  geom_histogram()

#save
fwrite(bug.sample, "./data/bug_sample.csv")

