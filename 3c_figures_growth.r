#preliminary growth analysis

#very preliminary/exploratory, needs a lot of refinement, just did this quick
#so I could put a growth result in an abstract.


require(tidyverse)
library(scales)
theme_set(theme_minimal())


#data has already been subset to moved less than 5 km
r <- read_csv("\\\\flag-server/Office/Grand Canyon Downstream/R_code_annual_report_analysis/GC_annual_report_analysis_2018/data/clean/recaptures.csv")

r$distance.moved <- r$START_RM_0 - r$START_RM_1

#sample size by project
n <- r %>%
  group_by(SAMPLE_TYPE_0, SAMPLE_TYPE_1) %>%
  summarise(n = n())


#subset to at large for < 2 years
#am binning by size, don't want fish that span multiple size classes
r <- r[which(r$days <= 365*2),]


unique(r$SPECIES_CODE)
summarise(group_by(r, SPECIES_CODE),
          n = length(SPECIES_CODE))

#subset to desired species
r <- r[which(r$SPECIES_CODE %in% c("RBT", "HBC", "FMS")),]

#size distr.
ggplot(r, aes(x = TL_0)) +
  geom_histogram(binwidth = 10) +
  facet_grid(.~SPECIES_CODE, scales = "free")

#spatial distr.
ggplot(r, aes(x = START_RM_0)) +
  geom_histogram(binwidth = 5) +
  facet_grid(SPECIES_CODE~., scales = "free")
#FMS most even, but more at LCR and JCM-west reach
#HBC skewed towards little C
##RBT relatively even trhough marble

#see what natural size breaks are
r %>%
  filter(SPECIES_CODE == "FMS") %>%
  ggplot(aes(TL_0)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
r %>%
  filter(SPECIES_CODE == "FMS") %>%
  ggplot(aes(TL_1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
#350 mm for flannies

r %>%
  filter(SPECIES_CODE == "HBC") %>%
  ggplot(aes(TL_0)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
r %>%
  filter(SPECIES_CODE == "HBC") %>%
  ggplot(aes(TL_1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
#Humpback chub: Remove fish < 150mm
#natural break, AND many small fish are likely coming out of LCR, not
#exclusively mainstem fish

#what projects are recaptures from?
who <- r %>%
  group_by(SAMPLE_TYPE_0, SAMPLE_TYPE_1) %>%
  summarize(n = n())

#add needed columns
r <- r %>%
  mutate(size.class = factor(case_when(TL_mean <= 330 ~ "< 350 mm",
                                TL_mean > 330 ~ "> 350 mm"),
                      levels = c("< 350 mm", "> 350 mm")))
#poster graphs

#where to subset on low end in terms of days at large
#i.e., fish at large over one summer appear to have high growth
#variability in growth rate vs. time at large
r %>%
  ggplot(aes(x = days, y = mm.month)) +
  geom_point() +
  facet_grid(SPECIES_CODE~.)
#remove fish at large less than 100 days - that takes care of worst high outliers
#or remove fish at large for less than 220 days - for flannies is more high
#variation up to 220 or so

r <- r %>%
  filter(days >= 220)


#Flannelmouth, two size classes
r %>%
  filter(SPECIES_CODE == "FMS") %>%
  ggplot(aes(x = START_RM_0, y = mm.month)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  facet_grid(size.class~., scales = "free", space = "free")


r %>%
  filter(SPECIES_CODE == "HBC" & TL_0 >= 150) %>%
  filter(SPECIES_CODE == "HBC" ) %>%
  ggplot(aes(x = START_RM_0, y = mm.month)) +
  scale_y_continuous() +
  geom_point(alpha = 0.2) +
  stat_smooth()



r %>%
  filter(SPECIES_CODE == "FMS" & TL_0 <= 250) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  ggtitle("Flannelmouth < 250mm")

r %>%
  filter(SPECIES_CODE == "FMS" & TL_0 >= 250 & TL_0 <= 350) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  ggtitle("Flannelmouth >= 250mm and < 400mm")
r %>%
  filter(SPECIES_CODE == "FMS" & TL_0 >= 350) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  ggtitle("Flannelmouth >= 400mm")

#possible weak correlation w/ bug curve within size groups
#no relationship if don't bin by size.
#seems like temp and tributaries are also important here though

r %>%
  filter(SPECIES_CODE == "HBC") %>%
  ggplot(aes(TL_0)) +
  geom_histogram()




r %>%
  filter(SPECIES_CODE == "HBC" & TL_0 >= 250) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  ggtitle("Humpback Chub >= 250mm")
#not enough data for HBC - all at LCR - too few recaps elsewhere

r %>%
  filter(SPECIES_CODE == "RBT" & TL_0 <= 250) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  scale_x_continuous(limits = c(-16, 75)) +
  geom_point(alpha = 0.2) +
  stat_smooth()
r %>%
  filter(SPECIES_CODE == "RBT" & TL_0 >= 250 & TL_0 <= 350) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  scale_x_continuous(limits = c(-16, 75)) +
  geom_point(alpha = 0.2) +
  stat_smooth()
r %>%
  filter(SPECIES_CODE == "RBT" & TL_0 >= 350) %>%
  ggplot(aes(x = START_RM_0, y = mm.day)) +
  scale_x_continuous(limits = c(-16, 75)) +
  geom_point(alpha = 0.2) +
  stat_smooth()
#no evidence for RBT
