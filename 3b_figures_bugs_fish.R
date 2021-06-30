#makes figures of bugs and fish by river mile
require(tidyverse) #ggplot, dplyr, tidyr functions used
require(scales) # for comma formatting on axis labels

source("./functions/jkb_ggplot_theme.r") #custom plot theme

#better plot theme defaults
theme_set(theme_minimal())

bug.sample <- read_csv("./data/bug_sample.csv")
fish.sample <- read_csv("./data/fish_sample.csv")
fish.cond <- read_csv("./data/fish_condition.csv")
fish.growth <- read_csv("./data/fish_growth.csv")

bug.sample <- bug.sample %>%
  filter(!is.na(rkm) & !is.na(MidgeHr))

### Sample distribution exploratoryplots #######

#add day of year variable to fish so can plot time of year o fsamples
fish.sample$DateSameYear <- as.Date(paste0("2000-",
                               strftime(fish.sample$START_DATE, format = "%m-%d")))

# replicate plots from Kennedy et al. 2016
#but with below diamond and RM instead of RKM

# SAMPLE DISTRIBUTION: spatial and temporal ######
#plot spatial and temporal distribution of samples
#not intended for publication, just exploratory plots
samples <- bind_rows(
  fish.sample %>%
  transmute(DateSameYear = DateSameYear,
            Date = START_DATE,
            RiverMile = START_RM,
            Year = year,
            type = "fish"),
  bug.sample %>%
  transmute(DateSameYear = DateSameYear,
            Date = Date,
            RiverMile = RiverMile,
            Year = as.numeric(substr(as.character(Date), 1, 4)),
            type = "bug"))

sample.dist <- samples %>%
  arrange(type) %>%
  filter(type == "bug sample" | Year >= 2016) %>%
ggplot(aes(x = RiverMile, y = DateSameYear,
             color = type, alpha = type, shape = type)) +
    geom_point() +
    scale_color_manual(values = c("black", "steelblue", "turquoise3", "seagreen4")) +
    scale_alpha_manual(values = c(0.3, 1, 1, 1)) +
    scale_shape_manual(values = c(1, 19, 19, 19)) +
    scale_y_date(date_breaks = "2 months", date_labels = "%b") +
    scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  theme(legend.position = "top",
        legend.title = element_blank())

sample.dist


#with real years
sample.dist.year <- samples %>%
  filter(Year >= 2012) %>%
  arrange(type) %>%
ggplot(aes(x = RiverMile, y = Date,
                   color = type, alpha = type, shape = type)) +
  geom_point(alpha = 0.5, shape = 1) +
  geom_hline(yintercept = as.Date(paste0(seq(2012, 2018), "-01-01"))) +
  scale_color_manual(values = c("black", "turquoise3")) +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_shape_manual(values = c(1, 19)) +
  scale_y_date(date_breaks = "1 year", date_labels = "%b %Y") +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  theme(legend.position = "top",
        legend.title = element_blank())
sample.dist.year

#fewer samples below Diamond Creek, but maybe enough?
#Maybe ok 226-250, but sample size too small 250-281?
#different temporal distribution of samples downstream of diamond
#(more spring samples, less summer/fall samples)


#replicate midge abundance plot #####
#but, with data below diamond creek added
#also a few more years

midge.rm <- ggplot(bug.sample, aes(x = RiverMile, y = MidgeHr)) +
  geom_point(alpha = 0.1, shape = 16) +
  coord_cartesian(xlim = c(-16, 300)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  stat_smooth(color = "red")
midge.rm

midge.rm.zoom <- midge.rm +
  coord_cartesian(ylim = c(0, 9000), xlim = c(-16, 300))
midge.rm.zoom

midge.rm.superzoom <- midge.rm +
  coord_cartesian(ylim = c(0, 1000), xlim = c(-16, 300))
midge.rm.superzoom

#are lack of samples below RM 250 skewing things?
midge.rm.truncated <- bug.sample %>%
  filter(RiverMile < 251) %>%
  ggplot(aes(x = RiverMile, y = MidgeHr)) +
  geom_point(alpha = 0.3, shape = 1) +
  coord_cartesian(ylim = c(0, 1000), xlim = c(-16, 300)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  stat_smooth(color = "red")

midge.rm.truncated
#diamond to 250 curve is horizontal or + without 250-281, vs. - with all RM
#what is correct?
#is low midges 251-281 a sampling size artifact?
#or is abundance truly lower because of habitat, turbidity, or similar?

#ABUNDANCE LIKELY IS LOW: lack of rocky substrate habitat (that midges need)
#in that area means low catches likely represent low abundance (pers. comm,
#Kennedy & Muehlbauer)

#Use all miles, do not truncate at 250

#manually get model fit so it can be plotted with fish data
midge.rm.fitted <- ggplot_build(midge.rm)$data[[2]]

#make one with kilometers, because science
midge.rkm <- ggplot(bug.sample, aes(x = rkm, y = MidgeHr)) +
  geom_point(alpha = 0.1, shape = 16) +
  coord_cartesian(xlim = c(0, 480)) +
  scale_x_continuous(breaks = seq(0, 500, by = 50)) +
  stat_smooth(color = "red")
midge.rkm

#manually get model fit so it can be plotted with fish data
midge.rkm.fitted <- ggplot_build(midge.rkm)$data[[2]]

#for poster
#Make secondary axis match color of line
p.midge.km <- ggplot(bug.sample, aes(x = rkm, y = MidgeHr)) +
  theme_jkb(base_size = 20, base_family = "sans") +
  geom_point(alpha = 0.1, shape = 16, size = 2) +
  coord_cartesian(xlim = c(0, 475), ylim = c(0, 9000)) +
  scale_x_continuous("River kilometers from Dam", breaks = seq(0, 500, by = 50),
                     expand = expand_scale(mult = c(0, 0.01))) +
  scale_y_continuous("Midge abundance (n/hour)",
                     breaks = seq(0,10000, by = 2000), labels = comma,
                     expand = expand_scale(mult = c(0.005, 0)),
                     sec.axis = sec_axis(name = "Mean midge abundance (n/hour)",
                                         breaks = seq(0, 800, by = 200),
                                         ~. /10)) +
  geom_line(inherit.aes = FALSE, data = midge.rkm.fitted,
            aes(x = x, y = y*10), color = "#a75c2e", size = 1) +
  geom_ribbon(inherit.aes = FALSE, data = midge.rkm.fitted,
              aes(x = x, ymin = ymin*10, ymax = ymax*10),
              fill = "#a75c2e", alpha = 0.3) +
  theme(axis.title.y.right = element_text(color = "#a75c2e"),
        axis.text.y.right = element_text(color = "#a75c2e"),
        axis.line.y.right = element_line(color = "#a75c2e"),
        axis.ticks.y.right = element_line(color = "#a75c2e"))

p.midge.km

ggsave(p.midge.km, file = "./output/figures/midge_curve_km.png",
       dpi = 300, width = 9, height = 5)


#manually get model fit so it can be plotted with fish data
midge.rm.fitted <- ggplot_build(midge.rm)$data[[2]]


#for poster
#Make secondary axis match color of line
p.midge.mile <- ggplot(bug.sample, aes(x = RiverMile, y = MidgeHr)) +
  theme_jkb(base_size = 20) +
  geom_point(alpha = 0.1, shape = 16, size = 2) +
  coord_cartesian(xlim = c(-16, 282), ylim = c(0, 9000)) +
  scale_x_continuous("River Mile", breaks = seq(0, 300, by = 50),
                     expand = expand_scale(mult = c(0.01, 0.01))) +
  scale_y_continuous("Midge abundance (n/hour)",
                     breaks = seq(0,10000, by = 2000), labels = comma,
                     expand = expand_scale(mult = c(0.005, 0)),
                       sec.axis = sec_axis(name = "Mean midge abundance (n/hour)",
                                           breaks = seq(0, 800, by = 200),
                                           ~. /10)) +
  geom_line(inherit.aes = FALSE, data = midge.rm.fitted,
            aes(x = x, y = y*10), color = "#a75c2e", size = 1) +
  geom_ribbon(inherit.aes = FALSE, data = midge.rm.fitted,
              aes(x = x, ymin = ymin*10, ymax = ymax*10),
              fill = "#a75c2e", alpha = 0.3) +
  theme(axis.title.y.right = element_text(color = "#a75c2e"),
        axis.text.y.right = element_text(color = "#a75c2e"),
        axis.line.y.right = element_line(color = "#a75c2e"),
        axis.ticks.y.right = element_line(color = "#a75c2e"))

p.midge.mile

ggsave(p.midge, file = "./output/figures/midge_curve_mi.png",
       dpi = 300, width = 9, height = 5)

#Fish: examine sample size #####

#condition graph #####
#TO DO: get years and sample sizes to annotate each plot
#remove fish that are measurement errors (implausible)

#annotate species inside plot area
cond.labels <- data.frame(SPECIES_CODE = c("FMS", "RBT"),
                        name = c("Flannelmouth Sucker",
                                 "Rainbow Trout"),
                        rkm = 460, kn = 143)

fish.cond %>%
  group_by(SEX_COND_CODE) %>%
  summarize(n = n())

condition <- fish.cond %>%
  filter(SPECIES_CODE %in% c("FMS", "RBT"))  %>%
  ggplot(aes(x = rkm, y = kn)) +
  theme_jkb(base_family = "sans", base_size = 20) +
  geom_point(alpha = 0.1, color = "#79869d", shape = 16) +
  geom_hline(yintercept = 1) +
  geom_line(data = midge.rkm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y/660 +0.5), color = "#a75c2e", size = 1) +
  stat_smooth(color = "#48566e") +
  geom_text(data = cond.labels, #dataframe to use
           inherit.aes = FALSE, #dont use dataframe from points in plot
            aes(x = rkm, y = kn, label = name),
            size = 6, hjust = 1, color = "#48566e") + #appearance
  scale_y_continuous(expression(paste("Condition (", italic(K[n]), ")")),
                     sec.axis = sec_axis(~(.-0.5)*660, "Midge abundance (midges/hr)")) +
  coord_cartesian(ylim = c(0.497,1.503), expand = FALSE) +
  scale_x_continuous("River Kilometer", limits = c(0, 478), breaks = seq(0,500, by = 50)) +
  facet_wrap(~SPECIES_CODE, ncol = 1) +
  theme(axis.line.x = element_blank(),
        axis.text.y.right = element_text(color = "#a75c2e"),
        axis.title.y.right = element_text(color = "#a75c2e", vjust = 1),
        axis.text.y.left = element_text(color = "#48566e"),
        axis.title.y.left = element_text(color = "#48566e"),
        strip.text = element_blank(),
        panel.spacing = unit(2, "lines"))

condition

ggsave(condition, file = "./output/figures/condition.png",
       dpi = 300, width = 8, height = 8)

fish.cond <- fish.cond %>%
  mutate(year = as.numeric(format(DATE, "%Y")))

#calculate sample size and dates
n  <- fish.cond %>%
  filter(SPECIES_CODE %in% c("FMS", "RBT")) %>%
  group_by(SPECIES_CODE, year) %>%
  summarize(n = n())

#does size affect condition?
#NO, do not need to bin by size
fish.cond %>%
  filter(SPECIES_CODE %in% c("FMS", "RBT")) %>%
  ggplot(aes(x = TOTAL_LENGTH, y = kn)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~SPECIES_CODE, ncol = 1) +
  scale_y_continuous(limit = c(0, 200)) +
  stat_smooth()

#little correclation with bug data
#why? no relationship? not enough data to detect? confounding factors?

#are skinny RBT caught by NO during crash skewing RBT condition high?
#apparently not - RBT in marble are just in good condition

#is mixing spring and fall confounding things? plot spring only

#is CPUE confounding things (i.e., are fish in buggy areas in worse condition
#because these areas also have higher fish density?)

# biomass #####
#todo: find a way to display all data (i.e., high outliers)
#     means by reach?
#     violin or boxplots instead of points?
#format pretty for poster
#switch grams to kg

m <- summarize(group_by(fish.sample, sample_reach_start, GEAR_CODE),
               mean.biomass.hr = mean(biomass.hr, na.rm = TRUE))


#All fish, electrofishing
biomass.all.ef <- fish.sample %>%
  filter(GEAR_CODE == "EL") %>% #electrofishing only
ggplot(aes(x = rkm, y = biomass.hr)) +
  theme_jkb(base_family = "sans", base_size = 26) +
  geom_point(alpha = 0.2, color = "#79869d", size = 2, shape = 16) +
  coord_cartesian(ylim = c(0, 50), xlim = c(0, 478),expand = FALSE) +
  stat_smooth(span = 0.3, color = "#48566e", fill = "#48566e") +
  geom_line(data = midge.rkm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y/14),
            color = "#a75c2e", size = 1) +
  scale_y_continuous(expression(atop("Boat Electrofishing",
                                     paste("Relative Biomass (kg fish/hr)"))),
    breaks = seq(0, 600, by = 10),
    sec.axis = sec_axis(~.*14, "Midge Abundance (midges/hr)", labels = comma))  +
  scale_x_continuous("River Kilometer",
                     breaks = seq(0, 500, by = 50)) +
  theme(axis.text.y.right = element_text(color = "#a75c2e"),
        axis.title.y.right = element_text(color = "#a75c2e", vjust = 1),
        axis.text.y.left = element_text(color = "#48566e"),
        axis.title.y.left = element_text(color = "#48566e"))

biomass.all.ef
#All fish, hoop nets
biomass.all.hoop <- fish.sample %>%
  filter(GEAR_CODE %in% c("HB", "MHB")) %>% #hoop nets only
  ggplot(aes(x = rkm, y = biomass.hr)) +
  geom_point(alpha = 0.4, color = "#79869d", size = 2, shape = 16) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 478), expand = FALSE) +
  stat_smooth(span = 0.3, color = "#48566e", fill = "#48566e") +
  geom_line(data = midge.rkm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y/1300),
            color = "#a75c2e", size = 1) +
  scale_y_continuous(expression(atop("Baited Hoop Nets",
                                     paste("Relative Biomass (kg fish/hr)"))),
                     breaks = seq(0, 0.5, by = 0.1),
                     sec.axis = sec_axis(~.*1300, "Midge Abundance (midges/hr)",
                                         labels = comma))  +
  scale_x_continuous("River Kilometer", breaks = seq(0,500, by = 50)) +
  theme_jkb(base_family = "sans", base_size = 26) +
  theme(axis.text.y.right = element_text(color = "#a75c2e"),
        axis.title.y.right = element_text(color = "#a75c2e", vjust = 1),
        axis.text.y.left = element_text(color = "#48566e"),
        axis.title.y.left = element_text(color = "#48566e"))

biomass.all.hoop

ggsave(biomass.all.ef, file = "./output/figures/biomass_ef.png",
       dpi = 300, width = 12.5, height = 6)

ggsave(biomass.all.hoop, file = "./output/figures/biomass_hoop.png",
       dpi = 300, width = 12.5, height = 6)

#calculate sample sizes
fish.sample %>%
  group_by(GEAR_CODE) %>%
  summarise(n = n())

#grouped by reach - looks similar
#ggplot(m, aes(x = sample_reach_start, y = mean.biomass.hr)) +
#  geom_point() +
#  facet_grid(GEAR_CODE~., scales = "free_y")

#Flannelmouth, electrofishing
biomass.fms.ef <- fish.sample %>%
  filter(year >= 2012 & GEAR_CODE == "EL" &
           (month >= 3 & month <= 7)) %>%
  ggplot(aes(x = START_RM, y = FMS.g.hr)) +
  geom_point(alpha = 0.2) +
  ggtitle("Flannelmouth biomass, electrofishing") +
  coord_cartesian(ylim = c(0, 20000)) +
  stat_smooth(span = 0.3) +
  geom_line(data = midge.rm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y*30), color = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./30, "midge/hr"))  +
  scale_x_continuous(breaks = seq(0,300, by = 50))

biomass.fms.ef

#Flannelmouth, hoop nets
biomass.fms.hoop <- fish.sample %>%
  filter(GEAR_CODE %in% c("HB", "MHB") &
           (month >= 3 & month <= 7)) %>%
  ggplot(aes(x = START_RM, y = FMS.g.hr)) +
  ggtitle("Flannelmouth biomass, hoop nets") +
  geom_point(alpha = 0.2) +
  coord_cartesian(ylim = c(0, 600)) +
  stat_smooth(span = 0.5) +
  geom_line(data = midge.rm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y), color = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./1, "midge/hr")) +
  scale_x_continuous(breaks = seq(0,300, by = 50))

biomass.fms.hoop

#Rainbow Trout, electrofishing
biomass.rbt.ef <- fish.sample %>%
  filter(year >= 2012 & GEAR_CODE == "EL" &
           (month >= 3 & month <= 7)) %>%
  ggplot(aes(x = START_RM, y = RBT.g.hr)) +
  geom_point(alpha = 0.2) +
  ggtitle("Rainbow Trout biomass, electrofishing") +
  coord_cartesian(ylim = c(0, 35000)) +
  stat_smooth(span = 0.3) +
  geom_line(data = midge.rm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y*30), color = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./30, "midge/hr")) +
  scale_x_continuous(breaks = seq(0,300, by = 50))

biomass.rbt.ef

#Humpback Chub, hoop nets
biomass.hbc.hoop <- fish.sample %>%
  filter(GEAR_CODE %in% c("HB", "MHB") &
           (month >= 3 & month <= 7)) %>%
  ggplot(aes(x = START_RM, y = HBC.g.hr)) +
  ggtitle("Humpback Chub biomass, hoop nets") +
  geom_point(alpha = 0.2) +
  coord_cartesian(ylim = c(0, 50)) +
  stat_smooth(span = 0.5) +
  geom_line(data = midge.rm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y/10), color = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, "midge/hr")) +
  scale_x_continuous(breaks = seq(0,300, by = 50))

biomass.hbc.hoop

#model with MidgeHr, month, TL to see whats important?

#save

#sample distribution
ggsave(sample.dist, file = "./output/figures/sample_distribution.png",
       dpi = 300, width = 4.5, height = 5.5)
#sample distribution
ggsave(sample.dist.year, file = "./output/figures/sample_distribution_year.png",
       dpi = 300, width = 4.5, height = 6.5)

#midge graphs
#sample distribution
ggsave(midge.rm.superzoom, file = "./output/figures/midges.png",
       dpi = 300, width = 7, height = 4.5)
ggsave(midge.rm.truncated, file = "./output/figures/midges_truncated.png",
       dpi = 300, width = 7, height = 4.5)

#biomass
ggsave(biomass.fms.ef, file = "./output/figures/biomass_fms_efish.png",
       dpi = 300, width = 7, height = 4.5)
ggsave(biomass.fms.hoop, file = "./output/figures/biomass_fms_hoop.png",
       dpi = 300, width = 7, height = 4.5)
ggsave(biomass.rbt.ef, file = "./output/figures/biomass_rbt_efish.png",
       dpi = 300, width = 7, height = 4.5)
ggsave(biomass.hbc.hoop, file = "./output/figures/biomass_hbc_hoop.png",
       dpi = 300, width = 7, height = 4.5)

#growth ##########

#sample size by project
n.recaps <- fish.growth %>%
  group_by(SAMPLE_TYPE_0, SAMPLE_TYPE_1) %>%
  summarise(n = n())

#some exploratory plots to visualize sample size and distribution
#size distr.
ggplot(fish.growth, aes(x = TL_0)) +
  geom_histogram(binwidth = 10) +
  facet_grid(.~SPECIES_CODE, scales = "free")

#spatial distr.
ggplot(fish.growth, aes(x = START_RM_0)) +
  geom_histogram(binwidth = 5) +
  facet_grid(SPECIES_CODE~., scales = "free")
#FMS most even, but more at LCR and JCM-west reach
#HBC skewed towards little C
##RBT relatively even trhough marble

#see what natural size breaks are
fish.growth %>%
  filter(SPECIES_CODE == "FMS") %>%
  ggplot(aes(TL_0)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
fish.growth %>%
  filter(SPECIES_CODE == "FMS") %>%
  ggplot(aes(TL_1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
#350 mm for flannies

fish.growth %>%
  filter(SPECIES_CODE == "HBC") %>%
  ggplot(aes(TL_0)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
fish.growth %>%
  filter(SPECIES_CODE == "HBC") %>%
  ggplot(aes(TL_1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,600, by = 25))
#Humpback chub: Remove fish < 150mm
#natural break, AND many small fish are likely coming out of LCR, not
#exclusively mainstem fish

#add size bin for to use for flannies
fish.growth$size.class <- factor(ifelse(fish.growth$SPECIES_CODE == "FMS" &
                                      fish.growth$TL_mean <= 350, "FMS < 350 mm",
                               ifelse(fish.growth$SPECIES_CODE == "FMS" &
                                        fish.growth$TL_mean > 350, "FMS > 350 mm",
                                ifelse(fish.growth$TL_mean >= 150 &
                                         fish.growth$SPECIES_CODE == "HBC", "HBC", NA))),
                             levels = c("FMS < 350 mm", "FMS > 350 mm", "HBC"))
#poster graphs

#where to subset on low end in terms of days at large
#i.e., fish at large over one summer appear to have high growth
#variability in growth rate vs. time at large
fish.growth %>%
  ggplot(aes(x = days, y = mm.month)) +
  geom_point() +
  facet_grid(SPECIES_CODE~.)

#or remove fish at large for less than 220 days - for flannies is more high
#variation up to 220 or so

#fish.growth <- fish.growth %>%
 # filter(days >= 220)

growth.labels <- data.frame(size.class = c("FMS < 350 mm", "FMS > 350 mm", "HBC"),
                          name = c("Flannelmouth Sucker 150 - 350 mm",
                                   "Flannelmouth Sucker > 351 mm",
                                   "Humpback Chub > 150mm"),
                          rkm = 460, mm.week = 12)

levels(fish.growth$size.class)




#for poster: Flannelmouth, two size classes and Humpback chub > 150
growth <- fish.growth %>%
  filter(!is.na(size.class)) %>%
  ggplot(aes(x = rkm_0, y = mm.month)) +
  geom_point(alpha = 0.4, color = "#79869d", shape = 16, size = 2) +
  #stat_smooth( color = "#48566e", fill = "#48566e") +
  geom_line(data = midge.rkm.fitted, inherit.aes = FALSE,
            aes(x = x, y = y/50),
            color = "#a75c2e", size = 1) +
   geom_text(data = growth.labels, #dataframe to use
            inherit.aes = FALSE, #dont use dataframe from points in plot
            aes(x = rkm, y = mm.week, label = name),
            size = 6, hjust = 1, color = "#48566e") + #appearance
  coord_cartesian(ylim = c(0, 12.5), xlim = c(0, 478),
                  expand = FALSE) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous("Growth rate (mm/month)", breaks = seq(0, 12, by = 2),
                     sec.axis = sec_axis(~.*50, "Midge Abundance (midges/hr)",
                                         labels = comma)) +
  scale_x_continuous("River Kilometer", breaks = seq(0,500, by = 50)) +
  facet_grid(size.class~., scales = "free", space = "free") +
  theme_jkb(base_family = "sans", base_size = 20) +
  theme(strip.text = element_blank(),
        axis.text.y.right = element_text(color = "#a75c2e"),
        axis.title.y.right = element_text(color = "#a75c2e", vjust = 1),
        axis.text.y.left = element_text(color = "#48566e"),
        axis.title.y.left = element_text(color = "#48566e"))
growth
#attempt to remove fitted values below zero - log tranfrom, then back
#for poster: Flannelmouth, two size classes and Humpback chub > 150
#1. make stat smooths on log transformed scales
growth.fit <- fish.growth %>%
  filter(!is.na(size.class)) %>%
  ggplot(aes(x = rkm_0, y = mm.month)) +
  geom_point(alpha = 0.4, color = "#79869d", shape = 16, size = 2) +
  stat_smooth( color = "#48566e", fill = "#48566e") +
  facet_grid(size.class~.) +
  scale_y_log10() +
  coord_trans(y = scales::exp_trans(10))
growth.fit
#2. extract values (as with midge curve)
growth.rkm.fitted <- ggplot_build(growth.fit)$data[[2]]
growth.rkm.fitted$size.class <- ifelse(growth.rkm.fitted$PANEL == 1,
                                       "FMS < 350 mm",
                                       ifelse(growth.rkm.fitted$PANEL == 2,
                                              "FMS > 350 mm", "HBC"))
#backtransform to return to mm values, not log values
growth.rkm.fitted$y <- 10^(growth.rkm.fitted$y)
growth.rkm.fitted$ymax <- 10^(growth.rkm.fitted$ymax)
growth.rkm.fitted$ymin <- 10^(growth.rkm.fitted$ymin)


#3. add to plot
growth <- growth +
  geom_ribbon(data = growth.rkm.fitted, inherit.aes = FALSE,
              aes(x = x, ymax = ymax, ymin = ymin),
              fill =  "#48566e", alpha = 0.4) +
  geom_line(data = growth.rkm.fitted, inherit.aes = FALSE,
              aes(x = x, y = y),
              size = 1, color = "#48566e")

growth

ggsave(growth, file = "./output/figures/growth.png",
       dpi = 300, width = 8, height = 8)



d <- data.frame(x=1:100)
d$y <- exp(rnorm(nrow(d), mean=-d$x/40, sd=0.8))

ggplot(d, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth()
g <- ggplot(d, aes(x = x, y = y)) +
  geom_point() +
 # stat_smooth() +
  scale_y_log10() +
  coord_trans(y = scales::exp_trans(10))

g


library(splines)
qplot(x, y, data=d) + stat_smooth(method="glm", family="quasipoisson",
                                      formula = y ~ ns(x, 3))
# calcualte sample size
fish.growth %>%
  filter(!is.na(size.class)) %>%
  group_by(size.class) %>%
  summarise(n = n())


#some exploratory plots
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

#possibilities for stats
#Kologorov-Smirnov test to compare curves
#Or, model fish variables with midge abundance as explanatory variable
