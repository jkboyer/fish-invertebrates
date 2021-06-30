#Makes figure with one week of Glen Canyon Dam discharge to show bug flows
#inputs: data downloaded from USGS Lees Ferry gauge using dataRetreival package
#outputs: ./output/figures/bug_flows.png figure, formatted for AFS poster
#dependencies: "./functions/jkb_ggplot_theme.r" custome ggplot theme

require(dataRetrieval) #to get USGS streamflow data from internet
require(tidyverse) #ggplot, dplyr, tidyr functions used
require(scales) # for comma formatting on axis labels

source("./functions/jkb_ggplot_theme.r")

#load data that can be downloaded from USGS NWIS website

#below will take a minute to run - but console will not display usual stop sign
#that it shows when code is chugging through a big dataset
w <- readNWISdata(siteNumbers = "09380000", #lees ferry
  parameterCd = c("00010", "00060"),
  startDate = "2018-05-06",
  endDate = "2018-05-16",
  service = "uv")
head(w)


names(w)

w <- w %>%
  transmute(datetime = dateTime,
         temp.C = X_00010_00000,
         discharge.cfs = X_00060_00000)
str(w)

w <- w %>%
  mutate(dow = format(datetime, "%a"),
         dom = as.numeric(format(datetime, "%d")))

w <- w %>%
  filter(dom >= 8 & dom <= 15)

df.label <- data.frame(label=c("Weekdays: typical hydropeaking \noperations generate revenue \nfor dam",
                               "Weekends: Steady low flows \nprevent egg  dessication, \nimprove insect production"),
  x = as.POSIXct(c("2018-05-08 09:00:00", "2018-05-12 04:00:00")),
                 y = c(8000, 8000))

p.bug.flows <- ggplot(w, aes(x = datetime, y = discharge.cfs)) +
  geom_line(color = "#576885") +
  geom_area(fill = "#576885", alpha = 0.5) +
  #and the geom_text method of adding text
#  geom_text(data = df.label, inherit.aes = FALSE,
   #         aes(label = label, x = x, y = y),
   #         size = 4.5,
  #          vjust = 1, hjust = 0) +
  theme_jkb(base_family = "sans", base_size = 20) +
  coord_cartesian(ylim = c(0, 16000), expand = FALSE) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  scale_y_continuous(expression(Discharge ~~(ft^3/s)),
                     labels = comma,
                     sec.axis = sec_axis(~ ./35.314666212661,
                                         name = expression(Discharge ~~(m^3/s)))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = 0),
        axis.text.y.left = element_text(angle = 90),
        axis.text.y.right = element_text(angle = 270),
        panel.grid.minor.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.background = element_blank())

p.bug.flows

ggsave(p.bug.flows, file = "./output/figures/bug_flows.png",
       dpi = 300, width = 10, height = 4)

