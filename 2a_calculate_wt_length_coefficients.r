#Calculates weight length relationships for all fish species with n >= 20
#using equation W = aL^b (logtransformed: logW = a + b(logL))
#saves coefficients (slope (b) and intercept(a)) as .csv
#inputs: fish.csv AGFD grand canyon fish data
#        fish.csv from Lees Ferry R project - more AGFD fish data!
#        fish_weights.csv NO/JCM/TRGD data, weights on fish < 150mm
#outputs: ./data/clean/length_wt_coefficients.csv

#NOTE: code is slow. Lots of looping through a 300k line dataframe

# load data, packages, clean data #####
require(ggplot2)
require(dplyr)

w <- read.csv("./data/all_weights.csv", stringsAsFactors = FALSE)

n <- summarise(group_by(w, SPECIES_CODE), n = length(SPECIES_CODE))
w <- merge(w, n, by = "SPECIES_CODE", all.x = TRUE)
w <- w[w$n >= 10,]

#visualize data ####
theme_set(theme_minimal()) #I dislike the default ggplot theme

#see what data looks like
ggplot(w, aes(x = FORK_LENGTH, y = WEIGHT)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~SPECIES_CODE, scales = "free")

#see what data looks like
ggplot(w, aes(x = TOTAL_LENGTH, y = WEIGHT)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~SPECIES_CODE, scales = "free")

#log transformed
#if looks linear, can fit log transformed data to lm()
ggplot(w, aes(x = log10(FORK_LENGTH), y = log10(WEIGHT))) +
  geom_point(alpha = 0.01) +
  facet_wrap(~SPECIES_CODE)

#SEE WHAT sample sizes are like for FL vs. TL
summarize(group_by(w, SPECIES_CODE),
          n.FL = sum(is.na(FORK_LENGTH) == FALSE),
          n.TL = sum(is.na(TOTAL_LENGTH) == FALSE))

# calculate wt length equations, for outlier removal ######

#blank dataframe to store wt:length model coefficient values for each species
wt.equations <- data.frame(SPECIES_CODE = factor(),
                           FL.intercept = as.numeric(),
                           FL.slope = as.numeric(),
                           TL.intercept = as.numeric(),
                           TL.slope = as.numeric())

# loop to calculate weight length equation for all species
species.list <- unique(w$SPECIES_CODE) #list of species w weight data
species.list
for (i in seq_along(species.list)) {

  print(species.list[[i]]) #print species code

  #fit weight length model: Fork
  # W = aL^b (transformed: log10(W) =
  lm.weight.FL <- lm(log10(WEIGHT) ~ log10(FORK_LENGTH),
                     data = w[w$SPECIES_CODE == species.list[i],])
  print(summary(lm.weight.FL))

  #define max length for species
  maxFL <- max(w$FORK_LENGTH[w$SPECIES_CODE == species.list[i]], na.rm = TRUE)

  #dataframe to plot model fit
  model.line <- data.frame(FORK_LENGTH = 20:maxFL,
                           WEIGHT = 10^(predict(lm.weight.FL,
                                                list(FORK_LENGTH = 20:maxFL))))
  #plot to check fit of model
  print(ggplot(w[w$SPECIES_CODE == species.list[i],],
               aes(x = FORK_LENGTH, y = WEIGHT)) +
          geom_point(alpha = 0.5) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]) +
          geom_line(data = model.line, aes(x = FORK_LENGTH, y = WEIGHT),
                    color = "red"))

  #fit weight length model: TOTAL
  lm.weight.TL <- lm(log10(WEIGHT) ~ log10(TOTAL_LENGTH),
                     data = w[w$SPECIES_CODE == species.list[i],])
  print(summary(lm.weight.TL))

  maxTL <- max(w$TOTAL_LENGTH[w$SPECIES_CODE == species.list[i]], na.rm = TRUE)

  model.line <- data.frame(TOTAL_LENGTH = 20:maxTL,
                           WEIGHT = 10^(predict(lm.weight.TL,
                                                list(TOTAL_LENGTH = 20:maxTL))))

  print(ggplot(w[w$SPECIES_CODE == species.list[i],],
               aes(x = TOTAL_LENGTH, y = WEIGHT)) +
          geom_point(alpha = 0.5) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]) +
          geom_line(data = model.line, aes(x = TOTAL_LENGTH, y = WEIGHT),
                    color = "green"))

  #extract coefficients
  coef.i <- data.frame(SPECIES_CODE = species.list[[i]],
                       FL.intercept = summary(lm.weight.FL)$coef["(Intercept)", "Estimate"],
                       FL.slope = summary(lm.weight.FL)$coef["log10(FORK_LENGTH)", "Estimate"],
                       TL.intercept = summary(lm.weight.TL)$coef["(Intercept)", "Estimate"],
                       TL.slope = summary(lm.weight.TL)$coef["log10(TOTAL_LENGTH)", "Estimate"])

  wt.equations <- bind_rows(wt.equations, coef.i) #bind into one dataframe

}

w <- merge(w, wt.equations , by = "SPECIES_CODE", all.x = TRUE)

#calculate predicted weight
w$pred.wt.fl <- 10^(w$FL.intercept + w$FL.slope*log10(w$FORK_LENGTH))
w$pred.wt.tl <- 10^(w$TL.intercept + w$TL.slope*log10(w$TOTAL_LENGTH))

w$kn.fl <- w$WEIGHT/w$pred.wt.fl #condition
w$kn.tl <- w$WEIGHT/w$pred.wt.tl

# remove outliers #####
#define outlier thresholds: outliers will be removed eventually
#adjust based on graphs to find level that removes measurement errors
w$outlier.fl <- ifelse((w$FORK_LENGTH < 150 & w$kn.fl >= 0.4 & w$kn.fl <= 2.0) |
                         (w$kn.fl >= 0.7 & w$kn.fl <= 1.7), "normal",
                       "outlier")
w$outlier.tl <- ifelse((w$TOTAL_LENGTH < 150 & w$kn.tl >= 0.4 & w$kn.tl <= 2.0) |
                         (w$kn.tl >= 0.7 & w$kn.tl <= 1.7), "normal",
                       "outlier")

# loop to see if outliers are at correct thresholds
species.list <- unique(wt.equations$SPECIES_CODE) #list of species w weight data
for (i in seq_along(species.list)) {

  #plot to check fit of model
  print(ggplot(w[w$SPECIES_CODE == species.list[i],],
               aes(x = FORK_LENGTH, y = WEIGHT, color = outlier.fl)) +
          geom_point(alpha = 0.5) +
          scale_color_manual(values = c("green", "red", "transparent")) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]) +
          labs(title = "fork"))

  print(ggplot(w[w$SPECIES_CODE == species.list[i],],
               aes(x = TOTAL_LENGTH, y = WEIGHT, color = outlier.tl)) +
          geom_point(alpha = 0.5) +
          scale_color_manual(values = c("green", "red", "transparent")) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]) +
          labs(title = "total"))
}

#Remove outliers
w$TOTAL_LENGTH <- ifelse(w$outlier.tl == "outlier", NA, w$TOTAL_LENGTH)
w$FORK_LENGTH <- ifelse(w$outlier.fl == "outlier", NA, w$FORK_LENGTH)

#look at sample sizes w/ outliers gone
n <- summarize(group_by(w, SPECIES_CODE),
               n.FL = sum(is.na(FORK_LENGTH) == FALSE),
               n.TL = sum(is.na(TOTAL_LENGTH) == FALSE))

#calculate wt.length equations for real #####

w <- w[, 1:6] #remove old, outlier influenced values

#blank dataframe to store wt:length model coefficient values for each species
wt.equations <- data.frame(SPECIES_CODE = factor(),
                           FL.intercept = as.numeric(),
                           FL.slope = as.numeric(),
                           TL.intercept = as.numeric(),
                           TL.slope = as.numeric())

# loop to calculate weight length equation for all species
species.list <- unique(w$SPECIES_CODE) #list of species w weight data
for (i in seq_along(species.list)) {

  print(species.list[[i]]) #print species code

  #fit weight length model: Fork
  # W = aL^b (transformed: log10(W) =
  lm.weight.FL <- lm(log10(WEIGHT) ~ log10(FORK_LENGTH),
                     data = w[w$SPECIES_CODE == species.list[i],])
  print(summary(lm.weight.FL))

  #define max length for species
  maxFL <- max(w$FORK_LENGTH[w$SPECIES_CODE == species.list[i]], na.rm = TRUE)

  #dataframe to plot model fit
  model.line <- data.frame(FORK_LENGTH = 20:maxFL,
                           WEIGHT = 10^(predict(lm.weight.FL,
                                                list(FORK_LENGTH = 20:maxFL))))
  #plot to check fit of model
  print(ggplot(w[w$SPECIES_CODE == species.list[i],],
               aes(x = FORK_LENGTH, y = WEIGHT)) +
          geom_point(alpha = 0.5) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]) +
          geom_line(data = model.line, aes(x = FORK_LENGTH, y = WEIGHT),
                    color = "red"))

  #fit weight length model: TOTAL
  lm.weight.TL <- lm(log10(WEIGHT) ~ log10(TOTAL_LENGTH),
                     data = w[w$SPECIES_CODE == species.list[i],])
  print(summary(lm.weight.TL))

  maxTL <- max(w$TOTAL_LENGTH[w$SPECIES_CODE == species.list[i]], na.rm = TRUE)

  model.line <- data.frame(TOTAL_LENGTH = 20:maxTL,
                           WEIGHT = 10^(predict(lm.weight.TL,
                                                list(TOTAL_LENGTH = 20:maxTL))))

  print(ggplot(w[w$SPECIES_CODE == species.list[i],],
               aes(x = TOTAL_LENGTH, y = WEIGHT)) +
          geom_point(alpha = 0.5) +
          annotate("text", x = 0, y = Inf, hjust = -0.2, vjust = 1, size = 8,
                   label = species.list[i]) +
          geom_line(data = model.line, aes(x = TOTAL_LENGTH, y = WEIGHT),
                    color = "green"))

  #extract coefficients
  coef.i <- data.frame(SPECIES_CODE = species.list[[i]],
                       FL.intercept = summary(lm.weight.FL)$coef["(Intercept)", "Estimate"],
                       FL.slope = summary(lm.weight.FL)$coef["log10(FORK_LENGTH)", "Estimate"],
                       FL.df = summary(lm.weight.FL)$df[2],
                       TL.intercept = summary(lm.weight.TL)$coef["(Intercept)", "Estimate"],
                       TL.slope = summary(lm.weight.TL)$coef["log10(TOTAL_LENGTH)", "Estimate"],
                       TL.df = summary(lm.weight.TL)$df[2])

  wt.equations <- bind_rows(wt.equations, coef.i) #bind into one dataframe

}

#Remove FL coefficients for species with small sample sizes (df < 20) and
#carp (TL sample size is 5x FL sample size, better to use TL model)
#TL model will be better for these species
#wt.equations$FL.intercept <- ifelse(wt.equations$FL.df <= 20|
#                                      wt.equations$SPECIES_CODE == "CRP", NA,
#                                    wt.equations$FL.intercept)
#wt.equations$FL.slope <- ifelse(wt.equations$FL.df <= 20 |
#                                  wt.equations$SPECIES_CODE == "CRP", NA,
#                                wt.equations$FL.slope)

wt.equations$FL.df <- NULL #no longer needed, remove
wt.equations$TL.df <- NULL

#For species with very small n (cannot fit model), assign coefficients from model
#for similar species. These estimates aren't perfect, but most estimated weights
#will be for small fish, 0.1g vs. 0.2g won't skew biomass estimates much
#FMS coefficients for catostomids and unidentified fish (these are probably FMS)
fms.ish <- data.frame(SPECIES_CODE = c("SUC", "UID", "FRH", "BFH", "RBS"),
                      FL.intercept = wt.equations$FL.intercept[wt.equations$SPECIES_CODE == "FMS"],
                      FL.slope = wt.equations$FL.slope[wt.equations$SPECIES_CODE == "FMS"],
                      TL.intercept = wt.equations$TL.intercept[wt.equations$SPECIES_CODE == "FMS"],
                      TL.slope = wt.equations$TL.slope[wt.equations$SPECIES_CODE == "FMS"])

#FHM for other small bodied fish
fhm.ish <- data.frame(SPECIES_CODE = c("PKF", "MOS", "RSH"),
                      FL.intercept = wt.equations$FL.intercept[wt.equations$SPECIES_CODE == "FHM"],
                      FL.slope = wt.equations$FL.slope[wt.equations$SPECIES_CODE == "FHM"],
                      TL.intercept = wt.equations$TL.intercept[wt.equations$SPECIES_CODE == "FHM"],
                      TL.slope = wt.equations$TL.slope[wt.equations$SPECIES_CODE == "FHM"])

#rename centrachids to species
centrarchids <- data.frame(SPECIES_CODE = centrarchid,
                           FL.intercept = wt.equations$FL.intercept[wt.equations$SPECIES_CODE == "centrarchid"],
                           FL.slope = wt.equations$FL.slope[wt.equations$SPECIES_CODE == "centrarchid"],
                           TL.intercept = wt.equations$TL.intercept[wt.equations$SPECIES_CODE == "centrarchid"],
                           TL.slope = wt.equations$TL.slope[wt.equations$SPECIES_CODE == "centrarchid"])

#rename catfish to species
catfishes <- data.frame(SPECIES_CODE = catfish,
                        FL.intercept = wt.equations$FL.intercept[wt.equations$SPECIES_CODE == "catfish"],
                        FL.slope = wt.equations$FL.slope[wt.equations$SPECIES_CODE == "catfish"],
                        TL.intercept = wt.equations$TL.intercept[wt.equations$SPECIES_CODE == "catfish"],
                        TL.slope = wt.equations$TL.slope[wt.equations$SPECIES_CODE == "catfish"])


wt.equations <- bind_rows(wt.equations, fms.ish, fhm.ish, catfishes, centrarchids)
wt.equations <- wt.equations[wt.equations$SPECIES_CODE != "catfish" &
                               wt.equations$SPECIES_CODE != "centrarchid",]
#save weight equations #####
#can use this file to merge w/ fish file and calculate kn, pred.wt etc.
write.csv(wt.equations, "./data/clean/length_wt_coefficients.csv",
          row.names = FALSE)
