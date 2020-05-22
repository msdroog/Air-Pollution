library(dplyr)
library(reshape2)
library(chron)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(fitdistrplus)

setwd("C:/Users/droog/Documents/Documents/EPFL/S2/APCC")
getwd()

source("GRB001.R.txt")

Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots


#--------------------
# DEFINE FUNCTIONS


ComputeMean <- function(x) {
  ## x is a vector of values
  stats <- c("mean"    = mean(x,na.rm=TRUE))
  data.frame(stat=factor(names(stats), names(stats)), value=stats)
}

ComputeSD <- function(x) {
  ## x is a vector of values
  stats <- c("sd"    = sd(x,na.rm=TRUE))
  data.frame(stat=factor(names(stats), names(stats)), value=stats)
}

ComputeMax <- function(x) {
  ## x is a vector of values
  stats <- c("max"    = max(x,na.rm=TRUE))
  data.frame(stat=factor(names(stats), names(stats)), value=stats)
}

ComputeCumSum <- function(x) {
  ## x is a vector of values
  stats <- c("sum"    = sum(x,na.rm=TRUE))
  data.frame(stat=factor(names(stats), names(stats)), value=stats)
}
Month2Season <- function(month) {
  ## month is an integer (1-12)
  ## a factor with levels {"DJF", "MAM", "JJA", "SON"} is returned
  seasons <- c("DJF", "MAM", "JJA", "SON")
  index <- findInterval(month %% 12, seq(0, 12, 3))
  factor(seasons[index], seasons)
}

Month2Season(c(1, 3, 12))
## [1] DJF MAM DJF
## Levels: DJF MAM JJA SON

ReadTSeries <- function(filename, timecolumn="datetime", timeformat="%d.%m.%Y %H:%M") {
  ## read the table, strip units in column names, rename time column
  ##   and change data type of time column from a string of characters to
  ##   a numeric type so that we can perform operations on it
  data <- read.table(filename, skip=5, header=TRUE, sep=",", check.names=FALSE)
  names(data) <- sub("[ ].*$","",names(data)) # strip units for simplification
  names(data) <- sub("Date/time", timecolumn, names(data), fixed=TRUE)
  data[,timecolumn] <- as.chron(data[,timecolumn], timeformat) - 1/24 # end time -> start time
  ## extract additional variables from the time column
  data[,"year"] <- years(data[,timecolumn])
  data[,"month"] <- months(data[,timecolumn])
  data[,"day"] <- days(data[,timecolumn])
  data[,"hour"] <- hours(data[,timecolumn])
  data[,"dayofwk"] <- weekdays(data[,timecolumn])
  data[,"daytype"] <- ifelse(data[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
  data[,"season"] <- Month2Season(unclass(data[,"month"]))
  ## return value
  data
}

ComputeStats <- function(x) {
  x <- na.omit(x)
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  t <- qt(.975, n - 1)
  data.frame(mean=m,
             conf.lower=m-t*s/sqrt(n),
             conf.upper=m+t*s/sqrt(n),
             sd.lower=m-s,
             sd.upper=m+s)
}

TTest <- function(value, daytype) {
  ## if all values are missing,
  ##   the t-test will raise an error
  wkend <- value[daytype=="Weekend"]
  wkday <- value[daytype=="Weekday"]
  if(length(na.omit(wkend)) > 0 & length(na.omit(wkday)) > 0) {
    out <- t.test(wkend, wkday, alternative="two.sided")
    pval <- out[["p.value"]]
  } else {
    pval <- NA
  }
  pval
}
ReplaceNonpositive <- function(x) {
  x <- na.omit(x)
  min.positive.value <- min(x[x>0])/2
  replace(x, x < min.positive.value, min.positive.value)
}
fit_stat <- function(data,location,substance) {
  hourly <- data %>% filter(site==location & month=="Jul" & variable==substance)
  concvec <- c(na.omit(hourly[["value"]]))
  fit <- fitdist(ReplaceNonpositive(concvec), "lnorm")
  print(fit)
  print(gofstat(fit))            # Goodness Of Fit
  print("p-value: ",quote=FALSE) # Xi^2 statistics
  pval.h <- gofstat(fit)[["chisqpvalue"]]
  print(pval.h)
  # plot statistics
  png_name <- sprintf("q7_%s_%s.png", location, substance) # give filename to png
  w <- 800 # [pixel] define width of plot
  h <- 800 # [pixel] define height of plot
  PlotPNG(fit,png_name,w,h)
  return(pval.h)
}
PlotPNG <- function(myplot,png_name,w,h){
  png(filename = png_name, 
      width = w, height = h, units = "px",
      bg = "white")
  plot(myplot)
  dev.off()
}
AddRow <- function(all_pval,location,substance,pval){
  row_val <- data.frame( location , substance , pval )
  names(row_val) <- c("site", "variable","p_value")
  return(rbind(all_pval, row_val))
}

#--------------------
### LOAD DATA


datapath <- file.path("data")

df <- full_join(cbind(site="PAY", ReadTSeries(file.path(datapath, "PAY.csv"))),
                cbind(site="SIO", ReadTSeries(file.path(datapath, "SIO.csv"))))
## Joining, by = c("site", "datetime", , "SO2" "O3", "NO2", "CO", "PM10", "PM2.5", "TEMP", "PREC", "RAD", "year", "month", "day", "hour", "dayofwk", "daytype", "season")


## Converting to long form
lf <- melt(df, id.vars=c("site", "datetime", "season", "year", "month", "day", "hour", "dayofwk", "daytype"))


#----------------------
### START DATA CLEANING


## Check which are negative
negatives <- lf %>% filter(!is.na(value) & value<0 & variable!="TEMP") %>% group_by(site, variable) # temperature is allowed to be negative
print("The following values are negative: ",quote=FALSE)
print(negatives)

# Take out NA values
lf_notCleaned <- lf
lf <- lf %>% filter(!is.na(value)) %>% group_by(site, variable)

# Find Maxima to check whether they make sense
maxima <- lf %>% group_by(site, variable) %>%
  do(ComputeMax(.[["value"]]))
# comment: all make sense, no change necessary

print("Data is cleaned. Cleaned data is safed as 'lf'.",quote=FALSE)

### END DATA CLEANING
#----------------------
##(2) Concentrations vs limit values

###Plot All Variables#####
ggp <- ggplot(lf)+                                   # `lf` is the data frame
  facet_grid(variable~site, scale="free_y")+         # panels created out of these variables
  geom_line(aes(datetime, value, color=site))+       # plot `value` vs. `time` as lines
  scale_x_chron()+                                   # format x-axis labels (time units)
  theme(axis.text.x=element_text(angle=30, hjust=1)) # rotate x-axis labels
print(ggp)                                           # view the plot                                         

## data frame of only pollutants wiht annual limit values
lf_24hpollutants <- filter(lf, variable == "SO2" | variable == "NO2" | variable == "CO" | variable == "PM10")

lf_24hpollutants$datetime <- as.Date(lf_24hpollutants$datetime, "%m/%d/%Y")
ag <- lf_24hpollutants %>% 
  group_by(site,variable,datetime) %>%
  summarise(mean(value, na.rm = TRUE)) %>%
  rename("mean_value" = "mean(value, na.rm = TRUE)")

limits_24h <- data.frame(variable = c("CO", "NO2", "PM10", "SO2"), Z = c(8, 80, 50, 100)) #set pollutant limit values

###Plot Pollutant Variables#####
ggp_pollutants <- ggplot(ag)+             # `lf` is the data frame
  facet_grid(variable~site, scale="free_y")+         # panels created out of these variables
  geom_line(aes(datetime, mean_value, color=site))+       # plot `value` vs. `time` as lines
  scale_x_chron()+                                   # format x-axis labels (time units)
  theme(axis.text.x=element_text(angle=30, hjust=1))+# rotate x-axis labels
  xlab("Date") +                                     #setting x label
  ylab("Pollutant Concentrations [??g/m^3] except CO in [mg/m^3]") +                 #setting y label
  geom_hline(data = limits_24h, aes(yintercept = Z), color = "red", linetype = "dashed") #add limit values to graphs
print(ggp_pollutants) 

lf_ozone <- filter(lf, variable == "O3")

limits_h_ozone <- data.frame(variable = c("O3"), Z = c(120)) #set pollutant limit values

###Plot Pollutant Variables#####
ggp_ozone <- ggplot(lf_ozone)+             # `lf` is the data frame
  facet_grid(variable~site, scale="free_y")+         # panels created out of these variables
  geom_line(aes(datetime, value, color=site))+       # plot `value` vs. `time` as lines
  scale_x_chron()+                                   # format x-axis labels (time units)
  theme(axis.text.x=element_text(angle=30, hjust=1))+# rotate x-axis labels
  xlab("Date") +                                     #setting x label
  ylab("Pollutant Concentrations [??g/m^3]") +                 #setting y label
  geom_hline(data = limits_h_ozone, aes(yintercept = Z), color = "red", linetype = "dashed") #add limit values to graphs
print(ggp_ozone)

### Annual mean of all variables
annual_mean<-lf %>% 
  group_by(site,variable) %>%
  #summarize(mean_CO=mean(variable == "CO"),mean_NO2=mean(variable == "NO2"),mean_O3=mean(variable == "O3"), mean_PM10=mean(variable == "PM10"),mean_PM2.5=mean(variable == "PM2.5"),mean_SO2=mean(variable == "SO2"))
  summarize(mean(value, na.rm = TRUE))
print(annual_mean)

  
#--------------------
## (3) SEASONAL DIFFERENCES


# Plot All Variables BY MONTH 
ggp <- ggplot(lf) +
  facet_grid(variable ~ site, scale = "free_y") +
  geom_boxplot(aes(month, value), outlier.size = 0.5, outlier.shape = 3)
print(ggp)

# Plot Seasons Boxplot
ggp1 <- ggplot(lf %>% filter( variable!="PREC" & variable!="EC" & variable!="NOX")) +
  facet_grid(variable ~ site, scale = "free_y") +
  geom_boxplot(aes(season, value), outlier.size = 0.5, outlier.shape = 3)
print(ggp1)

# Plot Seasonal Precipitation
ggp2 <- ggplot(lf %>% filter(variable=="PREC")) +
  facet_grid(variable ~ site, scale = "free_y") +
  geom_bar(aes(season, value), stat="sum", show.legend = FALSE) 
print(ggp2)
grid.arrange(ggp1, ggp2)   # library 'gridExtra' needed

# Compute seasonal means
seasonal_means <- lf %>% filter(!is.na(value) & variable!="PREC" & variable!="EC" & variable!="NOX") %>%
  group_by(site, season, variable) %>%
  do(ComputeMean(.[["value"]]))
#write.xlsx(seasonal_means, "c:/Users/corin/Documents/Uni/MA/EPFL/MA-2/Air Pollution/Assignment/seasonal-means.xlsx")   # library 'xlsx' needed
#write.table(seasonal_means, "c:/Users/corin/Documents/Uni/MA/EPFL/MA-2/Air Pollution/Assignment/seasonal-means.txt", sep="\t",dec = ",")
seasonal_prec <- lf %>% filter(!is.na(value) & variable=="PREC") %>%
  group_by(site, season,variable) %>%
  do(ComputeCumSum(.[["value"]]))
#write.table(seasonal_prec, "c:/Users/corin/Documents/Uni/MA/EPFL/MA-2/Air Pollution/Assignment/seasonal-prec.txt", sep="\t",dec = ".")

#--------------------
## (4) Diurnal DIFFERENCES

Percentile <- function(perc) function(x) 
  ## `perc` is the percentile which should be computed for the numeric vector `x`
  quantile(x, perc*1e-2, na.rm=TRUE)

##Payerne overall analysis
ggp <- ggplot(data=lf %>% filter(site=="PAY" & !is.na(value)),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(variable ~ season, scale = "free_y", drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("PAY")
print(ggp)

##Sion overall analysis
ggp <- ggplot(data=lf %>% filter(site=="SIO" & !is.na(value)),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(variable ~ season, scale = "free_y", drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("SIO")
print(ggp)

##Ozone
ggp <- ggplot(data=lf %>% filter(variable=="O3"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("O3")
print(ggp)

##NO2
ggp <- ggplot(data=lf %>% filter(variable=="NO2"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("NO2")
print(ggp)

##SO2
ggp <- ggplot(data=lf %>% filter(variable=="SO2"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("SO2")
print(ggp)

##CO
ggp <- ggplot(data=lf %>% filter(variable=="CO"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("CO")
print(ggp)

##PM10
ggp <- ggplot(data=lf %>% filter(variable=="PM10"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("PM10")
print(ggp)

##PM2.5
ggp <- ggplot(data=lf %>% filter(variable=="PM2.5"),
          mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("PM2.5")
print(ggp)

##NOx
ggp <- ggplot(data=lf %>% filter(variable=="NOX"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("NOx")
print(ggp)

##PREC
ggp <- ggplot(data=lf %>% filter(variable=="PREC"),
              mapping=aes(x=hour, y=value, group=site, color=site)) +
  facet_grid(season ~ dayofwk, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("Prec")
print(ggp)

##EC
ggp <- ggplot(data=lf %>% filter(variable=="EC"),
              mapping=aes(x=hour, y=value, group=daytype, color=daytype)) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("EC")
print(ggp)

##PM10-daily
ggp <- ggplot(data=lf %>% filter(variable=="PM10"),
              mapping=aes(x=hour, y=value, group=site, color=site)) +
  facet_grid(season ~ dayofwk, drop=TRUE) +
  geom_line(stat="summary", fun="median")+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75))+
  ggtitle("PM10")
print(ggp)


#--------------------
## (5) WEEKDAY-WEEKEND MEANS

##Payerne overall analysis
table <- lf %>% filter(site=="PAY" & !variable %in% c("EC", "NOx"))
mystats <- table %>%
  group_by(variable, season, daytype) %>%
  do(ComputeStats(.[["value"]]))
ggp <- ggplot(mystats)+
  facet_grid(variable ~ season, scale = "free_y", drop=TRUE) +
  geom_bar(aes(x=daytype, y=mean, fill=daytype),
           stat="identity")+
  geom_errorbar(aes(x=daytype,
                    ymin=conf.lower,
                    ymax=conf.upper),
                width=0.1)+
  ggtitle("PAY")
print(ggp)

##Sion overall analysis
table <- lf %>% filter(site=="SIO" & !variable %in% c("EC", "NOx"))
mystats <- table %>%
  group_by(variable, season, daytype) %>%
  do(ComputeStats(.[["value"]]))
ggp <- ggplot(mystats)+
  facet_grid(variable ~ season, scale = "free_y", drop=TRUE) +
  geom_bar(aes(x=daytype, y=mean, fill=daytype),
           stat="identity")+
  geom_errorbar(aes(x=daytype,
                    ymin=conf.lower,
                    ymax=conf.upper),
                width=0.1)+
  ggtitle("SIO")
print(ggp)


## T-STAT

#If the p-value is less than alpha=0.05, we can say that we would reject the null hypothesis
#(that on average, the weekend concentrations are the same as weekday concentrations) at the 5% significance level.

# if p is over  0.05 --> weekday and weekend are similar (no significant difference)
# if p is under 0.05 --> weekday and weekend are different

# for Pollutants
pvals <- lf %>%
  filter(!variable %in% c("EC", "NOX", "TEMP", "PREC", "RAD")) %>%
  group_by(site, season, variable) %>%
  summarize(p.value = TTest(value, daytype))
ggplot(pvals) + 
  facet_grid(site ~ season) +
  geom_point(aes(variable, p.value), shape=3, color=4)+
  geom_hline(yintercept = .05, linetype = 2) +
  #scale_y_log10() + 
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="", y="p-value")
#size(800:400)

# for meteorological values
pvals <- lf %>%
  filter(variable %in% c("TEMP", "PREC", "RAD")) %>%
  group_by(site, season, variable) %>%
  summarize(p.value = TTest(value, daytype))
ggplot(pvals) +
  facet_grid(site ~ season) +
  geom_point(aes(variable, p.value), shape=3, color=4)+
  geom_hline(yintercept = .05, linetype = 2) +
  #scale_y_log10() + 
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="", y="p-value")

##Ozone
table <- lf %>% filter(variable=="O3")
mystats <- table %>%
  group_by(site, season, daytype) %>%
  do(ComputeStats(.[["value"]]))
ggp <- ggplot(mystats) +
  facet_grid(site ~ season, drop=TRUE) +
  geom_bar(aes(x=daytype, y=mean, fill=daytype),
           stat="identity")+
  geom_errorbar(aes(x=daytype,
                    ymin=conf.lower,
                    ymax=conf.upper),
                width=0.1)+
  ggtitle("O3")
print(ggp)
# corresponding T-statistics for PAY in spring
# (because there the p-value is not clearly visible in the graph)
(out <- t.test(filter(table, site=="PAY" & season=="MAM" & daytype=="Weekend")[["value"]],
               filter(table, site=="PAY" & season=="MAM" & daytype=="Weekday")[["value"]],
               alternative="greater"))
out[["p.value"]]

pvals <- lf %>%
  filter(variable=="O3") %>%
  group_by(site, season) %>%
  summarize(p.value = TTest(value, daytype))
print(pvals)
#weekly periodicity
ggp <- ggplot(data=lf %>% filter(variable=="O3"),
              #mapping=aes(x=dayofwk, y=value, group=site, color=site)) +
              mapping=aes(x=dayofwk, y=value)) +
  #facet_grid(~ season, drop=TRUE) +
  #geom_line(stat="summary", fun="median")+
  # geom_errorbar(stat="summary",
  #              fun.min=Percentile(25),
  #             fun.max=Percentile(75))+
  facet_grid( site ~ season, scale = "free_y") +
  geom_boxplot(aes(dayofwk, value), outlier.size = 0.5, outlier.shape = 3)
ggtitle("O3")
print(ggp)

#--------------------
## (6) Correlations

dm <- lf %>% group_by(site, year, month, day, season, variable) %>%
  summarize(value=max(value, na.rm=TRUE))
daily.max <- dcast(dm, site + year + month + day + season ~ variable)
rm(dm)

###LATTICE
library(lattice)

#PAY
CorrelationValue <- function(x, y, ...) {
  i <- is.finite(x) & is.finite(y) 
  correlation <- cor(x[i], y[i]) 
  if(is.finite(correlation)) {
    cpl <- current.panel.limits()
    panel.text(mean(cpl$xlim),mean(cpl$ylim),
               bquote(italic(r)==.(sprintf("%.2f",correlation))),
               adj=c(0.5,0.5),col="blue")
  }
}
ix <- grepl("PAY", daily.max[["site"]], fixed=TRUE)
spp <- splom(~daily.max[ix,c("O3","NO2","CO","PM10","TEMP","PREC","RAD")] | daily.max[ix,"season"],
             upper.panel = CorrelationValue,
             pch=4)
print(spp)

#SIO
CorrelationValue <- function(x, y, ...) {
  i <- is.finite(x) & is.finite(y) 
  correlation <- cor(x[i], y[i]) 
  if(is.finite(correlation)) {
    cpl <- current.panel.limits()
    panel.text(mean(cpl$xlim),mean(cpl$ylim),
               bquote(italic(r)==.(sprintf("%.2f",correlation))),
               adj=c(0.5,0.5),col="blue")
  }
}
ix <- grepl("SIO", daily.max[["site"]], fixed=TRUE)
spp <- splom(~daily.max[ix,c("O3","NO2","PM10","TEMP","PREC","RAD")] | daily.max[ix,"season"],
             upper.panel = CorrelationValue,
             pch=4)
print(spp)

###Ozone-Temp
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(TEMP, O3))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(TEMP, O3, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(0,1))+
  facet_grid(.~site)
print(ggp)

###Ozone-Rad
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(RAD, O3))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(RAD, O3, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(0,1))+
  facet_grid(.~site)
print(ggp)

###Temp-Rad
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(RAD, TEMP))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(RAD, TEMP, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(0,1))+
  facet_grid(.~site)
print(ggp)

###PM10-Ozone
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(PM10, O3))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(PM10, O3, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(-1,1))+
  facet_grid(.~site)
print(ggp)

###Temp-PM10
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(PM10, TEMP))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(PM10, TEMP, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(-1,1))+
  facet_grid(.~site)
print(ggp)

###NO2-PM10
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(PM10, NO2))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(PM10, NO2, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(0,1))+
  facet_grid(.~site)
print(ggp)

###NO2-Ozone
#Plot
ggp <- ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(O3, NO2))
print(ggp)
#Correlation value
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(O3, NO2, use="pairwise.complete.obs")))
ggp <- ggplot(cor.values)+
  geom_bar(aes(season, correlation), stat="identity")+
  scale_y_continuous(limits=c(-1,1))+
  facet_grid(.~site)
print(ggp)

##Lagged PAY Rad -> Ozone
ix <- df[["site"]]=="PAY"
ccf(df[ix,"RAD"], df[ix,"O3"], lag.max=6, na.action=na.pass)

##Lagged PAY Rad -> Temp
ix <- df[["site"]]=="PAY"
ccf(df[ix,"RAD"], df[ix,"TEMP"], lag.max=6, na.action=na.pass)

#--------------------
## (7) LOG-NORMAL DISTRIBUTION

# create output table for p-values
all_pval  <-data.frame(site=character(),variable=character(),p_value=double(), stringsAsFactors=FALSE) # defining the column names
all_pval # print in console


# PAY
location   <- "PAY"
substances <- c( "O3", "NO2", "CO","SO2", "PM10", "PM2.5","EC","NOX")
for (val in substances) {
  all_pval <- AddRow(all_pval,location,val,fit_stat(lf,location,val))
}
print(all_pval)


# SIO
location   <- "SIO"
substances <- c( "O3", "NO2", "PM10", "PM2.5","NOX")
for (val in substances) {
  all_pval <- AddRow(all_pval,location,val,fit_stat(lf,location,val))
}
print(all_pval)

#--------------------
## (8) Anomalies
#select unusual pollutant and place
NO2.sio <- filter(lf, site=="SIO" & variable=="NO2")
NO2.sio[["date"]] <- format(dates(NO2.sio[["datetime"]]), "y.m.d")
NO2.sio[["day"]] <- as.numeric(as.character(NO2.sio[["day"]]))
tail(NO2.sio)
tail(NO2.sio.wind)

#Time series
ggp <- ggplot(mutate(NO2.sio, day.of.month=day+hour/24))+
  geom_line(aes(day.of.month, value))+
  facet_grid(.~month)+
  xlab("Day")+
  ylab(expression("NO"[2]~"concentration"~(mu*g/m^3)))
print(ggp)

#daily means during year
daily <- NO2.sio %>%
  group_by(date, variable) %>%
  summarize(month=month[1],
            day=day[1],
            value=mean(value,na.rm=TRUE))
ggp <- ggplot(daily)+
  geom_line(aes(day, value))+
  facet_grid(.~month)+
  xlab("Day")+
  ylab(expression("NO"[2]~"concentration"~(mu*g/m^3)))
print(ggp)

#daily menas PDF
ggp <- ggplot(daily)+
  geom_line(aes(x=value), stat="density", position="identity")+
  geom_point(aes(x=value), y=0, shape=4)+
  geom_vline(xintercept=80, linetype=2)+
  xlab(expression("Daily Mean NO"[2]~"concentration"~(mu*g/m^3)))+
  ylab("Probability density")
print(ggp)

#concentration was greater than 120 ??g/m3
(highvals <- filter(NO2.sio, value >120))
highdays <- filter(NO2.sio, date %in% highvals[["date"]])
ggp <- ggplot(highdays, aes(hour, value))+
  geom_line()+
  geom_point()+
  facet_grid(.~date)+
  xlab("Hour of day")+
  ylab(expression("NO"[2]~"concentration"~(mu*g/m^3)))
print(ggp)
#mean daily
highdays %>% group_by(date, variable) %>%
  summarize(value=mean(value))

#add wind to data
ReadMet <- function(filename) {
  data <- read.table(filename, skip=15, col.names=c("year", "month", "day", "hour", "minute", "WIGE", "WIRI"))
  data %>%
    mutate(datetime = as.chron(paste(year, month, day, hour, minute), "%Y %m %d %H %M"),
           year     = years(datetime),
           month    = months(datetime),
           day      = days(datetime),
           hour     = hours(datetime),
           minute   = minutes(datetime),
           WIGE     = ifelse(WIGE <= -9999, NA, WIGE),
           WIRI     = ifelse(WIRI <= -9999, NA, WIRI))
}
datapath <- file.path("data")
met <- ReadMet(file.path(datapath, "SIO_Wind_MM10_19.txt"))
# wind speed and direction
color <- hsv(seq(0, 360) / 360, 1, 1)
angle <- seq(0, 360, 60)
met_dec <- filter(met, month == "Dec" & minute== "0")
ggp2 <- ggplot(met_dec, aes(hour, value))+
  geom_line(aes(hour, WIGE), color="gray")+
  geom_point(aes(hour, WIGE, color=WIRI))+
  scale_color_gradientn(colors = color, breaks=angle, limits=range(angle), expand=c(0, 0))+
  facet_wrap(.~day)+
  ylab("WIGE")
print(ggp2)

#multivariable correlation
NO2.sio.DJF <- filter(lf, site=="SIO" & season=="DJF")
variables <- c("O3", "NO2", "PM10","PM2.5","NOX", "TEMP", "PREC", "RAD")
lf <- melt(NO2.sio.DJF, measure.vars=variables)
dm <- lf %>% group_by(site, year, month, day, season, variable) %>%
  summarize(value=max(value, na.rm=TRUE))
daily.max <- dcast(dm, site + year + month + day + season ~ variable)
lf <- melt(daily.max, measure.vars=setdiff(variables, "NO2"))
ggp <- ggplot(lf)+
  facet_grid(.~variable, scale="free_x")+
  geom_point(aes(NO2, value), shape=4)
print(ggp)
(cor.values <- lf %>% group_by(variable) %>%
    summarize(correlation=cor(NO2, value, use="pairwise.complete.obs")))
