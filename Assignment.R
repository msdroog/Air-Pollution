library(dplyr)
library(reshape2)
library(chron)
library(ggplot2)

setwd("C:/Users/droog/Documents/Documents/EPFL/S2/APCC")
getwd()

source("GRB001.R.txt")

Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots


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

datapath <- file.path("data")

df <- full_join(cbind(site="PAY", ReadTSeries(file.path(datapath, "PAY.csv"))),
                cbind(site="SIO", ReadTSeries(file.path(datapath, "SIO.csv"))))
## Joining, by = c("site", "datetime", , "SO2" "O3", "NO2", "CO", "PM10", "PM2.5", "TEMP", "PREC", "RAD", "year", "month", "day", "hour", "dayofwk", "daytype", "season")


## Converting to long form
lf <- melt(df, id.vars=c("site", "datetime", "season", "year", "month", "day", "hour", "dayofwk", "daytype"))

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

  

