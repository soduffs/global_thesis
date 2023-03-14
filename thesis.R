# Clear environment
rm(list = ls())

# set working directory
setwd("/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/Honors_2022/O'Duffy/data")

# Loading packages
library(tidyverse)
library(zoo)
library(mFilter)
library(tvReg)
library(stargazer)


#### data import ####
# Reading in data
oecd_gdp = read_csv('gdp_long.csv')
View(oecd_gdp)

# install old version of OECD (0.2.5 is bugged)
# library("devtools")
# devtools::install_version("OECD", version = "0.2.4", repos = "https://stat.ethz.ch/CRAN")

# using package
library("OECD")
dataset_list <- get_datasets() # get data list
search_dataset("quarterly national", data = get_datasets()) # search data list (QNA, MEI, etc.)
search_dataset("MEI", data = get_datasets())

# short-term interest rate
dataset = "MEI_FIN"
dstruc <- get_data_structure(dataset)
str(dstruc, max.level=1)
filter_list <- list("IR3TIB", "",  "Q")
mei_fin <- get_dataset(dataset = dataset, filter = filter_list)
countries_mei_fin <- unique(mei_fin$LOCATION)
mei_fin <- mei_fin %>% rename(short_rate = obsValue) %>% select("LOCATION", "obsTime", "short_rate")

# historical GDP expenditure approach
dataset = "QNA"
dstruc <- get_data_structure(dataset)
str(dstruc, max.level=1)
dstruc$SUBJECT
filter_list <- list("", "B1_GE", "VPVOBARSA", "Q")
qna <- get_dataset(dataset = dataset, filter = filter_list)
countries_qna <- unique(qna$LOCATION)
qna <- qna %>% rename(gdp = obsValue) %>% select("LOCATION", "obsTime", "gdp")

# CPI
dataset = "MEI"
dstruc <- get_data_structure(dataset)
str(dstruc, max.level=1)
dstruc$VAR_DESC
# filter_list <- list("", "CPALTT01","IXOBSA","Q")
filter_list <- list("", "CPALTT01","GY","Q")
mei <- get_dataset(dataset = dataset, filter = filter_list)
countries_mei <- unique(mei$LOCATION)
mei <- mei %>% rename(cpi = obsValue) %>% select("LOCATION", "obsTime", "cpi")


# plot all of the short-rates across time 
library(zoo)
mei_fin$Date <- as.Date(as.yearqtr(mei_fin$obsTime, format = "%Y-Q%q"))
ggplot(mei_fin, aes(x=Date,y=short_rate, color = LOCATION)) + geom_line() +
  theme_classic()

# merge
data_all <- inner_join(mei_fin,mei,by=c("LOCATION","obsTime"))
data_all <- inner_join(data_all,qna, by=c("LOCATION", "obsTime"))





#### HP-Filter to get output gap for each country ####
countries_all <- unique(data_all$LOCATION) # get list of each country
library(mFilter) # mFilter has hpfilter

data_gap <- data_all # copy data_all
data_gap$gap <- NA # preallocate column for outputgap
data_gap$potgdp <- NA # preallocate column for potential gdp
for (i in countries_all) {
  data_temp <- data_all %>% filter(LOCATION==i) # trim data to only one country
  data_temp <- arrange(data_temp,obsTime) # sort by Date
  hp_temp <- hpfilter(log(data_temp$gdp), freq=1600) # get trend and cycle
  # data_temp$gap <- ((hp_temp$cycle + hp_temp$trend)/hp_temp$trend -1) * 100 # compute the output gap from HP filtered data
  data_temp$gap <- 100*hp_temp$cycle
  data_temp$LOCATION <- i
  data_gap[data_gap$LOCATION==i,]$gap <- data_temp$gap # replace output gap for country i
  data_gap[data_gap$LOCATION==i,]$potgdp <- exp(hp_temp$trend[,1]) # replace potgdp for country i with trend
  print(i)
  }
View(data_gap)


#### lagged interest rate and tvLM for each country ####

# create lagged interest rate
data_final <- data_gap %>%
  group_by(LOCATION) %>%
  mutate(short_lag = lag(short_rate, n=1, default=NA))

# estimate tvLM for each country
countries_final <- unique(data_final$LOCATION)
data_coef <- data_final # copy data_all
data_coef$binf <- NA # preallocate column for inflation coef
data_coef$bgdp <- NA # " " for gdp coef
data_coef$brate <- NA # " " for nominal interest rate
# what about r*?: NULL==cross validation
library(tvReg)
for (i in countries_final){
  data_temp <- data_final %>% filter(LOCATION==i)
  tv_reg <- tvLM(short_rate ~ short_lag + cpi + gap, data = data_temp, bw=NULL)
  cpi_longrun <- (tv_reg$coefficients[,3] / (1 - tv_reg$coefficients[,2]))
  gdp_longrun <- (tv_reg$coefficients[,4] / (1 - tv_reg$coefficients[,2]))
  data_temp$LOCATION <- i
  data_coef[data_coef$LOCATION==i,]$binf <- c(NA,cpi_longrun) 
  data_coef[data_coef$LOCATION==i,]$bgdp <- c(NA,gdp_longrun) 
  data_coef[data_coef$LOCATION==i,]$brate <- c(NA,tv_reg$coefficients[,2])
}
  
# plot binf (inflation coef) worked
# get this 
data_coef$Date <- as.Date(as.yearqtr(data_gap$obsTime, format = "%Y-Q%q"))
ggplot(data_coef, aes(x=Date,y=binf, color = LOCATION)) + geom_line() +
  theme_classic()
# median binf
data_coef_check <- data_coef %>% na.omit()
median_binf <- median(data_coef_check$binf)

# dummy check: plot usa output gap. does it look reasonable
data_check <- data_final %>% filter(LOCATION=="USA")

# gap plot
data_gap$Date <- as.Date(as.yearqtr(data_gap$obsTime, format = "%Y-Q%q"))
ggplot(data_check, aes(x=Date,y=potgdp, color= LOCATION)) +
  geom_line() + geom_line(aes(x=Date,y=gdp), color='blue') +
  theme_classic()

ggplot(data_check) +
  geom_line(aes(x=Date,y=cpi), color='navy') +
  ggtitle("CPI, United States")+
  theme_classic()

# median output gap? plot median by weight of GDP?




#### Estimate US Taylor Rule ####
reg <- lm(short_rate ~ cpi + short_lag + gap, data = data_check)
summary(reg)
reg$coefficients
# long-run response to inflation
cpi_longrun <- reg$coefficients[2] / (1 - reg$coefficients[3])

# time-varying coefficients (BW=discipline on coefficient;NULL=cross validation)
library(tvReg)
tv_reg <- tvLM(short_rate ~ short_lag + cpi + gap, data = data_check, bw=NULL)
tv_reg <- tvAR(data_check$short_rate, p=1, exogen = cbind(data_check$cpi, data_check$gap), bw=NULL)

cpi_longrun <- (tv_reg$coefficients[,3] / (1 - tv_reg$coefficients[,2]))
gdp_longrun <- (tv_reg$coefficients[,4] / (1 - tv_reg$coefficients[,2]))
ggplot() +
  geom_line(aes(x=data_check[2:77,]$Date,y=cpi_longrun),color="red") +
  geom_line(aes(x=data_check[2:77,]$Date,y=gdp_longrun),color="blue") +
  geom_hline(aes(yintercept=1),linetype="dashed") +
  theme_classic()

# look at single country's result in data_coef
data_coef_check <- data_coef %>% filter(LOCATION=="USA")
ggplot() +
  geom_line(aes(x=data_coef_check$Date,y=data_coef_check$binf*(1-data_coef_check$brate)),color="red") +
  geom_line(aes(x=data_coef_check$Date,y=data_coef_check$brate),color="blue") +
  geom_hline(aes(yintercept=1),linetype="dashed") +
  ggtitle("USA Inflation and Short-Rate Coefficients") +
  xlab("Time") +
  ylab("Value") +
  theme_classic()

# find median binf by date since 1962
data_coef_med <- data_coef %>% 
  filter(Date > as.Date('1962-10-01')) %>%
  group_by(Date) %>% 
  summarize(binf_med = median(binf, na.rm=T), binf_sd = 1/sd(binf,na.rm=T), N = sum(!is.na(cpi)))

# median since 1975
data_coef_med <- data_coef %>% 
  filter(Date > as.Date('1975-01-01')) %>%
  group_by(Date) %>% 
  summarize(binf_med = median(binf, na.rm=T), binf_sd = 1/sd(binf,na.rm=T), N = sum(!is.na(cpi)))

# median since 1980
data_coef_med <- data_coef %>% 
  filter(Date > as.Date('1980-01-01')) %>%
  group_by(Date) %>% 
  summarize(binf_med = median(binf, na.rm=T), binf_sd = 1/sd(binf,na.rm=T), N = sum(!is.na(cpi)))


# plots for data.frame in further section

#### sum stats for each variable, entire sample INTL ####
# all countries, group_by soques suggestion 

# binf
binf_summary <- data_coef %>% group_by(LOCATION) %>% summarize(mean_binf=mean(binf, na.rm=T), sd_binf = round(sd(binf, na.rm=T),2))

summary(binf_summary)




binf_sd <- data_coef %>% group_by(LOCATION) %>%  summarize(sd_binf = round(sd(binf, na.rm=T),2))

summary(binf_sd)

binf_med <- data_coef %>% group_by(LOCATION) %>%  summarize(med_binf = median(binf, na.rm=T))
summary(binf_med)

# CPI
cpi_summary <- data_coef %>% group_by(LOCATION) %>% summarize(cpi, na.rm=T)
summary(cpi_summary)

cpi_sd <- data_coef %>% group_by(LOCATION) %>%  summarize(sd_cpi = sd(cpi, na.rm=T))
summary(cpi_sd)

# bgdp
bgdp_summary <- data_coef %>% group_by(LOCATION) %>% summarize(bgdp, na.rm=T)
summary(bgdp_summary)

bgdp_sd <- data_coef %>% group_by(LOCATION) %>%  summarize(sd_bgdp = sd(bgdp, na.rm=T))
summary(bgdp_sd)

# y
gap_summary <- data_coef %>% group_by(LOCATION) %>% summarize(gap, na.rm=T)
summary(gap_summary)

gap_sd <- data_coef %>% group_by(LOCATION) %>%  summarize(sd_gap = sd(gap, na.rm=T))
summary(gap_sd)

# brate ? same as rho term?
brate_summary <- data_coef %>% group_by(LOCATION) %>% summarize(brate, na.rm=T)
summary(brate_summary)

brate_sd <- data_coef %>% group_by(LOCATION) %>%  summarize(sd_brate = sd(brate, na.rm=T))
summary(brate_sd)

# short_rate
short_rate_summary <- data_coef %>% group_by(LOCATION) %>% summarize(short_rate, na.rm=T)
summary(short_rate_summary)

short_rate_sd <- data_coef %>% group_by(LOCATION) %>%  summarize(sd_short_rate = sd(short_rate, na.rm=T))
summary(short_rate_sd)

#### sum stats for variables, each country across time DOM ####
# united states
usa_summary <- data_coef %>% group_by(LOCATION=="USA")
summary(usa_summary)

# australia
aus_summary <- data_coef %>% group_by(LOCATION=="AUS")
summary(aus_summary)

# austria
aut_summary <- data_coef %>% group_by(LOCATION=="AUT")
summary(aut_summary)

# belarus
bel_summary <- data_coef %>% group_by(LOCATION=="BEL")
summary(aus_summary)

# canada
can_summary <- data_coef %>% group_by(LOCATION=="CAN")
summary(can_summary)

# czech republic
cze_summary <- data_coef %>% group_by(LOCATION=="CZE")
summary(cze_summary)

# denmark
dnk_summary <- data_coef %>% group_by(LOCATION=="DNK")
summary(dnk_summary)

# ...

#### PLOTS ####
library(dplyr)
library(tidyverse)
library(ggplot2)

# plot median binf across time
ggplot() +
  geom_line(aes(x=data_coef_med$Date,y=data_coef_med$binf_med),color="darkviolet") +
  ggtitle("Median Inflation Coefficient") +
  xlab("Time") +
  ylab("Value") +
  theme_classic()

# plot ALL sd binf across time + subperiod markers
ggplot() +
  geom_line(aes(x=data_coef_med$Date,y=data_coef_med$binf_sd),color="deeppink2") +
  ggtitle("Convergence of Inflation Responses") +
  xlab("Time") +
  ylab("Inverse of Standard Deviation") +
  geom_vline(xintercept=as.numeric(as.Date("1980-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("1989-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2004-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2010-01-01")), linetype="dotted", color="black") +
  theme_classic()

# plot sd binf across time: omitting single largest outlier
ggplot() +
  geom_line(aes(x = subset(data_coef_med$Date, !(data_coef_med$Date == as.Date("1963-07-01"))), 
                y = subset(data_coef_med$binf_sd, !(data_coef_med$Date == as.Date("1963-07-01")))),
            color = "deeppink2") +
  ggtitle("Convergence of Inflation Responses") +
  xlab("Time") +
  ylab("Inverse of Standard Deviation") +
  geom_vline(xintercept=as.numeric(as.Date("1980-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("1989-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2004-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2010-01-01")), linetype="dotted", color="black") +
  theme_classic()

# plot sd binf across time: omitting 20 largest outliers (beginning of sample N>7)
ggplot() +
  geom_line(aes(x = subset(data_coef_med$Date, 
                           !(data_coef_med$Date %in% as.Date(c("1963-07-01", "1963-10-01", "1963-04-01",
                                                               "1964-01-01", "1972-01-01", "1972-04-01",
                                                               "1971-10-01", "1964-04-01", "1964-10-01", 
                                                               "1964-07-01", "1972-07-01", "1965-01-01",
                                                               "1971-07-01", "1966-04-01", "1966-01-01",
                                                               "1963-01-01", "1965-04-01", "1965-10-01",
                                                               "1965-07-01", "1972-10-01")))), 
                y = subset(data_coef_med$binf_sd, 
                           !(data_coef_med$Date %in% as.Date(c("1963-07-01", "1963-10-01", "1963-04-01",
                                                               "1964-01-01", "1972-01-01", "1972-04-01",
                                                               "1971-10-01", "1964-04-01", "1964-10-01",
                                                               "1964-07-01", "1972-07-01", "1965-01-01",
                                                               "1971-07-01", "1966-04-01", "1966-01-01",
                                                               "1963-01-01", "1965-04-01", "1965-10-01",
                                                               "1965-07-01", "1972-10-01"))))),
            color = "deeppink2") +
  ggtitle("Convergence of Inflation Responses") +
  xlab("Time") +
  ylab("Inverse of Standard Deviation") +
  geom_vline(xintercept=as.numeric(as.Date("1980-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("1989-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2004-01-01")), linetype="dotted", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2010-01-01")), linetype="dotted", color="black") +
  theme_classic()


# plot sample N across time
ggplot() +
  geom_line(aes(x=data_coef_med$Date,y=data_coef_med$N),color="mediumblue") +
  ggtitle("Sample Size") +
  xlab("Time") +
  ylab("Number of countries") +
  theme_classic() + 
  geom_vline(data=data_coef_med, aes(xintercept=1980-01-01), linetype="dashed", size=.5)
  # geom_vline(xintercept=1980-01-01, 1989-01-01, 2004-01-01, 2010-01-01, linetype="dashed")

## good plot for ALL binf
# plot ALL binf (inflation coef) worked # get this 
data_coef$Date <- as.Date(as.yearqtr(data_gap$obsTime, format = "%Y-Q%q"))
ggplot(data_coef, aes(x=Date,y=binf, color = LOCATION)) + geom_line() +
  ggtitle("Global Inflation Coefficients") +
  xlab("Time") +
  ylab("Value") +
  theme_classic()

# Filter outliers for binf plot
# Convert the obsTime column to a Date object
data_coef$Date <- as.Date(as.yearqtr(data_gap$obsTime, format = "%Y-Q%q"))

# Filter out 12 large outliers for visual purposes
data_filtered <- subset(data_coef, !(LOCATION == "FIN" & Date == as.Date("2018-04-01")) & 
                          !(LOCATION == "JPN" & Date == as.Date("2006-07-01")) &
                          !(LOCATION == "AUT" & Date == as.Date("1992-10-01")) &
                          !(LOCATION == "AUT" & Date == as.Date("2013-01-01")) &
                          !(LOCATION == "AUT" & Date == as.Date("1991-07-01")) &
                          !(LOCATION == "AUT" & Date == as.Date("1992-07-01")) &
                          !(LOCATION == "IDN" & Date == as.Date("2014-04-01")) &
                          !(LOCATION == "IDN" & Date == as.Date("2014-01-01")) &
                          !(LOCATION == "CRI" & Date == as.Date("2020-04-01")) &
                          !(LOCATION == "IDN" & Date == as.Date("2013-10-01")) &
                          !(LOCATION == "GRC" & Date == as.Date("2000-01-01")) &
                          !(LOCATION == "GRC" & Date == as.Date("2000-04-01")) &
                          !(LOCATION == "GRC" & Date == as.Date("1999-10-01")))

# Create the plot using the filtered data
ggplot(data_filtered, aes(x=Date, y=binf, color = LOCATION)) + 
  geom_line() +
  ggtitle("Global Inflation Coefficients") +
  xlab("Time") +
  ylab("Value") +
  theme_classic()

# Plot only example countries
# Subset data_coef to only include rows with LOCATION = USA, IND, IRE, or CHE
data_coef_sub <- subset(data_coef, LOCATION %in% c("USA", "IND", "IRL", "CHE") &
                          !(LOCATION == "IRL" & Date == as.Date("2018-07-01")) &
                          !(LOCATION == "IRL" & Date == as.Date("2018-10-01")) &
                          !(LOCATION == "IRL" & Date == as.Date("2019-01-01")))

# Plot all binf for the SUBSET
data_coef_sub$Date <- as.Date(as.yearqtr(data_coef_sub$obsTime, format = "%Y-Q%q"))
ggplot(data_coef_sub, aes(x=Date,y=binf, color = LOCATION)) + 
  geom_line() +
  ggtitle("Global Inflation Coefficients (Subset)") +
  xlab("Time") +
  ylab("Value") +
  theme_classic()

## good plot for ALL binf
# plot ALL binf (inflation coef) worked # get this 
data_coef$Date <- as.Date(as.yearqtr(data_gap$obsTime, format = "%Y-Q%q"))
ggplot(data_coef, aes(x=Date,y=binf, color = LOCATION)) + geom_line() +
  ggtitle("Global Inflation Coefficients") +
  xlab("Time") +
  ylab("Value") +
  theme_classic()

# plot sd_binf
binf_summary$


#### SUBPERIOD SUMSTATS ####

## creating vectors per subperiod
# t1 (-1979)
library(dplyr)

t1_sd_binf <- data_coef_med %>% 
  filter(Date <= "1979-10-01") %>% 
  pull(binf_sd)

# t2 (1980-1987)
t2_sd_binf <- data_coef_med %>% 
  filter(Date >= "1980-01-01" & Date <= "1987-10-01") %>% 
  pull(binf_sd)

# t3 (1988-2003)
t3_sd_binf <- data_coef_med %>% 
  filter(Date >= "1988-01-01" & Date <= "2003-10-01") %>% 
  pull(binf_sd)

# t4 (2004-2009)
t4_sd_binf <- data_coef_med %>% 
  filter(Date >= "2004-01-01" & Date <= "2009-10-01") %>% 
  pull(binf_sd)

# t5 (2010-2022)
t5_sd_binf <- data_coef_med %>% 
  filter(Date >= "2010-01-01" & Date <= "2022-10-01") %>% 
  pull(binf_sd)


#### tables and figures to do ####
# binf and bgdp for three example countries across time
## this but get median for each country (and exclude Japan?)
ggplot(data_coef, aes(x=Date,y=binf, color = LOCATION)) + geom_line() +
  theme_classic()
# USA

# heatmap
library(gplots) # For heatmap function

library(googleVis)
data_binf_sd <- as.data.frame(binf_sd)
data_binf_sd <- data_binf_sd[,c()]
names(binf_sd) <- c("LOCATION","sd_binf")

map <- gvisGeoMap(data=binf_sd, locationvar = "LOCATION", numvar='sd_binf',options=list(width='800px',heigth='500px',colors="['0x0000ff', '0xff0000']"))
plot(map) # blank

print(map,file="Map.html")


# choropleth map
# Load required packages
library(sf)
library(ggplot2)
library(rnaturalearth)

st_drivers()
world <- st_read("world.svg")
world <- st_read("world.svg")


# Load world map shapefile
world <- st_read(system.file("world_map.csv", package = "rnaturalearth"))

# Merge data with shapefile
map_data <- merge(world, binf_sd, by.x = "sd_binf", by.y = "LOCATION")

# Create choropleth map
ggplot(map_data) +
  geom_sf(aes(fill = sd_binf)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray90") +
  labs(title = "Choropleth map of binf_sd data", fill = "sd_binf") +
  theme_void()

# tv_reg summary attempt 1, gives IND
tv_reg_summary <- summary(tv_reg)
cat("Summary statistics for", i, "\n")
print(tv_reg_summary)

# table of summary stats
library(stargazer) # makes pretty tables
stargazer(tv_reg_summary,
          title="Time Varing Regressions Summary",
          type="html",
          digits= 3,
          out="tv_reg_summary.doc")
system("tv_reg_summary.doc")


# attempt 2
# get summary stats for country i?
for(i in countries_final){
  summary(data_coef$tvLM)
} # no

# summary and print
{
  tv_reg_summary <- summary(tv_reg)
  cat("Summary statistics for", i, "\n")
  print(tv_reg_summary)
}

# Use stargazer to create a summary table
stargazer(tv_reg, 
          title = paste("Time Varing Regressions Summary", i), 
          type = "html", 
          digits = 3, 
          out = paste("tv_reg_summary", i, ".doc", sep = ""))

# create an empty list to store regression results
tv_reg_results <- list()

# create a for loop to generate multiple linear regressions
for(i in countries_final){
  reg_test <- tv_reg
}

#### export data.frames ####
library(rio)

# Define the file name and path where you want to save the xlsx file
file_name <- "binf_summary.xlsx"
file_path <- "/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/Honors_2022/O'Duffy/data"

# Write the dataframe to an xlsx file
export(binf_summary, file = paste0(file_path, file_name), "xlsx")
