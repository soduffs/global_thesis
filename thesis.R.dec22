# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Desktop/school/thesis")

# Loading packages
library(tidyverse)

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
filter_list <- list("", "CPALTT01","IXOBSA","Q")
mei <- get_dataset(dataset = dataset, filter = filter_list)
countries_mei <- unique(mei$LOCATION)
mei <- mei %>% rename(cpi = obsValue) %>% select("LOCATION", "obsTime", "cpi")

# merge
data_all <- inner_join(mei_fin,mei,by=c("LOCATION","obsTime"))
data_all <- inner_join(data_all,qna, by=c("LOCATION", "obsTime"))

# plot all of the short-rates across time 
library(zoo)
mei_fin$Date <- as.Date(as.yearqtr(mei_fin$obsTime, format = "%Y-Q%q"))
ggplot(mei_fin, aes(x=Date,y=short_rate, color = LOCATION)) + geom_line() +
  theme_classic()

# calculate inflation


# HP-Filter to get output gap

# Run regression to get Taylor Rule

