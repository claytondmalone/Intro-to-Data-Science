library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(plotly)
library(dplyr)
library(circlize)
library(countrycode)

# Loading and cleaning data

# Loading the data from online
country_dataset <- read.csv(url("https://github.com/kdrofenou/Intro-to-Data-Science/files/9661222/Country_Migration.csv"))
industry_dataset <- read.csv(url("https://github.com/kdrofenou/Intro-to-Data-Science/files/9661224/Industry.Migration.csv"))
skill_dataset <- read.csv(url("https://github.com/kdrofenou/Intro-to-Data-Science/files/9661180/skill_migration.csv"))

# Cleaning the country data
names(country_dataset)[names(country_dataset) == 'net_per_10K_2015'] <- 'net_per_10k'
names(country_dataset)[names(country_dataset) == 'net_per_10K_2016'] <- 'net_per_10k'
names(country_dataset)[names(country_dataset) == 'net_per_10K_2017'] <- 'net_per_10k'
names(country_dataset)[names(country_dataset) == 'net_per_10K_2018'] <- 'net_per_10k'
names(country_dataset)[names(country_dataset) == 'net_per_10K_2019'] <- 'net_per_10k'

country_dataset_2015 <- country_dataset[,1:13]
year <- 2015
country_dataset_2015$year <- year

country_dataset_2016 <- country_dataset[,c(1:12,14)]
year <- 2016
country_dataset_2016$year <- year

country_dataset_2017 <- country_dataset[,c(1:12,15)]
year <- 2017
country_dataset_2017$year <- year

country_dataset_2018 <- country_dataset[,c(1:12,16)]
year <- 2018
country_dataset_2018$year <- year

country_dataset_2019 <- country_dataset[,c(1:12,17)]
year <- 2019
country_dataset_2019$year <- year

country_dataset_cleaned <- bind_rows(country_dataset_2015,country_dataset_2016,country_dataset_2017,country_dataset_2018,country_dataset_2019)

# Cleaning the industry data
names(industry_dataset)[names(industry_dataset) == 'net_per_10K_2015'] <- 'net_per_10k'
names(industry_dataset)[names(industry_dataset) == 'net_per_10K_2016'] <- 'net_per_10k'
names(industry_dataset)[names(industry_dataset) == 'net_per_10K_2017'] <- 'net_per_10k'
names(industry_dataset)[names(industry_dataset) == 'net_per_10K_2018'] <- 'net_per_10k'
names(industry_dataset)[names(industry_dataset) == 'net_per_10K_2019'] <- 'net_per_10k'

industry_dataset_2015 <- industry_dataset[,1:9]
year <- 2015
industry_dataset_2015$year <- year

industry_dataset_2016 <- industry_dataset[,c(1:8,10)]
year <- 2016
industry_dataset_2016$year <- year

industry_dataset_2017 <- industry_dataset[,c(1:8,11)]
year <- 2017
industry_dataset_2017$year <- year

industry_dataset_2018 <- industry_dataset[,c(1:8,12)]
year <- 2018
industry_dataset_2018$year <- year

industry_dataset_2019 <- industry_dataset[,c(1:8,13)]
year <- 2019
industry_dataset_2019$year <- year

industry_dataset_cleaned <- bind_rows(industry_dataset_2015,industry_dataset_2016,industry_dataset_2017,industry_dataset_2018,industry_dataset_2019)

# Cleaning the skill data
skill_dataset <- skill_dataset[1:17617,]
names(skill_dataset)[names(skill_dataset) == 'net_per_10K_2015'] <- 'net_per_10k'
names(skill_dataset)[names(skill_dataset) == 'net_per_10K_2016'] <- 'net_per_10k'
names(skill_dataset)[names(skill_dataset) == 'net_per_10K_2017'] <- 'net_per_10k'
names(skill_dataset)[names(skill_dataset) == 'net_per_10K_2018'] <- 'net_per_10k'
names(skill_dataset)[names(skill_dataset) == 'net_per_10K_2019'] <- 'net_per_10k'

skill_dataset_2015 <- skill_dataset[,1:8]
year <- 2015
skill_dataset_2015$year <- year

skill_dataset_2016 <- skill_dataset[,c(1:7,9)]
year <- 2016
skill_dataset_2016$year <- year

skill_dataset_2017 <- skill_dataset[,c(1:7,10)]
year <- 2017
skill_dataset_2017$year <- year

skill_dataset_2018 <- skill_dataset[,c(1:7,11)]
year <- 2018
skill_dataset_2018$year <- year

skill_dataset_2019 <- skill_dataset[,c(1:7,12)]
year <- 2019
skill_dataset_2019$year <- year

skill_dataset_cleaned <- bind_rows(skill_dataset_2015,skill_dataset_2016,skill_dataset_2017,skill_dataset_2018,skill_dataset_2019)

# Tab 1 Data
# -----------------------------------------
Figure21_Data <- country_dataset_cleaned[c("target_country_name","target_country_code","target_lat","target_long","net_per_10k","year")]
Figure21_Data <- aggregate(net_per_10k ~ target_country_name + target_country_code + target_lat + target_long + year, data=Figure21_Data, FUN=sum)
Figure21_Data$target_country_name <- countrycode(Figure21_Data$target_country_name, origin='country.name',destination='iso3c')

mdat <- map_data("world")
mdat$region <- countrycode(mdat$region, origin='country.name',destination='iso3c')
Figure21_Data <- merge(x=Figure21_Data, y=mdat, by.x="target_country_name", by.y="region")
Figure21_Data <- arrange(Figure21_Data,order)
# -----------------------------------------

# Tab 2 Data
# -----------------------------------------
Figure23_Data <- industry_dataset_cleaned[c("country_name","country_code","isic_section_name","net_per_10k","year")]

Figure23_Data <- aggregate(net_per_10k ~ country_name + country_code + isic_section_name + year, data=Figure23_Data, FUN=sum)
Figure23_Data$country_name <- countrycode(Figure23_Data$country_name, origin='country.name',destination='iso3c')

mdat <- map_data("world")
mdat$region <- countrycode(mdat$region, origin='country.name',destination='iso3c')
Figure23_Data <- merge(x=Figure23_Data, y=mdat, by.x="country_name", by.y="region")
Figure23_Data <- arrange(Figure23_Data,order)

isic_section_name_choices <- as.character(unique(Figure23_Data$isic_section_name))[1:18]
# -----------------------------------------

# Tab 3 Data
# -----------------------------------------
Figure22_Data <- skill_dataset_cleaned[c("country_name","country_code","skill_group_category","net_per_10k","year")]
Figure22_Data <- within(Figure22_Data, skill_group_category[skill_group_category == 'Disruptive Tech Skills'] <- 'Tech Skills')

Figure22_Data <- aggregate(net_per_10k ~ country_name + country_code + skill_group_category + year, data=Figure22_Data, FUN=sum)
Figure22_Data$country_name <- countrycode(Figure22_Data$country_name, origin='country.name',destination='iso3c')

mdat <- map_data("world")
mdat$region <- countrycode(mdat$region, origin='country.name',destination='iso3c')
Figure22_Data <- merge(x=Figure22_Data, y=mdat, by.x="country_name", by.y="region")
Figure22_Data <- arrange(Figure22_Data,order)

skill_name_choices <- as.character(unique(Figure22_Data$skill_group_category))
# -----------------------------------------

# Tab 4 Data
# -----------------------------------------
country_net =
  country_dataset_cleaned %>%
  select(base_country_name,base_country_wb_income,base_lat, base_long,target_country_name,target_country_wb_income,
         target_lat, target_long, net_per_10k, year) %>%
  mutate(net_per_10k = as.numeric(net_per_10k)) %>%
  group_by(base_country_name,base_lat, base_long,base_country_wb_income,target_country_name,
           target_lat, target_long,target_country_wb_income, year) %>% 
  summarize(Total_net_gain = sum(net_per_10k))

na.omit(country_net)

region_net1 =
  country_dataset_cleaned %>%
  filter(net_per_10k >=0)  %>%
  select(base_country_wb_region, target_country_wb_region,net_per_10k,year) %>%
  mutate(net_per_10k = as.numeric(net_per_10k), from = base_country_wb_region, to =target_country_wb_region ) %>%
  group_by(from, to, year) %>% 
  summarize(value = sum(net_per_10k))
na.omit(region_net1) # drop missing values

region_net1_final <- data.frame(matrix(ncol = 4, nrow = 245))
colnames(region_net1_final) <- c("from", "to", "year", "value")
region_net1_final$from[1:35] <- unique(region_net1$from)[1]
region_net1_final$from[36:70] <- unique(region_net1$from)[2]
region_net1_final$from[71:105] <- unique(region_net1$from)[3]
region_net1_final$from[106:140] <- unique(region_net1$from)[4]
region_net1_final$from[141:175] <- unique(region_net1$from)[5]
region_net1_final$from[176:210] <- unique(region_net1$from)[6]
region_net1_final$from[211:245] <- unique(region_net1$from)[7]

region_net1_final$to[1:35] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])
region_net1_final$to[36:70] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])
region_net1_final$to[71:105] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])
region_net1_final$to[106:140] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])
region_net1_final$to[141:175] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])
region_net1_final$to[176:210] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])
region_net1_final$to[211:245] <- c(unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[1],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[2],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[3],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[4],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[5],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[6],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7],unique(region_net1$from)[7])

region_net1_final$year[1:35] <- c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net1_final$year[36:70] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net1_final$year[71:105] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net1_final$year[106:140] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net1_final$year[141:175] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net1_final$year[176:210] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net1_final$year[211:245] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)

region_net1_final$value[1:245] <- 0
region_net1_final <- merge(region_net1_final, region_net1, by=c("from","to","year"),all.x=TRUE)

region_net1_final <- region_net1_final[c(1,2,3,5)]
colnames(region_net1_final) <- c("from", "to", "year", "value")

region_net1_final[is.na(region_net1_final)] <- 0

region_net2 =
  country_dataset_cleaned %>%
  filter(net_per_10k < 0)  %>%
  select(base_country_wb_region, target_country_wb_region,net_per_10k,year) %>%
  mutate(net_per_10k = as.numeric(-net_per_10k), to = base_country_wb_region, from =target_country_wb_region ) %>%
  group_by(from, to, year) %>% 
  summarize(value = sum(net_per_10k))
na.omit(region_net2) # drop missing values

region_net2_final <- data.frame(matrix(ncol = 4, nrow = 245))
colnames(region_net2_final) <- c("from", "to", "year", "value")
region_net2_final$from[1:35] <- unique(region_net2$from)[1]
region_net2_final$from[36:70] <- unique(region_net2$from)[2]
region_net2_final$from[71:105] <- unique(region_net2$from)[3]
region_net2_final$from[106:140] <- unique(region_net2$from)[4]
region_net2_final$from[141:175] <- unique(region_net2$from)[5]
region_net2_final$from[176:210] <- unique(region_net2$from)[6]
region_net2_final$from[211:245] <- unique(region_net2$from)[7]

region_net2_final$to[1:35] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])
region_net2_final$to[36:70] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])
region_net2_final$to[71:105] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])
region_net2_final$to[106:140] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])
region_net2_final$to[141:175] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])
region_net2_final$to[176:210] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])
region_net2_final$to[211:245] <- c(unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[1],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[2],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[3],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[4],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[5],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[6],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7],unique(region_net2$from)[7])

region_net2_final$year[1:35] <- c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net2_final$year[36:70] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net2_final$year[71:105] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net2_final$year[106:140] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net2_final$year[141:175] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net2_final$year[176:210] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
region_net2_final$year[211:245] <-c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)

region_net2_final$value[1:245] <- 0
region_net2_final <- merge(region_net2_final, region_net2, by=c("from","to","year"),all.x=TRUE)

region_net2_final <- region_net2_final[c(1,2,3,5)]
colnames(region_net2_final) <- c("from", "to", "year", "value")

region_net2_final[is.na(region_net2_final)] <- 0

region_net_total = rbind(region_net1_final,region_net2_final)
# -----------------------------------------

# Dashboard code
ui <- dashboardPage(
  dashboardHeader(title = "Talent Migraton (2015-2019)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Population Migration", tabName = "population", 
               icon = icon("map")),
      menuItem("Industry Movement", tabName = "industry", 
               icon = icon("briefcase")),
      menuItem("Skill Movement", tabName = "skill", 
               icon = icon("hammer")),
      menuItem("Migration Chord Diagram", tabName = "chord", 
               icon = icon("circle"))
    )    
  ),
  dashboardBody(
    tabItems(
    # First tab content
    tabItem(tabName = "population",
            h2("Population Migration"),
            fluidRow(
              column(5,
                     wellPanel(
                       sliderInput("myYears1",
                                   "Migration year",
                                   min = 2015,
                                   max = 2019,
                                   value = 2015),
                     )
              ),
              column(6, 
                     plotlyOutput("myMap1",height = "400px", width="800px")
              )
              )
            )
    ,
    # Second tab content
    tabItem(tabName = "industry",
            h2("Global Industry Movement"),
            fluidRow(
              column(5,
                     wellPanel(
                       selectInput("selectedIndustry", 
                                   label = "Choose an industry to display",
                                   choices = isic_section_name_choices, 
                                   selected = isic_section_name_choices[1]),
                       sliderInput("myYears2",
                                   "Migration year",
                                   min = 2015,
                                   max = 2019,
                                   value = 2015),
                     )
              ),
              column(6, 
                     plotlyOutput("myMap2",height = "400px", width="800px")
              )      
            )
    ),
      # Third tab content
      tabItem(tabName = "skill",
              h2("Global Skill Movement"),
              fluidRow(
                column(5,
                       wellPanel(
                         selectInput("selectedSkill", 
                                     label = "Choose a skill to display",
                                     choices = skill_name_choices, 
                                     selected = skill_name_choices[1]),
                         sliderInput("myYears3",
                                     "Migration year",
                                     min = 2015,
                                     max = 2019,
                                     value = 2015),
                       )
                ),
                column(6, 
                       plotlyOutput("myMap3",height = "400px", width="800px")
                )      
              )
      ),
        # Fourth tab content
        tabItem(tabName = "chord",
                h2("Migration Chord Diagram"),
                fluidRow(
                  column(5,
                         wellPanel(
                           sliderInput("myYears4",
                                       "Migration year",
                                       min = 2015,
                                       max = 2019,
                                       value = 2015),
                         )
                  ),
                  column(6, 
                         plotOutput("myMap4",height = "400px", width="400px")
                  )
                )
        )
    
    
    )
  )
)

server <- function(input, output) {
  
  dataInput1 <- reactive({
    subset(Figure21_Data,
           year==input$myYears1)
  })
  
  dataInput2 <- reactive({
    subset(Figure23_Data,
           year==input$myYears2 & 
             isic_section_name==input$selectedIndustry)
  })
  
  dataInput3 <- reactive({
    subset(Figure22_Data,
           year==input$myYears3 & 
             skill_group_category==input$selectedSkill)
  })
  
  dataInput4 <- reactive({
    subset(region_net_total,
           year==input$myYears4)
  })
  
  output$myMap1 <- renderPlotly({
    data1 <- dataInput1()
    
    a <- ggplot(data1, aes(x=long, y=lat,group=group)) +
      theme_bw() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank() )+
      geom_polygon(aes(x=long, y=lat, group = group, fill=net_per_10k, label=target_country_name), colour = alpha("white", 1/2), size = 0.2)+
      scale_fill_gradient2(low="red",mid="gray",high="blue",name="Net per 10k",trans='pseudo_log',breaks = c(-1e+03, -1e+02, -1e+01,0,1e+01,1e+02, 1e+03))
 
    ggplotly(a)
     })
  
  output$myMap2 <- renderPlotly({
    data2 <- dataInput2()
    
    b <- ggplot(data2, aes(x=long, y=lat,group=group)) +
      theme_bw() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank() )+
      geom_polygon(aes(x=long, y=lat, group = group, fill=net_per_10k, label=country_name), colour = alpha("white", 1/2), size = 0.2)+
      scale_fill_gradient2(low="red",mid="gray",high="blue",name="Net per 10k",trans='pseudo_log',breaks = c(-1e+03, -1e+02, -1e+01,0,1e+01,1e+02, 1e+03))
      
      ggplotly(b)
    })
  
  output$myMap3 <- renderPlotly({ 
    data3 <- dataInput3()
    
   c <- ggplot(data3, aes(x=long, y=lat,group=group)) +
      theme_bw() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank() )+
      geom_polygon(aes(x=long, y=lat, group = group, fill=net_per_10k, label=country_name), colour = alpha("white", 1/2), size = 0.2)+
      scale_fill_gradient2(low="red",mid="gray",high="blue",name="Net per 10k",trans='pseudo_log',breaks = c(-1e+04,-1e+03, -1e+02, -1e+01,0,1e+01,1e+02, 1e+03,1e+04))
  
   ggplotly(c)
   })
  
  output$myMap4 <- renderPlot({ 
    data4 <- dataInput4()
    data4 <- data4[c(1,2,4)]
    set.seed(950)
    chordDiagram(data4)
  })
  
  
}

shinyApp(ui, server)
