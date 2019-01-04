#######################################################################
#1 Clean environment
#######################################################################

rm(list=ls())

#######################################################################
#2 Packages
#######################################################################

#######################################################################
#2.1 Install packages
#######################################################################

#Check if packages already installed
#library()
#If not installed - install following packages
#install.packages("data.table")
#install.packages("tidyverse")

#######################################################################
#2.2 Load packages
#######################################################################

library(data.table)
library(tidyverse)

#######################################################################
#3 Read in data and investigate structure
#######################################################################

big_mac_data = fread('/Users/manuelheller/Documents/Data Science/Economist Github/Raw data/big-mac-source-data.csv')  %>%
  .[order(date, name)]                          # sort by date and then by country name, for easy reading

#Investigate data structure
str(big_mac_data)

#######################################################################
#4 Calculate index
#######################################################################

#generate dollar price
big_mac_data[, dollar_price := local_price / dollar_ex]
#check if new variable has been created
tail(big_mac_data)

#identify base currencies
base_currencies = c('USD', 'EUR', 'GBP', 'JPY', 'CNY')

#identify countries we want to caluclate BigMac Index for
big_mac_countries = c('ARG', 'AUS', 'BRA', 'GBR', 'CAN', 'CHL', 'CHN', 'CZE', 'DNK',
                      'EGY', 'HKG', 'HUN', 'IDN', 'ISR', 'JPN', 'MYS', 'MEX', 'NZL',
                      'NOR', 'PER', 'PHL', 'POL', 'RUS', 'SAU', 'SGP', 'ZAF', 'KOR',
                      'SWE', 'CHE', 'TWN', 'THA', 'TUR', 'ARE', 'USA', 'COL', 'CRI',
                      'PAK', 'LKA', 'UKR', 'URY', 'IND', 'VNM', 'GTM', 'HND', # Venezuela removed
                      'NIC', 'AZE', 'BHR', 'HRV', 'JOR', 'KWT', 'LBN', 'MDA', 'OMN',
                      'QAT', 'ROU', 'EUZ')

#Keep variables and countries used for creation of index
big_mac_index = big_mac_data[
    iso_a3 %in% big_mac_countries
    ,.(date, iso_a3, currency_code, name, local_price, dollar_ex, dollar_price)]

#######################################################################

# We could instead also write the following. Including the additional code makes
# sure only non-missing values are used. However, the only missing values we have are
# for Venezuala, which we already remove in our "big_mac_countries" list.

# big_mac_index = big_mac_data[
#    !is.na(dollar_price) & iso_a3 %in% big_mac_countries
#    ,.(date, iso_a3, currency_code, name, local_price, dollar_ex, dollar_price)]

# Test what additional command does
test1 = big_mac_data[
  !is.na(dollar_price)]

test2 = big_mac_data[
  is.na(dollar_price)]

rm(test1)
rm(test2)
#######################################################################

#Create index for each currency
for(currency in base_currencies) {
  big_mac_index[
    ,                           
    (currency) := dollar_price / .SD[currency_code == currency]$dollar_price - 1,
    by=date
    ]
}

#not sure why the above calculation creates the value "CNY", it is not needed.
rm(currency)

#order by date and name
setkey(big_mac_index,date,name)
head(big_mac_index)

#Round index to three digits
big_mac_index[, (base_currencies) := round(.SD, 3), .SDcols=base_currencies]

fwrite(big_mac_index, '/Users/manuelheller/Documents/Data Science/Economist Github/Outputs/big-mac-raw-index.csv')

#######################################################################
#5 Create plot
#######################################################################

#determine last available date
latest_date = big_mac_data$date %>% max

#plot
to_plot = big_mac_index[date == latest_date]
to_plot$name = factor(to_plot$name, levels=to_plot$name[order(to_plot$USD)])
# I don't fully understand this command. I realize it orders the data, but
# doesn't leave a "visible trace"

ggplot(to_plot[, over := USD > 0], aes(x=name, y=USD, color=over)) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin=0, ymax=USD)) +
  geom_point() +
  coord_flip()

#######################################################################
#6 Adjusted index
#######################################################################

