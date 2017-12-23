#-----------------------
# Tidy election results 
# 1949 Bundestag
#----------------------- 

# rm(list = ls())

library(tidyr)
library(dplyr)

#----------------------- 
# Loading Data

# load raw data
data1949_raw <- read.csv2(file = "../data_raw/btw49_kerg.csv", header = TRUE, skip = 5, stringsAsFactors = FALSE, na.strings = "")

# alternative: load directly from Federal Returning Officer's website
# data1949_raw <- read.csv2(file = "https://bundeswahlleiter.de/dam/jcr/d1ffdb9b-3d79-45a3-8e25-6a867c34d202/btw49_kerg.csv", header = TRUE, skip = 5, stringsAsFactors = FALSE, na.strings = "")

#----------------------- 
# Remove unnecessary information from data

# first two columns still belong to title - need to be dropped
data1949_raw <- data1949_raw[-c(1,2),]

# identify columns which are completely empty - can be dropped
na_per_col <- apply(data1949_raw, 2, function(x) {sum(is.na(x))})
data1949_raw <- data1949_raw[, -(which(na_per_col == nrow(data1949_raw)))]

# identify rows which are completely empty (divider rows) - can be dropped
na_per_row <- apply(data1949_raw, 1, function(x) {sum(is.na(x))})
data1949_raw <- data1949_raw[-(which(na_per_row == ncol(data1949_raw))), ]


#----------------------- 
# Processing data: variable names, data transformation

# proper variable names for key variables
colnames(data1949_raw)[1:7] <- c("district_no",
                                 "district_name",
                                 "state",
                                 "eligible_district",
                                 "voters_district",
                                 "invalidvotes_district",
                                 "validvotes_district")

# reduced dataset: only electoral district numbers and votes
data1949_reduced <- data1949_raw[, c(1, 8:ncol(data1949_raw))]

# transformation: wide to long (with reduced set of variables)
data1949_long <- gather(data = data1949_reduced, key = party_short, value = votes_party_district, -district_no)

# create dataset context variables (everything except party votes)
data1949_context <- data1949_raw[, 1:7]

# create turnout variable within context data
data1949_context$turnout_district <- data1949_context$voters_district / data1949_context$eligible_district * 100

# add year variable
data1949_context$year <- 1949

# merge vote data and context data
tidy_bundestag1949 <- left_join(x = data1949_long, y = data1949_context, by = "district_no")

# create voteshare variable
tidy_bundestag1949$voteshare_party_district <- tidy_bundestag1949$votes_party_district / tidy_bundestag1949$validvotes_district * 100
tidy_bundestag1949$vote_type <- "Zweitstimme" # note: the 1949 electoral system only had a proportional component, closely resembling the "Zweitstimme" in later versions of the German federal electoral system

# create variable with long party name (merge via external dataset)
partynames <- data.frame(party_short = c("SPD", "CDU", "FDP", "CSU", "KPD", "Parteilose", "BP", "DP", "Zentrum", "WAV", "DKP.DRP", "RSF", "SSW", "EVD", "RWVP"),
                         party_long = c("Sozialdemokratische Partei Deutschlands", "Christlich-Demokratische Union Deutschlands", "Freie Demokratische Partei", "Christlich-soziale Union in Bayern", "Kommunistische Partei Deutschlands", "Parteilose", "Bayernpartei", "Deutsche Partei", "Deutsche Zentrums-Partei", "Wirtschaftliche Aufbau-Vereinigung", "Deutsche Konservative Partei – Deutsche Rechtspartei", "Radikal-Soziale Freiheitspartei", "Südschleswigscher Wählerverband", "Sammlung zur Tat/Europäische Volksbewegung Deutschlands", "Rheinisch-Westfälische Volkspartei"))

partynames[] <- lapply(partynames, as.character) # convert all variables to characters

tidy_bundestag1949 <- left_join(x = tidy_bundestag1949, y = partynames, by = "party_short")

# rearrange variables in order to meet overall structure
tidy_bundestag1949 <- tidy_bundestag1949 %>%
  select(district_no, district_name, state, party_short, party_long, vote_type, year, votes_party_district, voteshare_party_district, voters_district, validvotes_district, invalidvotes_district, eligible_district, turnout_district)

#----------------------- 
# Splitting Data

# splitting into one dataset on district level and one dataset on state level
tidy_bundestag1949_districts <- filter(tidy_bundestag1949, district_no < 900)
tidy_bundestag1949_states <- filter(tidy_bundestag1949, district_no > 900)

## Check 1: Internal Check
## Compare district-level and state-level results within the data set

# check_votecount <- tidy_bundestag1949_districts %>%
#   group_by(state, party_short) %>%
#   summarize(state_votecount = sum(votes_party_district))
# 
# check_votecount <- left_join(x = check_votecount, y = tidy_bundestag1949_state,
#                              by = c("state" = "state", "party_short" = "party_short"))
# 
# check_votecount <- select(check_votecount, state, party_short, state_votecount, votes_party_district)
# check_votecount$check <- check_votecount$state_votecount == check_votecount$votes_party_district
# rm(check_votecount) # to remove this check object from the workspace again

## Check 2: External Check
## Compare aggregated federal result with figures as reported by Wikipedia and the Federal Returning Officer

# tidy_bundestag1949_states %>%
#   filter(district_no < 999) %>%
#   group_by(party_short) %>%
#   summarise(votes = sum(votes_party_district)) %>%
#   arrange(desc(votes))

# Source 1 - Wikipedia: https://de.wikipedia.org/wiki/Bundestagswahl_1949#Gesamtergebnis
# Source 2 - Federal Returning Officer: https://www.bundeswahlleiter.de/bundestagswahlen/1949.html

#----------------------- 
# Data Export

# export results
save(tidy_bundestag1949_districts, tidy_bundestag1949_states, file = "../data_tidy/tidy_bundestag1949.Rdata")
# write.csv2(tidy_bundestag1949_districts, file = "../data_tidy/tidy_bundestag1949_districts.csv")
# write.csv2(tidy_bundestag1949_states, file = "../data_tidy/tidy_bundestag1949_states.csv")

# clean up workspace
rm(list = c("data1949_context", "data1949_long", "data1949_raw", "data1949_reduced", "na_per_col", "na_per_row", "partynames", "tidy_bundestag1949"))

