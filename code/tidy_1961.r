#-----------------------
# Tidy election results 
# 1961 Bundestag
#----------------------- 

#---
# Note: code is identical to 1953, except for party names. Seems like there is the same structure for all elections up to (and including) 2002. As of 2005, there is a different structure also reporting votes for the previous election period. Consider writing one general function as a wrapper.
#---

# rm(list = ls())

library(tidyr)
library(dplyr)

#----------------------- 
# Loading Data

# load raw data
data1961_raw <- read.csv2(file = "../data_raw/btw61_kerg.csv", header = FALSE, skip = 8, stringsAsFactors = FALSE, na.strings = "")

# load separately: variable names
# variable names are stretched over two rows
colnames1961_raw <- read.csv2(file = "../data_raw/btw61_kerg.csv", header = FALSE, skip = 5, nrows = 2, stringsAsFactors = FALSE, na.strings = "")

# alternative: load directly from Federal Returning Officer's website
# data1961_raw <- read.csv2(file = "https://www.bundeswahlleiter.de/dam/jcr/31655810-7fd3-46bd-827c-98e0fd39661f/btw61_kerg.csv", header = TRUE, skip = 8, stringsAsFactors = FALSE, na.strings = "")
# colnames1961_raw <- read.csv2(file = "https://www.bundeswahlleiter.de/dam/jcr/31655810-7fd3-46bd-827c-98e0fd39661f/btw61_kerg.csv", header = FALSE, skip = 5, nrows = 2, stringsAsFactors = FALSE, na.strings = "")

# create clean vector of variable names
colnames1961_raw <- as.data.frame(t(colnames1961_raw)) # transpose for easier editing
colnames1961_raw[] <- lapply(colnames1961_raw, as.character) # convert all variables to characters
colnames1961 <- do.call(paste, c(colnames1961_raw[c("V1", "V2")], sep = "_")) # paste together
colnames(data1961_raw) <- colnames1961 # overwriting empty/generic variable names in dataset


#----------------------- 
# Remove unnecessary information from data

# identify columns which are completely empty - can be dropped
na_per_col <- apply(data1961_raw, 2, function(x) {sum(is.na(x))})
data1961_raw <- data1961_raw[, -(which(na_per_col == nrow(data1961_raw)))]

# identify rows which are completely empty (divider rows) - can be dropped
na_per_row <- apply(data1961_raw, 1, function(x) {sum(is.na(x))})
data1961_raw <- data1961_raw[-(which(na_per_row == ncol(data1961_raw))), ]


#----------------------- 
# Processing data

# proper variable names for key variables
colnames(data1961_raw)[1:9] <- c("district_no",
                                 "district_name",
                                 "state",
                                 "eligible_district",
                                 "voters_district",
                                 "invalidvotes_district_erst",
                                 "invalidvotes_district_zweit",
                                 "validvotes_district_erst",
                                 "validvotes_district_zweit")

# Splitting into separate dataframes for Erststimme and Zweitstimme
data1961_erst <- select(data1961_raw, matches("district_no"), ends_with("Erststimmen"))
data1961_zweit <- select(data1961_raw, matches("district_no"), ends_with("Zweitstimmen"))

# transformation: wide to long
data1961_erst_long <- gather(data = data1961_erst, key = party_votetype, value = votes_party_district, -district_no)
data1961_zweit_long <- gather(data = data1961_zweit, key = party_votetype, value = votes_party_district, -district_no)

# separating party and vote type information into separate columns
data1961_erst_long <- separate(data1961_erst_long, col = "party_votetype", into = c("party_short", "vote_type"), sep = "_")
data1961_zweit_long <- separate(data1961_zweit_long, col = "party_votetype", into = c("party_short", "vote_type"), sep = "_")


#----------------------- 
# Context dataset

data1961_context <- select(data1961_raw, -ends_with("stimmen"))

# create turnout variable within context data
data1961_context$turnout_district <- data1961_context$voters_district / data1961_context$eligible_district * 100

# add year variable
data1961_context$year <- 1961

# merge vote data and context data
data1961_erst_long <- left_join(x = data1961_erst_long, y = data1961_context, by = "district_no")
data1961_erst_long <- select(data1961_erst_long, -ends_with("zweit"))
data1961_erst_long <- rename(data1961_erst_long,
                             invalidvotes_district = invalidvotes_district_erst,
                             validvotes_district = validvotes_district_erst)

data1961_zweit_long <- left_join(x = data1961_zweit_long, y = data1961_context, by = "district_no")
data1961_zweit_long <- select(data1961_zweit_long, -ends_with("erst"))
data1961_zweit_long <- rename(data1961_zweit_long,
                             invalidvotes_district = invalidvotes_district_zweit,
                             validvotes_district = validvotes_district_zweit)

# join data for vote types
tidy_bundestag1961 <- bind_rows(data1961_erst_long, data1961_zweit_long)

# create voteshare variable
tidy_bundestag1961$voteshare_party_district <- tidy_bundestag1961$votes_party_district / tidy_bundestag1961$validvotes_district * 100
tidy_bundestag1961$vote_type <- gsub(pattern = "stimmen", replacement = "stimme", x = tidy_bundestag1961$vote_type)

# create variable with long party name (merge via external dataset)
partynames <- as.data.frame(rbind(
  c("SPD", "Sozialdemokratische Partei Deutschlands"),
  c("CDU", "Christlich-Demokratische Union Deutschlands"),
  c("FDP", "Freie Demokratische Partei"),
  c("CSU", "Christlich-soziale Union in Bayern"),
  c("GDP", "Gesamtdeutsche Partei"),
  c("DFU", "Deutsche Friedensunion"),
  c("DRP", "Deutsche Rechtspartei"),
  c("DG", "Deutsche Gemeinschaft"),
  c("SSW", "Südschleswigscher Wählerverband"),
  c("WGnD", "Wählergemeinschaft für ein neutrales Deutschland"),
  c("Übrige", "Übrige Wählergruppen/Einzelbewerber")
))

colnames(partynames) <- c("party_short", "party_long")
partynames[] <- lapply(partynames, as.character) # convert all variables to characters

tidy_bundestag1961 <- left_join(x = tidy_bundestag1961, y = partynames, by = "party_short")

# rearrange variables in order to meet overall structure
tidy_bundestag1961 <- tidy_bundestag1961 %>%
  select(district_no, district_name, state, party_short, party_long, vote_type, year, votes_party_district, voteshare_party_district, voters_district, validvotes_district, invalidvotes_district, eligible_district, turnout_district)

#----------------------- 
# Splitting Data

# splitting into one dataset on district level and one dataset on state level
tidy_bundestag1961_districts <- filter(tidy_bundestag1961, district_no < 900)
tidy_bundestag1961_states <- filter(tidy_bundestag1961, district_no > 900)

## Check 1: Internal Check
## Compare district-level and state-level results within the data set

# check_votecount <- tidy_bundestag1961_districts %>%
#   group_by(state, party_short, vote_type) %>%
#   summarize(state_votecount = sum(votes_party_district))
# 
# check_votecount <- left_join(x = check_votecount, y = tidy_bundestag1961_states,
#                              by = c("state" = "state", "party_short" = "party_short", "vote_type" = "vote_type"))
# 
# check_votecount <- select(check_votecount, state, party_short, state_votecount, vote_type, votes_party_district)
# check_votecount$check <- check_votecount$state_votecount == check_votecount$votes_party_district
# table(check_votecount$check)
# rm(check_votecount) # to remove this check object from the workspace again

## Check 2: External Check
## Compare aggregated federal result with figures as reported by Wikipedia and the Federal Returning Officer

# tidy_bundestag1961_states %>%
#   filter(district_no < 999) %>%
#   group_by(party_short, vote_type) %>%
#   summarise(votes = sum(votes_party_district)) %>%
#   arrange(vote_type, desc(votes)) %>%
#   View()

# Source 1 - Wikipedia: https://de.wikipedia.org/wiki/Bundestagswahl_1961#Amtliches_Endergebnis
# Source 2 - Federal Returning Officer: https://www.bundeswahlleiter.de/bundestagswahlen/1961.html

#----------------------- 
# Data Export

# export results
save(tidy_bundestag1961_districts, tidy_bundestag1961_states, file = "../data_tidy/tidy_bundestag1961.Rdata")
# write.csv2(tidy_bundestag1961_districts, file = "../data_tidy/tidy_bundestag1961_districts.csv")
# write.csv2(tidy_bundestag1961_states, file = "../data_tidy/tidy_bundestag1961_states.csv")

# clean up workspace
rm(list=setdiff(ls(), c("tidy_bundestag1961_districts", "tidy_bundestag1961_states")))


