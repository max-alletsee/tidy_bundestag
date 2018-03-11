#-----------------------
# Tidy election results 
# 1976 Bundestag
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
data1976_raw <- read.csv2(file = "../data_raw/btw76_kerg.csv", header = FALSE, skip = 8, stringsAsFactors = FALSE, na.strings = "")

# load separately: variable names
# variable names are stretched over two rows
colnames1976_raw <- read.csv2(file = "../data_raw/btw76_kerg.csv", header = FALSE, skip = 5, nrows = 2, stringsAsFactors = FALSE, na.strings = "")

# alternative: load directly from Federal Returning Officer's website
# data1976_raw <- read.csv2(file = "https://www.bundeswahlleiter.de/dam/jcr/eb10180e-d751-4827-8e12-94818bd0992c/btw76_kerg.csv", header = TRUE, skip = 8, stringsAsFactors = FALSE, na.strings = "")
# colnames1976_raw <- read.csv2(file = "https://www.bundeswahlleiter.de/dam/jcr/9b3b215a-2f4e-42b5-9abe-1f8b73e6717a/btw72_kerg.csv", header = FALSE, skip = 5, nrows = 2, stringsAsFactors = FALSE, na.strings = "")

# create clean vector of variable names
colnames1976_raw <- as.data.frame(t(colnames1976_raw)) # transpose for easier editing
colnames1976_raw[] <- lapply(colnames1976_raw, as.character) # convert all variables to characters
colnames1976 <- do.call(paste, c(colnames1976_raw[c("V1", "V2")], sep = "_")) # paste together
colnames(data1976_raw) <- colnames1976 # overwriting empty/generic variable names in dataset


#----------------------- 
# Remove unnecessary information from data

# identify columns which are completely empty - can be dropped
na_per_col <- apply(data1976_raw, 2, function(x) {sum(is.na(x))})
if(any(na_per_col == nrow(data1976_raw))) {
  data1976_raw <- data1976_raw[, -(which(na_per_col == nrow(data1976_raw)))]
}


# identify rows which are completely empty (divider rows) - can be dropped
na_per_row <- apply(data1976_raw, 1, function(x) {sum(is.na(x))})
data1976_raw <- data1976_raw[-(which(na_per_row == ncol(data1976_raw))), ]


#----------------------- 
# Processing data

# proper variable names for key variables
colnames(data1976_raw)[1:9] <- c("district_no",
                                 "district_name",
                                 "state",
                                 "eligible_district",
                                 "voters_district",
                                 "invalidvotes_district_erst",
                                 "invalidvotes_district_zweit",
                                 "validvotes_district_erst",
                                 "validvotes_district_zweit")

# Splitting into separate dataframes for Erststimme and Zweitstimme
data1976_erst <- select(data1976_raw, matches("district_no"), ends_with("Erststimmen"))
data1976_zweit <- select(data1976_raw, matches("district_no"), ends_with("Zweitstimmen"))

# transformation: wide to long
data1976_erst_long <- gather(data = data1976_erst, key = party_votetype, value = votes_party_district, -district_no)
data1976_zweit_long <- gather(data = data1976_zweit, key = party_votetype, value = votes_party_district, -district_no)

# separating party and vote type information into separate columns
data1976_erst_long <- separate(data1976_erst_long, col = "party_votetype", into = c("party_short", "vote_type"), sep = "_")
data1976_zweit_long <- separate(data1976_zweit_long, col = "party_votetype", into = c("party_short", "vote_type"), sep = "_")


#----------------------- 
# Context dataset

data1976_context <- select(data1976_raw, -ends_with("stimmen"))

# create turnout variable within context data
data1976_context$turnout_district <- data1976_context$voters_district / data1976_context$eligible_district * 100

# add year variable
data1976_context$year <- 1976

# merge vote data and context data
data1976_erst_long <- left_join(x = data1976_erst_long, y = data1976_context, by = "district_no")
data1976_erst_long <- select(data1976_erst_long, -ends_with("zweit"))
data1976_erst_long <- rename(data1976_erst_long,
                             invalidvotes_district = invalidvotes_district_erst,
                             validvotes_district = validvotes_district_erst)

data1976_zweit_long <- left_join(x = data1976_zweit_long, y = data1976_context, by = "district_no")
data1976_zweit_long <- select(data1976_zweit_long, -ends_with("erst"))
data1976_zweit_long <- rename(data1976_zweit_long,
                             invalidvotes_district = invalidvotes_district_zweit,
                             validvotes_district = validvotes_district_zweit)

# join data for vote types
tidy_bundestag1976 <- bind_rows(data1976_erst_long, data1976_zweit_long)

# create voteshare variable
tidy_bundestag1976$voteshare_party_district <- tidy_bundestag1976$votes_party_district / tidy_bundestag1976$validvotes_district * 100
tidy_bundestag1976$vote_type <- gsub(pattern = "stimmen", replacement = "stimme", x = tidy_bundestag1976$vote_type)

# correct party names: remove dots and whitespaces in names
tidy_bundestag1976$party_short <- gsub("\\.", "", x = tidy_bundestag1976$party_short)
tidy_bundestag1976$party_short <- gsub(" ", "", x = tidy_bundestag1976$party_short)

# create variable with long party name (merge via external dataset)
partynames <- as.data.frame(rbind(
  c("SPD", "Sozialdemokratische Partei Deutschlands"),
  c("CDU", "Christlich-Demokratische Union Deutschlands"),
  c("FDP", "Freie Demokratische Partei"),
  c("CSU", "Christlich-soziale Union in Bayern"),
  c("AUD", "Aktionsgemeinschaft Unabhängiger Deutscher"),
  c("AVP", "Aktionsgemeinschaft Vierte Partei"),
  c("CBV", "Christliche Bayerische Volkspartei"),
  c("DKP", "Deutsche Kommunistische Partei"),  
  c("EAP", "Europäische Arbeiter-Partei"),
  c("5%-BLOCK", "5%-Block"),
  c("GIM", "Gruppe Internationale Marxisten"),
  c("KPD", "Kommunistische Partei Deutschlands"),
  c("KBW", "Kommunistischer Bund Westdeutschland"),
  c("NPD", "Nationaldemokratische Partei Deutschlands"),
  c("RFP", "Recht und Freiheit Partei"),
  c("UAP", "Unabhängige Arbeiter-Partei"),
  c("VL", "Vereinigte Linke"),
  c("Übrige", "Übrige Wählergruppen/Einzelbewerber")
))

colnames(partynames) <- c("party_short", "party_long")
partynames[] <- lapply(partynames, as.character) # convert all variables to characters

tidy_bundestag1976 <- left_join(x = tidy_bundestag1976, y = partynames, by = "party_short")

# rearrange variables in order to meet overall structure
tidy_bundestag1976 <- tidy_bundestag1976 %>%
  select(district_no, district_name, state, party_short, party_long, vote_type, year, votes_party_district, voteshare_party_district, voters_district, validvotes_district, invalidvotes_district, eligible_district, turnout_district)

#----------------------- 
# Splitting Data

# splitting into one dataset on district level and one dataset on state level
tidy_bundestag1976_districts <- filter(tidy_bundestag1976, district_no < 900)
tidy_bundestag1976_states <- filter(tidy_bundestag1976, district_no > 900)

## Check 1: Internal Check
## Compare district-level and state-level results within the data set

# check_votecount <- tidy_bundestag1976_districts %>%
#   group_by(state, party_short, vote_type) %>%
#   summarize(state_votecount = sum(votes_party_district))
# 
# check_votecount <- left_join(x = check_votecount, y = tidy_bundestag1976_states,
#                              by = c("state" = "state", "party_short" = "party_short", "vote_type" = "vote_type"))
# 
# check_votecount <- select(check_votecount, state, party_short, state_votecount, vote_type, votes_party_district)
# check_votecount$check <- check_votecount$state_votecount == check_votecount$votes_party_district
# table(check_votecount$check)
# rm(check_votecount) # to remove this check object from the workspace again

## Check 2: External Check
## Compare aggregated federal result with figures as reported by Wikipedia and the Federal Returning Officer

# tidy_bundestag1976_states %>%
#   filter(district_no < 999) %>%
#   group_by(party_short, vote_type) %>%
#   summarise(votes = sum(votes_party_district)) %>%
#   arrange(vote_type, desc(votes)) %>%
#   View()

# Source 1 - Wikipedia: https://de.wikipedia.org/wiki/Bundestagswahl_1976#Amtliches_Endergebnis
# Source 2 - Federal Returning Officer: https://www.bundeswahlleiter.de/bundestagswahlen/1976.html

#----------------------- 
# Data Export

# export results
save(tidy_bundestag1976_districts, tidy_bundestag1976_states, file = "../data_tidy/tidy_bundestag1976.Rdata")
# write.csv2(tidy_bundestag1976_districts, file = "../data_tidy/tidy_bundestag1976_districts.csv")
# write.csv2(tidy_bundestag1976_states, file = "../data_tidy/tidy_bundestag1976_states.csv")

# clean up workspace
rm(list=setdiff(ls(), c("tidy_bundestag1976_districts", "tidy_bundestag1976_states")))


