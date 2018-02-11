#-----------------------
# Tidy election results 
# 2005 Bundestag
#----------------------- 

# rm(list = ls())

library(tidyr)
library(dplyr)
# library(stringr)

#----------------------- 
# Loading Data

# load raw data
data2005_raw <- read.csv2(file = "../data_raw/btw05_kerg.csv", header = FALSE, skip = 8, stringsAsFactors = FALSE, na.strings = "")

# load separately: variable names
# variable names are stretched over three rows
# NOTE: DIFFERENT VS EARLIER PERIODS: two instead of three rows
colnames2005_raw <- read.csv2(file = "../data_raw/btw05_kerg.csv", header = FALSE, skip = 5, nrows = 3, stringsAsFactors = FALSE, na.strings = "")

# alternative: load directly from Federal Returning Officer's website
# data2005_raw <- read.csv2(file = "https://www.bundeswahlleiter.de/dam/jcr/01e65a67-6d5b-47ee-b8ec-0fd243493ebc/btw05_kerg.csv", header = TRUE, skip = 8, stringsAsFactors = FALSE, na.strings = "")
# colnames2005_raw <- read.csv2(file = "https://www.bundeswahlleiter.de/dam/jcr/01e65a67-6d5b-47ee-b8ec-0fd243493ebc/btw05_kerg.csv", header = FALSE, skip = 5, nrows = 3, stringsAsFactors = FALSE, na.strings = "")

# create clean vector of variable names
colnames2005_raw <- as.data.frame(t(colnames2005_raw)) # transpose for easier editing
colnames2005_raw[] <- lapply(colnames2005_raw, as.character) # convert all variables to characters

# NOTE: DIFFERENT VS EARLIER PERIODS: original file leaves cells blank where the value from the cell on the left should have been repeated

# correction for party variable: use value from cell directly to the left
while(any(is.na(colnames2005_raw[,1]))) { # loop condition: NAs in first variable (parties)
  colnames2005_raw[which(is.na(colnames2005_raw[,1])), 1] <- colnames2005_raw[which(is.na(colnames2005_raw[,1]))-1, 1]
}

# NOTE: DIFFERENT VS EARLIER PERIODS:
# correction for vote type variable: also use value from cell directly to the left
# since "there is always only one value missing, we "only" half of the values is missing, a loop is not necessary
# since we have a missing in the first value (for which there is no "earlier" value), a NA needs to be added at the beginning
colnames2005_raw[which(is.na(colnames2005_raw[,2])), 2] <- c(NA, colnames2005_raw[which(is.na(colnames2005_raw[,2]))-1, 2])

colnames2005 <- do.call(paste, c(colnames2005_raw[c("V1", "V2", "V3")], sep = "_")) # paste together // NOTE: DIFFERENT VS EARLIER PERIODS: V3 needs to be added
colnames(data2005_raw) <- colnames2005 # overwriting empty/generic variable names in dataset


#----------------------- 
# Remove unnecessary information from data

# identify columns which are completely empty - can be dropped
na_per_col <- apply(data2005_raw, 2, function(x) {sum(is.na(x))})
data2005_raw <- data2005_raw[, -(which(na_per_col == nrow(data2005_raw)))]

# identify rows which are completely empty (divider rows) - can be dropped
# NOTE: DIFFERENT VS EARLIER PERIODS: 2005 data has no empty rows anymore, thus the function needs to be wrapped into an if() condition (otherwise, the whole dataset is dropped)
na_per_row <- apply(data2005_raw, 1, function(x) {sum(is.na(x))})

if(any(na_per_row == ncol(data2005_raw))) {
  data2005_raw <- data2005_raw[-(which(na_per_row == ncol(data2005_raw))), ]
}

# NOTE: DIFFERENT VS EARLIER PERIODS: remove all information about previous election period
data2005_raw <- select(data2005_raw, -ends_with("Vorperiode"))
# ... and make the remaining variable names a bit prettier
colnames(data2005_raw) <- gsub(pattern = "_Endgültig", replacement = "", x = colnames(data2005_raw))

#----------------------- 
# Processing data

# proper variable names for key variables
colnames(data2005_raw)[1:9] <- c("district_no",
                                 "district_name",
                                 "state",
                                 "eligible_district",
                                 "voters_district",
                                 "invalidvotes_district_erst",
                                 "invalidvotes_district_zweit",
                                 "validvotes_district_erst",
                                 "validvotes_district_zweit")

# Splitting into separate dataframes for Erststimme and Zweitstimme
data2005_erst <- select(data2005_raw, matches("district_no"), ends_with("Erststimmen"))
data2005_zweit <- select(data2005_raw, matches("district_no"), ends_with("Zweitstimmen"))

# transformation: wide to long
data2005_erst_long <- gather(data = data2005_erst, key = party_votetype, value = votes_party_district, -district_no)
data2005_zweit_long <- gather(data = data2005_zweit, key = party_votetype, value = votes_party_district, -district_no)

# separating party and vote type information into separate columns
data2005_erst_long <- separate(data2005_erst_long, col = "party_votetype", into = c("party_short", "vote_type"), sep = "_")
data2005_zweit_long <- separate(data2005_zweit_long, col = "party_votetype", into = c("party_short", "vote_type"), sep = "_")


#----------------------- 
# Context dataset

data2005_context <- select(data2005_raw, -ends_with("stimmen"))

# create turnout variable within context data
data2005_context$turnout_district <- data2005_context$voters_district / data2005_context$eligible_district * 100

# add year variable
data2005_context$year <- 2005

# merge vote data and context data
data2005_erst_long <- left_join(x = data2005_erst_long, y = data2005_context, by = "district_no")
data2005_erst_long <- select(data2005_erst_long, -ends_with("zweit"))
data2005_erst_long <- rename(data2005_erst_long,
                             invalidvotes_district = invalidvotes_district_erst,
                             validvotes_district = validvotes_district_erst)

data2005_zweit_long <- left_join(x = data2005_zweit_long, y = data2005_context, by = "district_no")
data2005_zweit_long <- select(data2005_zweit_long, -ends_with("erst"))
data2005_zweit_long <- rename(data2005_zweit_long,
                             invalidvotes_district = invalidvotes_district_zweit,
                             validvotes_district = validvotes_district_zweit)

# join data for vote types
tidy_bundestag2005 <- bind_rows(data2005_erst_long, data2005_zweit_long)

# create voteshare variable
tidy_bundestag2005$voteshare_party_district <- tidy_bundestag2005$votes_party_district / tidy_bundestag2005$validvotes_district * 100

# NOTE: DIFFERENT VS EARLIER PERIODS: rewrote using gsub() to avoid dependency on stringr
tidy_bundestag2005$vote_type <- gsub(pattern = "stimmen", replacement = "stimme", x = tidy_bundestag2005$vote_type)

# NOTE: DIFFERENT VS EARLIER PERIODS: "state" variable has numerical codes instead of two-letter abbreviations for states
# state variable: for electoral districts, the numeric state variable already contains the information
tidy_bundestag2005$state <- recode(tidy_bundestag2005$state, "1" = 'Schleswig-Holstein',
                                   "2" = 'Hamburg',
                                   "3" = 'Niedersachsen',
                                   "4" = 'Bremen',
                                   "5" = 'Nordrhein-Westfalen',
                                   "6" = 'Hessen',
                                   "7" = 'Rheinland-Pfalz',
                                   "8" = 'Baden-Württemberg',
                                   "9" = 'Bayern',
                                   "10" = 'Saarland',
                                   "11" = 'Berlin',
                                   "12" = 'Brandenburg',
                                   "13" = 'Mecklenburg-Vorpommern',
                                   "14" = 'Sachsen',
                                   "15" = 'Sachsen-Anhalt',
                                   "16" = 'Thüringen',
                                   .default = NA_character_)

# state variable: states were set to NA in the previous step. for states, the information is stored in the "district name" variable
tidy_bundestag2005$state[which(is.na(tidy_bundestag2005$state))] <- tidy_bundestag2005$district_name[which(is.na(tidy_bundestag2005$state))]

# create variable with long party name (merge via external dataset)
# NOTE: DIFFERENT VS EARLIER PERIODS: created via rbind so that short and long are next to each other during the creation - avoids making mistakes by row glitches
partynames <- as.data.frame(rbind(
  c("SPD", "Sozialdemokratische Partei Deutschlands"),
  c("CDU", "Christlich-Demokratische Union Deutschlands"),
  c("CSU", "Christlich-soziale Union in Bayern"),
  c("GRÜNE", "Bündnis 90/Die Grünen"),
  c("FDP", "Freie Demokratische Partei"),
  c("Die Linke.", "Die Linkspartei.PDS"),
  c("Offensive D", "Partei rechtsstaatliche Offensive"),
  c("REP", "Die Republikaner"),
  c("NPD", "Nationaldemokratische Partei Deutschlands"), 
  c("Die Tierschutzpartei", "Partei Mensch Umwelt Tierschutz"), 
  c("GRAUE", "Die Grauen - Graue Panther"), 
  c("PBC", "Partei Bibeltreuer Christen"), 
  c("DIE FRAUEN", "Feministische Partei - Die Frauen"), 
  c("FAMILIE", "Familien-Partei Deutschlands"),
  c("BüSo", "Bürgerrechtsbewegung Solidarität"),
  c("BP", "Bayernpartei"),
  c("ZENTRUM", "Deutsche Zentrumspartei"),
  c("Deutschland", "Ab jetzt... - Bündnis für Deutschland Partei für Volksabstimmung und gegen Zuwanderung ins soziale Netz"),
  c("AGFG", "Allianz für Gesundheit, Frieden und soziale Gerechtigkeit"),
  c("APPD", "Anarchistische Pogo-Partei Deutschlands"),
  c("MLPD", "Marxistisch-Leninistische Partei Deutschland"), 
  c("Die PARTEI", "Partei für Arbeit, Rechtsstaat, Tierschutz, Elitenförderung und basisdemokratische Initiative"),
  c("CM", "Christliche Mitte"),
  c("DSU", "Deutsche Soziale Union"),
  c("HP", "Humanistische Partei"),
  c("HUMANWIRTSCHAFTS- PARTEI", "Humanwirtschaftspartei"),
  c("STATT Partei", "STATT Partei - Die Unabhängigen"),
  c("UNABHÄNGIGE", "UNABHÄNGIGE für bürgernahe Demokratie"),
  c("50Plus", "50Plus Bürger- und Wählerinitiative für Brandenburg"),
  c("PSG", "Partei für Soziale Gleichheit, Sektion der Vierten Internationale"),
  c("Pro DM", "Pro Deutsche Mitte – Initiative Pro D-Mark"),
  c("Übrige", "Übrige Wählergruppen/Einzelbewerber")  
))

colnames(partynames) <- c("party_short", "party_long")

partynames[] <- lapply(partynames, as.character) # convert all variables to characters

tidy_bundestag2005 <- left_join(x = tidy_bundestag2005, y = partynames, by = "party_short")

# rearrange variables in order to meet overall structure
tidy_bundestag2005 <- tidy_bundestag2005 %>%
  select(district_no, district_name, state, party_short, party_long, vote_type, year, votes_party_district, voteshare_party_district, voters_district, validvotes_district, invalidvotes_district, eligible_district, turnout_district)

#----------------------- 
# Splitting Data

# splitting into one dataset on district level and one dataset on state level
tidy_bundestag2005_districts <- filter(tidy_bundestag2005, district_no < 900)
tidy_bundestag2005_states <- filter(tidy_bundestag2005, district_no > 900)

## Check 1: Internal Check
## Compare district-level and state-level results within the data set

# NOTE: DOES NOT WORK ANYMORE in recent dataset because the "state" variable does not hold abbreviations of state names anymore
# check_votecount <- tidy_bundestag2005_districts %>%
#   group_by(state, party_short, vote_type) %>%
#   summarize(state_votecount = sum(votes_party_district, na.rm = TRUE))
# 
# check_votecount <- left_join(x = check_votecount, y = tidy_bundestag2005_states,
#                              by = c("state" = "state", "party_short" = "party_short", "vote_type" = "vote_type"))
# 
# check_votecount <- select(check_votecount, state, party_short, state_votecount, vote_type, votes_party_district)
# check_votecount$check <- check_votecount$state_votecount == check_votecount$votes_party_district
# table(check_votecount$check)
# rm(check_votecount) # to remove this check object from the workspace again

## Check 2: External Check
## Compare aggregated federal result with figures as reported by Wikipedia and the Federal Returning Officer

tidy_bundestag2005_states %>%
  filter(district_no < 999) %>%
  group_by(party_short, vote_type) %>%
  summarise(votes = sum(votes_party_district, na.rm = TRUE)) %>%
  arrange(vote_type, desc(votes)) %>%
  View()

# Source 1 - Wikipedia: https://de.wikipedia.org/wiki/Bundestagswahl_2005#Amtliches_Endergebnis
# Source 2 - Federal Returning Officer: https://www.bundeswahlleiter.de/bundestagswahlen/2005.html

#----------------------- 
# Data Export

# export results
save(tidy_bundestag2005_districts, tidy_bundestag2005_states, file = "../data_tidy/tidy_bundestag2005.Rdata")
# write.csv2(tidy_bundestag2005_districts, file = "../data_tidy/tidy_bundestag2005_districts.csv")
# write.csv2(tidy_bundestag2005_states, file = "../data_tidy/tidy_bundestag2005_states.csv")

# clean up workspace
rm(list=setdiff(ls(), c("tidy_bundestag2005_districts", "tidy_bundestag2005_states")))


