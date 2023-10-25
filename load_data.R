# Source code for main pages
# Load and format daily obs data

# Load libraries
library(tidyverse)

# Load data
data <- read.csv2("data/NordicanaD daily wildlife observations 2007-2022_avec observateurs.csv", sep = ",")

# Clean data
data$Observed_sp <- str_trim(data$Observed_sp, "right")
data$Observed_sp <- gsub("\x92", "", data$Observed_sp, useBytes = TRUE)
data$Observed_sp <- gsub("'s", "s", data$Observed_sp)
data$Sector <- as.factor(data$Sector)
data$Transportation <- as.factor(data$Transportation)
data$Field_activity <- as.factor(data$Field_activity)
data$Nb_field_hours <- as.numeric(data$Nb_field_hours)

# Create unique groupID
data <- data |> 
  group_by(Year, Date, Sector, Observers, Nb_observers, 
           Nb_field_hours, Transportation, Field_activity) |> 
  mutate(groupID = cur_group_id())


# merge brown and collared lemming
data[which(data$Observed_sp %in% c("Brown Lemming", "Collared Lemming")), "Observed_sp"] <- "Lemming sp."

data <- data |> 
  ungroup() |> 
  mutate(Observed_sp = as.factor(Observed_sp)) |> 
  mutate(Nb_field_hours = Nb_field_hours / Nb_observers) |> # Correct time from total time (time x person) to real time
  mutate(Nb_ind_h = Nb_ind / Nb_field_hours) |>  # Individual observed per hour
  select(-c(Transportation, Field_activity, Notes, Observers))

# Remove row with empty sp vector 
data <- data[-which(data$Observed_sp==""), ]
