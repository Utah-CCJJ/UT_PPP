# read in admin data
prison_admin = read_excel("data/PPP 2025 Data_Feb_25.xlsx", sheet = "Admits")
#head(prison_admin)

# group "G" & "N" gender labels (due to small n's)
prison_admin$gender = ifelse(prison_admin$gender == "G", "Other", prison_admin$gender) 
prison_admin$gender = ifelse(prison_admin$gender == "N", "Other", prison_admin$gender) 

# rename offense_type to "UNKNOWN" if missing values
prison_admin$primary_offense_type[is.na(prison_admin$primary_offense_type)] = "OTHER"

# group certain offenses (that have small n's)
prison_admin$primary_offense_type = ifelse(prison_admin$primary_offense_type == "SEX/REGISTERABLE", "SEX OFFENSE", prison_admin$primary_offense_type) 
prison_admin$primary_offense_type = ifelse(prison_admin$primary_offense_type == "SEX/NON-REGISTERABLE", "SEX OFFENSE", prison_admin$primary_offense_type)

# keep only complete data
prison_admin = prison_admin %>% filter(admit_year < 2025) 

# read in releases data
prison_rel = read_excel("data/PPP 2025 Data_Feb_25.xlsx", sheet = "Releases")
#head(prison_rel)

# keep only complete data
prison_rel = prison_rel %>% filter(release_year < 2025) 

# create year/month var for admin file
prison_admin <- unite(prison_admin, "Time", admit_year, admit_month, sep = "-", remove = TRUE, na.rm = FALSE)
prison_admin$Year_Quarter <- yearquarter(prison_admin$Time)

# create year/month var for releases file
prison_rel <- unite(prison_rel, "Time", release_year, release_month, sep = "-", remove = TRUE, na.rm = FALSE)
prison_rel$Year_Quarter <- yearquarter(prison_rel$Time)

# agg admin data
prison_admin_agg <- prison_admin %>%
  group_by(Year_Quarter) %>%
  summarize(Count = n())

# agg release data
prison_rel_agg <- prison_rel %>%
  group_by(Year_Quarter) %>%
  summarize(Count = n()) 

# merge data sets
admin_rel_merged_df <- left_join(
  prison_admin_agg %>% rename(Admissions = Count), 
  prison_rel_agg %>% rename(Releases = Count),
  by = "Year_Quarter"
) %>% mutate(diff = Admissions - Releases)

#write csv
write.csv(admin_rel_merged_df, "data/admin_rel_merged_df.csv")



