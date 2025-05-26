# load packages ####
library(dplyr)
library(readxl)
library(VIM)

# load data ####
# Load the data from the Excel file
raw_potential <- read_excel("C:/Users/janek/OneDrive/uni/Master/Semester_2/Gründungen und Innovationen im räumlichen Kontext/FISCon/01_data/raw/1_POTENTIAL.xlsx")

# get view of the data ####
# view the first few rows of each dataset
head(raw_potential)

# rename collums
raw_potential <- raw_potential %>%
  rename(
    CNT  = Land,
    REG  = Region,
    SEX  = Geschlecht,
    AGE  = Alter,
    EMP  = Arbeitsstatus,
    INC  = Einkommen,
    EDU  = Ausbildung,
    EXP  = Gründungserfahrung,
    CON  = Gründerkontakt,
    POT  = POTENTIAL,
    MKT_NAT  = Marktfreiheit_national,
    PROS_REG = Prosperität_regional
  )

# view the structure of each dataset
str(raw_potential)


# descriptive statistics ####
summary(raw_potential)

# total observations
nrow(raw_potential)

# every -9 in to NA
raw_potential[raw_potential == -9] <- NA

# check for missing values
missing_values <- colSums(is.na(raw_potential))

aggr(raw_potential, 
     numbers = TRUE,
     sortVars = TRUE,
     labels = names(raw_potential),
     cex.axis = 0.6,    # Textgröße auf X-Achse
     las = 2,           # Dreht die Beschriftung (2 = senkrecht)
     gap = 3,
     ylab = c("Fehlende Werte", "Muster"))

# print missing values
print(missing_values)

# delete observations with NAs
clean_potential <- na.omit(raw_potential)


# aggregate data ####
potential_agg <- raw_potential %>%
  group_by(Region) %>%
  summarise(
    Arbeitsstatus = sum(Arbeitsstatus, na.rm = TRUE),
    .groups = 'drop'
  )

head(potential_agg)

function() {
  # This is a placeholder for the function body
  # You can add your code here
}

# build logit model

