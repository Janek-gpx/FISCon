# load packages ####
library(dplyr)
library(xl)
library(readxl)

# load data ####
# Load the data from the Excel file
raw_potential <- read_excel("C:/Users/janek/OneDrive/uni/Master/Semester_2/Gründungen und Innovationen im räumlichen Kontext/FISCon/01_data/raw/1_POTENTIAL.xlsx")
raw_nascent <- read_excel("C:/Users/janek/OneDrive/uni/Master/Semester_2/Gründungen und Innovationen im räumlichen Kontext/FISCon/01_data/raw/2_NASCENT.xlsx")
raw_young <- read_excel("C:/Users/janek/OneDrive/uni/Master/Semester_2/Gründungen und Innovationen im räumlichen Kontext/FISCon/01_data/raw/3_YOUNG.xlsx")
raw_nascent_opp <- read_excel("C:/Users/janek/OneDrive/uni/Master/Semester_2/Gründungen und Innovationen im räumlichen Kontext/FISCon/01_data/raw/4_NASCENT-OPP.xlsx")
raw_young_opp <- read_excel("C:/Users/janek/OneDrive/uni/Master/Semester_2/Gründungen und Innovationen im räumlichen Kontext/FISCon/01_data/raw/5_YOUNG-OPP.xlsx")

# get view of the data ####
# view the first few rows of each dataset
head(raw_potential)
head(raw_nascent)
head(raw_young)
head(raw_nascent_opp)
head(raw_young_opp)
# view the structure of each dataset
str(raw_potential)
str(raw_nascent)
str(raw_young)
str(raw_nascent_opp)
str(raw_young_opp)
