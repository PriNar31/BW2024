#test before running code at last step
ID <- "XJTRBQXx4S2"
sumPID_info(ID)




# Install & load dplyr
install.packages("dplyr", dependencies = TRUE)                   
library("dplyr")

# Install & load ggplot2
install.packages("ggplot2", dependencies = TRUE)               
library("ggplot2")


# Step 1: Install and load required packages

install.packages("data.table")

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

install.packages("tidyverse")

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(readr)
library(readxl)




# Define the directory path where your files are located
directory_path <- "C:/Users/goupr74p/OneDrive - University of Otago/Desktop/Birthweight analysis PNapril2024"

#Load  datasets into R environment
library(readxl)
birthwt <- read_excel("C:/Users/goupr74p/OneDrive - University of Otago/Desktop/Birthweight analysis PNapril2024/birthwt.xlsx")
View(birthwt)

# Set the working directory to the specified directory path
setwd(directory_path)


# Step 3: Understand data structure (optional)
# str(birthwt)
# summary(birthwt)
column_names <- colnames(birthwt)
print(column_names)



str(birthwt)
summary(birthwt)
head(birthwt)



## remove duplicates
distinct(birthwt)

##new name
BWnew <- distinct(birthwt)

nrow(distinct(BWnew))
length(unique(BWnew$PERSON_ID))
nrow(distinct(BWnew,PERSON_ID))

babyID <- "paNppFN3056"

babyDF <- BWnew %>% filter(PERSON_ID == babyID)

babyDF

print(babyDF)

babygender <- max(babyDF$GENDER_CODE, na.rm = TRUE)

year_birth <- min(babyDF$'YEAR OF BIRTH', na.rm = TRUE)

low_BW <- min(babyDF$LOW_BIRTHWEIGHT, na.rm = TRUE)

gest_age <- min(babyDF$GESTATIONAL_AGE, na.rm = TRUE)


ethniccodes <- unique(babyDF$Ethnic_Group, na.rm = TRUE)
ethnic_4 <- 4 %in% c(ethniccodes)
ethnic_36 <- 36 %in% c(ethniccodes)
ethnic_43 <- 43 %in% c(ethniccodes)
ethnic_3 <- 3 %in% c(ethniccodes)


# View the first few rows to check if the age groups are assigned correctly
head(babyDF)

finalDF <- data.frame (babyID = babyID, 
                      babygender = babygender,
                      year_birth = year_birth,
                      low_BW = low_BW,
                      gest_age = gest_age,
                      ethnic_3 = ethnic_3,
                      ethnic_4 = ethnic_4,
                      ethnic_36 = ethnic_36,
                      ethnic_43 = ethnic_43)



sumPID_info <- function(babyID){

  babyDF <- BWnew %>% filter(PERSON_ID == babyID)
  
  babyDF
  
  babygender <- max(babyDF$GENDER_CODE, na.rm = TRUE)
  
  year_birth <- min(babyDF$'YEAR OF BIRTH', na.rm = TRUE)
  
  low_BW <- min(babyDF$LOW_BIRTHWEIGHT, na.rm = TRUE)
  
  gest_age <- min(babyDF$GESTATIONAL_AGE, na.rm = TRUE)
  
  
  ethniccodes <- unique(babyDF$Ethnic_Group, na.rm = TRUE)
  ethnic_4 <- 4 %in% c(ethniccodes)
  ethnic_36 <- 36 %in% c(ethniccodes)
  ethnic_43 <- 43 %in% c(ethniccodes)
  ethnic_3 <- 3 %in% c(ethniccodes)
  
  
  finalDF <- data.frame (babyID = babyID, 
                         babygender = babygender,
                         year_birth = year_birth,
                         low_BW = low_BW,
                         gest_age = gest_age,
                         ethnic_3 = ethnic_3,
                         ethnic_4 = ethnic_4,
                         ethnic_36 = ethnic_36,
                         ethnic_43 = ethnic_43)
  
  return(finalDF)
}

 
# loop the entire PERSON_ID
avail_ID <- unique(BWnew$PERSON_ID)

DF <- vector()
for(i in 1:length(avail_ID)){
  ID <- avail_ID[i]
  print(i)
  intDF <- sumPID_info(ID)
  DF <- rbind(DF, intDF)
}

fwrite(DF, "BW_2_april2024")

