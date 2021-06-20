#install packages
#install.packages(c("dplyr", "tidyverse", "data.table", "writexl"))
library(dplyr)
library(tidyverse)
library(data.table)
library(writexl)

#upload data as a csv below
SASS_data <-fread("Spring 21 SI PASS Combined.csv")

#Delete the SI_Leader, `Mid-Term_Grade` columns
SASS_data <- subset(SASS_data, select = -c(Term, SI_Leader, `Mid-Term_Grade`, Student_ID))

#total students in each course -- ("print(studentTotal)")
studentTotal <- data.frame(lapply(SASS_data[ , c('Course_ID')], table))

#all unique course IDs -- ("print(Courses)") 
Courses <- data.frame(Course = unique(SASS_data$Course_ID))

#add GPA 
SASS_data <- mutate(SASS_data, GPA = Final_Grade)
#change letter B to GPA
    SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "A", 4.0))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "A-", 3.7))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "B+", 3.3))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "B", 3.0))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "B-", 2.7))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "C+", 2.3))
  SASS_data <- SASS_data %>%
  mutate(GPA = replace(GPA, GPA == "C", 2.0))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "D", 1.0))
  SASS_data <- SASS_data %>%
    mutate(GPA = replace(GPA, GPA == "F", 0.0))
         
#sort by course ID and instructor 
  SASS_data %>%
  arrange(Course_ID, Instructor)

#make dep. column
SASS_data <- 
    mutate(SASS_data, Department = substr(Course_ID, 0, 3))

#categorize courses by department 
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "ACG"| Department == "FIN", "Accounting"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "BSC"| Department == "MCB"| Department == "PCB", "Biology"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "CHM"| Department == "BCH", "Chemistry"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "EGN"| Department == "EMA"| Department == "EML", "Engineering"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "MAN", "Management"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "MAC"| Department == "STA", "Math"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "COP", "Comp Sci"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "PHY", "Physics"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "PHI", "Philosophy"))

#add and fill No visits and regular columns (the students who visited 1-4 times are the ones remaining)
SASS_data <-  mutate(SASS_data, NoVisits = ifelse(is.na(Visits), TRUE, FALSE))
SASS_data <-  mutate(SASS_data, Regular = ifelse(!is.na(Visits) & (Visits >= 5), TRUE, FALSE))

view(SASS_data)
write_xlsx(SASS_data, "SASS_data.xlsx")


