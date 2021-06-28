#install packages
install.packages(c("tidyverse", "data.table", "writexl"))
library(tidyverse)
library(data.table)
library(writexl)

#upload data as a csv below
#Insert the csv file name between the quotation marks
SASS_data <-fread("Spring 21 SI PASS Combined.csv")

#Delete the SI_Leader, `Mid-Term_Grade` columns
SASS_data <- subset(SASS_data, select = -c(Term, SI_Leader, `Mid-Term_Grade`, Student_ID))

#total students in each course -- ("print(studentTotal)")
studentTotal <- data.frame(lapply(SASS_data[ , c('Course_ID')], table))
#all unique course IDs -- ("print(Courses)") 
Courses <- data.frame(Course = unique(SASS_data$Course_ID))

#creating "results" folder in sass data folder on desktop 
dir.create("./Results")

#add GPA and change letter to GPA
SASS_data <- mutate(SASS_data, GPA = Final_Grade)
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
  mutate(Department = replace(Department, Department == "BSC"| Department == "MCB"| Department == "PCB" | Department == "ESC", "Biology"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "CHM"| Department == "BCH", "Chemistry"))
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "EGN"| Department == "EMA"| Department == "EML" | Department == "EMA", "Engineering"))
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
SASS_data <- SASS_data %>%
  mutate(Department = replace(Department, Department == "ECO", "Economics"))

#add and fill No visits and regular columns (the students who visited 1-4 times are the ones remaining)
SASS_data <-  mutate(SASS_data, NoVisits = ifelse(is.na(Visits), TRUE, FALSE))
SASS_data <-  mutate(SASS_data, Regular = ifelse(!is.na(Visits) & (Visits >= 5), TRUE, FALSE))
#view(SASS_data)
write_xlsx(SASS_data, "SASS_data.xlsx")
"________________________________________________________________________________________________________________________________________"
"________________________________________________________________________________________________________________________________________"
#Data analysis by instructor (lines 81-274)
instructor_data <- SASS_data %>% group_by(Course_ID) %>% group_by(Instructor)

#table with mean GPA for each subset (0,1,5) in each course
regularSubsetI <- subset(instructor_data, Regular == TRUE)
regularSubsetI <- subset(regularSubsetI, GPA!="W")
regularSubsetI <- transform(regularSubsetI, GPA = as.numeric(GPA))
meanGpaRI <- data.frame(aggregate (x=regularSubsetI[,c("GPA")], by=data.frame(regularSubsetI$Instructor), FUN=mean))
meanGpaRI <- setnames(meanGpaRI, c("Instructor", "GPA"))
#view(meanGpaRI)

underFiveSubsetI <- subset(instructor_data, Visits > 0 & Visits < 5)
underFiveSubsetI <- subset(underFiveSubsetI, GPA!="W")
underFiveSubsetI <- transform(underFiveSubsetI, GPA = as.numeric(GPA))
meanGpaUFI <- data.frame(aggregate (x=underFiveSubsetI[,c("GPA")], by=data.frame(underFiveSubsetI$Instructor), FUN=mean))
meanGpaUFI <- setnames(meanGpaUFI, c("Instructor", "GPA"))
#view(meanGpaUFI)

noVisitSubsetI <- subset(instructor_data, NoVisits == TRUE)
noVisitSubsetI <- subset(noVisitSubsetI, GPA!="W")
noVisitSubsetI <- transform(noVisitSubsetI, GPA = as.numeric(GPA))
meanGpaNVI <- data.frame(aggregate (x=noVisitSubsetI[,c("GPA")], by=data.frame(noVisitSubsetI$Instructor), FUN=mean))
meanGpaNVI <- setnames(meanGpaNVI, c("Instructor", "GPA"))
#view(meanGpaNVI)

GPA_avgI <- subset(instructor_data, GPA!="W")
GPA_avgI <- transform(GPA_avgI, GPA = as.numeric(GPA))
GPA_avgI <- data.frame( aggregate (x=GPA_avgI[,c("GPA")], by=data.frame(GPA_avgI$Instructor), FUN=mean))
GPA_avgI <- setnames(GPA_avgI, c("Instructor", "GPA"))

GPA_totalI <- merge.data.frame(merge.data.frame(merge.data.frame(
  meanGpaRI, 
  meanGpaUFI, by = "Instructor", all = TRUE),
  meanGpaNVI, by = "Instructor", all = TRUE),
  GPA_avgI, by = "Instructor", all = TRUE)
GPA_totalI <- setnames(GPA_totalI, c("Instructor", "Regulars", "Under5", "NoVisits", "Avg_Total"))
#view(GPA_totalI)

"________________________________________________________________________________________________________________________________________"
#Counts per Instructor

studentTotalI <- data.frame(lapply(instructor_data[ , c('Instructor')], table))
totalI <- mutate(studentTotalI, n = as.numeric(studentTotalI$Instructor.Freq))
totalI <- subset(totalI, select = c(1,3))
totalI <- setnames(totalI, c("Instructor", "n"))

#count of regulars in each course
reg_countI <- regularSubsetI %>% group_by(Instructor) %>% tally()
#count of 1-4x in each course
under5_countI <- underFiveSubsetI %>% group_by(Instructor) %>% tally()
#count of no visits in each course
noVisits_countI <- noVisitSubsetI %>% group_by(Instructor) %>% tally()

# regular %
reg_I <- mutate(reg_countI, n = as.numeric(reg_countI$n))
reg_percentI <- merge.data.frame(reg_I, totalI, by = "Instructor", all = TRUE)
reg_percentI <- setnames(reg_percentI, c("Instructor", "Regulars", "Total"))
reg_percentI[is.na(reg_percentI)] <- 0 
reg_percentI <- mutate(reg_percentI , "%_regulars" = round((Regulars/ Total) * 100))

# 1-4 visits %
under5_I <- mutate(under5_countI, n = as.numeric(under5_countI$n))
under5_percentI <- merge.data.frame(under5_I, totalI, by = "Instructor", all = TRUE)
under5_percentI <- setnames(under5_percentI, c("Instructor", "under5", "Total"))
under5_percentI[is.na(under5_percentI)] <- 0 
under5_percentI <- mutate(under5_percentI , "%_under5" = round((under5 / Total) * 100))

# no visits %
noVisits_I <- mutate(noVisits_countI, n = as.numeric(noVisits_countI$n))
noVisits_percentI <- merge.data.frame(noVisits_I, totalI, by = "Instructor", all = TRUE)
noVisits_percentI <- setnames(noVisits_percentI, c("Instructor", "noVisits", "Total"))
noVisits_percentI[is.na(noVisits_percentI)] <- 0 
noVisits_percentI <- mutate(noVisits_percentI , "%_noVisits" = round((noVisits / Total) * 100))

count_by_instructor <- cbind(reg_percentI,under5_percentI, noVisits_percentI)
count_by_instructor <- subset(count_by_instructor, select = c(1,2,4,6,8,10,12,11))
#view(count_by_instructor)
"________________________________________________________________________________________________________________________________________"
#Grades per Instructor 

#regulars
As_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "4") %>% tally()
Am_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.7") %>% tally()
Bp_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.3") %>% tally()
B_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.7") %>% tally()
Cp_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.3") %>% tally()
C_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_RegsI <- regularSubsetI %>% group_by(Instructor) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
regularSubsetI1 <- subset(instructor_data, Regular == TRUE)
W_RegsI <- regularSubsetI1 %>% group_by(Instructor) %>% filter(Final_Grade == "W") %>% tally()


Grades_RegsI <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(
    As_RegsI, 
    Am_RegsI, by = "Instructor", all = TRUE),
    Bp_RegsI, by = "Instructor", all = TRUE),
    B_RegsI, by = "Instructor", all = TRUE),
    Bm_RegsI, by = "Instructor", all = TRUE),
    Cp_RegsI, by = "Instructor", all = TRUE),
    C_RegsI, by = "Instructor", all = TRUE),
    D_RegsI, by = "Instructor", all = TRUE),
    F_RegsI, by = "Instructor", all = TRUE)

Grades_RegsI <-  merge.data.frame(Grades_RegsI, W_RegsI, by = "Instructor", all = TRUE)  

Grades_RegsI <- setnames(Grades_RegsI, c("Instructor", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_RegsI <- cbind(Grades_RegsI, total = rowSums(Grades_RegsI[,2:11], na.rm = TRUE))
#view(Grades_RegsI)


#under5 
As_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "4") %>% tally()
Am_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.7") %>% tally()
Bp_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.3") %>% tally()
B_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.7") %>% tally()
Cp_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.3") %>% tally()
C_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Under5I <- underFiveSubsetI %>% group_by(Instructor) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
underFiveSubsetI1 <- subset(instructor_data, Visits > 0 & Visits < 5)
W_Under5I <- underFiveSubsetI1 %>% group_by(Instructor) %>% filter(Final_Grade == "W") %>% tally()

Grades_Under5I <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_Under5I, 
    Am_Under5I, by = "Instructor", all = TRUE),
    Bp_Under5I, by = "Instructor", all = TRUE),
    B_Under5I, by = "Instructor", all = TRUE),
    Bm_Under5I, by = "Instructor", all = TRUE),
    Cp_Under5I, by = "Instructor", all = TRUE),
    C_Under5I, by = "Instructor", all = TRUE),
    D_Under5I, by = "Instructor", all = TRUE),
    F_Under5I, by = "Instructor", all = TRUE),
    W_Under5I, by = "Instructor", all = TRUE)

Grades_Under5I <- setnames(Grades_Under5I, c("Instructor", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Under5I <- cbind(Grades_Under5I, total = rowSums(Grades_Under5I[,2:11], na.rm = TRUE))
#view(Grades_Under5I)


#never visited 
As_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "4") %>% tally()
Am_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.7") %>% tally()
Bp_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.3") %>% tally()
B_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.7") %>% tally()
Cp_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.3") %>% tally()
C_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_NoVisitsI <- noVisitSubsetI %>% group_by(Instructor) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
noVisitSubsetI1 <- subset(instructor_data, NoVisits == TRUE)
W_NoVisitsI <- noVisitSubsetI1 %>% group_by(Instructor) %>% filter(Final_Grade == "W") %>% tally()


Grades_NoVisitsI <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_NoVisitsI, 
    Am_NoVisitsI, by = "Instructor", all = TRUE),
    Bp_NoVisitsI, by = "Instructor", all = TRUE),
    B_NoVisitsI, by = "Instructor", all = TRUE),
    Bm_NoVisitsI, by = "Instructor", all = TRUE),
    Cp_NoVisitsI, by = "Instructor", all = TRUE),
    C_NoVisitsI, by = "Instructor", all = TRUE),
    D_NoVisitsI, by = "Instructor", all = TRUE),
    F_NoVisitsI, by = "Instructor", all = TRUE),
    W_NoVisitsI, by = "Instructor", all = TRUE)

Grades_NoVisitsI <- setnames(Grades_NoVisitsI, c("Instructor", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_NoVisitsI <- cbind(Grades_NoVisitsI, total = rowSums(Grades_NoVisitsI[,2:11], na.rm = TRUE))
#view(Grades_NoVisits)

"________________________________________________________________________________________________________________________________________"
#DFW % per Instructor
DFWs_RegsI <- subset(Grades_RegsI, select = c("Instructor", "D", "F", "W", "total"))
DFWs_RegsI <- mutate(DFWs_RegsI, DFW = rowSums(DFWs_RegsI[, 2:4], na.rm = TRUE))
DFWs_RegsI <- mutate(DFWs_RegsI, "DFW%" = round((DFWs_RegsI$DFW/DFWs_RegsI$total)*100))

DFWs_Under5I <- subset(Grades_Under5I, select = c("Instructor", "D", "F", "W", "total"))
DFWs_Under5I <- mutate(DFWs_Under5I, DFW = rowSums(DFWs_Under5I[, 2:4], na.rm = TRUE))
DFWs_Under5I <- mutate(DFWs_Under5I, "DFW%" = round((DFWs_Under5I$DFW/DFWs_Under5I$total)*100))

DFWs_NoVisitsI <- subset(Grades_NoVisitsI, select = c("Instructor", "D", "F", "W", "total"))
DFWs_NoVisitsI <- mutate(DFWs_NoVisitsI, DFW = rowSums(DFWs_NoVisitsI[, 2:4], na.rm = TRUE))
DFWs_NoVisitsI <- mutate(DFWs_NoVisitsI, "DFW%" = round((DFWs_NoVisitsI$DFW/DFWs_NoVisitsI$total)*100))

#saves excel sheet with data analysis by instructor into folder called "results" 
write_xlsx(list(count_by_instructor = count_by_instructor, GPA_totalI = GPA_totalI, Grades_RegsI = Grades_RegsI, Grades_Under5I = Grades_Under5I, 
                Grades_NoVisitsI = Grades_NoVisitsI, DFWs_RegsI =DFWs_RegsI, DFWs_Under5I = DFWs_Under5I, DFWs_NoVisitsI = DFWs_NoVisitsI),
           "./Results/Data by Instructor.xlsx")
"________________________________________________________________________________________________________________________________________"
"________________________________________________________________________________________________________________________________________"
#Data analysis by department (lines 276-460)

#table with mean GPA for each subset (0,1,5) in each department
regularSubsetDepartment <- subset(SASS_data, Regular == TRUE)
regularSubsetDepartment <- subset(regularSubsetDepartment, GPA!="W")
regularSubsetDepartment <- mutate(regularSubsetDepartment, GPA = as.numeric(regularSubsetDepartment$GPA))

meanGpaDep <- data.frame(aggregate (x=regularSubsetDepartment[,c(GPA)], 
                                    by=data.frame(regularSubsetDepartment$Department), FUN=mean))
meanGpaDep <- setNames(meanGpaDep, c("Dep", "RegularGPA"))

underFiveSubsetDepartment <- subset(SASS_data, Visits > 0 & Visits < 5)
underFiveSubsetDepartment <- subset(underFiveSubsetDepartment, GPA!="W")
underFiveSubsetDepartment <- mutate(underFiveSubsetDepartment, GPA = as.numeric(underFiveSubsetDepartment$GPA))
underFiveSubsetDepartment1 <- aggregate (x=underFiveSubsetDepartment[,c(GPA)], 
                                         by=data.frame(underFiveSubsetDepartment$Department), FUN=mean)
underFiveSubsetDepartment1 <- setNames(underFiveSubsetDepartment1, c("Dep", "Under5VisitsGPA"))

meanGpaDep <- mutate(meanGpaDep, underFiveSubsetDepartment1["Under5VisitsGPA"])

noVisitSubsetDepartment <- subset(SASS_data, NoVisits == TRUE)
noVisitSubsetDepartment <- subset(noVisitSubsetDepartment, GPA!="W")
noVisitSubsetDepartment <- mutate(noVisitSubsetDepartment, GPA = as.numeric(noVisitSubsetDepartment$GPA))
noVisitSubsetDepartment1 <- aggregate (x=noVisitSubsetDepartment[,c(GPA)], 
                                       by=data.frame(noVisitSubsetDepartment$Department), FUN=mean)
noVisitSubsetDepartment1 <- setNames(noVisitSubsetDepartment1, c("Dep", "NoVisitGPA"))

meanGpaDep <- mutate(meanGpaDep, noVisitSubsetDepartment1["NoVisitGPA"])

GPA_total <- subset(SASS_data, GPA!="W")
GPA_total <- mutate(GPA_total, GPA = as.numeric(GPA_total$GPA))
GPA_total <- aggregate (x=GPA_total[,c(GPA)], by=data.frame(GPA_total$Department), FUN=mean)
GPA_total <- rename(GPA_total, 
                    GPA_all = x)

meanGpaDep <- mutate(meanGpaDep, GPA_total[2])
is.num <- sapply(meanGpaDep, is.numeric)
meanGpaDep[is.num] <- lapply(meanGpaDep[is.num], round, 1)
#view(meanGpaDep)

"________________________________________________________________________________________________________________________________________"
#Counts by department
#count of regulars in each course
reg_count <- regularSubsetDepartment %>% group_by(Department) %>% tally()
#count of 1-4x in each course
under5_count <- underFiveSubsetDepartment %>% group_by(Department) %>% tally()
#count of no visits in each course
noVisits_count <- noVisitSubsetDepartment %>% group_by(Department) %>% tally()
#total
total <- SASS_data %>% group_by(Department) %>% tally()

Dep_count <- merge.data.frame(merge.data.frame(merge.data.frame(
  reg_count, 
  under5_count, by = "Department", all = TRUE),
  noVisits_count, by = "Department", all = TRUE),
  total, by = "Department", all = TRUE)

Dep_count <- set_names(Dep_count, "Department",
                       "regulars" , 
                       "under5" , 
                       "No_Visits" ,
                       "Total" )
Dep_count <- mutate(Dep_count, 
                    "regulars%" = round((regulars/Total * 100)),
                    "under_5%" = round((under5/Total *100)), 
                    "No_Visits%" = round((No_Visits/Total *100)))

Dep_count <- Dep_count[, c(1,2,6,3,7,4,8,5)]
#view(Dep_count)
"________________________________________________________________________________________________________________________________________"
#GRADES PER DEP 
As_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "4") %>% tally()
Am_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.7") %>% tally()
Bp_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.3") %>% tally()
B_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.7") %>% tally()
Cp_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.3") %>% tally()
C_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Regs_Dep <- regularSubsetDepartment %>% group_by(Department) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
regularSubsetDepartment1 <- subset(SASS_data, Regular == TRUE)
W_Regs_Dep <- regularSubsetDepartment1 %>% group_by(Department) %>% filter(Final_Grade == "W") %>% tally()


Grades_Regs_Dep <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(
    As_Regs_Dep, 
    Am_Regs_Dep, by = "Department", all = TRUE),
    Bp_Regs_Dep, by = "Department", all = TRUE),
    B_Regs_Dep, by = "Department", all = TRUE),
    Bm_Regs_Dep, by = "Department", all = TRUE),
    Cp_Regs_Dep, by = "Department", all = TRUE),
    C_Regs_Dep, by = "Department", all = TRUE),
    D_Regs_Dep, by = "Department", all = TRUE),
    F_Regs_Dep, by = "Department", all = TRUE)

Grades_Regs_Dep <-  merge.data.frame(Grades_Regs_Dep, W_Regs_Dep, by = "Department", all = TRUE)  

Grades_Regs_Dep <- setnames(Grades_Regs_Dep, c("Department", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Regs_Dep <- cbind(Grades_Regs_Dep, total = rowSums(Grades_Regs_Dep[,2:11], na.rm = TRUE))
#view(Grades_Regs_Dep)

#Under5_Dep 
As_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "4") %>% tally()
Am_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.7") %>% tally()
Bp_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.3") %>% tally()
B_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.7") %>% tally()
Cp_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.3") %>% tally()
C_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Under5_Dep <- underFiveSubsetDepartment %>% group_by(Department) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
underFiveSubsetDepartment2 <- subset(SASS_data, Visits > 0 & Visits < 5)
W_Under5_Dep <- underFiveSubsetDepartment2 %>% group_by(Department) %>% filter(Final_Grade == "W") %>% tally()

Grades_Under5_Dep <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_Under5_Dep, 
    Am_Under5_Dep, by = "Department", all = TRUE),
    Bp_Under5_Dep, by = "Department", all = TRUE),
    B_Under5_Dep, by = "Department", all = TRUE),
    Bm_Under5_Dep, by = "Department", all = TRUE),
    Cp_Under5_Dep, by = "Department", all = TRUE),
    C_Under5_Dep, by = "Department", all = TRUE),
    D_Under5_Dep, by = "Department", all = TRUE),
    F_Under5_Dep, by = "Department", all = TRUE),
    W_Under5_Dep, by = "Department", all = TRUE)

Grades_Under5_Dep <- setnames(Grades_Under5_Dep, c("Department", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Under5_Dep <- cbind(Grades_Under5_Dep, total = rowSums(Grades_Under5_Dep[,2:11], na.rm = TRUE))
#view(Grades_Under5_Dep)

#never visited 
As_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "4") %>% tally()
Am_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.7") %>% tally()
Bp_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.3") %>% tally()
B_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.7") %>% tally()
Cp_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.3") %>% tally()
C_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_NoVisits_Dep <- noVisitSubsetDepartment %>% group_by(Department) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
noVisitSubsetDepartment1 <- subset(SASS_data, NoVisits == TRUE)
W_NoVisits_Dep <- noVisitSubsetDepartment1 %>% group_by(Department) %>% filter(Final_Grade == "W") %>% tally()


Grades_NoVisits_Dep <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_NoVisits_Dep, 
    Am_NoVisits_Dep, by = "Department", all = TRUE),
    Bp_NoVisits_Dep, by = "Department", all = TRUE),
    B_NoVisits_Dep, by = "Department", all = TRUE),
    Bm_NoVisits_Dep, by = "Department", all = TRUE),
    Cp_NoVisits_Dep, by = "Department", all = TRUE),
    C_NoVisits_Dep, by = "Department", all = TRUE),
    D_NoVisits_Dep, by = "Department", all = TRUE),
    F_NoVisits_Dep, by = "Department", all = TRUE),
    W_NoVisits_Dep, by = "Department", all = TRUE)

Grades_NoVisits_Dep <- setnames(Grades_NoVisits_Dep, c("Department", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_NoVisits_Dep <- cbind(Grades_NoVisits_Dep, total = rowSums(Grades_NoVisits_Dep[,2:11], na.rm = TRUE))
#view(Grades_NoVisits_Dep)

"________________________________________________________________________________________________________________________________________"
#DFW % PER DEP
DFWs_Regs_Dep <- subset(Grades_Regs_Dep, select = c("Department", "D", "F", "W", "total"))
DFWs_Regs_Dep <- mutate(DFWs_Regs_Dep, DFW = rowSums(DFWs_Regs_Dep[, 2:4], na.rm = TRUE))
DFWs_Regs_Dep <- mutate(DFWs_Regs_Dep, "DFW%" = round((DFWs_Regs_Dep$DFW/DFWs_Regs_Dep$total)*100))
#view(DFWs_Regs_Dep)

DFWs_Under5_Dep <- subset(Grades_Under5_Dep, select = c("Department", "D", "F", "W", "total"))
DFWs_Under5_Dep <- mutate(DFWs_Under5_Dep, DFW = rowSums(DFWs_Under5_Dep[, 2:4], na.rm = TRUE))
DFWs_Under5_Dep <- mutate(DFWs_Under5_Dep, "DFW%" = round((DFWs_Under5_Dep$DFW/DFWs_Under5_Dep$total)*100))
#view(DFWs_Under5_Dep)

DFWs_NoVisits_Dep <- subset(Grades_NoVisits_Dep, select = c("Department", "D", "F", "W", "total"))
DFWs_NoVisits_Dep <- mutate(DFWs_NoVisits_Dep, DFW = rowSums(DFWs_NoVisits_Dep[, 2:4], na.rm = TRUE))
DFWs_NoVisits_Dep <- mutate(DFWs_NoVisits_Dep, "DFW%" = round((DFWs_NoVisits_Dep$DFW/DFWs_NoVisits_Dep$total)*100))
#view(DFWs_NoVisits_Dep)

#saves excel of data anlysis by department into folder called "results" 
write_xlsx(list(Dep_count = Dep_count, meanGpaDep = meanGpaDep, Grades_Regs_Dep = Grades_Regs_Dep, 
                Grades_Under5_Dep = Grades_Under5_Dep, Grades_NoVisits_Dep = Grades_NoVisits_Dep, DFWs_Regs_Dep = DFWs_Regs_Dep, 
                DFWs_Under5_Dep = DFWs_Under5_Dep, DFWs_NoVisits_Dep = DFWs_NoVisits_Dep),
           "./Results/Data by Department.xlsx")
"________________________________________________________________________________________________________________________________________"
"________________________________________________________________________________________________________________________________________"
#Data analysis by program (SI/PASS) (lines 462-660)
#make program column
SASS_data_programs <- 
  mutate(SASS_data, Program = substr(Course_ID, 0, 3))

#categorize courses by programs 
SASS_data_programs <- SASS_data_programs %>%
  mutate(Program = replace(Program, Program == "MAC"| Program == "COP"| Program == "STA", 
                           "PASS"))

SASS_data_programs <- SASS_data_programs %>%
  mutate(Program = replace(Program, Program == "BSC"| Program == "MCB"| Program == "PCB"|
                             Program == "CHM"| Program == "PHY"| Program == "PHI" | Program == "EGN"| 
                             Program == "EMA"| Program == "EML"| Program == "MAN" | Program == "ACG"| 
                             Program == "FIN"| Program == "BCH"| Program == "ESC" | Program == "EMA"| Program == "ECO",
                           "SI"))
#view(SASS_data_programs)
"________________________________________________________________________________________________________________________________________"
#table with mean GPA for each subset (0,1,5) in each program
regularSubsetProgram <- subset(SASS_data_programs, Regular == TRUE)
regularSubsetProgram <- subset(regularSubsetProgram, GPA!="W")
regularSubsetProgram <- mutate(regularSubsetProgram, GPA = as.numeric(regularSubsetProgram$GPA))

meanGpaProgram <- data.frame(aggregate (x=regularSubsetProgram[,c(GPA)], by=data.frame(regularSubsetProgram$Program), FUN=mean))
meanGpaProgram <- setNames(meanGpaProgram, c("Program", "RegularGPA"))

underFiveSubsetProgram <- subset(SASS_data_programs, Visits > 0 & Visits < 5)
underFiveSubsetProgram <- subset(underFiveSubsetProgram, GPA!="W")
underFiveSubsetProgram <- mutate(underFiveSubsetProgram, GPA = as.numeric(underFiveSubsetProgram$GPA))
underFiveSubsetProgram1 <- data.frame(aggregate (x=underFiveSubsetProgram[,c(GPA)], by=data.frame(underFiveSubsetProgram$Program), FUN=mean))
underFiveSubsetProgram1 <- setNames(underFiveSubsetProgram1, c("Program", "Under5VisitsGPA"))

meanGpaProgram <- mutate(meanGpaProgram, underFiveSubsetProgram1["Under5VisitsGPA"])

noVisitSubsetProgram <- subset(SASS_data_programs, NoVisits == TRUE)
noVisitSubsetProgram <- subset(noVisitSubsetProgram, GPA!="W")
noVisitSubsetProgram <- mutate(noVisitSubsetProgram, GPA = as.numeric(noVisitSubsetProgram$GPA))
noVisitSubsetProgram1 <- aggregate (x=noVisitSubsetProgram[,c(GPA)], by=data.frame(noVisitSubsetProgram$Program), FUN=mean)
noVisitSubsetProgram1 <- setNames(noVisitSubsetProgram1, c("Program", "NoVisitGPA"))

meanGpaProgram <- mutate(meanGpaProgram, noVisitSubsetProgram1["NoVisitGPA"])

GPA_total <- subset(SASS_data_programs, GPA!="W")
GPA_total <- mutate(GPA_total, GPA = as.numeric(GPA_total$GPA))
GPA_total <- aggregate (x=GPA_total[,c(GPA)], by=data.frame(GPA_total$Program), FUN=mean)
GPA_total <- rename(GPA_total, 
                    GPA_all = x)

meanGpaProgram <- mutate(meanGpaProgram, GPA_total[2])
is.num <- sapply(meanGpaProgram, is.numeric)
meanGpaProgram[is.num] <- lapply(meanGpaProgram[is.num], round, 1)
#view(meanGpaProgram)

"________________________________________________________________________________________________________________________________________"
#Counts per Program 
#count of regulars in each Program
reg_countProgram <- regularSubsetProgram %>% group_by(Program) %>% tally()
#count of 1-4x in each Program
under5_countProgram <- underFiveSubsetProgram %>% group_by(Program) %>% tally()
#count of no visits in each Program
noVisits_countProgram <- noVisitSubsetProgram %>% group_by(Program) %>% tally()
#total
total <- SASS_data_programs %>% group_by(Program) %>% tally()

Program_count <- merge.data.frame(merge.data.frame(merge.data.frame(
  reg_countProgram, 
  under5_countProgram, by = "Program", all = TRUE),
  noVisits_countProgram, by = "Program", all = TRUE),
  total, by = "Program", all = TRUE)

Program_count <- set_names(Program_count, "Program",
                           "regulars" , 
                           "under5" , 
                           "No_Visits" ,
                           "Total" )
Program_count <- mutate(Program_count, 
                        "regulars%" = round((regulars/Total * 100)),
                        "under_5%" = round((under5/Total *100)), 
                        "No_Visits%" = round((No_Visits/Total *100)))

Program_count <- Program_count[, c(1,2,6,3,7,4,8,5)]
#view(Program_count)
"________________________________________________________________________________________________________________________________________"
#GRADES PER Program 
As_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "4") %>% tally()
Am_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.7") %>% tally()
Bp_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.3") %>% tally()
B_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.7") %>% tally()
Cp_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.3") %>% tally()
C_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Regs_Pro <- regularSubsetProgram %>% group_by(Program) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
regularSubsetProgram1 <- subset(SASS_data_programs, Regular == TRUE)
W_Regs_Pro <- regularSubsetProgram1 %>% group_by(Program) %>% filter(Final_Grade == "W") %>% tally()


Grades_Regs_Pro <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(
    As_Regs_Pro, 
    Am_Regs_Pro, by = "Program", all = TRUE),
    Bp_Regs_Pro, by = "Program", all = TRUE),
    B_Regs_Pro, by = "Program", all = TRUE),
    Bm_Regs_Pro, by = "Program", all = TRUE),
    Cp_Regs_Pro, by = "Program", all = TRUE),
    C_Regs_Pro, by = "Program", all = TRUE),
    D_Regs_Pro, by = "Program", all = TRUE),
    F_Regs_Pro, by = "Program", all = TRUE)

Grades_Regs_Pro <-  merge.data.frame(Grades_Regs_Pro, W_Regs_Pro, by = "Program", all = TRUE)  
Grades_Regs_Pro <- setnames(Grades_Regs_Pro, c("Program", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Regs_Pro <- cbind(Grades_Regs_Pro, total = rowSums(Grades_Regs_Pro[,2:11], na.rm = TRUE))
#view(Grades_Regs_Pro)

#Under5_Pro 
As_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "4") %>% tally()
Am_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.7") %>% tally()
Bp_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.3") %>% tally()
B_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.7") %>% tally()
Cp_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.3") %>% tally()
C_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Under5_Pro <- underFiveSubsetProgram %>% group_by(Program) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
underFiveSubsetProgram2 <- subset(SASS_data_programs, Visits > 0 & Visits < 5)
W_Under5_Pro <- underFiveSubsetProgram2 %>% group_by(Program) %>% filter(Final_Grade == "W") %>% tally()

Grades_Under5_Pro <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_Under5_Pro, 
    Am_Under5_Pro, by = "Program", all = TRUE),
    Bp_Under5_Pro, by = "Program", all = TRUE),
    B_Under5_Pro, by = "Program", all = TRUE),
    Bm_Under5_Pro, by = "Program", all = TRUE),
    Cp_Under5_Pro, by = "Program", all = TRUE),
    C_Under5_Pro, by = "Program", all = TRUE),
    D_Under5_Pro, by = "Program", all = TRUE),
    F_Under5_Pro, by = "Program", all = TRUE),
    W_Under5_Pro, by = "Program", all = TRUE)

Grades_Under5_Pro <- setnames(Grades_Under5_Pro, c("Program", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Under5_Pro <- cbind(Grades_Under5_Pro, total = rowSums(Grades_Under5_Pro[,2:11], na.rm = TRUE))
#view(Grades_Under5_Pro)


#never visited 
As_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "4") %>% tally()
Am_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.7") %>% tally()
Bp_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.3") %>% tally()
B_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.7") %>% tally()
Cp_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.3") %>% tally()
C_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_NoVisits_Pro <- noVisitSubsetProgram %>% group_by(Program) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
noVisitSubsetProgram1 <- subset(SASS_data_programs, NoVisits == TRUE)
W_NoVisits_Pro <- noVisitSubsetProgram1 %>% group_by(Program) %>% filter(Final_Grade == "W") %>% tally()


Grades_NoVisits_Pro <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_NoVisits_Pro, 
    Am_NoVisits_Pro, by = "Program", all = TRUE),
    Bp_NoVisits_Pro, by = "Program", all = TRUE),
    B_NoVisits_Pro, by = "Program", all = TRUE),
    Bm_NoVisits_Pro, by = "Program", all = TRUE),
    Cp_NoVisits_Pro, by = "Program", all = TRUE),
    C_NoVisits_Pro, by = "Program", all = TRUE),
    D_NoVisits_Pro, by = "Program", all = TRUE),
    F_NoVisits_Pro, by = "Program", all = TRUE),
    W_NoVisits_Pro, by = "Program", all = TRUE)

Grades_NoVisits_Pro <- setnames(Grades_NoVisits_Pro, c("Program", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_NoVisits_Pro <- cbind(Grades_NoVisits_Pro, total = rowSums(Grades_NoVisits_Pro[,2:11], na.rm = TRUE))
#view(Grades_NoVisits_Pro)

"________________________________________________________________________________________________________________________________________"
#DFW % PER PROGRAM
DFWs_Regs_Pro <- subset(Grades_Regs_Pro, select = c("Program", "D", "F", "W", "total"))
DFWs_Regs_Pro <- mutate(DFWs_Regs_Pro, DFW = rowSums(DFWs_Regs_Pro[, 2:4], na.rm = TRUE))
DFWs_Regs_Pro <- mutate(DFWs_Regs_Pro, "DFW%" = round((DFWs_Regs_Pro$DFW/DFWs_Regs_Pro$total)*100))
#view(DFWs_Regs_Pro)

DFWs_Under5_Pro <- subset(Grades_Under5_Pro, select = c("Program", "D", "F", "W", "total"))
DFWs_Under5_Pro <- mutate(DFWs_Under5_Pro, DFW = rowSums(DFWs_Under5_Pro[, 2:4], na.rm = TRUE))
DFWs_Under5_Pro <- mutate(DFWs_Under5_Pro, "DFW%" = round((DFWs_Under5_Pro$DFW/DFWs_Under5_Pro$total)*100))
#view(DFWs_Under5_Pro)

DFWs_NoVisits_Pro <- subset(Grades_NoVisits_Pro, select = c("Program", "D", "F", "W", "total"))
DFWs_NoVisits_Pro <- mutate(DFWs_NoVisits_Pro, DFW = rowSums(DFWs_NoVisits_Pro[, 2:4], na.rm = TRUE))
DFWs_NoVisits_Pro <- mutate(DFWs_NoVisits_Pro, "DFW%" = round((DFWs_NoVisits_Pro$DFW/DFWs_NoVisits_Pro$total)*100))
#view(DFWs_NoVisits_Pro)

#saves excel of data analysis by program into folder called "results" 
write_xlsx(list(Program_count = Program_count, meanGpaProgram= meanGpaProgram, Grades_Regs_Pro = Grades_Regs_Pro, 
                Grades_Under5_Pro = Grades_Under5_Pro, Grades_NoVisits_Pro = Grades_NoVisits_Pro, DFWs_Regs_Pro = DFWs_Regs_Pro, 
                DFWs_Under5_Pro = DFWs_Under5_Pro, DFWs_NoVisits_Pro = DFWs_NoVisits_Pro),
           "./Results/Data by Program.xlsx")
"________________________________________________________________________________________________________________________________________"
"________________________________________________________________________________________________________________________________________"
#Data analysis of all students in all courses and programs (lines 663-810)
#table with mean GPA for each subset (0,1,5) 
regularSubsetAll <- subset(SASS_data, Regular == TRUE)
regularSubsetAll <- subset(regularSubsetAll, GPA!="W")
regularSubsetAll <- mutate(regularSubsetAll, GPA = as.numeric(regularSubsetAll$GPA))

underFiveSubsetAll <- subset(SASS_data, Visits > 0 & Visits < 5)
underFiveSubsetAll <- subset(underFiveSubsetAll, GPA!="W")
underFiveSubsetAll <- mutate(underFiveSubsetAll, GPA = as.numeric(underFiveSubsetAll$GPA))

noVisitSubsetAll <- subset(SASS_data, NoVisits == TRUE)
noVisitSubsetAll <- subset(noVisitSubsetAll, GPA!="W")
noVisitSubsetAll <- mutate(noVisitSubsetAll, GPA = as.numeric(noVisitSubsetAll$GPA))

GPA_totalAll <- subset(SASS_data, GPA!="W")
GPA_totalAll <- mutate(GPA_totalAll, GPA = as.numeric(GPA_totalAll$GPA))

meanGpaAll <- data.frame(Gouping = c("regulars Avg", "under5 Avg", "No visits Avg", "Total Avg"),
                         meanGPA = c(mean(regularSubsetAll$GPA), mean(underFiveSubsetAll$GPA),mean(noVisitSubsetAll$GPA),mean(GPA_totalAll$GPA))
)
"_______________________________________________________________________________________________________________________________________"
#COUNTS FOR ALL
total <- sum(studentTotal$Course_ID.Freq)
#count of regulars 
reg_countAll <- regularSubsetAll %>% tally()
#count of 1-4x 
under5_countAll <- underFiveSubsetAll %>% tally()
#count of no visits 
noVisits_countAll <- noVisitSubsetAll %>% tally()
# regular %
reg_percentAll <- transmute(reg_countAll, n = as.numeric(reg_countAll$n))
reg_percentAll <- round((reg_percentAll / total) * 100)
# 1-4 visits %
underFive_percentAll <- transmute(under5_countAll, n = as.numeric(under5_countAll$n))
underFive_percentAll <- round((underFive_percentAll / total) * 100)
# no visits %
noVisits_percentAll <- transmute(noVisits_countAll, n = as.numeric(noVisits_countAll$n))
noVisits_percentAll <- round((noVisits_percentAll / total) * 100)
All_count <- cbind.data.frame(reg_countAll, reg_percentAll, under5_countAll, underFive_percentAll, noVisits_countAll, noVisits_percentAll, total)
All_count <- setnames(All_count, c(
  "regulars", 
  "regulars %", 
  "under 5", 
  "under 5 %", 
  "no Visits",
  "no Visits %", 
  "total"))

"________________________________________________________________________________________________________________________________________"
#GRADES ALL
As_Regs_All <- regularSubsetAll %>% group_by(GPA) %>%filter(GPA== "4") %>% tally()
Am_Regs_All <- regularSubsetAll %>% group_by(GPA) %>%filter(GPA== "3.7") %>% tally()
Bp_Regs_All <- regularSubsetAll %>% group_by(GPA) %>%filter(GPA== "3.3") %>% tally()
B_Regs_All <- regularSubsetAll %>% group_by(GPA) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Regs_All <- regularSubsetAll %>% group_by(GPA) %>%filter(GPA== "2.7") %>% tally()
Cp_Regs_All <- regularSubsetAll %>% group_by(GPA) %>%filter(GPA== "2.3") %>% tally()
C_Regs_All <- regularSubsetAll %>% group_by(GPA) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Regs_All <- regularSubsetAll %>% group_by(GPA) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Regs_All <- regularSubsetAll %>% group_by(GPA) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
regularSubsetAll1 <- subset(SASS_data, Regular == TRUE)
W_Regs_All <- regularSubsetAll1 %>% filter(Final_Grade == "W") %>% tally()

Grades_Regs_All <- data.frame(
  "A" = As_Regs_All[1,2], 
  "A-" = Am_Regs_All[1,2],
  "B+" = Bp_Regs_All[1,2],
  "B" = B_Regs_All[1,2],
  "B-" = Bm_Regs_All[1,2],
  "C+" = Cp_Regs_All[1,2],
  "C" = C_Regs_All[1,2],
  "D" = D_Regs_All[1,2],
  "F" = F_Regs_All[1,2], 
  "W" = W_Regs_All)
Grades_Regs_All <- set_names(Grades_Regs_All, c("A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))

As_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "4") %>% tally()
Am_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "3.7") %>% tally()
Bp_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "3.3") %>% tally()
B_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "2.7") %>% tally()
Cp_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "2.3") %>% tally()
C_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Under5_All <- underFiveSubsetAll %>% group_by(GPA) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
underFiveSubsetAll1 <- subset(SASS_data, Regular == TRUE)
W_Under5_All <- underFiveSubsetAll1 %>% filter(Final_Grade == "W") %>% tally()

Grades_Under5_All <- data.frame(
  "A" = As_Under5_All[1,2], 
  "A-" = Am_Under5_All[1,2],
  "B+" = Bp_Under5_All[1,2],
  "B" = B_Under5_All[1,2],
  "B-" = Bm_Under5_All[1,2],
  "C+" = Cp_Under5_All[1,2],
  "C" = C_Under5_All[1,2],
  "D" = D_Under5_All[1,2],
  "F" = F_Under5_All[1,2], 
  "W" = W_Under5_All)
Grades_Under5_All <- setnames(Grades_Under5_All, c("A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))

As_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)  %>% filter(GPA== "4") %>% tally()
Am_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)  %>% filter(GPA== "3.7") %>% tally()
Bp_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)  %>% filter(GPA== "3.3") %>% tally()
B_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)   %>%  filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)  %>% filter(GPA== "2.7") %>% tally()
Cp_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)  %>% filter(GPA== "2.3") %>% tally()
C_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)   %>%  filter(GPA== "2.0" | GPA== "2") %>% tally()
D_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)   %>%  filter(GPA== "1.0" | GPA== "1") %>% tally()
F_NoVisits_All <- noVisitSubsetAll %>% group_by(GPA)   %>%  filter(GPA== "0.0" | GPA== "0") %>% tally()
noVisitSubsetAll1 <- subset(SASS_data, Regular == TRUE)
W_NoVisits_All <- noVisitSubsetAll1 %>% filter(Final_Grade == "W") %>% tally()

Grades_NoVisits_All <- data.frame(
  "A" = As_NoVisits_All[1,2], 
  "A-" = Am_NoVisits_All[1,2],
  "B+" = Bp_NoVisits_All[1,2],
  "B" = B_NoVisits_All[1,2],
  "B-" = Bm_NoVisits_All[1,2],
  "C+" = Cp_NoVisits_All[1,2],
  "C" = C_NoVisits_All[1,2],
  "D" = D_NoVisits_All[1,2],
  "F" = F_NoVisits_All[1,2], 
  "W" = W_NoVisits_All)
Grades_NoVisits_All <- set_names(Grades_NoVisits_All, c("A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
"________________________________________________________________________________________________________________________________________"
#DFW 
DFWs_Regs_All <- mutate(Grades_Regs_All, total = sum(Grades_Regs_All))
DFWs_Regs_All <- subset(DFWs_Regs_All, select = -c(1:7))
DFWs_Regs_All <- mutate(DFWs_Regs_All, DFW = rowSums(DFWs_Regs_All[1, 1:3], na.rm = TRUE))
DFWs_Regs_All <- mutate(DFWs_Regs_All, "DFW%" = round((DFWs_Regs_All$DFW/DFWs_Regs_All$total)*100))
#view(DFWs_Regs_All)

DFWs_Under5_All <- mutate(Grades_Under5_All, total = sum(Grades_Under5_All))
DFWs_Under5_All <- subset(DFWs_Under5_All, select = -c(1:7))
DFWs_Under5_All <- mutate(DFWs_Under5_All, DFW = rowSums(DFWs_Under5_All[1, 1:3], na.rm = TRUE))
DFWs_Under5_All <- mutate(DFWs_Under5_All, "DFW%" = round((DFWs_Under5_All$DFW/DFWs_Under5_All$total)*100))
#view(DFWs_Under5_All)

DFWs_NoVisits_All <- mutate(Grades_NoVisits_All, total = sum(Grades_NoVisits_All))
DFWs_NoVisits_All <- subset(DFWs_NoVisits_All, select = -c(1:7))
DFWs_NoVisits_All <- mutate(DFWs_NoVisits_All, DFW = rowSums(DFWs_NoVisits_All[1, 1:3], na.rm = TRUE))
DFWs_NoVisits_All <- mutate(DFWs_NoVisits_All, "DFW%" = round((DFWs_NoVisits_All$DFW/DFWs_NoVisits_All$total)*100))
#view(DFWs_NoVisits_All)

#saves excel sheet with data analyses of all students into folder called "results" 
write_xlsx(list(All_count = All_count, meanGpaAll = meanGpaAll, Grades_Regs_All = Grades_Regs_All, Grades_Under5_All = Grades_Under5_All, 
                Grades_NoVisits_All = Grades_NoVisits_All, DFWs_Regs_All = DFWs_Regs_All, DFWs_Under5_All = DFWs_Under5_All, 
                DFWs_NoVisits_All = DFWs_NoVisits_All), "./Results/Data by All students.xlsx")
"________________________________________________________________________________________________________________________________________"
"________________________________________________________________________________________________________________________________________"
#Data analysis by course (lines 814-end)
#table with mean GPA for each subset (0,1,5) in each course
regularSubset <- subset(SASS_data, Regular == TRUE)
regularSubset <- subset(regularSubset, GPA!="W")
regularSubset <- mutate(regularSubset, GPA = as.numeric(regularSubset$GPA))

meanGpaCourse <- data.frame(aggregate (x=regularSubset[,c(GPA)], by=data.frame(regularSubset$Course_ID), FUN=mean))
meanGpaCourse <- setNames(meanGpaCourse, c("CourseID", "RegularGPA"))

underFiveSubset <- subset(SASS_data, Visits > 0 & Visits < 5)
underFiveSubset <- subset(underFiveSubset, GPA!="W")
underFiveSubset <- mutate(underFiveSubset, GPA = as.numeric(underFiveSubset$GPA))
underFiveSubset1 <- aggregate (x=underFiveSubset[,c(GPA)], by=data.frame(underFiveSubset$Course_ID), FUN=mean)
underFiveSubset1 <- setNames(underFiveSubset1, c("Course", "Under5VisitsGPA"))

meanGpaCourse <- mutate(meanGpaCourse, underFiveSubset1["Under5VisitsGPA"])

noVisitSubset <- subset(SASS_data, NoVisits == TRUE)
noVisitSubset <- subset(noVisitSubset, GPA!="W")
noVisitSubset <- mutate(noVisitSubset, GPA = as.numeric(noVisitSubset$GPA))
noVisitSubset1 <- aggregate (x=noVisitSubset[,c(GPA)], by=data.frame(noVisitSubset$Course_ID), FUN=mean)
noVisitSubset1 <- setNames(noVisitSubset1, c("Course", "NoVisitGPA"))

meanGpaCourse <- mutate(meanGpaCourse, noVisitSubset1["NoVisitGPA"])

GPA_total <- subset(SASS_data, GPA!="W")
GPA_total <- mutate(GPA_total, GPA = as.numeric(GPA_total$GPA))
GPA_total <- aggregate (x=GPA_total[,c(GPA)], by=data.frame(GPA_total$Course_ID), FUN=mean)
GPA_total <- rename(GPA_total, 
                    GPA_all = x)

meanGpaCourse <- mutate(meanGpaCourse, GPA_total[2])
is.num <- sapply(meanGpaCourse, is.numeric)
meanGpaCourse[is.num] <- lapply(meanGpaCourse[is.num], round, 1)

"________________________________________________________________________________________________________________________________________"
#COUNTS PER COURSE
#count of regulars in each course
reg_count <- regularSubset %>% group_by(Course_ID) %>% tally()
#count of 1-4x in each course
under5_count <- underFiveSubset %>% group_by(Course_ID) %>% tally()
#count of no visits in each course
noVisits_count <- noVisitSubset %>% group_by(Course_ID) %>% tally()

# regular %
reg_percent <- transmute(reg_count, n = as.numeric(reg_count$n))
total <- transmute(studentTotal, n = as.numeric(studentTotal$Course_ID.Freq))
reg_percent <- round((reg_percent / total) * 100)

# 1-4 visits %
underFive_percent <- transmute(under5_count, n = as.numeric(under5_count$n))
total <- transmute(studentTotal, n = as.numeric(studentTotal$Course_ID.Freq))
underFive_percent <- round((underFive_percent / total) * 100)

# no visits %
noVisits_percent <- transmute(noVisits_count, n = as.numeric(noVisits_count$n))
total <- transmute(studentTotal, n = as.numeric(studentTotal$Course_ID.Freq))
noVisits_percent <- round((noVisits_percent / total) * 100)

course_count <- cbind(reg_count, reg_percent, under5_count, underFive_percent, noVisits_count, noVisits_percent, studentTotal)
course_count <- subset(course_count, select = -c(4,7, 10))
course_count <- rename(course_count, 
                       "regulars" = n, 
                       "regulars %" = n.1, 
                       "under5" = n.2, 
                       "under5 %" = n.3, 
                       "No Visits" = n.4,
                       "No Visits %" = n.5, 
                       "Total" = Course_ID.Freq)

"________________________________________________________________________________________________________________________________________"
#Grades per course for each subset 
#regulars
As_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "4") %>% tally()
Am_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "3.7") %>% tally()
Bp_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "3.3") %>% tally()
B_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "2.7") %>% tally()
Cp_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "2.3") %>% tally()
C_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Regs <- regularSubset %>% group_by(Course_ID) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
regularSubset1 <- subset(SASS_data, Regular == TRUE)
W_Regs <- regularSubset1 %>% group_by(Course_ID) %>% filter(Final_Grade == "W") %>% tally()


Grades_Regs <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(
    As_Regs, 
    Am_Regs, by = "Course_ID", all = TRUE),
    Bp_Regs, by = "Course_ID", all = TRUE),
    B_Regs, by = "Course_ID", all = TRUE),
  Bm_Regs, by = "Course_ID", all = TRUE),
  Cp_Regs, by = "Course_ID", all = TRUE),
  C_Regs, by = "Course_ID", all = TRUE),
  D_Regs, by = "Course_ID", all = TRUE),
  F_Regs, by = "Course_ID", all = TRUE)

Grades_Regs <-  merge.data.frame(Grades_Regs, W_Regs, by = "Course_ID", all = TRUE)  

Grades_Regs <- setnames(Grades_Regs, c("Course_ID", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Regs <- cbind(Grades_Regs, total = rowSums(Grades_Regs[,2:11], na.rm = TRUE))
#view(Grades_Regs)


#under5 
As_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "4") %>% tally()
Am_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "3.7") %>% tally()
Bp_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "3.3") %>% tally()
B_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "2.7") %>% tally()
Cp_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "2.3") %>% tally()
C_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_Under5 <- underFiveSubset %>% group_by(Course_ID) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
underFiveSubset1 <- subset(SASS_data, Visits > 0 & Visits < 5)
W_Under5 <- underFiveSubset1 %>% group_by(Course_ID) %>% filter(Final_Grade == "W") %>% tally()

Grades_Under5 <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_Under5, 
    Am_Under5, by = "Course_ID", all = TRUE),
    Bp_Under5, by = "Course_ID", all = TRUE),
    B_Under5, by = "Course_ID", all = TRUE),
    Bm_Under5, by = "Course_ID", all = TRUE),
  Cp_Under5, by = "Course_ID", all = TRUE),
  C_Under5, by = "Course_ID", all = TRUE),
  D_Under5, by = "Course_ID", all = TRUE),
  F_Under5, by = "Course_ID", all = TRUE),
  W_Under5, by = "Course_ID", all = TRUE)

Grades_Under5 <- setnames(Grades_Under5, c("Course_ID", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_Under5 <- cbind(Grades_Under5, total = rowSums(Grades_Under5[,2:11], na.rm = TRUE))
#view(Grades_Under5)


#never visited 
As_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "4") %>% tally()
Am_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "3.7") %>% tally()
Bp_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "3.3") %>% tally()
B_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "2.7") %>% tally()
Cp_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "2.3") %>% tally()
C_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_NoVisits <- noVisitSubset %>% group_by(Course_ID) %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
noVisitSubset1 <- subset(SASS_data, NoVisits == TRUE)
W_NoVisits <- noVisitSubset1 %>% group_by(Course_ID) %>% filter(Final_Grade == "W") %>% tally()


Grades_NoVisits <- merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
  merge.data.frame(merge.data.frame(merge.data.frame(merge.data.frame(
    As_NoVisits, 
    Am_NoVisits, by = "Course_ID", all = TRUE),
    Bp_NoVisits, by = "Course_ID", all = TRUE),
    B_NoVisits, by = "Course_ID", all = TRUE),
    Bm_NoVisits, by = "Course_ID", all = TRUE),
  Cp_NoVisits, by = "Course_ID", all = TRUE),
  C_NoVisits, by = "Course_ID", all = TRUE),
  D_NoVisits, by = "Course_ID", all = TRUE),
  F_NoVisits, by = "Course_ID", all = TRUE),
  W_NoVisits, by = "Course_ID", all = TRUE)

Grades_NoVisits <- setnames(Grades_NoVisits, c("Course_ID", "A", "A-", "B+", "B", "B-", "C+", "C", "D", "F", "W"))
Grades_NoVisits <- cbind(Grades_NoVisits, total = rowSums(Grades_NoVisits[,2:11], na.rm = TRUE))
#view(Grades_NoVisits)

"________________________________________________________________________________________________________________________________________"
#DFW % PER COURSE

DFWs_Regs <- subset(Grades_Regs, select = c("Course_ID", "D", "F", "W", "total"))
DFWs_Regs <- mutate(DFWs_Regs, DFW = rowSums(DFWs_Regs[, 2:4], na.rm = TRUE))
DFWs_Regs <- mutate(DFWs_Regs, "DFW%" = round((DFWs_Regs$DFW/DFWs_Regs$total)*100))

DFWs_Under5 <- subset(Grades_Under5, select = c("Course_ID", "D", "F", "W", "total"))
DFWs_Under5 <- mutate(DFWs_Under5, DFW = rowSums(DFWs_Under5[, 2:4], na.rm = TRUE))
DFWs_Under5 <- mutate(DFWs_Under5, "DFW%" = round((DFWs_Under5$DFW/DFWs_Under5$total)*100))

DFWs_NoVisits <- subset(Grades_NoVisits, select = c("Course_ID", "D", "F", "W", "total"))
DFWs_NoVisits <- mutate(DFWs_NoVisits, DFW = rowSums(DFWs_NoVisits[, 2:4], na.rm = TRUE))
DFWs_NoVisits <- mutate(DFWs_NoVisits, "DFW%" = round((DFWs_NoVisits$DFW/DFWs_NoVisits$total)*100))

#saves excel of by course data analysis to folder called "results"
write_xlsx(list(course_count = course_count, meanGpaCourse = meanGpaCourse, Grades_Regs = Grades_Regs, Grades_Under5 = Grades_Under5, 
                Grades_NoVisits = Grades_NoVisits, DFWs_Regs =DFWs_Regs, DFWs_Under5 = DFWs_Under5, DFWs_NoVisits = DFWs_NoVisits),
           "./Results/Data by Course.xlsx")





