instructor_data <- SASS_data %>% group_by(Course_ID) %>% group_by(Instructor)

"________________________________________________________________________________________________________________________________________"
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
#COUNTS PER COURSE
#total students in each INSTRUCTORS COURSE 
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

"________________________________________________________________________________________________________________________________________"
#GRADES PER Instructor 

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
#DFW % PER Instructor

DFWs_RegsI <- subset(Grades_RegsI, select = c("Instructor", "D", "F", "W", "total"))
DFWs_RegsI <- mutate(DFWs_RegsI, DFW = rowSums(DFWs_RegsI[, 2:4], na.rm = TRUE))
DFWs_RegsI <- mutate(DFWs_RegsI, "DFW%" = round((DFWs_RegsI$DFW/DFWs_RegsI$total)*100))

DFWs_Under5I <- subset(Grades_Under5I, select = c("Instructor", "D", "F", "W", "total"))
DFWs_Under5I <- mutate(DFWs_Under5I, DFW = rowSums(DFWs_Under5I[, 2:4], na.rm = TRUE))
DFWs_Under5I <- mutate(DFWs_Under5I, "DFW%" = round((DFWs_Under5I$DFW/DFWs_Under5I$total)*100))

DFWs_NoVisitsI <- subset(Grades_NoVisitsI, select = c("Instructor", "D", "F", "W", "total"))
DFWs_NoVisitsI <- mutate(DFWs_NoVisitsI, DFW = rowSums(DFWs_NoVisitsI[, 2:4], na.rm = TRUE))
DFWs_NoVisitsI <- mutate(DFWs_NoVisitsI, "DFW%" = round((DFWs_NoVisitsI$DFW/DFWs_NoVisitsI$total)*100))


write_xlsx(list(count_by_instructor = count_by_instructor, GPA_totalI = GPA_totalI, Grades_RegsI = Grades_RegsI, Grades_Under5I = Grades_Under5I, 
                Grades_NoVisitsI = Grades_NoVisitsI, DFWs_RegsI =DFWs_RegsI, DFWs_Under5I = DFWs_Under5I, DFWs_NoVisitsI = DFWs_NoVisitsI),
           "Data by Instructor.xlsx")

