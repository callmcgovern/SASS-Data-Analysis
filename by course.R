#BY COURSE 

"________________________________________________________________________________________________________________________________________"
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

"________________________________________________________________________________________________________________________________________"


write_xlsx(list(course_count = course_count, meanGpaCourse = meanGpaCourse, Grades_Regs = Grades_Regs, Grades_Under5 = Grades_Under5, 
                Grades_NoVisits = Grades_NoVisits, DFWs_Regs =DFWs_Regs, DFWs_Under5 = DFWs_Under5, DFWs_NoVisits = DFWs_NoVisits),
           "Data by Course.xlsx")



