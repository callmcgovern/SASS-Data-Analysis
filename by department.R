#view(SASS_data)

"________________________________________________________________________________________________________________________________________"
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
#COUNTS PER DEPARTMENT 

#count of regulars in each course
reg_count <- regularSubset %>% group_by(Department) %>% tally()
#count of 1-4x in each course
under5_count <- underFiveSubset %>% group_by(Department) %>% tally()
#count of no visits in each course
noVisits_count <- noVisitSubset %>% group_by(Department) %>% tally()
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

"________________________________________________________________________________________________________________________________________"

write_xlsx(list(Dep_count = Dep_count, meanGpaDep = meanGpaDep, Grades_Regs_Dep = Grades_Regs_Dep, 
                Grades_Under5_Dep = Grades_Under5_Dep, Grades_NoVisits_Dep = Grades_NoVisits_Dep, DFWs_Regs_Dep = DFWs_Regs_Dep, 
                DFWs_Under5_Dep = DFWs_Under5_Dep, DFWs_NoVisits_Dep = DFWs_NoVisits_Dep),
            "Data by Department.xlsx")






