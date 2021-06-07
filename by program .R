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
                             Program == "FIN"| Program == "BCH",
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
#COUNTS PER Program 

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

"________________________________________________________________________________________________________________________________________"
library(xlsx)

wb = createWorkbook()
sheet = createSheet(wb, "count")
addDataFrame(Program_count, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "meanGpa")
addDataFrame(meanGpaProgram, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Grades_Regs_Pro")
addDataFrame(Grades_Regs_Pro, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Grades_Under5_Pro")
addDataFrame(Grades_Under5_Pro, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Grades_NoVisits_Pro")
addDataFrame(Grades_NoVisits_Pro, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "DFWs_Regs_Pro")
addDataFrame(DFWs_Regs_Pro, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "DFWs_Under5_Pro")
addDataFrame(DFWs_Under5_Pro, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "DFWs_NoVisits_Pro")
addDataFrame(DFWs_NoVisits_Pro, sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, "Data by Program.xlsx")



