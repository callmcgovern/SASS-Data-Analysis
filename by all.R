
"________________________________________________________________________________________________________________________________________"
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


#DFW 
DFWs_Regs_All <- mutate(Grades_Regs_All, total = sum(Grades_Regs_All))
DFWs_Regs_All <- subset(DFWs_Regs_All, select = -c(1:7))
DFWs_Regs_All <- mutate(DFWs_Regs_All, DFW = rowSums(DFWs_Regs_All[1, 1:3], na.rm = TRUE))
DFWs_Regs_All <- mutate(DFWs_Regs_All, "DFW%" = round((DFWs_Regs_All$DFW/DFWs_Regs_All$total)*100))
view(DFWs_Regs_All)

DFWs_Under5_All <- mutate(Grades_Under5_All, total = sum(Grades_Under5_All))
DFWs_Under5_All <- subset(DFWs_Under5_All, select = -c(1:7))
DFWs_Under5_All <- mutate(DFWs_Under5_All, DFW = rowSums(DFWs_Under5_All[1, 1:3], na.rm = TRUE))
DFWs_Under5_All <- mutate(DFWs_Under5_All, "DFW%" = round((DFWs_Under5_All$DFW/DFWs_Under5_All$total)*100))
view(DFWs_Under5_All)

DFWs_NoVisits_All <- mutate(Grades_NoVisits_All, total = sum(Grades_NoVisits_All))
DFWs_NoVisits_All <- subset(DFWs_NoVisits_All, select = -c(1:7))
DFWs_NoVisits_All <- mutate(DFWs_NoVisits_All, DFW = rowSums(DFWs_NoVisits_All[1, 1:3], na.rm = TRUE))
DFWs_NoVisits_All <- mutate(DFWs_NoVisits_All, "DFW%" = round((DFWs_NoVisits_All$DFW/DFWs_NoVisits_All$total)*100))
view(DFWs_NoVisits_All)

"________________________________________________________________________________________________________________________________________"

write_xlsx(list(All_count = All_count, meanGpaAll = meanGpaAll, Grades_Regs_All = Grades_Regs_All, Grades_Under5_All = Grades_Under5_All, 
                Grades_NoVisits_All = Grades_NoVisits_All, DFWs_Regs_All = DFWs_Regs_All, DFWs_Under5_All = DFWs_Under5_All, 
                DFWs_NoVisits_All = DFWs_NoVisits_All), "Data by All students.xlsx")




