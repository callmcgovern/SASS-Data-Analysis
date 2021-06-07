
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

As_All <- SASS_data %>% filter(GPA== "4") %>% tally()
Am_All <- SASS_data %>% filter(GPA== "3.7") %>% tally()
Bp_All <- SASS_data %>% filter(GPA== "3.3") %>% tally()
B_All <- SASS_data %>% filter(GPA== "3.0" | GPA== "3") %>% tally()
Bm_All <- SASS_data %>% filter(GPA== "2.7") %>% tally()
Cp_All <- SASS_data %>% filter(GPA== "2.3") %>% tally()
C_All <- SASS_data %>% filter(GPA== "2.0" | GPA== "2") %>% tally()
D_All <- SASS_data %>% filter(GPA== "1.0" | GPA== "1") %>% tally()
F_All <- SASS_data %>% filter(GPA== "0.0" | GPA== "0") %>% tally()
W_All <- SASS_data %>% filter(Final_Grade == "W") %>% tally()

Grades_All <- rename(Grades_All, 
                     total = "y")

#DFW % All
DFWs_All <- subset(Grades_All, select = c("total", "D", "F", "W"))
DFWs_All <- mutate(DFWs_All, DFW = rowSums(DFWs_All[, 2:4], na.rm = TRUE))
DFWs_All <- mutate(DFWs_All, "DFW%" = round((DFWs_All$DFW/DFWs_All$total)*100))
DFWs_All <- subset(DFWs_All, select = -c(1))

#binding grades and DFWs
all <- cbind(Grades_All, DFWs_All)
all <- subset(all, select = -c(12,13,14))
All_Grades <- all[ , c(1,2,3,4,5,6,7,8,9,10,12,13,11)]
#view(All_Grades)

"________________________________________________________________________________________________________________________________________"
library(xlsx)

wb = createWorkbook()
sheet = createSheet(wb, "All_count")
addDataFrame(All_count, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "meanGpaAll")
addDataFrame(meanGpaAll, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "All_Grades")
addDataFrame(All_Grades, sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, "Data by All students.xlsx")




