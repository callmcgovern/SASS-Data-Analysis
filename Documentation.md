# SASS-Data-Analysis
SASS Data Analysis with R: Documentation and Troubleshooting

Part 1: The set up

A)   Arrange data in the “SI and PASS data.csv” file as shown below.
 



     Notes:
·  	Make sure all columns correspond exactly to the ones shown above.
·  	Do not worry about the blank boxes.
·  	Save file to your desktop as a .csv -- (Ex. “Spring 21 SI PASS Combined.csv”)
 
B)   Download RStudio onto your computer.
1.	Go here: https://www.rstudio.com/products/rstudio/download/#download
2.	Choose the Free download called “RStudio Desktop”
3.	Download the version corresponding to your operating system.
4. 	If you already have RStudio make sure you download the most recent    version 
5.	Install RStudio
 
C)  From the SASS-Data-Analysis GitHub, click on the Green “Code” button, and select “Download Zip” from the dropdown menu. If the zip file goes to your Downloads folder, move it to your desktop and decompress the file.
 
D)   Move the csv from step A into that SASS-Data-Analysis folder on your desktop.




 

Part 2: Running the program

The “SASS data analysis program.R” will organize the data and analyze it based on 5 categorizations: courses, department, program(SI/PASS), professor, and all. The program will then produce 5 excel documents (one for each category). Each excel document will consist of of several tabs which include: 
 	o  The number of students in each category (regular, 1-4 visits, never visited, total)
o  The mean GPA in each category of student (regular, 1-4 visits, never visited, total)
o  Number of each grade, A through F and W in each category of student (regular, 1-4 visits, never visited, total)
o  DFWs and the DFW % in each category of student (regular, 1-4 visits, never visited, total)

Steps to use the program:
Open SASS data analysis program.R (from the folder in your desktop) in RStudio.
In the program, find the code on line 10:
SASS_data <- fread("Spring 21 SI PASS Combined.csv")
Delete the text between the quotation marks on line 10 and type in the name of the data file you made in Step A between quotation marks. Type it exactly as it is named on your desktop so that R studio can find it.
Hit “source” on the top right-hand side of R studio.
RStudio will now install several packages. This may take a couple of minutes.
- If the program prompts you by saying “do you want to download this package, yes/no/cancel?” type “yes” into the command line and hit enter. 
- If the program asks you if it can shut down and reopen R inorder to download the packages, hit yes. Then you may need to hit “source” again. 
If the program runs successfully, you will see a new folder called “Results” in the “SASS-Data-Analysis” folder on your desktop. This folder will include all 5 excel documents. 



 
Troubleshooting:
	The trouble shooting chapter will include several common challenges associated with downloading and using this data analysis program. 

If the program doesn't run and the error says that the argument was not found for fread(), try following these directions in this order:
Close R-studio.
Open the csv document from step A in excel and minimize it but keep it open in the background. 
Open the program in R studio again. 
Hit source.


Making revisions: 
Adding a course to the program:
Add the course into the correct department. This begins on line 51 in the code. 
Example: If you are adding the program called “DOG” into the “Management” department, go to line 61 and add: Department == “DOG” 

Current code on line 61: 
mutate(Department = replace(Department, Department == "MAN", "Management"))
Code on line 61 revised with DOG course added into management department: 
mutate(Department = replace(Department, Department == "MAN", Department == “DOG” , "Management"))

Then, add the course to the correct program (SI/PASS) in the section of the code that begins on line 463. You will see two subsections, “#PASS” on line 468 and “#SI” on line 472. You will add your course into one of these two subsections. 

Example: If you are adding a course called DOG into the PASS program then type: 
| Program == "DOG" after the last course that you already see listed. 

Current code on line 470: 
mutate(Program = replace(Program, Program == "MAC"| Program == "COP"| Program == "STA", "PASS"))
Code on line 470 revised with DOG course added into PASS program:
 mutate(Program = replace(Program, Program == "MAC"| Program == "COP"| Program == "STA" | Program == “DOG” , "PASS"))

Note: If you don't know which courses are on the csv you are adding, Type “view(Courses)” into the console and hit enter to get a list of courses that are included in your data for this semester. 





