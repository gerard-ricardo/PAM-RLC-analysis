# PAM-RLC-analysis


This is a code to automate rapid light curves analysis from Pulse Amplitude Modulation Fluorometry (PAM). 

Step 1) Labelling
1)Each RLC run needs to be saved in a csv file.
2) Every file needs to be in an allocated folder, and each run labelled with a unique ID, with each label the same length seperate by spaces. I use Bulk Rename Utility. An example may be 'd001 mille', 'd050 mille',  'd100 mille'. Here the unique ID is the 4-digit ID, and the second label describes the experiment. The code will read the first label. 


Step 2)
IMprting folder of RLC. 
1) Set working directory to folder > RUn this code . Note that if you have delimited you read_delim(filename, delim = ';'). I have the unique ID saved as 'disc'

library(tidyr)
library(readr)
library(dplyr)
library(plyr)
read_csv_filename1 <- function(filename){
    ret <- read_csv(filename)
    ret$disc <- filename #EDIT
    ret
}
filenames = list.files(full.names = TRUE)
data1 <- ldply(filenames, read_csv_filename1)
head(data1)
options(scipen = 999)  # turn off scientific notation
