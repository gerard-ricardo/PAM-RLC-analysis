# PAM-RLC-analysis


This is a code to automate rapid light curves analysis from Pulse Amplitude Modulation Fluorometry (PAM). 

Step 1) Labelling
1)Each RLC run needs to be saved in a csv file.
2) Every file needs to be in an allocated folder, and each run labelled with a unique ID, with each label the same length seperate by spaces. I use Bulk Rename Utility. An example may be 'd001 mille', 'd050 mille',  'd100 mille'. Here the unique ID is the 4-digit ID, and the second label describes the experiment. The code will read the first label. 


Step 2)
Importing the folder of RLC. 
1) Set working directory to folder > Run this code . Note that if you have delimited you read_delim(filename, delim = ';'). I have the unique ID saved as 'disc'

Step 3) 
Importing the environmental treatment data. 
Note: As you likely subsampled for the RLC curves, this code uses left_join to merge the two data.frames. You do not need them to match, as long as each has the same unique ID


Step 4) Create an rETR column and data clean for anomalies


Step 5) Get starting values
1) Run the SSplatt.my function. This is from the modified from the Platt package (they did all the hard work). 


Step 6) Run the nonlinear models and derive all the RLC parameters
