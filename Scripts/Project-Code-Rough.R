library(tidyverse)

ALLPringtingsCSVFiles <- "Individual-Project/Data/AllPrintingsCSVFiles.zip" 
file_list <- unzip(ALLPringtingsCSVFiles, list = TRUE)
print(file_list)

file_to_extract <- "data1.csv" # Replace with the name of the file inside the zip
