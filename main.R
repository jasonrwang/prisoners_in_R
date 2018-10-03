### Main File to run things in!
setwd("D:/Program Files/Git/prisoners_in_R")
source('scripts/raw_parser.R', chdir = TRUE)
source('scripts/extract_statistics.R', chdir = TRUE)

## Run parser to create string
data_frame = axelrod_encode(raw_parser('data/tournament.csv'))

## Run parsed array through statistics
axelrod_string = extract_statistics(data_frame)
View(axelrod_string)
print(axelrod_string)

#string for now: "C","C","C","D","C","C","C","D","C","C","C","D","C","C","C","D","C","D","C","D","C","D","D","D","C","D","D","D","D","D","D","D","C","C","C","D","C","D","D","D","C","D","D","D","D","D","D","D","D","D","D","D","C","D","C","D","C","D","D","D","D","D","D","D"

#string from pdf: "D","D","D","D","D","D","C","C","C","D","D","C","D","D","D","D","D","D","C","D","D","D","D","C","D","C","C","D","D","C","D","D","D","D","C","D","C","C","C","D","D","C","D","C","D","C","D","C","D","D","C","C","D","D","D","D","C","D","D","D","D","D","C","C"
