### Main File to run things in!
source('scripts/raw_parser.R', chdir = TRUE)
source('scripts/extract_statistics.R', chdir = TRUE)

## Run parser to create string
data_frame = axelrod_encode(raw_parser('Example_Tournament/bid_register.csv'))

## Run parsed array through statistics
axelrod_string = extract_statistics(data_frame)
View(axelrod_string)

## Modify main string with statistical results
# Did Siemon complete this step already? Or is this still to be done?

## Run agent

out = raw_parser('Example_Tournament/bid_register.csv')

# setwd("/Users/jrwang/Documents/Coding/prisoners_in_R/Example_Tournament")