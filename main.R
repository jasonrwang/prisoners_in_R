### Main File to run things in!
source('scripts/raw_parser.R', chdir = TRUE)
source('scripts/extract_statistics.R', chdir = TRUE)

## Run parser to create string
data_frame=axelrod_encode(raw_parser('data/tournament.csv'))
typeof(data_frame)
## Run parsed array through statistics
b=extract_statistics(data_frame)
View(b)
## Modify main string with statistical  results

## Run agent
