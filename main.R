### Main File to run things in!
source('scripts/raw_parser.R', chdir = TRUE)
source('scripts/extract_statistics.R', chdir = TRUE)

## Run parser to create string
data_frame = axelrod_encode(raw_parser('Example_Tournament/bid_register.csv'))

## Run parsed array through statistics
axelrod_string = extract_statistics(data_frame)

# The string above can be put into the agent manually now
