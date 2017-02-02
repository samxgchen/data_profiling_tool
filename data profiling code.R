# Data Profiling Code - quickly understand patterns in string data

library(data.table)

# Function to convert abstract letters, numbers, and symbols
# Input: vector
# Output: vector
convert_to_abstract <- function(input) {
  converted_input <- input
  converted_input <- gsub('[[:alpha:]]', 'a', converted_input, perl = T)
  converted_input <- gsub('[[:punct:]]', '#', converted_input, perl = T)
  converted_input <- gsub('[[:digit:]]', '1', converted_input, perl = T)
  return(converted_input)
}

# Function to convert and summarize data
convert_and_summarize <- function(input) {
  converted_input <- convert_to_abstract(input)
  converted_input_dt <- data.table(converted_input)
  result <- converted_input_dt[, .N, converted_input]
  names(result) <- c('converted_input', 'occurance')
  result <- result[order(-occurance)]
  return(result)
}


# Vignette: Check list of US president names for consistency. 
# Load a list of US presidents from wikipedia
presidents <- fread('https://commondatastorage.googleapis.com/ckannet-storage/2012-05-08T122246/USPresident-Wikipedia-URLs-Thmbs-HS.csv')
str(presidents)

# Analyze their names
president_summary <- convert_and_summarize(presidents$President)

# Conclusions: most names are just first + last. Some have middle initial. One has an extra note (designating Cleveland's 2nd term).


##########################################################
##########################################################
# Testing code
x <- c('R is a collaborative project with many contributors. 1234 1234', 'R version 3.3.2 (2016-10-31) -- Sincere Pumpkin Patch', 'Platform: x86_64-apple-darwin13.4.0 (64-bit)')
presidents <- fread('https://commondatastorage.googleapis.com/ckannet-storage/2012-05-08T122246/USPresident-Wikipedia-URLs-Thmbs-HS.csv')
str(presidents)

# Remembering differences in R regex func's
grep('is', x)
grep('salkdjfs', x)
sub('is', 'are', x, perl=T)
gsub('[[:alpha:]]', 'a', x, perl=T)
gsub('[[:digit:]]', '1', x, perl=T)

convert_to_abstract(x)

converted_x <- convert_to_abstract(x)
converted_x
dt_converted_x <- data.table(converted_x)
str(dt_converted_x)

y <- convert_and_summarize(x)
y

president_summary <- convert_and_summarize(presidents$President)

