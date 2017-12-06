setwd("/home/igor/RProjects/Fuzzy-Models-Comparing")
source("Tools.R")

name_of_file <- "diabetes.csv"
target_column_name_or_number <- "class"
is_header_present <- TRUE
delimiter <- ","
num_of_labels <- 2
percent_of_train_examples <- 0.7
number_of_iterations <- 1
is_category_numerical <- TRUE
name_of_saved_file <- "stats.csv"

run_tests(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, num_of_labels, percent_of_train_examples, number_of_iterations, name_of_saved_file)
