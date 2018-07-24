setwd("/home/igor/RProjects/Fuzzy-Models-Comparing")
source("Tools.R")

name_of_file <- "./Data/iris.data"
target_column_name_or_number <- 5
is_header_present <- FALSE
delimiter <- ","
num_of_labels <- 3
percent_of_train_examples <- 0.7
number_of_iterations <- 10
is_category_numerical <- FALSE
name_of_saved_file <- "irisStats.csv"

run_tests(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, num_of_labels, percent_of_train_examples, number_of_iterations, name_of_saved_file)
