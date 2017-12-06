require(caTools)
require(unbalanced)
require(DMwR)
require(fmsb)
require(stats)
require(clusterSim)
source("CHI_GBML.R")

prepare_data <- function(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, num_of_labels, percent_of_train_examples)
{
  if(is_header_present)
  {
    Xy <- read.table(name_of_file, sep = delimiter, head = TRUE) 
    target_column_number <- match(target_column_name_or_number, names(Xy))
  }else
  {
    Xy <- read.table(name_of_file, sep = delimiter, head = FALSE)
    target_column_number <- target_column_name_or_number
  }
  if(!is_category_numerical)
  {
    Xy[, target_column_number] <- unclass(Xy[, target_column_number])
  }
  Xy <- Xy[sample(nrow(Xy)),] 
  sample <- sample.split(Xy, SplitRatio = percent_of_train_examples)
  train <- subset(Xy, sample == TRUE)
  is_zero_present = FALSE
  for(i in 1:NROW(train))
  {
    if(train[i, target_column_number] == 0)
    {
      is_zero_present = TRUE
      break
    }
  }
  if(is_zero_present)
  {
    for(i in 1:NROW(train))
    {
      train[i, target_column_number] <- train[i, target_column_number] + 1
    }
  }
  Xrange <- sapply(train[1:NCOL(train) - 1], range)
  test  = subset(Xy, sample == FALSE)
  X = subset(test, select = -target_column_number)
  y = subset(test, select = target_column_number)
  if(is_zero_present)
  {
    for(i in 1:NROW(y))
    {
      y[i, ] <- y[i, ] + 1
    }
  }
  return(list(matrix(as.numeric(unlist(X)), nr=nrow(X)), y, matrix(as.numeric(unlist(train)), nr=nrow(train)), Xrange))
}

run_tests <- function(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, num_of_labels, percent_of_train_examples, number_of_iterations, name_of_saved_file)
{
  FRBCS.CHI.accuracies <- vector()
  FRBCS.CHI.scores <- vector()
  FH.GBML.accuracies <- vector()
  FH.GBML.scores <- vector()
  for(i in 1:number_of_iterations)
  {
    data <-prepare_data(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, num_of_labels, percent_of_train_examples)
    X <- data[[1]]
    y <- data[[2]]
    train <- data[[3]]
    Xrange <-data[[4]]
    pred <- FRBCS.CHI(num_of_labels, train, X, Xrange)
    FRBCS.CHI.accuracies[[i]] <- accuracy(pred, y)
    FRBCS.CHI.scores[[i]] <- score(pred, y)$Result$estimate
    pred <- FH.GBML(num_of_labels, train, X, Xrange)
    FH.GBML.accuracies[[i]] <- accuracy(pred, y)
    FH.GBML.scores[[i]] <- score(pred, y)$Result$estimate
  }
  FRBCS.CHI_stats <- c(min(FRBCS.CHI.accuracies), median(FRBCS.CHI.accuracies), max(FRBCS.CHI.accuracies), min(FRBCS.CHI.scores), median(FRBCS.CHI.scores), max(FRBCS.CHI.scores))
  FRBCS.CHI_stats_labels <- c("min_acc_FRBCS.CHI", "median_acc_FRBCS.CHI", "max_acc_FRBCS.CHI", "min_score_FRBCS.CHI", "median_score_FRBCS.CHI", "max_score_FRBCS.CHI")
  FH.GBML_stats <- c(min(FH.GBML.accuracies), median(FH.GBML.accuracies), max(FH.GBML.accuracies), min(FH.GBML.scores), median(FH.GBML.scores), max(FH.GBML.scores))
  FH.GBML_stats_labels <- c("min_acc_FH.GBML", "median_acc_FH.GBML", "max_acc_FH.GBML", "min_score_FH.GBML", "median_score_FH.GBML", "max_score_FH.GBML")
  write.table(FRBCS.CHI_stats, file = name_of_saved_file, row.names = FRBCS.CHI_stats_labels)
  write.table(FH.GBML_stats, file = name_of_saved_file, row.names = FH.GBML_stats_labels, append = TRUE)
}

accuracy <- function(y_1, y_2)
{
  return(sum(y_1 == y_2)/length(y_1))
}

score <- function(y_1, y_2)
{
  return (Kappa.test(c(t(y_1)), c(t(y_2))))
}