require(caTools)
require(unbalanced)
require(DMwR)
require(fmsb)
require(stats)
require(clusterSim)
source("Models.R")

prepare_data <- function(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, percent_of_train_examples)
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
  Xy_y <- Xy[, target_column_number]
  Xy_norm <- data.Normalization(Xy[, -target_column_number], type = "n4", normalization = "column") 
  Xy <-Xy_norm
  Xy$class <- Xy_y
  is_zero_present = FALSE
  for(i in 1:NROW(Xy))
  {
    if(Xy[i, NCOL(Xy)] == 0)
    {
      is_zero_present = TRUE
      break
    }
  }
  if(is_zero_present)
  {
    for(i in 1:NROW(Xy))
    {
      Xy[i, NCOL(Xy)] <- Xy[i, NCOL(Xy)] + 1
    }
  }
  Xrange <- sapply(Xy[, 1:NCOL(Xy) - 1], range)
  sample <- sample.split(Xy, SplitRatio = percent_of_train_examples)
  train <- subset(Xy, sample == TRUE)
  test  = subset(Xy, sample == FALSE)
  X = subset(test, select = -NCOL(test))
  y = subset(test, select = NCOL(test))
  return(list(matrix(as.numeric(unlist(X)), nr=nrow(X)), y, matrix(as.numeric(unlist(train)), nr=nrow(train)), Xrange))
}

run_tests <- function(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, num_of_labels, percent_of_train_examples, number_of_iterations, name_of_saved_file)
{
  FRBCS.CHI.accuracies <- vector()
  FRBCS.CHI.scores <- vector()
  FRBCS.W.accuracies <- vector()
  FRBCS.W.scores <- vector()
  for(i in 1:number_of_iterations)
  {
    data <-prepare_data(name_of_file, target_column_name_or_number, is_header_present, is_category_numerical, delimiter, percent_of_train_examples)
    X <- data[[1]]
    y <- data[[2]]
    train <- data[[3]]
    Xrange <-data[[4]]
    pred <- FRBCS.CHI(num_of_labels, train, X, Xrange)
    FRBCS.CHI.accuracies[[i]] <- accuracy(pred, y)
    FRBCS.CHI.scores[[i]] <- score(pred, y)$Result$estimate
    pred <- FRBCS.W(num_of_labels, train, X, Xrange)
    FRBCS.W.accuracies[[i]] <- accuracy(pred, y)
    FRBCS.W.scores[[i]] <- score(pred, y)$Result$estimate
  }
  FRBCS.CHI_stats <- c(min(FRBCS.CHI.accuracies), median(FRBCS.CHI.accuracies), max(FRBCS.CHI.accuracies), min(FRBCS.CHI.scores), median(FRBCS.CHI.scores), max(FRBCS.CHI.scores))
  FRBCS.CHI_stats_labels <- c("min_acc_FRBCS.CHI", "median_acc_FRBCS.CHI", "max_acc_FRBCS.CHI", "min_score_FRBCS.CHI", "median_score_FRBCS.CHI", "max_score_FRBCS.CHI")
  FRBCS.W_stats <- c(min(FRBCS.W.accuracies), median(FRBCS.W.accuracies), max(FRBCS.W.accuracies), min(FRBCS.W.scores), median(FRBCS.W.scores), max(FRBCS.W.scores))
  FRBCS.W_stats_labels <- c("min_acc_FRBCS.W", "median_acc_FRBCS.W", "max_acc_FRBCS.W", "min_score_FRBCS.W", "median_score_FRBCS.W", "max_score_FRBCS.W")
  write.table(FRBCS.CHI_stats, file = name_of_saved_file, row.names = FRBCS.CHI_stats_labels)
  write.table(FRBCS.W_stats, file = name_of_saved_file, row.names = FRBCS.W_stats_labels, append = TRUE)
}

accuracy <- function(y_1, y_2)
{
  return(sum(y_1 == y_2)/length(y_1))
}

score <- function(y_1, y_2)
{
  return (Kappa.test(c(t(y_1)), c(t(y_2))))
}