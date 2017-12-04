require(frbs)

FRBCS.CHI <- function(num_of_labels, train, X, Xrange)
{
  method.type <- "FRBCS.CHI"
  control.FRBCS.CHI <- list(num.labels = num_of_labels, type.mf = "GAUSSIAN")
  object.FRBCS.CHI <- frbs.learn(train, Xrange, method.type, control.FRBCS.CHI)
  pred <- predict(object.FRBCS.CHI, X)
  pred <- round(pred, digits = 0)
  return(pred)
}

FH.GBML <- function(num_of_labels, train, X, Xrange)
{
  method.type <- "FH.GBML"
  control.FH.GBML <- list(popu.size = 20, type.mf = "GAUSSIAN", num.class = num_of_labels, persen_cross = 0.5,  persen_mutant = 0.1, max.gen = 20)
  object.FH.GBML <- frbs.learn(train, Xrange, method.type, control.FH.GBML)
  pred <- predict(object.FH.GBML, X)
  pred <- round(pred, digits = 0)
  return(pred)
}




	