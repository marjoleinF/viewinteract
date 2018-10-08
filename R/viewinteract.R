#' Internal function for generating 0-1 dummy coded rule matrix
#' 
#' \code{get_rule_mat} generates a matrix of 0-1 coded dummary indicator 
#' variables, based on a character vector of rules and matrices of input
#' variables.
#' 
#' @param rules character vector with rules. Rules are conditions of input 
#' variable values.  
#' @param views named list containing matrices with different views of the
#' same observations, containing the variables specfied \code{rules}.
#' @param firstview the index of the matrix in \code{views} that was
#' used for creating the first split. 
#' @param secondview the index of the matrix in \code{views} that was used 
#' for creating the second and third splits.
get_rule_mat <- function(rules, views, firstview, secondview) {
  if (length(rules) == 0) { 
    return(NULL) 
  } else {
    ## Evaluate single-condition rules:
    rule1 <- eval(parse(text = rules[1]), views[[firstview]])
    rule2 <- eval(parse(text = rules[2]), views[[secondview]])
    rule3 <- eval(parse(text = rules[3]), views[[secondview]])
    
    ## Combine into matrix with two-condition rules and return:
    cbind(rule1, rule2, rule3, 
          rule4 = rule1*rule2, 
          rule5 = rule1*(!rule2), 
          rule6 = (!rule1)*rule3, 
          rule7 = (!rule1)*(!rule3))
  }
}




#' Internal function for taking rules from an rpart stump
#' 
#' \code{get_rule} returns a character vector of rules, 
#' derived from the rpart tree supplied as input.
#' 
#' @param tree rpart tree.
#' @param keepcomplement logical or character vector of length one.
#' If \code{TRUE}, two perfectly collinear rules will be returned. 
#' If \code{"right"}, the rule indicating membership of the right 
#' node of \code{tree} will be returned. If \code{"left"}, the rule 
#' indicating membership of the left node of \code{tree} will be 
#' returned.
get_rule <- function(tree, keepcomplement = TRUE) {
  paths <- unname(sapply(sapply(
    path.rpart(tree, nodes = rownames(tree$frame), print.it = FALSE), 
    `[`, index = -1), paste, collapse = " & ")[-1])
  if (keepcomplement == "left") {
    return(paths[grep("<", paths)])
  } else if (keepcomplement == "right") {
    return(paths[grep(">=", paths)])
  } else {
    return(paths)
  }
}




#' Internal function for updating response in gradient boosting
#' 
#' \code{get_y_learn_logistic} returns the pseudoresponse, based
#' on a current weightd sum of linear predictors (eta) and the 
#' original response (y).
#' 
#' @param eta weighted predictions up to the current iteration.
#' @param y pseudo response from the first iteration.
get_y_learn_logistic <- function(eta, y) {
  trunc_fac <- 12
  eta <- pmin(pmax(eta, -trunc_fac), trunc_fac)
  p <- 1 / (1 + exp(-eta))
  (y - p) / sqrt(p * (1 - p))
}




#' Function for generating rule sets with main and interaction
#' effects between a set of views
#' 
#' \code{get_rules} generates rules containing main and interacion
#' effects from the specified set of views.
#' 
#' @param views (named) list of 2 or more views, possibly involved in 
#' cross-view interactions. Each view should be a matrix, with the number 
#' of rows equal between views.     
#' @param y binary factor with length equal to the number of rows in each
#' of the views specified by \code{views}.
#' @param ntrees integer vector of length one. Number of trees that should be
#' generated.
#' @param sampfrac numeric value between 0 and 1. Specifies the fraction of
#' observations that should be randomly sampled for generating each tree.
#' Values between 0 nd 1 yield subsamples of the training data, a value of
#' 1 yields bootstrap samples of the training data.
#' @param learnrate numeric value between 0 and 1. Learning rate of boosting
#' parameter. Controls the influence of earlier trees on the next tree to 
#' be generated. Specifying a value of 0 will yield a (su)bagging approach
#' to tree induction, values \eqn{> 0} a boosting approach to tree induction.
#' Higher values increase the influence of earlier trees. Small non-zero
#' values are likely to perform best, which is reflected in the default
#' learing rate of .01.
#' @return A named list of three objects: \code{rules_main}, 
#' \code{rules_interact} and \code{dictionaty}. The first is a matrix, 
#' with 0-1 coded indicator variables for rules with one
#' condition. The second is a matrix with o-1 coded indicator variables 
#' for rules with two conditions. Two-condition rules always contain 
#' conditions involving two variables from different views, thereby
#' accomodating cross-view interactions.
#' @export
get_rules <- function(views, y, ntrees = 500, sampfrac = .632, learnrate = .01) {
  
  ## Check whether response is a binary factor:
  if (!(is.factor(y) && nlevels(y) == 2L)) {
    stop("Only binary factor response variables are currently supported.")
  }
  
  ## Check whether y and all views have same number of observations:
  if (var(sapply(views, nrow) != 0)) {
    stop("All views should have the same number of observations")
  } else if (length(y) != nrow(views[[1]])) {
    stop("The number of observations should be equal for the response y and the views.")
  }
  
  ## Check whether there are missing response values:
  if (any(is.na(y))) {
    stop("Response variable y does but should not contain missings.")
  }
  if (any(colSums(sapply(views, is.na)) > 0)) {
    stop("One or more views do but should not contain missings.")    
  }
  
  ## Check whether sampfrac is between 0 and 1:
  if (!(is.numeric(sampfrac) && sampfrac <= 1 && sampfrac > 0)) {
    stop("Argument sampfrac should specify a value between 0 and 1.")
  } 
  
  ## Check whether learnrate is between 0 and 1:
  if (!(is.numeric(learnrate) && learnrate <= 1 && learnrate >= 0)) {
    stop("Argument learnrate should specify a value between 0 and 1.")
  } 
  
  ## Check whether ntrees is larger than 0:
  if (!(is.numeric(ntrees) && ntrees > 1)) {
    stop("Argument ntrees should specify an integer value > 0.")
  }
  
  N <- nrow(views[[1]])
  rulemat <- matrix(NA, nrow = N, ncol = 7L*ntrees)
  dictionary <- data.frame(rule = rep(NA, times = 7L*ntrees), 
                           description = rep(NA, times = 7L*ntrees), 
                           ruletype = rep(NA, times = 7L*ntrees),
                           view1 = rep(NA, times = 7L*ntrees),
                           view2 = rep(NA, times = 7L*ntrees))
  
  ## Initialize gradient boosting:
  if (learnrate > 0) {
    y <- y == levels(y)[1]
    p_bar <- mean(y)
    eta_0 <- log(p_bar / (1 - p_bar))
    eta <- rep(eta_0, length(y))
    p_0 <- 1 / (1 + exp(-eta))
    y <- pseudo_y <- ifelse(y, log(p_0), log(1 - p_0))
    tree_predictions <- rep(NA, times = length(y))
  } else {
    pseudo_y <- y
  }
  
  for (i in 1:ntrees) {
    
    ## Sample observations:
    if (sampfrac == 1L) {
      sample_ids <- sample(1L:N, size = N, replace = TRUE)
    } else {
      sample_ids <- sample(1L:N, size = sampfrac*N, replace = FALSE)
    }
    
    ## Sample views:
    sampledviews <- sample(1L:length(views), size = 2L, replace = FALSE)
    firstview <- sampledviews[1] 
    secondview <- sampledviews[2] 
    
    ## Fit stump using firstview:
    stump1 <- rpart(y ~ ., data = cbind(y = pseudo_y[sample_ids], 
                                        views[[firstview]][sample_ids,]), 
                    maxdepth = 1L)
    
    ## Check if first split was made:
    if (nrow(stump1$frame) < 3L) next
    
    if (learnrate > 0) {
      tree_predictions <- predict(stump1, newdata = views[[firstview]])
    }
    
    ## TODO: if learnrate == 0 and variance of y is zero in one of the nodes,
    ##  should not continue, as this yields an error
    
    ## Fit stump using secondview of observations in left node:
    stump2 <- rpart(y ~ ., 
                    data = cbind(y = pseudo_y[sample_ids][stump1$where==2L], 
                                 subset(views[[secondview]][sample_ids,], 
                                        subset = stump1$where==2L)), 
                    maxdepth = 1L)
    ## Update predictions when gradient boosting:
    if (learnrate > 0) {
      tree_predictions[stump1$where==2L] <- predict(
        stump2, newdata = subset(views[[secondview]], 
                                 subset = stump1$where==2L))
    }
    
    ## Fit stump using secondview of observations in right node:
    stump3 <- rpart(y ~ ., 
                    data = cbind(y = pseudo_y[sample_ids][stump1$where==3L], 
                                 subset(views[[secondview]][sample_ids,], 
                                        subset = stump1$where==3L)), 
                    maxdepth = 1L)
    ## Update predictions when gradient boosting:
    if (learnrate > 0) {
      tree_predictions[stump1$where==3L] <- predict(
        stump3, newdata = subset(views[[secondview]], 
                                 subset = stump1$where==3L))
    }
    
    ## Update pseudo response when gradient boosting:
    if (learnrate > 0) {
      eta <- eta + learnrate * tree_predictions
      pseudo_y <- get_y_learn_logistic(eta, y)
    }
    
    ## Take each split as a rule:
    split1 <- get_rule(stump1, "left")
    if (length(split1) != 0) {
      split2 <- get_rule(stump2, "left")
      if (length(split2) != 0) {
        rules <- c(split1, split2)
      } else {
        rules <- c(split1, NA)
      }
      split3 <- get_rule(stump3, "left")
      if (length(split3) != 0) {
        rules <- c(rules, split3)
      } else {
        rules <- c(rules, NA)
      }
      ## Take combinations of rules:
      if (length(split1) != 0 && length(split2) != 0) {
        split4 <- paste(split1, "&", split2)
        split5 <- paste(split1, "&", get_rule(stump2, "right"))
        rules <- c(rules, split4, split5)
      } else {
        rules <- c(rules, NA, NA)
      }
      if (length(split1) != 0 && length(split3) != 0) {
        split6 <- paste(get_rule(stump1, "right"), "&", split3)
        split7 <- paste(get_rule(stump1, "right"), "&", get_rule(stump3, "right"))
        rules <- c(rules, split6, split7)
      } else {
        rules <- c(rules, NA, NA)
      }
      
      ## Save results to dictionary and rulematrix:
      ids <- ((i-1L)*7L+1L):((i-1L)*7L+length(rules))
      rulemat[,ids] <- get_rule_mat(rules, views, firstview, secondview)
      dictionary[ids, "description"] <- rules
      dictionary[ids, "view1"] <- rep(firstview, times = length(rules))
      dictionary[ids, "view2"] <- rep(secondview, times = length(rules))
      dictionary[ids, "ruletype"] <- c(rep("main", times = 3), rep("interaction", times = 4))
    }
  }
  
  colnames(rulemat) <- dictionary$rule <- paste0("rule", 1:ncol(rulemat))
  rulemat <- rulemat[ , colSums(is.na(rulemat)) == 0]
  dictionary <- na.omit(dictionary)
  unique_rules <- !duplicated(dictionary$description)
  dictionary <- dictionary[unique_rules,]
  rulemat <- rulemat[,unique_rules]
  ## TODO: identify collinear rules
  rules_main <- rulemat[,colnames(rulemat) %in% 
                          dictionary$rule[dictionary$ruletype == "main"]]
  rules_interact <- rulemat[,colnames(rulemat) %in% 
                              dictionary$rule[dictionary$ruletype == "interaction"]]
  
  return(list(rules_main = rules_main,
              rules_interact = rules_interact, 
              dictionary = dictionary))    
}