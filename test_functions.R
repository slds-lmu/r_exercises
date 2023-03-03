library(learnr)
library(rchallenge)
library(mlr3)
library(mlr3learners)
library(skimr)

data("german", package = "rchallenge")
data("BostonHousing", package = "mlbench")

### needed for r example r chunks
task_regr = as_task_regr(BostonHousing, id = "BostonHousing", target = "medv")
split_regr <-  mlr3::partition(task_regr, ratio = 0.8)
lrn_regr = lrn("regr.rpart")
lrn_regr$train(task_regr, row_ids = split_regr$train)
lrn_regr$predict(task_regr, row_ids = split_regr$test)
pred_regr <- lrn_regr$predict(task_regr, row_ids = split_regr$test)


### Endpoint for various checkerfunctions
### Relevant input Arguments:
### label = String corresponding to the name of the chunk from which the checker is calles (e.g. "task")
### user_code = String containing the user code
### solution_code = String containing the code from the solution chunk
### envir_result = environment containing all the global variables from the set up chunks and the variables defined in the current chunk
### last_value = last_value of exercise chunk
checker_endpoint <-
  function(label = exercise$label,
           user_code = exercise$code,
           solution_code = exercise$solution,
           check_code = exercise$check,
           envir_result = envir,
           evaluate_result = evaluate_result,
           envir_prep = envir_prep,
           last_value = last_value,
           ...) {
    print(label)
    if (label == "task") {
      return(checker_task(envir_result, german))
    }
    else if (label == "learner") {
      return(checker_learner(envir_result))
    }
    else if (label == "predict") {
      return(checker_predict(user_code, solution_code, envir_result))
    }
    else if (label == "train") {
      return(checker_train(user_code, solution_code, envir_result))
    }
    else if (label == "resampling") {
      return(checker_resampling(user_code, solution_code, envir_result))
    }
    else if (label == "data") {
      return(checker_data(user_code, solution_code, envir_result))
    }
    else if (label == "pe") {
      return(checker_pe(user_code, solution_code, envir_result))
    }
  }

# Check function for tasks
checker_task <- function(envir, backend) {
  message <- "Everything looks fine! Run your code!"
  task_classif_sol <-
    as_task_classif(german, id = "GermanCredit", target = "credit_risk")

  # check if variable "task_classif" (= task specified by user) is in exercise environment
  if (!("task_classif" %in% ls(envir))) {
    message <-
      "Make sure you use the variable name specified in the exercise chunk!"
    return(list(message = message,
           correct = FALSE,
           location = "append"))
  }
  task <- envir$task_classif
  # check if task-Variable is of class Task
  if (!("Task" %in% class(task))) {
    message <- "Make sure you initialize an object of mlr3 class Task"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  data <-
    as.data.frame(task$backend$data(
      rows = seq_len(nrow(backend)),
      cols = colnames(backend),
      data_format = "data.table"
    ))
  # check backend/data of task -> has to be done this way as data()- is a function and not necessarily the attribute to retrieve all data of the backend
  if (!identical(data, backend)) {
    message <-
      "There is something wrong with the backend you've specified. Make sure to use the correct data set!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  if (task$target_names != task_classif_sol$target_names) {
    message <-
      "There is something wrong with the backend you've specified. Make sure to use the correct column as target!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  # if user code is correct
  assign("task_classif", task_classif_sol, envir = .GlobalEnv)
  return(list(
    message = message,
    correct = TRUE,
    location = "append"
  ))
}

# Check function for data split
checker_data <- function(user_code, solution_code, envir) {
  message <- "Everything looks fine! Run your code!"
    #task_classif <- get("task_classif", envir = .GlobalEnv)
    set.seed(123)
    split_sol <- mlr3::partition(task_classif, ratio = 0.8)
    ### User uses the correct variable name
    if (!("split_classif" %in% ls(envir))) {
      message <- "Make sure you use the variable name specified in the exercise chunk!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    split <- envir$split_classif
    if (is.null(split$train) | is.null(split$test)) {
      message <- "Make sure that your list contains two elements named \"train\" and \"test\""
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    if (!(class(split$train) %in% c("numeric", "integer")) |
        !(class(split$test) %in% c("numeric", "integer"))) {
      message <-
        "Make sure that your list elements are integer vectors"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    if (length(split$train) != length(split_sol$train) |
        length(split$test) != length(split_sol$test))  {
      message <- "Make sure to use the ratio 0.8 and the correct task for creating your split!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    #if () #### Überlappung testen
    if (length(setdiff(split$test, split$test)) > 0) {
      message <- "Make sure that your train/test-splits don't overlap!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    ### if everything is solved correctly
    assign("split_classif", split_sol, envir = .GlobalEnv)
    return(list(
      message = message,
      correct = TRUE,
      location = "append"
    ))
  }


# check function for learner
checker_learner <- function(envir) {
  message <- "Everything looks fine! Run your code!"
  lrn_classif_sol <- lrn("classif.rpart")
  if (!("lrn_classif" %in% ls(envir))) {
    message <-
      "Make sure you use the variable name specified in the exercise chunk!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  lrn <- envir$lrn_classif
  # check if learner-Variable is of class Learner
  if (!("Learner" %in% class(lrn))) {
    message <-
      "Make sure you initialize an object of mlr3 learner class!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  if (!(class(lrn)[[1]] == class(lrn_classif_sol))[[1]]) {
    message <- "Make sure you initialize a classification tree!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  if (!identical(lrn$param_set$values, lrn_classif_sol$param_set$values)) {
    message <- "Don't use any different parameter values. Use the default values!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  assign("lrn_classif", lrn_classif_sol, envir = .GlobalEnv)
  return(list(
    message = message,
    correct = TRUE,
    location = "append"
  ))
}

# Check function for training
checker_train <-function(user_code, solution_code, envir) {
    message <- "Everything looks fine! Run your code!"
    lrn_classif_sol <- lrn("classif.rpart")
    set.seed(123)
    eval(parse(text = user_code))
    set.seed(123)
    lrn_classif_sol$train(row_ids = split_classif$train, task = task_classif)
    if (!(identical(lrn_classif_sol$model, lrn_classif$model))) {
      message <- "There is something wrong with your function. Check for spelling errors and that you use the correct data/ids for training!"
         return(list(
             message = message,
             correct = FALSE,
             location = "append"
           ))
    }
    assign("lrn_classif", lrn_classif_sol, envir = .GlobalEnv)
    return(
      list(
        message = message,
        correct = TRUE,
        location = "append"
      )
    )
  }


# Check function for prediction
checker_predict <- function(user_code, solution_code, envir) {
    #lrn_classif <- get("lrn_classif", envir = .GlobalEnv)
    preds <- lrn_classif$predict(row_ids = split_classif$test, task = task_classif)
    message <- "Everything looks fine! Run your code!"
    if (!("pred_classif" %in% ls(envir))) {
      message <-
        "Make sure you use the variable name specified in the exercise chunk!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    pred_classif <- envir$pred_classif
    # check if prediction-Variable is of class Prediction
    if (!("PredictionClassif" %in% class(pred_classif))) {
      message <-
        "Make sure you use the predict()-function on the correct task!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    if (!identical(pred_classif$row_ids,split_classif$test)){
      message <-
        "Make sure you use the correct data set for prediction!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    #eval(parse(text = user_code))
    assign("pred_classif", preds, envir = .GlobalEnv)
    return(
      list(
        message = message,
        correct = TRUE,
        location = "append"
      )
    )
  }


 checker_pe <-
   function(user_code,
            solution_code, envir) {
     user_code <- gsub(" |[\r\n]+", "", user_code)
     solution_code <- gsub(" ", "", solution_code)
     if (!(user_code %in% solution_code)) {
       return(
         list(
           message = "Make sure to explicitly define the classification error as performance measure",
           correct = FALSE,
           location = "append"
         )
       )
     }
     return(
       list(
         message = "Everything looks fine! Run your code!",
         correct = TRUE,
         location = "append"
       )
     )
   }
