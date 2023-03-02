library(learnr)
library(rchallenge)
library(mlr3)
library(mlr3learners)
library(skimr)

data("german", package = "rchallenge")
data("BostonHousing", package = "mlbench")

### needed for r example r chunks
taskRegr = as_task_regr(BostonHousing, id = "BostonHousing", target = "medv")
splitsRegr <-  mlr3::partition(taskRegr, ratio = 0.8)
lrnRegr = lrn("regr.rpart")
lrnRegr$train(taskRegr, row_ids = splitsRegr$train)
lrnRegr$predict(taskRegr, row_ids = splitsRegr$test)
predRegr <- lrnRegr$predict(taskRegr, row_ids = splitsRegr$test)
resampling = rsmp("holdout", ratio = 2 / 3)
resRegr = resample(taskRegr, learner = lrnRegr, resampling = resampling)


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
  taskClassifSol <-
    as_task_classif(german, id = "GermanCredit", target = "credit_risk")

  # check if variable "taskClassif" (= task specified by user) is in exercise environment
  if (!("taskClassif" %in% ls(envir))) {
    message <-
      "Make sure you use the variable name specified in the exercise chunk!"
    return(list(message = message,
           correct = FALSE,
           location = "append"))
  }
  task <- envir$taskClassif
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
  if (task$target_names != taskClassifSol$target_names) {
    message <-
      "There is something wrong with the backend you've specified. Make sure to use the correct column as target!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  # if user code is correct
  assign("taskClassif", taskClassifSol, envir = .GlobalEnv)
  return(list(
    message = message,
    correct = TRUE,
    location = "append"
  ))
}

# Check function for data split
checker_data <- function(user_code, solution_code, envir) {
  message <- "Everything looks fine! Run your code!"
    #taskClassif <- get("taskClassif", envir = .GlobalEnv)
    set.seed(123)
    split_sol <- mlr3::partition(taskClassif, ratio = 0.8)
    ### User uses the correct variable name
    if (!("splitsClassif" %in% ls(envir))) {
      message <- "Make sure you use the variable name specified in the exercise chunk!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    split <- envir$splitsClassif
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
    ### if everything is solved correctly
    assign("splitsClassif", split_sol, envir = .GlobalEnv)
    return(list(
      message = message,
      correct = TRUE,
      location = "append"
    ))
  }


# check function for learner
checker_learner <- function(envir) {
  message <- "Everything looks fine! Run your code!"
  lrnClassif_sol <- lrn("classif.ranger")
  if (!("lrnClassif" %in% ls(envir))) {
    message <-
      "Make sure you use the variable name specified in the exercise chunk!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  learner <- envir$lrnClassif
  # check if learner-Variable is of class Learner
  if (!("Learner" %in% class(learner))) {
    message <-
      "Make sure you initialize an object of mlr3 learner class!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  if (!(class(learner)[[1]] == class(lrnClassif_sol))[[1]]) {
    message <- "Make sure you initialize a random forest for classification!"
    return(list(message = message,
                correct = FALSE,
                location = "append"))
  }
  assign("lrnClassif", envir$lrnClassif, envir = .GlobalEnv)
  return(list(
    message = message,
    correct = TRUE,
    location = "append"
  ))
}

# Check function for training
checker_train <-function(user_code, solution_code, envir) {
    user_code <- gsub(" |[\r\n]+", "", user_code)
    solution_code <- gsub(" ", "", solution_code)
    #lrnClassif <- get("lrnClassif", envir = .GlobalEnv)
    #taskClassif <- get("taskClassif", envir = .GlobalEnv)
    message <- "Everything looks fine! Run your code!"
    if (!(user_code %in% solution_code)) {
      message <- "There is something wrong with your function. Check for spelling errors and that you use the correct data/ids for training!"
      return(list(
          message = message,
          correct = FALSE,
          location = "append"
        ))
    }
    #eval(parse(text = user_code))
    set.seed(123)
    lrnClassif$train(row_ids = splitsClassif$train, task = taskClassif)
    assign("lrnClassif", lrnClassif, envir = .GlobalEnv)
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
    #lrnClassif <- get("lrnClassif", envir = .GlobalEnv)
    preds <- lrnClassif$predict(row_ids = splitsClassif$test, task = taskClassif)
    message <- "Everything looks fine! Run your code!"
    if (!("predClassif" %in% ls(envir))) {
      message <-
        "Make sure you use the variable name specified in the exercise chunk!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    predClassif <- envir$predClassif
    # check if learner-Variable is of class Learner
    if (!("PredictionClassif" %in% class(predClassif))) {
      message <-
        "Make sure you use the predict()-function on the correct task!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    if (!identical(predClassif$row_ids,splitsClassif$test)){
      message <-
        "Make sure you use the correct test set for prediction!"
      return(list(message = message,
                  correct = FALSE,
                  location = "append"))
    }
    #eval(parse(text = user_code))
    assign("predClassif", preds, envir = .GlobalEnv)
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
