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
#lrnRegr$predict(taskRegr, row_ids = splitsRegr$test)
predRegr <- lrnRegr$predict(taskRegr, row_ids = splitsRegr$test)

### needed for checks in backend
taskClassifSol = as_task_classif(german, id = "GermanCredit", target = "credit_risk")
lrnClassifSol = lrn("classif.log_reg")
#lrnClassifSol$train(taskClassifSol)
#lrnClassifSol$predict(taskClassifSol)
#splitsClassifSol = mlr3::partition(taskClassifSol, ratio = 0.8)
#predClassifSol <- lrnClassifSol$predict(taskClassifSol, row_ids = splitsClassifSol$test)

taskClassif <- as_task_classif(german, id = "GermanCredit", target = "credit_risk")
lrnClassif = lrn("classif.log_reg")
splitsClassif = mlr3::partition(taskClassif, ratio = 0.8)
lrnClassif$train(taskClassif, row_ids = splitsClassif$train)
predClassif <- lrnClassif$predict(taskClassif, row_ids = splitsClassif$test)


### needed for PE
resampling = rsmp("holdout", ratio = 2/3)
resRegr = resample(taskRegr, learner = lrnRegr, resampling = resampling)
resClassSol = resample(taskClassifSol, learner = lrnClassifSol, resampling = resampling)



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
    else if (label == "resampling"){
      return(checker_resampling(user_code, solution_code, envir_result))
    }
    else if (label == "data"){
      return(checker_data(user_code, solution_code, envir_result))
    }
    else if (label == "pe"){
      return(checker_pe(user_code, solution_code, envir_result))
    }
  }


checker_task <- function(envir, backend) {
    print(ls(envir))
    #taskClassifSol <- envir$taskClassifSol
    bool_correctness <- TRUE
    message <- "Everything looks fine! Run your code!"
    # check if variable "taskClassif" (= task specified by user) is in exercise environment
    if (!("taskClassif" %in% ls(envir))) {
      message <-
        "Make sure you use the variable name specified in the exercise chunk!"
      bool_correctness <- FALSE
      return(list(
        message = message,
        correct = bool_correctness,
        location = "append"
      ))
    }
    task <- envir$taskClassif
    print(class(task))
    # check if task-Variable is of class Task
    if (!("Task" %in% class(task))) {
      return(
        list(
          message = "Make sure you initialize an object of mlr3 class Task",
          correct = FALSE,
          location = "append"
        )
      )
    }
    data <-
      as.data.frame(task$backend$data(
        rows = seq_len(nrow(backend)),
        cols = colnames(backend),
        data_format = "data.table"
      ))
    # check backend/data of task -> has to be done this way as data()- is a function and not necessarily the attribute to retrieve all data of the backend
    if (!identical(data, backend)) {
      bool_correctness <- FALSE
      message <-
        "There is something wrong with the backend you've specified. Make sure to use the correct data set!"
    }
    ## check target of task
    if (task$target_names != taskClassifSol$target_names) {
      bool_correctness <- FALSE
      message <-
        "There is something wrong with the backend you've specified. Make sure to use the correct column as target!"
    }
    assign("taskClassif", task, envir = .GlobalEnv)
    #taskClassif <<- task
    return(list(
      message = message,
      correct = bool_correctness,
      location = "append"
    ))

}


checker_data <-
  function(user_code,
           solution_code, envir) {
    print(ls(envir))
    print(user_code)
    print(solution_code)
    user_code <- gsub(" |[\r\n]+", "", user_code)
    solution_code <- gsub(" ", "", solution_code)
    if (user_code %in% solution_code) {
      assign("splitsClassif", envir$splitsClassif, envir = .GlobalEnv)
      return(
        list(
          message = "Everything looks fine! Run your code!",
          correct = TRUE,
          location = "append"
        )
      )
    }
    else {
      return(
        list(
          message = "There is something wrong with your split. Make sure you use the correct task, the correct split ration (0.8) and the variable name given in the exercise.",
          correct = FALSE,
          location = "append"
        )
      )
    }
  }



checker_learner <- function(envir) {
  print(ls(envir))
  bool_correctness <- TRUE
  message <- "Everything looks fine! Run your code!"
  if (!("lrnClassif" %in% ls(envir))) {
    message <-
      "Make sure you use the variable name specified in the exercise chunk!"
    bool_correctness <- FALSE
    return(list(
      message = message,
      correct = bool_correctness,
      location = "append"
    ))
  }
  learner <- envir$lrnClassif
  print(class(learner))
  print(class(lrnClassifSol))
  # check if learner-Variable is of class Learner
  if (!("Learner" %in% class(learner))) {
    return(
      list(
        message = "Make sure you initialize an object of mlr3 learner class",
        correct = FALSE,
        location = "append"
      )
    )
  }
  print(class(learner) == class(lrnClassifSol))
  ### check learner
  if (!(class(learner)[[1]] == class(lrnClassifSol))[[1]]) {
    bool_correctness <- FALSE
    message <-
      "There is something wrong with the learner you've specified"
  }
  assign("lrnClassif", envir$lrnClassif, envir = .GlobalEnv)
  return(list(
    message = message,
    correct = bool_correctness,
    location = "append"
  ))
}

checker_train <-
  function(user_code,
           solution_code, envir) {
    print(ls(envir))
    print(envir)
    print(user_code)
    print(solution_code)
    user_code <- gsub(" |[\r\n]+", "", user_code)
    solution_code <- gsub(" ", "", solution_code)
    if (user_code %in% solution_code) {
      #assign("lrnClassif", envir$lrnClassif, envir = .GlobalEnv)
      #do.call(get(user_code), list(), .GlobalEnv)
      #assign("lrnClassif", get(user_code), envir = .GlobalEnv)
      eval(parse(text=user_code))
      return(
        list(
          message = "Everything looks fine! Run your code!",
          correct = TRUE,
          location = "append"
        )
      )
    }
    else {
      return(
        list(
          message = "There is something wrong with your function. Check for spelling errors and that you use the correct data/ids for training!",
          correct = FALSE,
          location = "append"
        )
      )
    }
  }


checker_predict <-
  function(user_code,
           solution_code, envir) {
    print(ls(envir))
    print(user_code)
    print(solution_code)
    user_code <- gsub(" |[\r\n]+", "", user_code)
    solution_code <- gsub(" ", "", solution_code)
    if (user_code %in% solution_code) {
      eval(parse(text=user_code))
      return(
        list(
          message = "Everything looks fine! Run your code!",
          correct = TRUE,
          location = "append"
        )
      )
    }
    else {
      return(
        list(
          message = "There is something wrong with your function. Check for spelling errors and that you use the correct data/ids for prediction!",
          correct = FALSE,
          location = "append"
        )
      )
    }
  }


checker_resampling <-
  function(user_code,
           solution_code, envir) {
    print(ls(envir))
    print(user_code)
    print(solution_code)
    user_code <- gsub(" |[\r\n]+", "", user_code)
    solution_code <- gsub(" ", "", solution_code)
    if (user_code %in% solution_code) {
      return(
        list(
          message = "Everything looks fine! Run your code!",
          correct = TRUE,
          location = "append"
        )
      )
    }
    else {
      return(
        list(
          message = "There is something wrong with your function. Check for spelling errors or if your have used the correct data for training!",
          correct = FALSE,
          location = "append"
        )
      )
    }
  }

checker_pe <-
  function(user_code,
           solution_code, envir) {
    print(ls(envir))
    print(user_code)
    print(solution_code)
    user_code <- gsub(" |[\r\n]+", "", user_code)
    solution_code <- gsub(" ", "", solution_code)
    if (user_code %in% solution_code) {
      return(
        list(
          message = "Everything looks fine! Run your code!",
          correct = TRUE,
          location = "append"
        )
      )
    }
    else {
      return(
        list(
          message = "Make sure you use the classification error as measure and the correct predictions!",
          correct = FALSE,
          location = "append"
        )
      )
    }
  }

