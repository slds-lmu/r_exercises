library(learnr)
library(rchallenge)
library(mlr3)
library(mlr3learners)
library(skimr)

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
    taskClassifSol <- envir$taskClassifSol
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
    return(list(
      message = message,
      correct = bool_correctness,
      location = "append"
    ))

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
  ### check learner
  if (!(class(learner) == class(envir$lrnClassifSol))) {
    bool_correctness <- FALSE
    message <-
      "There is something wrong with the learner you've specified"
  }
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

checker_data <-
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
          message = "There is something wrong with your split. Make sure you use the correct task, the correct split ration (0.8) and the variable name given in the exercise.",
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

