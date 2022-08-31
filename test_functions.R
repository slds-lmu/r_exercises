checker_endpoint <-
  function(label = exercise$label,
           user_code = exercise$code,
           solution_code = exercise$solution,
           check_code = exercise$check,
           envir_result = envir,
           evaluate_result = evaluate_result,
           envir_prep = envir_prep,
           last_value = last_value, type = "test",
           ...) {
    if (label == "task") {
      return(checker_task(last_value, german, task_classif))
    }
    else if (label == "learner") {
      return(checker_learner(last_value, task_classif))
    }
    else if (label == "predict") {
      return(checker_predict(last_value, task_classif, user_code, solution_code))
    }
    else if (label == "train") {
      return(checker_train(last_value, task_classif, user_code, solution_code))
    }
  }


checker_task <- function(last_value = last_value, backend, task_classif) {
  bool_correctness <- TRUE
  print(ls(last_value))
  print(ls(last_value$backend))
  print((last_value$target_names))
  message <- "Everything looks fine! Run your code!"
  data <- as.data.frame(last_value$data(rows = c(1:1000),
                                        cols = colnames(backend),
                                        data_format = "data.table"))
  ### check backend of task
  if(!identical(data,backend)){
    bool_correctness <- FALSE
    message <- "There is something wrong with the target  you've specified"
  }
  ## check target of task
  if(last_value$target_names != task_classif$target_names){
    bool_correctness <- FALSE
    message <- "There is something wrong with the target you've specified"
  }
  return(list(message = message, correct = bool_correctness, location = "append"))
}

checker_learner <- function(last_value = last_value, lrn_classif) {
  bool_correctness <- TRUE
  message <- "Everything looks fine! Run your code!"
  print(last_value)
  print(typeof(last_value))
  print(class(last_value))
  ### check backend of task
  if(!identical(last_value, lrn_classif) | !(class(last_value) %in% c("LearnerClassifRpart", "LearnerClassif","Learner" ,
                                                                    "R6"))){
    bool_correctness <- FALSE
    message <- "There is something wrong with the learner you've specified"
  }
  return(list(message = message, correct = bool_correctness, location = "append"))
}

checker_predict <- function(last_value, task_classif, user_code, solution_code){
  print(user_code)
  print(solution_code)
  if (user_code == solution_code){
    return(list(message = "Everything looks fine! Run your code!", correct = TRUE, location = "append"))
  }
  else {
    return(list(message = "There is something wrong with your function. Check for spelling errors or if your have used the correct data for prediction!", correct = FALSE, location = "append"))
  }
}

checker_train <- function(last_value, task_classif, user_code, solution_code){
  print(user_code)
  print(solution_code)
  if (user_code %in% solution_code){
    return(list(message = "Everything looks fine! Run your code!", correct = TRUE, location = "append"))
  }
  else {
    return(list(message = "There is something wrong with your function. Check for spelling errors or if your have used the correct data for training!", correct = FALSE, location = "append"))
  }
}
