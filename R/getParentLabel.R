#' @title Get Parent Question Label
#' 
#' @description Retrieves parent question label for sub-questions of a LimeSurvey questionnaire. Implies that the question label 
#'  has been previously assigned by calling the \code{\link{set_question_labels}} function. The parent question label is stored 
#'  as attribute "parent_label" for each sub-question variable of the data frame. 
#' 
#' @param x Variable column of the result data frame
#' 
#' @return String of parent question label or \code{NULL} if no parent label has been set. 
#' 
#'
#' @export get_lsParentlabel
#' 
get_lsParentlabel <- function(x){
    
    return(attr(x, which="parent_label"))
    
}