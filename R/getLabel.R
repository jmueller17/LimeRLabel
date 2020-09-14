#' @title Get Question Label
#' 
#' @description Retrieves question or sub-question text used in a LimeSurvey questionnaire. Implies that the question label 
#'  has been previously assigned by calling the \code{\link{set_question_labels}} function. The question label is stored 
#'  as attribute "label" for each column of the data frame. 
#'  
#'  In case of sub-questions, the label of the sub-question item is returned, not the text of the overarching (parent) 
#'  question. To retrieve the corresponding parent question see \code{\link{get_lsParentlabel}}.
#' 
#' @param x Variable column of the result data frame
#' 
#' @return String of question label or \code{NULL} if no label has been set. 
#' 
#' @export get_lsLabel
#'
get_lsLabel <- function(x){
    
    return(attr(x, which="label"))

}

