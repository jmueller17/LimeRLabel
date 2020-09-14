#' @title Checks if variable is of sub-question
#' 
#' @description Columns in LimeSurvey based result data frames hold variables that can represent questions or
#'  sub-questions. In case a variable represents a sub-question, it can be useful to call \code{\link{get_lsParentlabel}} 
#'  in order to retrieve the text of the overarching question. 
#'  
#'  The function simply checks if the parent label is NULL or not. 
#'  
#' @param x Variable column of the result data frame
#' 
#' @return Bool. \code{TRUE} in case the column holds responses of a sub-question
#'
#' @export is_lsSubquestion
#'
is_lsSubquestion <- function(x){
    
    if (is.null(get_lsParentlabel(x))){
        return(FALSE)
        
    } else {
        return(TRUE)
    } 
    
}