#' @import dplyr
#' @import stringr
#'
#'
#' @title Set variable label on exported LS result data
#'
#' @description LimeSurvey offers the option to export survey result data as an R data frame. Column names are question
#'  codes while variable values are response codes. This function assigns the question or sub-question texts in the selected
#'  language to each variable (column). The question text is available as attribute "label" for each column. Useful in
#'  conjunction with \code{sjlabelled::get_label()} or \code{ggplot} which can automatically retrieve variable
#'  labels for graphics and/or frequency tables.
#'
#' @param data Data frame of LimeSurvey R result data set.
#' @param labels Data frame extracted from LimeSurvey *.lss xml file. See \code{\link{extract_question_labels}}
#' @param plang char language code. One of the available language codes from the survey
#' 
#' @details Speical case conserns question and sub-question texts. For example a question might inquire how 
#'  respondents rate their job satisfaction along several dimensions. In such a question, the overarching question could be: 
#'  
#'  "Q1. To what extent do you agree or disagree with the following statements about your job?" 
#'  
#'  Respondents can then indicate their agreement via likert scale answer options on the sub-questions such as: 
#'  
#'  "Q1-S1. My job offers good prospects for career advancement"
#'  "Q1-S2. I generally get on well with my work colleagues"
#'  "Q1-S3. I might lose my job in the next 6 months"
#'  etc. 
#'  
#'  The individual responses to each sub-question are then stored in the corresponding columns of the result data
#'  frame. In case a column holds a sub-question, the "label" stores the sub-question text, e.g. "My job offers 
#'  good prospects for career advancement", etc. This makes sense to take advantage of the plotting likert scales with
#'  the correct sub-question label attached for each item. In case of sub-questions, the corresponding parent question
#'  text (e.g. "Q1. To what extent do you agree ...") is stored with a distinct attribute "parent_label". It can be 
#'  retrieved with \code{\link{get_lsParentlabel}}. 
#'   
#'
#' @return data frame with attached text label for each variable (question or sub-question).
#'
#' @seealso \code{\link{extract_question_labels}}
#'
#' @export set_question_labels
#'
set_question_labels  <- function(data, labels, plang){

    cnames <- colnames(data)

    # ExportR manual exported data frames have subquestion codes of the format
    # QUECODE01[SQ001], QUECODE01[SQ002], QUECODE01[SQ003], etc.
    # RemoteApi export data column names have format QUECODE01.SQ001., QUECODE01.SQ002.
    # unify column names
    if (any(stringr::str_detect(cnames, "\\[.*\\]"))){
        data <- dplyr::rename_with(data, ~ stringr::str_replace_all(.x, "\\[|\\]", "."))

        # remote api exported
        data <- as.data.frame(data)
    }


    #loop over all column names
    for (pcode in colnames(data)){

        # find labels for given question code. "onecode" contains code.subquestioncode in case it exists 
        # or question code only. 
        labelrow <- labels %>%
            filter(lang == plang & onecode == pcode)


        # if no question label exists use columname, i.e. existing code (applies for example to metainfo columns)
        if (is.null(labelrow) | nrow(labelrow) == 0) {
            attr(data[,pcode], "label") <- pcode
            warning("No text labels available for: ", pcode)
            next
        }


        # Simple question
        if (is.na(labelrow$subqcode)){

            attr(data[,pcode], "label") <- labelrow$qtxt

        # Question with sub-questions: use label attribute for sub-question text
        # and create new label for parent question label
        } else {

            attr(data[,pcode], "label") <- labelrow$subqtxt
            attr(data[,pcode], "parent_label") <- labelrow$qtxt

        }

        # store the LimeSurvey question id
        attr(data[,pcode], "lsqid") <- labelrow$qid

        # store LimeSurvey question type
        attr(data[,pcode], "lsqtype") <- labelrow$qtype
        
        # store info if question has "other" answer option
        attr(data[,pcode], "lsother") <- labelrow$qoth

    }

    data

}







