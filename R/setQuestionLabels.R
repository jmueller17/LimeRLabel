#' @import dplyr
#' @import stringr
#'
#'
#' @title Set variable label on exported LS result data
#'
#' @description LimeSurvey offers the option to export survey result data as an R data frame. Column names are question
#'  codes while variable values are response codes. This function assigns the question texts in the selected
#'  language to each variable (column). The question text is available as attribute "label" for each column. Useful in
#'  conjunction with \code{sjlabelled::get_label()} or \code{ggplot} which can automatically retrieves variable
#'  labels for graphics and/or frequency tables.
#'
#'
#'
#' @param data Data frame of LimeSurvey R result data set.
#' @param labels Data frame extracted from LimeSurvey *.lss xml file. See \code{\link{extract_question_labels}}
#' @param plang char language code. One of the available language codes from the survey
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

        # find labels for given question code
        labelrow <- labels %>%
            filter(lang == plang & onecode == pcode)


        # if no label exists use columname (question code or metainfo columns)
        if (is.null(labelrow) | nrow(labelrow) == 0) {
            attr(data[,pcode], "label") <- pcode
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

        # store the LimeSurvey question type
        attr(data[,pcode], "lsqid") <- labelrow$qid

        # store LimeSurvey question type
        attr(data[,pcode], "lsqtype") <- labelrow$qtype

    }

    data

}







