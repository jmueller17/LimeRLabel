#' @import dplyr
#'
#'
#' @title Set response labels on exported LS result data
#'
#' @description LimeSurvey offers the option to export survey result data as an R data frame. Variable values are
#'  stored as response codes. This function assigns value labels for each variable. Overall, for most variable types
#'  this means to convert the corresponding column and its values to a \code{factor} variable. Except for numberic,
#'  date and different open text fields (short, middle and long), all LimeSurvey and hence question types are converted
#'  to \code{factor}. LimeSurvey response codes are converted to factor levels while LimeSurvey response texts are assigned
#'  to the corresponding factor labels.
#'
#' @param data Data frame of LimeSurvey R result data
#' @param labels Data frame extracted from LimeSurvey *.lss xml file. See \code{\link{extract_response_labels}}
#' @param lang char language code
#' @param other vector of length two. First entry contains LS code used for "Other" entry in variable. Second entry 
#'  NA will lookup label for given language code, or use English label as default. See details.  
#'
#' @details Answer labels that are "Other" with text field have their own variable names of the form "questioncode.other" and
#' question type "!" (dropdown). 
#' to indicate that response option is
#'  "other" (text).  which is stored in an 
#'  additional variable. Default value by LS is "-oth-", but could be different.
#'
#' @return Data frame with value (answer) labels assigned to each variable (column).
#'
#' @seealso \code{\link{extract_response_labels}}
#'
#' @export set_response_labels
#'
set_response_labels <- function(data, labels, plang, other=c("-oth-", NA)){

    # "Other" code as used by ls to indicate "other" response option
    lsOtherCode <- other[1]
    
    # "Other" label to be used; lookup i18n entry for given language code. 
    lsOtherLabel <- if_else(is.na(other[2]), get_i18n(other[1], plang), other[2])
    
    
    # all available colnames 
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


    # loop over all columns (variables)
    for (pcode in colnames(data)){

        pqid <- attr(data[,pcode], "lsqid")

        # retrieve question type of variable
        qtype <- attr(data[,pcode], "lsqtype")
        
        # question has "other" option? 
        qother <-  attr(data[,pcode], "lsother")

        # in case of metadata columns, skype factor conversion
        qtype <- if_else(is.null(qtype), "skip", qtype)
        
        #save all attributes 
        ccattr <- attributes(data[,pcode])

        
        # skip columns of metadata (no qid) or
        # variables are numerical, date or (short, mid, long) text questions no factor assigned
        if (is.null(pqid) | qtype %in% c("skip", "N", "D", "S", "T", "U")) next


        # convert rest of answer values to factors.
        # retrieve all answers
        answers <- labels %>%
            filter(qid == pqid & lang == plang) %>%
            arrange(aorder)

        fct_levels <- answers$acode
        fct_labels <- answers$atxt

        # LS exports "Other" options (with text field) as "-oth-" which needs to be assigned 
        # the correct label
        if (qother == "Y"){
            fct_levels <- c(fct_levels, lsOtherCode)
            fct_labels <- c(fct_labels, lsOtherLabel)
        }
                
        # conver to factor
        data[,pcode] <- factor(data[,pcode], levels=fct_levels, labels=fct_labels)
        
        # reassign all attributes
        #ccattr <- c(ccattr, attributes(data[,pcode]))
        
        attributes(data[,pcode]) <- c(ccattr, attributes(data[,pcode]))

    }


    data

}
