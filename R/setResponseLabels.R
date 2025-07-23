#' @import dplyr
#'
#'
#' @title Set response labels on exported LS result data
#'
#' @description LimeSurvey offers the option to export survey result data as an R data frame. Variable values are
#'  stored as response codes. This function assigns value labels for each variable. Overall, for most variable types
#'  this means to convert the corresponding column and its values to a \code{factor} variable. Except for numeric,
#'  date and different open text fields (short, middle and long), all LimeSurvey and hence question types are converted
#'  to \code{factor}. LimeSurvey response codes are converted to factor levels while LimeSurvey response texts are assigned
#'  to the corresponding factor labels.
#'
#' @param data Data frame of LimeSurvey R result data
#' @param labels Data frame extracted from LimeSurvey *.lss xml file. See \code{\link{extract_response_labels}}
#' @param lang char language code
#' @param other vector of length two. First entry contains LS code used for "Other" entry in variable. Second entry 
#'  NA will lookup label for given language code, or use English label as default. See details.  
#' @param txtcom LS label used for comments, e.g. in P quesition type: multiple choice with comments.  
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
set_response_labels <- function(data, labels, plang, other=c("-oth-", NA), txtcom=c("comment")){

    # "Other" code as used by ls to indicate "other" response option
    lsOtherCode <- other[1]
    

    # "Other" label to be used; lookup i18n entry for given language code. 
    lsOtherLabel <- if_else(is.na(other[2]), get_i18nx(other[1], plang), other[2])
    
    # "Comment" label used by LS to indicate that a text field is related to other and contains a comment
    lsComPattern <- paste0(txtcom[1], "\\.")
    
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

        # in case of metadata columns, skip factor conversion
        #qtype <- if_else(is.null(qtype), "skip", qtype)
        
        if (is.null(qtype)){
            qtype <- "skip"
        }
        
        #save all attributes 
        ccattr <- attributes(data[,pcode])

        
        # skip columns and use cell entries "as is". This applies to differnt LS question types: 
        # metadata columns (which have no qid) 
        # numerical, date or (short, mid, long) text questions. 
        # for a list of question object types see https://www.limesurvey.org/manual/Question_object_types
        if (is.null(pqid) | qtype %in% c("skip", "N", "D", "S", "T", "U", "Q", "K")) next
        
        # Multiple Choice with comment has qtype="P" and uses two fields, the multiple choice 
        # and the second input field which is a text field. Both appear as "P" however. We skip
        # and use plain text only if it is the comment part. 
        if (qtype == "P" & stringr::str_detect(pcode, lsComPattern)) next

        
        # Multiple choice question has no labels, just 0,1 ("not check" or "checked", NA)
        if (qtype == "M" | qtype == "P"){
            answers <- data.frame(acode=c(0,1), 
                                  atxt=c(get_i18nx("not-checked", plang), get_i18nx("checked", plang)), 
                                  stringsAsFactors = F)
            
        # Yes/No question type has no labels. 
        } else if (qtype == "Y"){
            answers <- data.frame(acode=c(1,2), 
                                  atxt = c(get_i18nx("yes", plang), get_i18nx("no", plang)), 
                                  stringsAsFactors = F)
            
            
        } else {

            # retrieve all answers
            answers <- labels %>%
                filter(qid == pqid & lang == plang) %>%
                arrange(aorder)
        
        }

        
        fct_levels <- answers$acode
        fct_labels <- answers$atxt

        # LS exports "Other" options (with text field) as "-oth-" which needs to be assigned 
        # the correct label. Except: multiple choice which has just 0|1|NA but no -oth- 
        if (qother == "Y" & qtype != "M"){
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
