#' @import xml2
#' @import purrr
#' @import dplyr
#'
#'
#' @title Extract Question Labels
#'
#' @description Extract all question labels from a LimeSurvey Survey Structure file (*.lss). The *.lss file is
#'  exported manually and downloaded. This function extracts all question and sub-question texts in
#'  all available languages and returns them as data frame.
#'
#' @param file char path to exported LimeSurvey Survey Structure *.lss file
#' @param strip_html logical. Default set to \code{TRUE} will remove any simple HTML tags from the labels. Otherwise leaves 
#'  label string as exported from LS. 
#'
#' @return data frame containing the following fields:
#'  \itemize{
#'    \item{qid - question with id}
#'    \item{qcode - LS question code, e.g. SDEM001, GlickM01, }
#'    \item{subqcode - LS sub-question code, e.g. SQ001, SQ002, etc. }
#'    \item{qtxt - LS question text}
#'    \item{subqtxt - LS sub-question text}
#'    \item{onecode - question and subquestion code as one string}
#'    \item{lang - language of answer text}
#'  }
#'
#'
#' @export extract_question_labels
#'
extract_question_labels <- function(file, strip_html=T){

    doc <- xml2::read_xml(file)

    message("Extracting question and subquestion labels...")

    # all question ids
    qid <-xml2::xml_find_all(doc, ".//questions/rows/row") %>%
        purrr::map_df(function(x) {
            list(
                qid=xml2::xml_find_first(x, "qid") %>% xml2::xml_text(),      # question id 
                qcode=xml2::xml_find_first(x, "title") %>% xml2::xml_text(),  # question code
                qtype=xml2::xml_find_first(x, "type") %>% xml2::xml_text(),   # question type
                qoth=xml2::xml_find_first(x, "other") %>% xml2::xml_text(),   # has "other" response option
                gid=xml2::xml_find_first(x, "gid") %>% xml2::xml_text()      # question group id
            )
        })

    # all sub-questions
    qsub <- xml2::xml_find_all(doc, ".//subquestions/rows/row") %>%
        purrr::map_df(function(x) {
            list(
                pid=xml2::xml_find_first(x, "parent_qid") %>% xml2::xml_text(),
                qid=xml2::xml_find_first(x, "qid") %>% xml2::xml_text(),
                qtxt=xml2::xml_find_first(x, "title") %>% xml2::xml_text(),
                qtype=xml2::xml_find_first(x, "type") %>% xml2::xml_text(),
                qoth=xml2::xml_find_first(x, "other") %>% xml2::xml_text()
            )
        })

    # all question texts
    qtxt <- xml2::xml_find_all(doc, ".//question_l10ns/rows/row") %>%
        purrr::map_df(function(x) {
            list(
                qid=xml2::xml_find_first(x, "qid") %>% xml2::xml_text(),
                qtxt=xml2::xml_find_first(x, "question") %>% xml2::xml_text(),
                lang=xml2::xml_find_first(x, "language") %>% xml2::xml_text()
            )
        })


    # retrieve labels for simple questions
    df_que <- qid %>%
        dplyr::left_join(qtxt, by="qid")

    # retrieve labels for subquestions
    df_sub <- qsub %>%
        dplyr::left_join(qtxt, by="qid")

    # merge questions and subquestions together
    df_question_labels <- df_que %>%
        dplyr::full_join(df_sub, by=c("qid"="pid", "lang")) %>%
        dplyr::rename(subid = qid.y, qtype=qtype.x, subqcode=qtxt.x, subqtxt=qtxt.y, qoth=qoth.x) %>%
        dplyr::mutate(onecode = if_else(!is.na(subqcode), paste0(qcode,".",subqcode,"."), qcode)) %>%
        dplyr::select(-qtype.y, -qoth.y)
    
    # remove html tags from string
    if (strip_html){
        df_question_labels$qtxt <- stringr::str_remove_all(df_question_labels$qtxt, "<.*?>")
        df_question_labels$subqtxt <- stringr::str_remove_all(df_question_labels$subqtxt, "<.*?>")
    }

    avail_lang <- paste(unique(df_question_labels$lang),sep="-", collapse = "-")

    message("Available languages: ", avail_lang)


    df_question_labels

}

