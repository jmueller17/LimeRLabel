#' @import xml2
#' @import purrr
#' @import dplyr
#' @import stringr
#'
#'
#'
#' @title Extract Response Labels
#'
#' @description Extract all response labels from a LimeSurvey Survey Structure file (*.lss). The *.lss file is
#'  exported manually and downloaded. This function extracts all answer codes, their corresponding answer texts in
#'  all available languages and returns them as data frame.
#'
#' @param file char path to exported LimeSurvey Survey Structure *.lss file
#' @param strip_html logical. Default set to \code{TRUE} will remove any simple HTML tags from the labels. Otherwise leaves 
#'  label string as exported from LS. 
#'
#' @return data frame containing the following fields:
#'  \itemize{
#'    \item{qid - answer correspond to question with id}
#'    \item{aid - answer id}
#'    \item{acode - LS answer code}
#'    \item{atxt - LS answer text}
#'    \item{lang - language of answer text}
#'  }
#'
#'
#' @export extract_response_labels
#'
extract_response_labels  <- function(file, strip_html=T){

    doc <- xml2::read_xml(file)

    message("Extracting answer labels...")

    # all answer codes
    aid <- xml2::xml_find_all(doc, ".//answers/rows/row") %>%
        purrr::map_df(function(x) {
            list(
                qid=xml2::xml_find_first(x, "qid") %>% xml2::xml_text(),
                aid=xml2::xml_find_first(x, "aid") %>% xml2::xml_text(),
                acode=xml2::xml_find_first(x, "code") %>% xml2::xml_text(),
                aorder=xml2::xml_find_first(x,"sortorder") %>% xml2::xml_text()
            )
        })

    # all answer txts
    atxt <- xml2::xml_find_all(doc, ".//answer_l10ns/rows/row") %>%
        purrr::map_df(function(x) {
            list(
                id=xml2::xml_find_first(x, "id") %>% xml2::xml_text(),
                aid=xml2::xml_find_first(x, "aid") %>% xml2::xml_text(),
                atxt=xml2::xml_find_first(x, "answer") %>% xml2::xml_text(),
                lang=xml2::xml_find_first(x,"language") %>% xml2::xml_text()
            )
        })



    # construct answer (variable) labels data frame
    df_answer_labels <- aid %>%
        dplyr::left_join(atxt, by="aid")
    
    if (strip_html){
        df_answer_labels$atxt <- stringr::str_remove_all(df_answer_labels$atxt, "<.*?>")
    }
    
    avail_lang <- paste(unique(df_answer_labels$lang),sep="-", collapse = "-")
    
    message("Available languages: ", avail_lang)

    df_answer_labels

}

