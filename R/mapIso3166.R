#' @title Map answer codes 
#'
#' 
#' @description Maps country answer codes between different translated version of the GEAM questionnaire. 
#'  Nationality or country selection choices in a survey are usually listed alphabetically. 
#'  For translated versions this can be a problem as the answer codes for countries do not match. 
#'  For example. "L013" is the answer code for Hungary in the English version while "L026" is the answer code 
#'  for "Wegry" (i.e. Hungary) in the Polish version. 
#'  
#'  When labeling the answers that use country codes, one has to bear in mind that answer codes need to be matched
#'  with the target language. For example, a questionnaire exists in English and Spanish. When labelling the 
#'  result data in Spanish (to produce a Spanish report), the English submissions need to be
#'  recodified as answer codes do not match. In case result data needs to be labelled in English, the Spanish submissions
#'  need to be recodified to make sure that answer codes refer to the same items (countries). 
#' 
#' 
#' @param isofrom String code of source language code (e.g. "pl", "de", "en") to be converted
#' @param isoto  String code of target language 
#' @param alabels Data frame of all answer labels extracted from LS survey archive file 
#' @param qid ID of associated LS question for answer labels. 
#' 
#' @return data frame with four columns containing the answer code in the source language, and the 
#'  corresponding answer code in the target language. 
#'  
#' @export map_iso3166
#' 
map_iso3166 <- function(isofrom, isoto, alabels, lqid){
    
    
    # get all country code labels of the source language, i.e. which need conversion
    # answers in this source language will be recoded to answer codes in target language
    src_labels <- alabels %>% 
        filter(qid == lqid & lang == isofrom) %>% 
        mutate(isocode = stringr::str_extract(atxt, pattern="\\([A-Z]{2}\\)")) %>% 
        select(acode.from=acode, 
               lang.from=lang, 
               isocode=isocode)
    
    # get all country code labels of the target language 
    target_labels <- alabels %>% 
        filter(qid == lqid & lang == isoto) %>% 
        mutate(isocode = stringr::str_extract(atxt, pattern="\\([A-Z]{2}\\)")) %>% 
        distinct(isocode, .keep_all = T) %>% 
        select(acode.to=acode, 
               lang.to=lang, 
               isocode=isocode)
    
    matched_labels <- src_labels %>% 
        left_join(target_labels, by="isocode")
    
    
    matched_labels
    
}