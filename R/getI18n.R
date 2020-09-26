#' 
#' @title Get translation of label 
#'
#' @description Hack to retrieve the translated "other" label. TODO: should be done via base::gettext
#'
#' @param othcode String. The message key. 
#' @param plang String. Language code
#' 
#' @return Translated label if code and language exists. English if language not available, or code if 
#'  language does not exist. 
#'
get_i18n <- function(othcode, plang){
    
    # hack for i18n. Redo/fix with gettext
    i18n <- data.frame(msgid=c("-oth-", "checked", "not-checked"), 
                       en=c("Other", "checked", "not-checked"), 
                       pl=c("Inne", "wybrany", "nie zaznaczone"), 
                       es=c("Otro", "selecionado", "no selectionado"), 
                       de=c("Sonstiges", "Ausgewählt", "Nicht ausgewählt"), 
                       lt=c("Kita", "pasirinktas", "nepasirinkta"), 
                       fr=c("Autre", "sélectionné", "non sélectionné"), 
                       pt=c("Outro", "selecionado", "não selecionado"), 
                       stringsAsFactors = F)
    
    # code doesn't exist, return code
    othlabel <- othcode
    
    
    # translation and code exists
    if (plang %in% colnames(i18n)){
        
        othlabel <- i18n[which(i18n$msgid==othcode), plang]
        
    # translation does not exist, return english        
    } else {
        othlabel <- i18n[which(i18n$msgid==othcode), "en"]
    }
    
    othlabel
    
}
