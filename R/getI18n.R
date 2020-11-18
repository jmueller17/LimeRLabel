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
get_i18nx <- function(othcode, plangcode){
    
    # hack for i18n. Redo/fix with gettext
    i18n <- data.frame(msgid=c("-oth-", "checked", "not-checked", "yes", "no"), 
                       en=c("Other", "checked", "not-checked", "yes", "no"), 
                       pl=c("Inne", "wybrany", "nie zaznaczone", "tak", "nie"), 
                       es=c("Otro", "selecionado", "no selectionado", "sí", "no"), 
                       de=c("Sonstiges", "Ausgewählt", "Nicht ausgewählt", "ja", "nein"), 
                       lt=c("Kita", "pasirinktas", "nepasirinkta", "taip", "ne"), 
                       fr=c("Autre", "sélectionné", "non sélectionné", "oui", "non"), 
                       pt=c("Outro", "selecionado", "não selecionado", "sim", "não"), 
                       stringsAsFactors = F)
    
    # code doesn't exist, return code
    othlabel <- othcode
    

    
    # translation and code exists
    if (plangcode %in% colnames(i18n)){
        
        othlabel <- i18n[which(i18n$msgid==othcode), plangcode]
        
    # translation does not exist, return english        
    } else {
        othlabel <- i18n[which(i18n$msgid==othcode), "en"]
    }
    
    othlabel
    
}
