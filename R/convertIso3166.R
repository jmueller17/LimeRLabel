#' @title Convert answer codes
#'
#' @description Convert actual answer codes in a result data set from one language to the other. Requires a 
#'  mapping table produced by \code{\link{map_iso3166}}. If an answer code does not match, the original code will 
#'  be returned. 
#' 
#' @param from.acode Specifies the answer code to be converted. Usually a vector of variable (column) which 
#'  stores answer codes regarding nationality or country. 
#' @param from.alang Specifies the specific language of the labels to be converted. This is synonymous to the 
#'  language of the submission (startlanguage)
#' @param iso3166map Data frame which maps answer codes from one language to the other. @seealso \code{\link{map_iso3166}}. 
#' 
#' @return Vector of answer codes in the target language. 
#'
#' @export convert_iso3166
#' 
convert_iso3166 <- function(from.acode, from.alang, iso3166map){
    
    newLcode <- ""
    
    newLcode <- iso3166map %>% 
        filter(acode.from == from.acode & lang.from == from.alang) %>% 
        pull(acode.to)
    
    if (length(newLcode) == 0){
        return(from.acode)
    } else {
        return(newLcode[1])
    }
    
}