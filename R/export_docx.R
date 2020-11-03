#' @import officer
#' 
#' @title Export Labels to Word
#' 
#' @description Question and answer labels can be exported as word document (docx). Only contains basic formatting. 
#' 
#' @param file String specifying path and file name 
#' @param plang String language code. 
#' 
#' @return Nothing. Writes file to disc. 
#' 
#' 
export_docx <- function(file, plang){

    my_doc <- read_docx()
    
    qlabels <- qlabels %>% 
        filter(lang == plang)
    
    alabels <- alabels %>% 
        filter(lang == plang)
    
    
    for (i in c(1:nrow(qlabels))){
        
        add_question(i)
    }
    
    print(my_doc, target=file)     
}


add_question <- function(i){
    
    previ <- (i - 1) 
    
    
    if (previ>0){
        prevl <- qlabels[previ,]$qtxt
    } else {
        prevl <- "NULLabel"        
    }
    
    label <- qlabels[i,]$qtxt
    
    qid <- qlabels[i,]$qid
    
    # if we have a new label
    if (!is.null(label) & prevl != label){
        my_doc %>% 
            body_add_par(label, style="Normal") %>% 
            body_add_par(" ", style="Normal")
        
        # add response scale
        add_responses(qid)
        
    }   
    
    # add sub-questions
    add_subquestion(i)
}



add_subquestion <- function(i){
    
    sublabel <-  qlabels[i,]$subqtxt
    
    if (!is.na(sublabel)){
        my_doc %>% 
            body_add_par(sublabel, style="Normal")
    }
    
}

add_responses <- function(question_id){
    
    response <- alabels %>% 
        filter(qid == question_id) %>% 
        select(atxt)
    
    if (!is.null(response)){
        response <- paste(response, sep=" - ", collapse=" - ")
        
        my_doc %>% 
            body_add_par(response, style="Normal") %>% 
            body_add_par(" ", style="Normal")
        
    }
    
}






