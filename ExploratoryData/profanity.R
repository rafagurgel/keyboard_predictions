is.clean <- function(str){
    letter.variation <- function(x) {
        x%>%
            gsub("a","[a@4]",.)%>%
            gsub("e","[e]",.)%>%
            gsub("i","[i1]",.)%>%
            gsub("o","[o0]",.)%>%
            gsub("t","[t7]",.)
    }
    bw.part <- c(
        "bitch",
        "cock",
        "cunt",
        "damn",
        "fuck",
        "nigg",
        "pussy",
        "shit",
        "suck")
    
    bw.complete <- c(
        "anus",
        "ass",
        "assbang(er)?",
        "asshole",
        "bang",
        "blowjob",
        "butt",
        "cum",
        "cunt",
        "dick")
    
    
    bw.complete <- bw.complete%>%
        gsub("","+",.)%>%
        gsub("^\\+","",.)%>%
        gsub("\\+","+([-.]+)?",.)%>%
        paste0(collapse = '$)|(^')%>%
        paste0('(^', ., '$)')%>%
        letter.variation()
    
    bw.part<- bw.part%>%
        gsub("","+",.)%>%
        gsub("^\\+","",.)%>%
        gsub("\\+","+([-.]+)?",.)%>%
        paste0(collapse = ')|(')%>%
        paste0('(', ., ')')%>%
        letter.variation()
    
    sapply(str,(function(x,bw.complete,bw.part){x<- lapply(x,tolower); (!grepl(bw.complete,x))&(!grepl(bw.part,x))}),bw.complete,bw.part) %>%
        as_tibble()%>%
        apply(1,prod)%>%
        as.logical()
}