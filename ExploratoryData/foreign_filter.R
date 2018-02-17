is.english <- function(str){
    str<- str%>%
        (function(xx){(xx%>%(function(x){x%>%lapply(cld2::detect_language)%>%unlist()}) == 'en')|
                      (xx%>%(function(x){x%>%lapply(cld3::detect_language)%>%unlist()}) == 'en')})%>%
        as.logical()
    str[is.na(str)] <- F
    str
}
is.ascii <- function(str) {
    #!grepl("[áàãäéèëíìïóòõöúùüçčȼñ]", tolower(str))
    grepl("[!-~]", tolower(str))
}

#ngram1.all_sources[!is.ascii(ngram1.all_sources$Word),]%>%View()
#text.data[!(text.data$Line%>%is.english),]%>%View()

#sapply(ngram2,is.ascii)
