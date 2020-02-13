setwd("C:/Users/Ronakjhanwar/Desktop/excelr/text mining")
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/OnePlus-Frosted-Display-Storage-3800mAH/product-reviews/B07DJCVTDN/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"oneplus.txt",row.names = F)


############# IMDB reviews Extraction ################
 a<-10
 first_man<-NULL
 url1<-"https://www.imdb.com/title/tt1213641/reviews?ref_=tt_ql_3"
 for(i in 0:22){
   url<-read_html(as.character(paste(url1,i*a,sep="")))
   w<-url %>%
    html_nodes(".show-more__control") %>%
     html_text() 
  first_man<-c(first_man,w)
 }
write.table(first_man,file="first_man.txt")
