# A general summary :
#   1)  Read text from file as string.
#   2)  Firstly that string is splited into sentences afterwards it is splited into words to be converted.
#   4)  Once all translation is complete, all the converted words are merged into one string then it is saved into a output file.

# Limitations:
#   1)  punctuations and special characters are dealt based on only this football.txt dataset.
#   3)  mixed capitalisation (such as "ABset") is not properly handlaed. Only first letter capitalisation is dealt.
# From my opinion these limitations are minor, and do not have a considerable impact on the effectiveness of the
# translator.

library(stringr) 
library(readr)
library(methods)
library(utils)
library(rebus)

#main logic function for word conversion
WordConverter <- function(word) {
  convertedWord<- ''
  
  #first character of word
  first<-substr(word, start = 1, stop =1)
  #second character of word
  second<-substr(word, start = 2, stop =2)
  #third character of word
  third<-substr(word, start = 3, stop =3)
  
  # Create pattern of vowels
  vowel<-"[aeiou]"
  # Create pattern of consonants
  consonant<-"[bcdfghjklmnpqrstvwxz]"
  
  #checking special characters where length is one
  if ((CheckSpecialChr(word) == TRUE) && (nchar(word)==1)) { 
    convertedWord <- word
  
  #checking first characters whether it is vowel  
  } else if (grepl(vowel,first,ignore.case=TRUE) == TRUE) { 
    convertedWord <- paste(word,"yay",sep = "")
    
    #checking first characters whether it is consonant and length is one 
  } else if  ((grepl(consonant,first,ignore.case=TRUE) == TRUE) && (nchar(word)==1)) {
    convertedWord <- paste(word,"ay",sep = "")
    
    #checking second characters whether it is vowel 
   } else if  (grepl(vowel,second,ignore.case=TRUE) == TRUE) {
     w1<-substr(word, start = 2, stop =nchar(word))
     w2<-substr(word, start = 1, stop =1)
     convertedWord <- paste(w1,w2,"ay",sep = "")
  
  #checking second characters for "Y" condition 
  } else if  (grepl("^[y]",second,ignore.case=TRUE) == TRUE) {
    w1<-substr(word, start = 2, stop =nchar(word))
    w2<-substr(word, start = 1, stop =1)
    convertedWord <- paste(w1,w2,"ay",sep = "")
  
  #checking third characters for "Y" condition  
  } else if  (grepl("^[y]",third,ignore.case=TRUE) == TRUE) {
    w1<-substr(word, start = 3, stop =nchar(word))
    w2<-substr(word, start = 1, stop =2)
    convertedWord <- paste(w1,w2,"ay",sep = "")
  
  #checking for consonant cluster  
  } else if  (grepl(consonant,second,ignore.case=TRUE) == TRUE) {
    positon<-gregexpr(pattern ="[aeiouAEIOU]",word)
    w1<-substr(word, start = positon[[1]][1], stop =nchar(word))
    w2<-substr(word, start = 1, stop =positon[[1]][1]-1)
    convertedWord <- paste(w1,w2,"ay",sep = "")
    
  } else {
    convertedWord<-word
  }
  
  #checking for capitalization, Uppercase and LowerCase letter
  convertedWord <- CaseConverter(word, convertedWord)
  
  return(convertedWord)
}
#checking special characters if found function will return true
CheckSpecialChr<- function(specialChr){
  
  result <- FALSE
  if(specialChr == '"'){
    result = TRUE
  }else if (specialChr == '-'){
    result = TRUE
  }else if (specialChr == ","){
    result = TRUE
  }else if (specialChr == "'"){
  result = TRUE
  }else if (specialChr == "!"){
  result = TRUE
  }else if (specialChr == ":"){
  result = TRUE}
  else if (specialChr == " "){
    print("space")
    result = TRUE}
  
  return(result)
}

#Function for Case convertion whether the word has capitalization, Uppercase and LowerCase letter
CaseConverter<- function(previousWord, latesWord){
  
  caseConverterWord<-''
  #uppercase conversion
  if(previousWord == toupper(previousWord)){
    caseConverterWord <- toupper(latesWord)
    
  #lowercase conversion
  }else if (previousWord == tolower(previousWord)){
    caseConverterWord <- tolower(latesWord)
    
  #capitalization conversion
  }else if((!grepl("\\b(?=[a-z])", previousWord, perl = TRUE)) == TRUE){
      tempWord<-tolower(latesWord)
       caseConverterWord <- paste(toupper(substr(tempWord, 1, 1)), substr(tempWord, 2, 
                                                                           nchar(tempWord)), sep="")
  }else{
    caseConverterWord<-latesWord
   }
  
  return(caseConverterWord)
}
#function for handling QuotationMark
QuotationMarkOparation <- function(word) {
  convertedWord<- ''
  temp<-strsplit(word,split='"', fixed=TRUE)
  convertedWord<-str_replace_all(word, '\\"', "")
  
  #checking if ExclamationMark before  QuotationMark {E.X-  (!") } 
  if(grepl("\\!",convertedWord,ignore.case=TRUE) == TRUE){
    convertedWord<-ExclamationMarkOparation(convertedWord)
  
  #checking if comma before  QuotationMark {E.X-  (,") }   
  }else if(grepl("\\,",convertedWord,ignore.case=TRUE) == TRUE){
    convertedWord<-CommaOperation(convertedWord)
    
  }else
    convertedWord<-WordConverter(convertedWord) 
  
  #setting QuotationMark before first charecter of word
  if(temp[[1]][1] == ''){
    convertedWord <- paste('"',convertedWord,sep = "")
  
  #setting QuotationMark after last charecter of word  
  }else{
    convertedWord <- paste(convertedWord,'"',sep = "")
    
  }
  return(convertedWord)
}

#function for handling Fullstop
FullstopOperation <- function(word) {
  convertedWord<- ''
  convertedWord<-str_replace_all(word, "\\.", "")
  #set fullstop as a last character of word after conversion
  convertedWord <- paste(WordConverter(convertedWord),".",sep = "")
  return(convertedWord)
}

#function for handling ExclamationMark
ExclamationMarkOparation <- function(word) {
  convertedWord<- ''
  convertedWord<-str_replace_all(word, "\\!", "")
  #set ExclamationMark as a last character of word after conversion
  convertedWord <- paste(WordConverter(convertedWord),"!",sep = "")
  return(convertedWord)
}

#function for handling Comma
CommaOperation <- function(word) {
  convertedWord<- ''
  
  #if comma is in a numeric a word than the word does not need conversion
  if(grepl("[0-9]",word,ignore.case=TRUE) == TRUE){
    print("Number found")
    convertedWord <-word
  }else
  {
    #set Comma as a last character of word after conversion
    convertedWord<-str_replace_all(word, "\\,", "")
    convertedWord <- paste(WordConverter(convertedWord),",",sep = "")}
  
  return(convertedWord)
}
#function for handling Dash
DashOperation <- function(word) {
  convertedWord<- ''
  #spliting word based on dash
  tempw<-strsplit(word,split="-", fixed=TRUE)
  
  #converting splited word and then merged together with dash in the middle.
  tempw2 <-lapply(tempw[[1]],WordConverter)
  convertedWord<-paste(tempw2, collapse = "-")
  return(convertedWord)
}

InvitedCommaOperation <- function(word) {
  convertedWord<- ''
  #spliting word based on InvitedComma
  tempw<-strsplit(word,split="'", fixed=TRUE)
  
  #converting splited word and then merged together with InvitedComma in the middle.
  tempw2 <-lapply(tempw[[1]],WordConverter)
  convertedWord<-paste(tempw2, collapse = "'")
  #print(convertedWord)
  return(convertedWord)
}

#main algorthm for piglatin translation
PigLatinOperation <- function(word) {
  convertedWord<- ''
  
  #check if the word has dash
  if((grepl("\\-",word,ignore.case=TRUE) == TRUE) && (nchar(word)>1)){
    convertedWord<-DashOperation(word)
    
    #check if the word has QuotationMark
    }else if((grepl('\\"',word,ignore.case=TRUE) == TRUE) && (nchar(word)>1)){
    convertedWord<-QuotationMarkOparation(word)
    
    #check if the word has InvitedComma
    }else if((grepl("\\'",word,ignore.case=TRUE) == TRUE) && (nchar(word)>1)){
    convertedWord<-InvitedCommaOperation(word)
    
    #check if the word has ExclamationMark
    }else if((grepl("\\!",word,ignore.case=TRUE) == TRUE) && (nchar(word)>1)){
    convertedWord<-ExclamationMarkOparation(word)
    
    #check if the word has comma
    }else if((grepl("\\,",word,ignore.case=TRUE) == TRUE) && (nchar(word)>1)){
      convertedWord<-CommaOperation(word)
     
      #check if the word has fullstop 
    }else if((grepl("\\.",word,ignore.case=TRUE) == TRUE) && (nchar(word)>1)){
      convertedWord<-FullstopOperation(word)
      
    }else
    convertedWord<-WordConverter(word) 
  
  return(convertedWord)
  
}

#logic for per sentence 
SentenceOperation <- function(splitedSentence) {
  converted<- ''
  
  #slipting sentences based on space
  z<-strsplit(splitedSentence,split=' ', fixed=TRUE)
  
  #apply PigLatinOperation to all elaments of vecor using lapply
  converted <-lapply(z[[1]],PigLatinOperation)
  
  #merged all words into a converted sentence
  converted1<-paste(converted, collapse = ' ')
   print(converted1)
  return(converted1)
  
}

#reding file from the same directory of this R script
currentDirectory <-dirname(rstudioapi::getSourceEditorContext()$path)
filepathDir<- paste(currentDirectory,"/football.txt",sep = "")
Filedata <- read_file(filepathDir)

#spliting read file based intu fullstop which creates vector of sentences
FiledataSplited<-strsplit(Filedata,split='.', fixed=TRUE)
lengthofFile<-length(FiledataSplited[[1]])

#apply sentence operation into all sentences
convertedtemp <-lapply(FiledataSplited[[1]][1:lengthofFile-1],SentenceOperation)
#merged all sentences into a converted string
convertedtemp1<-paste(convertedtemp, collapse = '.')
#adding fulstop at the last of the conversion string
convertedtemp1<-paste(convertedtemp1,".",sep = "")
cat(convertedtemp1)

#saving the converted string into a file into the same directory of this R script
fileSaveDir<- paste(currentDirectory,"/football_PigLatin.txt",sep = "")
fileConn<-file(fileSaveDir)
writeLines(convertedtemp1, fileConn)
close(fileConn)






