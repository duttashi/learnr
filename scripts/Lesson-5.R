# Structuring Data: String manipulation in R
# clear all
rm(list=ls())
# Processing strings in R
library(stringr)

# strings in R are character vectors
stringA <- "Hello world"
stringB <- "Sun shines!"
stringA
stringB
is.character(stringA) # TRUE
as.character(200*5)
as.numeric("1000")
as.double("3.14")

# Using " and '
# either:
stringA <- "Hello 'World'"
stringA
# or
stringA <- 'Hello "World"'
stringA # prints: "Hello \"World\"" - what is this: \ ?
print(stringA)
# try:
writeLines(stringA)
print(stringA)
# Escaping in R: use \, the R escape character
stringA <- 'Hello \"World\"'
stringA
print(stringA)
writeLines(stringA)
# Escaping escaping
writeLines("\\") # nice

# Length of strings
length(stringA) # of course
stringC <- c("First", "Second");
length(stringC)
nchar(stringA) # base function
str_length(stringA) # stringr function

# String Concatenation in R
stringC <- c(stringA, stringB) # a character vector of length == 2
length(stringC)
stringC <- paste(stringA, stringB,
                 sep=", ") # length == 1, base function
writeLines(stringC)
# sep w. collapse (paste args)
stringC <- c(stringA, stringB)
stringC <- paste(stringC, collapse="__")
writeLines(stringC)
# paste0 is paste w. sep="", faster than paste(), base function
strA <- "One"
strB <- "Two"
strC <- "Three"
paste0(strA, strB, strC)
# the collapse argument is used in paste0 as well 
strD <- c(strA,strB,strC)
paste0(strD, collapse="-")
# stringr concatenation, also has sep and collapse as args
str_c(strA,strB,strC)
str_c(strA,strB,strC, sep="...")
str_c(strD,collapse="...")
# both paste {base} and str_c {stringr} are vectorized
paste("Prefix-", strD, sep="-")
str_c("Prefix-", strD, sep="-")

# Splitting strings in R
# with strsplit {base}
stringA <- "The quick brown fox jumps over the lazy dog";
splitA <- strsplit(stringA," ") # is.list(splitA) == T
splitA <- unlist(strsplit(stringA," "))
# "The quick brown" from "The quick brown fox jumps over the lazy dog"
splitA <- paste(unlist(strsplit(stringA," "))[1:3],collapse=" ")
# or
splitA <- paste(strsplit(stringA," ")[[1]][1:3],collapse=" ")
# advice: use
splitA <- strsplit(stringA," ",fixed=T) # fixed=T says: match the split argument exactly,
# otherwise, split is an regular expression; # default is: fixed = FALSE
# string split w. {stringr}
is.list(str_split(stringA, " "))
# this is interesting:
str_split(stringA," ", n=3)
# "The quick brown" from "The quick brown fox jumps over the lazy dog"
paste0(str_split(stringA," ", n=4)[[1]][1:3],collapse=" ")
# default: str_split(string, pattern, n = Inf), where pattern is regex
str_split(stringA, boundary("word"))
# very useful:
stringA1 <- "The quick brown fox     jumps over the lazy dog"
str_split(stringA1, boundary("word"))
stringA1 <- "Above all, don't lie to yourself. 
The man who lies to himself and listens to his own lie comes to a point that he cannot distinguish the truth within him, or around him, and so loses all respect for himself and for others. 
And having no respect he ceases to love."
str_split(stringA1, boundary("word"))
str_split(stringA1, boundary("word", skip_word_none = F)) # including punctuation and special
str_split(stringA1, boundary("line_break"))
writeLines(str_split(stringA1, boundary("line_break"))[[1]])

# play:
stringA <- c("1 2 3 4 5 6 7 8 9 10") # how about this one?
strA <- as.numeric(str_split(stringA, " ")[[1]])
w <- which(strA %% 2 == 0)
strA <- as.character(strA)
strA[w] <- 'evenNum';

# Subsetting strings
stringA <- c("Belgrade", "Zagreb", "Ljubljana") # {stringr}
str_sub(stringA, 1, 2)
# counting backwards
str_sub(stringA, -3, -1)
# {base}
substr(stringA, 1, 3)
# play:
substr(stringA,c(1,2,3),c(2,3,4))
# nope:
substr(stringA, -2, -1) # {base}

# Replacing characters in strings
stringB <- stringA # just a copy of stringA
str_sub(stringB,1,2) <- "00"
stringB
# {base}
stringB <- stringA # just a copy of stringA
substr(stringB,1,3) <- "WowWow" # check the result!
stringB
substr(stringB,1,4) <- "WoWWow" # check the result!
stringB
substr(stringB,1,6) <- "WowWow" # check the result!
stringB

# UPPER CASE to lower case and vice versa in R
stringA <- "ABRACADABRA"
# {base}
tolower(stringA)
stringA <- tolower(stringA)
toupper(stringA)
stringA <- toupper(stringA)
# {stringr}
str_to_lower(stringA)
stringB <- str_to_lower(stringA)
str_to_upper(stringA)
# capitalize first letter
str_to_title(stringB)

# Remove whitespace
stringA <- c("  Remove whitespace  ");
str_trim(stringA)
# remove leading whitespace
str_trim(stringA,side="left")
# remove trailing whitespace
str_trim(stringA,side="right")
# remove all whitespace?
stringA <- c("  Remove    whitespace  ") # how about this one?
# there are different ways to do it. Try:
gsub(" ", "", stringA, fixed=T) # (!(fixed==T)), the first (pattern) argument is regex
# in general:
stringA <- "The quick brown fox jumps over the lazy dog The quick brown"
gsub("The quick brown", "The slow red", stringA, fixed=T)

# Searching for something in a string
# Does a string encompass a substring?
grepl("The quick brown", stringA, fixed = T)
grepl("The fast red", stringA, fixed = T)
stringB <- "Uraaaaaaaa"
grep("The quick brown", c(stringA, stringB), fixed = T)
# where?
stringA <- "The quick brown fox jumps over the lazy dog The quick brown"
w <- gregexpr("The quick brown", stringA)
str(w)
b1 <- w[[1]][1] # first match starts at
b2 <- w[[1]][2] # second match starts at
# now, match.length is an attribute of w[[1]], not w itself:
e1 <- attr(w[[1]],"match.length",exact = T)[1]
e2 <- attr(w[[1]],"match.length",exact = T)[2]
# first match extraction:
str_sub(stringA,b1,b1+e1-1)
# second match extraction:
str_sub(stringA,b2,b2+e2-1)
# Ok, but easier and more convenient with {stringr}
str_detect(stringA,"The quick brown") # T or F
str_locate(stringA,"The quick brown") # first match
str_locate_all(stringA,"The quick brown") # all matches
# term frequency, as we know, is very important in text-mining:
term1 <- str_locate_all(stringA,"The quick brown")[[1]] # all matches for term1 ie. "The quick brown"
dim(term1)[1] # how many matches = how many rows in the str_locate_all output matrix...

# Sorting strings in R
stringA <- c("New York", "Paris", "London", "Moscow", "Tokyo")
str_sort(stringA, locale="en") # locale
str_sort(stringA, locale="haw") # locale = Hawaiian
str_sort(stringA)
letters
str_sort(letters, locale="en") # locale = en
str_sort(letters, locale="haw") # locale = Hawaiian
# backwards
str_sort(letters,decreasing = T)
# handy:
stringA <- c("New York", "Paris", NA, "Moscow", "Tokyo")
str_sort(stringA, na_last=T)
# [1] "Moscow"   "New York" "Paris"    "Tokyo"    NA   
str_sort(stringA, na_last=F)
# [1] NA         "Moscow"   "New York" "Paris"    "Tokyo" 
# {base}
sort(stringA)
sort(stringA, decreasing=T)

# Encodings
Encoding(stringA)
# [1] "unknown" "unknown" "unknown" "unknown" "unknown"
Encoding(stringA) <- "UTF-8"
Encoding(stringA)
# [1] "unknown" "unknown" "unknown" "unknown" "unknown"
# Encoding() returns "unknown" if a character string has a "native encoding" mark
# or if it is in ASCII
# try this!
library(stringi)
stri_enc_isutf8(stringA) # stringi
# [1] TRUE TRUE   NA TRUE TRUE
# list all suported encodings
iconvlist() # {base}
# get default encoding
stri_enc_get()
# set encoding # COMMENTED CODE with a good reason; DO NOT DO THIS
# stri_enc_set("windows-1253")
# force UTF-8
x <- "Arijadna"
Encoding(x)
stri_enc_isutf8(x) # {stringi}
x <- iconv(x,to='UTF-8',sub='byte'); # {byte}
stri_enc_isutf8(x) # {stringi}
Encoding(x)
stri_enc_detect(x) # {stringi}
x <- "Čaršija"
stri_enc_detect(x) # {stringi} # a heuristic + statistics;
# you need to provide at least a few hundreds bytes of character data:

dnevnik <- "On je suzan i plah kao jagnje. Treći stoji desno. On je rumen. O njemu ne mogu 
ništa  da  kažem.  U  tuđini  sam,  postelju  sam  pokrio  bačvanskim  ćilimima. 
Sigurno ih je kakva Bunjevka tkala prosto joj bilo.
On je veseo i bogat, uvek me razveseli u tuđini, kad ga pogledam nebo zarumeni. 
Ja joj ne rekoh ni reči. Ja je ne ljubim. Ja ljubim nebo, moja je ljubav blaga, u 
snu, i neprolazna. Nje nema u životu mome. Njene su oči tamne i tople kao urme.
Kosa joj je rumena kao trava u jesen. Ili se to samo čini, tako čini. Ta kosa me dirnu, 
te kose mi je žao, te  uvele  trave.  Tu  kosu  poljubih  prvo.  Njen  glas  je
detinjast,  uzalud  mi  ona donosi i čita Bergsona: njena je maramica namirisana, 
a i da nije, mirisala bi od čistote.  Njene  su  oči  mračne  i  rumene  kao  krv,  one 
su  moja  radost"
# Excerpt from: Journal of Carnojevic (Dnevnik o Čarnojeviću) (1921)
# Miloš Crnjanski, https://en.wikipedia.org/wiki/Milo%C5%A1_Crnjanski

stri_enc_detect(x) # {stringi} # a heuristic + statistics
stri_enc_mark(x) # how does stringi see the encoding

# Take home message on encodings
# 1. Most of the time, you simply need to know the source encoding
# 2. All of the time *** convert everything to UTF-8*** - as soon as possible
# 3. Most {base}, and all {stringr} and {stringi} functions that process strings in R
# will convert their output to UTF-8 automatically
# Working inside R only, running an English locale, will never cause you any trouble
# However, in Data Science you will probably need to do a lot of web-scraping for a living
#  - and that's where the fan starts.
# God bless iconv() - but don't get to excited, it does not avoid all problems