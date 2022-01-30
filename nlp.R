library(stringr)
library(dplyr)
library(sqldf)
library(spacyr)
library(tidytext)
library(textdata)
library(utf8)
library(tm)
library(qdap)
library(tidyr)
library(ggplot2)

##### read the txt
friendsLines <- readLines("Friends.txt", encoding = "UTF-8")
length(friendsLines)

##### check encodings
friendsLines[!utf8_valid(friendsLines)]

friendsLines_NFC <- utf8_normalize(friendsLines)
sum(friendsLines_NFC != friendsLines)

##### data process
#corpus <- Corpus(VectorSource(friendsLines))
#corpus <- tm_map(corpus, content_transformer(function(x){replace_contraction(x)}))

df <- data.frame(script= friendsLines)

characters <- c("Chandler", "Joey", "Monica", "Phoebe", "Rachel", "Ross" )
#df <- df[-1]
colnames(df) <- "script"

df$char <- with(df, case_when(startsWith (df$script, "Monica:")~"Monica",
                startsWith (df$script, "Phoebe:")~"Phoebe",
                startsWith (df$script, "Rachel:")~"Rachel",
                startsWith (df$script, "Ross:")~"Ross",
                startsWith (df$script, "Chandler:")~"Chandler",
                startsWith (df$script, "Joey:")~"Joey"))
df <- na.omit(df)


for (i in characters){
  df$script <- gsub(paste0(i, ": "), "", df$script)
  
}

df_grouped<- aggregate(script ~ char, data = df, c) 


##### tokenization
for (j in characters){
  
  phrases <- paste0("phrases", j)
  assign(phrases, spacy_tokenize(unlist(unlist(df_grouped$script[df_grouped$char == j])), what = "sentence")) 
}



#spacy_install()

spacy_initialize()

v_phrasesChandler <- unlist(phrasesChandler) 
v_phrasesJoey <- unlist(phrasesJoey) 
v_phrasesMonica <- unlist(phrasesMonica) 
v_phrasesPhoebe <- unlist(phrasesPhoebe) 
v_phrasesRachel <- unlist(phrasesRachel) 
v_phrasesRoss <- unlist(phrasesRoss)


numphrases <- length(v_phrasesChandler) 
sum(v_phrasesChandler=="")
v_phrasesChandler <- v_phrasesChandler[-which(v_phrasesChandler=="")]

hist(nchar(v_phrasesRoss),
     main = "Histogram of sentence size - Ross",
     xlab = "Sentece size (number of characters)",
     ylab = "Ocurrences"
)

for (j in characters){
  tokens <- paste0("tokens", j)
  assign(tokens, spacy_tokenize(unlist(unlist(df_grouped$script[df_grouped$char == j])), remove_punct = TRUE)) 
}


v_tokensChandler <- unlist(tokensChandler)
v_tokensJoey <- unlist(tokensJoey)
v_tokensMonica <- unlist(tokensMonica)
v_tokensPhoebe <- unlist(tokensPhoebe)
v_tokensRachel <- unlist(tokensRachel)
v_tokensRoss <- unlist(tokensRoss)


length(v_tokensMonica) 

length(unique(v_tokensMonica)) 

head(sort(table(v_tokensMonica), decreasing = TRUE), n = 100)

plot(head(sort(table(v_tokensMonica), decreasing = TRUE), n = 10),
     xlab = "Token",
     ylab = "Ocurrences"
)


##### SENTIMENT ###########################
nrc <- get_sentiments("nrc")

df_tokensMonica <- as.data.frame(v_tokensMonica) 
colnames(df_tokensMonica) <- "word"


sentimentMonica <-  df_tokensMonica %>%
  right_join(nrc) %>%
  count(word, sort = TRUE)

nrc_grouped <- aggregate(sentiment ~ word, data = nrc, c) 


sentimentMonica <-  sentimentMonica %>%
  right_join(nrc_grouped) 


############# Sentence Analysis ###########

sentenceAnalysis <- lapply(v_phrasesMonica[1:100],
              spacy_parse, #This is the function to apply to every element in v_phrases
              dependency = TRUE, nounphrase = TRUE #These are the arguments of the function
)
df_results <- sentenceAnalysis[[1]] #A data frame with the first results

for (i in 2:length(sentenceAnalysis)){ #Attention! The loop starts from 2
  df_results <- rbind(df_results, sentenceAnalysis[[i]])
}


########### BPE  Model ##########################

library(tokenizers.bpe)

text_Chandler <- paste(unlist(df_grouped$script[df_grouped$char ==  "Chandler"]), collapse="\n")
text_Joey <- paste(unlist(df_grouped$script[df_grouped$char ==  "Joey"]), collapse="\n")
text_Monica <- paste(unlist(df_grouped$script[df_grouped$char ==  "Monica"]), collapse="\n")
text_Phoebe <- paste(unlist(df_grouped$script[df_grouped$char ==  "Phoebe"]), collapse="\n")
text_Rachel <- paste(unlist(df_grouped$script[df_grouped$char ==  "Rachel"]), collapse="\n")
text_Ross <- paste(unlist(df_grouped$script[df_grouped$char ==  "Ross"]), collapse="\n")

model <- bpe(unlist(df_grouped$script[df_grouped$char ==  "Monica"]))

subtoks2 <- bpe_encode(model, x = text_Joey, type = "subwords")
head(unlist(subtoks2), n=20)
v <- unlist(subtoks2)

#############Distance between the texts########################

library(quanteda)

texts <- c(text_Chandler, text_Joey, text_Monica, text_Phoebe, text_Rachel, text_Ross)
names(texts) <- paste("char", 1:length(texts)) #assigns a name to each string
corpus_friends <- corpus(texts)
#docvars(corpus_friends, field="Chapter") <- 1:length(texts) #docvar with chapter number
corpus_friends


dfm_friends <- dfm(tokens(corpus_friends),
                 #Default values:
                 # tolower = TRUE #Convers to lowercase
                 # remove_padding = FALSE #Does padding (fills with blanks)
)
#Does a dendrogram
distMatrix <-dist(as.matrix(dfm_friends),
                  method="euclidean")
groups <-hclust(distMatrix , method="ward.D")

plot(groups,
     cex =1, #Size of labels
     hang= -1, #Same hight labels
     xlab = "", #Text of axis x
     ylab = "", #Text of axis y
     main = "" #Text of drawing
)
rect.hclust(groups, k=2)

dfm_friends_1 <- dfm(tokens(corpus_friends,
                          remove_punct = TRUE
                          #Default values:
                          # remove_punct = FALSE,
                          # remove_symbols = FALSE,
                          # remove_numbers = FALSE,
                          # remove_url = FALSE,
                          # remove_separators = TRUE,
                          # split_hyphens = FALSE
),
#Default values:
# tolower = TRUE #Convert to lowercase
# remove_padding = FALSE #Does padding (fill up blanks)
)
#Without stop words
dfm_friends_2 <- dfm_remove(dfm_friends_1, stopwords("en"))
topfeatures(dfm_friends_2)
