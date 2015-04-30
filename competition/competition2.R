library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(Test$Headline))
corpus <- tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

frequencies <- DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.95)
headline = as.data.frame(as.matrix(sparse))
colnames(headline) = paste0("head_",make.names(colnames(headline)))

corpus <- Corpus(VectorSource(Test$Snippet))
corpus <- tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

frequencies <- DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.95)
snippet = as.data.frame(as.matrix(sparse))
colnames(snippet) = paste0("snip_",make.names(colnames(snippet)))


corpus <- Corpus(VectorSource(Test$Abstract))
corpus <- tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

frequencies <- DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.95)
abstract = as.data.frame(as.matrix(sparse))
colnames(abstract) = paste0("ab_",make.names(colnames(abstract)))

library(lubridate)
ctest <- cbind(headline, snippet, abstract)

ctest$NewsDesk <- Test$NewsDesk
ctest$SectionName <- Test$SectionName
ctest$SubsectionName <- Test$SubsectionName
ctest$WordCount <- Test$WordCount
ctest$PubDate <- Test$PubDate
ctest$Popular <- Test$Popular

ctest$NewsDesk <- as.factor(ctest$NewsDesk)
ctest$SectionName <- as.factor(ctest$SectionName)
ctest$SubsectionName <- as.factor(ctest$SubsectionName)

ctest$PubDate <- ymd_hms(ctest$PubDate)


library(randomForest)

library(caret)
library(e1071)

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
Forrest <- randomForest(Popular ~ ., data=ctest)

rf_model<-train(Popular~.,data=ctrain,method="rf",
                +                 trControl=trainControl(method="cv",number=5),
                +                 prox=TRUE,allowParallel=TRUE)


