library(tm)
library(SnowballC)
library(randomForest)
library(caret)
library(e1071)
library(lubridate)
library(ROCR)

Train = read.csv("competition/NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

Test = read.csv("competition/NYTimesBlogTest.csv", stringsAsFactors=FALSE)

CorpusHeadline = Corpus(VectorSource(c(Train$Headline, Test$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = make.names(paste0("head",colnames(HeadlineWords)))


## Adding the other fields

HeadlineWords$NewsDesk <- c(Train$NewsDesk,Test$NewsDesk)
HeadlineWords$SectionName <- c(Train$SectionName,Test$SectionName) 
HeadlineWords$SubsectionName <- c(Train$SubsectionName,Test$SubsectionName)
HeadlineWords$WordCount <- c(Train$WordCount,Test$WordCount)
HeadlineWords$PubDate <- c(Train$PubDate,Test$PubDate) 
HeadlineWords$NewsDesk <- as.factor(HeadlineWords$NewsDesk)
HeadlineWords$SectionName <- as.factor(HeadlineWords$SectionName)
HeadlineWords$SubsectionName <- as.factor(HeadlineWords$SubsectionName)
HeadlineWords$PubDate <- ymd_hms(HeadlineWords$PubDate)

#Relevel
HeadlineWords$NewsDesk <- relevel(HeadlineWords$NewsDesk)
HeadlineWords$SectionName <- relevel(HeadlineWords$SectionName)
HeadlineWords$SubsectionName <- relevel(HeadlineWords$SubsectionName)


HeadlineWordsTrain = head(HeadlineWords, nrow(Train))
HeadlineWordsTrain$Popular <- Train$Popular
HeadlineWordsTest = tail(HeadlineWords, nrow(Test))


CorpusSnipit = Corpus(VectorSource(c(Train$Snippet, Test$Snippet)))
CorpusSnipit = tm_map(CorpusSnipit, tolower)
CorpusSnipit = tm_map(CorpusSnipit, PlainTextDocument)
CorpusSnipit = tm_map(CorpusSnipit, removePunctuation)
CorpusSnipit = tm_map(CorpusSnipit, removeWords, stopwords("english"))
CorpusSnipit = tm_map(CorpusSnipit, stemDocument)
dtm = DocumentTermMatrix(CorpusSnipit)
sparse = removeSparseTerms(dtm, 0.99)
SnipitWords = as.data.frame(as.matrix(sparse))

colnames(SnipitWords) = make.names(paste0("snip",colnames(SnipitWords)))
SnipitWordsTrain = head(SnipitWords, nrow(Train))
SnipitWordsTest = tail(SnipitWords, nrow(Test))


CorpusAbstract = Corpus(VectorSource(c(Train$Abstract, Test$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtm = DocumentTermMatrix(CorpusAbstract)
sparse = removeSparseTerms(dtm, 0.99)
AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
colnames(AbstractWords) = make.names(paste0("abs",colnames(AbstractWords)))
AbstractWordsTrain = head(AbstractWords, nrow(Train))
AbstractWordsTest = tail(AbstractWords, nrow(Test))

# Combine everything
ctrain <- cbind(HeadlineWordsTrain, SnipitWordsTrain, AbstractWordsTrain)
ctest <- cbind(HeadlineWordsTest, SnipitWordsTest, AbstractWordsTest)
ctest$UniqueID <- Test$UniqueID


#ctrain$NewsDesk <- Train$NewsDesk
#ctrain$SectionName <- Train$SectionName
#ctrain$SubsectionName <- Train$SubsectionName
#ctrain$WordCount <- Train$WordCount
#ctrain$PubDate <- Train$PubDate
#ctrain$Popular <- Train$Popular
#ctrain$NewsDesk <- as.factor(ctrain$NewsDesk)
#ctrain$SectionName <- as.factor(ctrain$SectionName)
#ctrain$SubsectionName <- as.factor(ctrain$SubsectionName)
#ctrain$PubDate <- ymd_hms(ctrain$PubDate)

#ctest$NewsDesk <- Test$NewsDesk
#ctest$SectionName <- Test$SectionName
#ctest$SubsectionName <- Test$SubsectionName
#ctest$WordCount <- Test$WordCount
#ctest$PubDate <- Test$PubDate
#ctest$Popular <- Test$Popular
#ctest$NewsDesk <- as.factor(ctest$NewsDesk)
#ctest$SectionName <- as.factor(ctest$SectionName)
#ctest$SubsectionName <- as.factor(ctest$SubsectionName)
#ctest$PubDate <- ymd_hms(ctest$PubDate)

set.seed(999)
Forrest <- randomForest(Popular ~ ., data=ctrain, ntree = 500, proximity = TRUE, mtry = 32)

ptrain <- predict(Forrest)
table(ctrain$Popular, ptrain > .5)
ptest <- predict(Forrest, newdata=ctest)
MySubmission = data.frame(UniqueID = ctest$UniqueID, Probability1 = ptest)
write.csv(MySubmission, "Submission101.csv", row.names=FALSE)


#numFolds <- trainControl(method="cv", number=10)
#cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
#Forrest <- randomForest(Popular ~ ., data=ctrain)

#rf_model<-train(Popular~.,data=ctrain,method="rf",
#                                trControl=trainControl(method="cv",number=5),
#                                 prox=TRUE,allowParallel=TRUE)


#Forrest <- randomForest(Popular ~ ., data=ctrain, ntree = 500, proximity = TRUE, mtry = 49)
