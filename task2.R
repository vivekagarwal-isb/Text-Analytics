rm(list=ls())
library("RTextTools")
library("tm")
setwd("")
getwd()
search_terms = c('GST','HIB','Data science')

#reading the text of all three topics
file.cr = read.csv(paste0("C:\\Users\\vivek\\Downloads\\",
                          "GST google search.csv"))

file.mi = read.csv(paste0("C:\\Users\\vivek\\Downloads\\",
                          "H1B google search.csv"))

file.lin = read.csv(paste0("C:\\Users\\vivek\\Downloads\\",
                           "Data science google search.csv"))


#sanity checks
file.cr = file.cr[!is.na(file.cr$text)|file.cr$text != '',]

file.mi = file.mi[!is.na(file.mi$text)|file.mi$text != '',]

file.lin = file.lin[!is.na(file.lin$text)|file.lin$text != '',]

file.cr$topic = 1
file.mi$topic = 2
file.lin$topic = 3

#appended corpus
combined =  rbind(file.cr,file.mi, file.lin)
dim(combined)
names(combined)
head(combined,2) 

#------------------------------------------------------#
#------------------------------------------------------#
#setting a seed and creating a corpus having 70 % training data and 30% test data
set.seed(453056)                          # To fix the sample 
samp_id = sample(1:nrow(combined),
                 round(nrow(combined)*.70),     # 70% records will be used for training
                 replace = F)

train.data = combined[samp_id,]                      # 70% of training data set
test.data = combined[-samp_id,]                      # remaining 30% of training data set

dim(train.data) ; dim(test.data)

train.data$text = tolower(train.data$text)  # Convert to lower case

train.data$text = tolower(train.data$text)  # Convert to lower case 

data = rbind(train.data,test.data)

#------------------------------------------------------#
#------------------------------------------------------#

text = data$text                      
text = removePunctuation(text)              # remove punctuation marks
text = removeNumbers(text)                  # remove numbers
text = stripWhitespace(text)                # remove blank space
cor = Corpus(VectorSource(text))            # Create text corpus
dtm = DocumentTermMatrix(cor, control = list(weighting =             # Craete DTM
                                               function(x)
                                                 weightTfIdf(x, normalize = F)))
# weightTf(x)))
training_codes = data$topic       # Coded labels
dim(dtm)

#------------------------------------------------------#
#------------------------------------------------------#

container <- create_container(dtm,t(training_codes),trainSize=1:nrow(train.data), testSize=(nrow(train.data)+1):nrow(data),virgin=FALSE)

models <- train_models(container, algorithms=c("MAXENT")) #"MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"
results <- classify_models(container, models)

head(results)


out = data.frame(model_topic = results$MAXENTROPY_LABEL,
                 model_prob = results$MAXENTROPY_PROB,
                 actual_text = data$text[((nrow(train.data)+1):nrow(data))],
                 actual_topic = data$topic[((nrow(train.data)+1):nrow(data))]
)

#------------------------------------------------------#
#------------------------------------------------------#

(z = as.matrix(table(out$model_topic,out$actual_topic)))


(pct = round(pct = (sum(diag(z))/sum(z))*100))
