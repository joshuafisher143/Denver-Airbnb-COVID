library(dplyr)
library(tidyverse)
library(fastDummies)
library(caret)
library(e1071)
library(car)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ROSE)
library(pROC)
library(ISLR)
library(randomForest)
library(psych)
library(quanteda)
library(stopwords)
library(quanteda.textmodels)

listings_with_exits <- read.csv('listings_with_exit_column.csv')

str(listings_with_exits)
summary(listings_with_exits)

# columns to keep
#host_response_time, host_response_rate, host_is_superhost
#host_listings_count, host_has_profile_pic, host_identity_verified
#neighborhood_cleansed, zipcode, property_type, room_type, accomodates, bedrooms, beds
#square_feet, price, guests_included, minimum_nights, maximum_nights
#availability_30/60/90/365, number_of_reviews, review_scores_###, instant_bookable
#cancellation_policy, reviews_per_month, exited


listings_subset <- listings_with_exits[, which(names(listings_with_exits) %in% c('host_response_time', 'host_response_rate', 'host_is_superhost',
                                                                                 'host_listings_count', 'host_has_profile_pic', 'host_identity_verified',
                                                                                 'neighborhood_cleansed', 'zipcode', 'property_type', 'room_type', 'accommodates',
                                                                                 'bedrooms', 'beds', 'square_feet', 'price', 'guests_included', 'minimum_nights',
                                                                                 'maximum_nights', 'availability_30','availability_60','availability_90',
                                                                                 'availability_365', 'number_of_reviews', 'review_scores_rating', 'review_scores_accuracy',
                                                                                 'review_scores_cleanliness', 'review_scores_checkin', 'review_scores_communication',
                                                                                 'review_scores_location', 'review_scores_value', 'instant_bookable',
                                                                                 'cancellation_policy', 'reviews_per_month', 'exited'))]

#host_response_time and zipcode have missing empty strings. replace with 'unknown'
listings_subset$host_response_time[listings_subset$host_response_time == ''] <- 'Unknown'
listings_subset$zipcode[listings_subset$zipcode == ''] <- '00000'
listings_subset$zipcode[listings_subset$zipcode == 'CO 80219'] <- '80219'


listings_subset$exited <- factor(listings_subset$exited)
listings_subset$host_is_superhost <- factor(listings_subset$host_is_superhost)
listings_subset$host_has_profile_pic <- factor(listings_subset$host_has_profile_pic)
listings_subset$host_identity_verified <- factor(listings_subset$host_identity_verified)
listings_subset$instant_bookable <- factor(listings_subset$instant_bookable)
listings_subset$host_response_time <- factor(listings_subset$host_response_time)
listings_subset$room_type <- factor(listings_subset$room_type)
listings_subset$cancellation_policy <- factor(listings_subset$cancellation_policy)
listings_subset$zipcode <- factor(listings_subset$zipcode)

#check for multicollinearity
vif(glm(formula=exited ~ . , family=binomial(link='logit'), data=listings_subset[,2:34]))

#remove property type of availability 60 which have highest VIF values
listings_vif <- listings_subset[, -which(names(listings_subset) %in% c('property_type', 'availability_60', 'zipcode'))]
vif(glm(formula=exited ~ . , family=binomial(link='logit'), data=listings_vif[,2:31]))

listings_num <- listings_vif[,c(3,5,9:26, 30)]

corr_matrix <- cor(listings_num)
corrplot(corr_matrix, type='upper', order='hclust', tl.col='black', tl.srt=45)


multi.hist(listings_vif[,sapply(listings_vif, is.numeric)])


#baseline logistic regression model

set.seed(101)
trainIndex <- createDataPartition(listings_vif$exited, p=0.7, list=FALSE, times=1)

df.train <- listings_vif[trainIndex,]
df.valid <- listings_vif[-trainIndex,]

baseline.model <- train(exited~.,
                        data=df.train,
                        method='glm',
                        family='binomial',
                        na.action=na.pass)

summary(baseline.model)

LR_prediction <- predict(baseline.model, newdata = df.valid)
confusionMatrix(LR_prediction, df.valid$exited)

LR_prob <- predict(baseline.model, newdata = df.valid, type='prob')
LR.ROC <- roc(predictor=LR_prob$'1',
                      response = df.valid$exited,
                      levels = levels(df.valid$exited))
plot(LR.ROC)
LR.ROC$auc

#baseline decision tree
tree.model <- train(exited~.,
                    data=df.train,
                    method='rpart',
                    na.action=na.pass)
tree.model

DT_prediction <- predict(tree.model, newdata=df.valid, na.action=na.pass)
confusionMatrix(DT_prediction, df.valid$exited)

#These models are predicting all class '0' indicating there is an issue with the unbalanced target variable
#need to do some sampling methods to resolve this issue
#One way to do this is using ROSE
ROSE_balanced_train <- ovun.sample(exited~., data=df.train, method='over')$data
summary(ROSE_balanced_train$exited)


#Try logistic regression again
balanced_LR <- train(exited~.,
                        data=ROSE_balanced_train,
                        method='glm',
                        family='binomial',
                        na.action=na.pass)
summary(balanced_LR)
balanced_LR_pred <- predict(balanced_LR, newdata = df.valid)
confusionMatrix(balanced_LR_pred, df.valid$exited, positive='1')

#from this confusion matrix we can see that the model was much better able to predict the postive class. 
#it was able to predict 139/195 positive cases. However, this came a cost of increase type 1 and type 2 errors

balanced_LR_prob <- predict(balanced_LR, newdata = df.valid, type='prob')
regression.ROC <- roc(predictor=balanced_LR_prob$'1',
                      response = df.valid$exited,
                      levels = levels(df.valid$exited))
plot(regression.ROC)
regression.ROC$auc
#even though the AUC was a tad lower, we know it is much more trustworthy than the baseline models. 
#An AUC between 0.7-0.8 is considered acceptable

#################################################################
#retrain using just the significant variables
significant_df <- listings_vif[, which(names(listings_vif) %in% c('host_response_rate', 'host_is_superhost', 'host_listings_count', 'room_type',
                                                                  'beds', 'price', 'minimum_nights', 'availability_90', 'availability_365',
                                                                  'review_scores_rating', 'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin', 'exited'))]
set.seed(101)
trainIndex <- createDataPartition(significant_df$exited, p=0.7, list=FALSE, times=1)

significant_df.train <- significant_df[trainIndex,]
significant_df.valid <- significant_df[-trainIndex,]

significant_ROSE_balanced_train <- ovun.sample(exited~., data=significant_df.train, method='over')$data

sig_balanced_LR <- train(exited~.,
                     data=significant_ROSE_balanced_train,
                     method='glm',
                     family='binomial',
                     na.action=na.pass)
summary(sig_balanced_LR)
sig_balanced_LR_pred <- predict(sig_balanced_LR, newdata = significant_df.valid)
confusionMatrix(sig_balanced_LR_pred, significant_df.valid$exited, positive='1')

sig_balanced_LR_prob <- predict(sig_balanced_LR, newdata = significant_df.valid, type='prob')
regression.ROC <- roc(predictor=sig_balanced_LR_prob$'1',
                      response = significant_df.valid$exited,
                      levels = levels(significant_df.valid$exited))
plot(regression.ROC)
regression.ROC$auc
#####################################################################################################################

#retrain the decision tree using balanced data
balanced_tree.model <- train(exited~.,
                    data=ROSE_balanced_train,
                    method='rpart',
                    na.action=na.pass)
balanced_tree.model

balanced_DT_prediction <- predict(balanced_tree.model, newdata=df.valid, na.action=na.pass)
confusionMatrix(balanced_DT_prediction, df.valid$exited)

#############################################################################################################


########## SVM ##########
trControl <- trainControl(method='cv',
                          number=10,
                          search='grid')
svm_linear <- train(exited~.,
                    data=ROSE_balanced_train,
                    method='svmLinear',
                    trControl=trControl,
                    preProcess=c('center','scale'))
print(svm_linear)
svm_pred <- predict(svm_linear,df.valid)
confusionMatrix(svm_pred,df.valid$exited)

grid_radial <- expand.grid(sigma = c(0.0,0.5,0.75,1.0,1.3,1.5),
                           C = c(0,0.05, 0.25, 0.5, 0.75, 1, 1.1, 1.5))
svm_radial_tune <- train(exited~.,
                         data=ROSE_balanced_train,
                         method='svmRadial',
                         trControl=trControl,
                         preProcess=c('center','scale'),
                         tuneGrid=grid_radial)
print(svm_radial_tune)

radial_tune_pred <- predict(svm_radial_tune,df.valid)
confusionMatrix(radial_tune_pred,df.valid$exited)


################# TEXT MINING ON DESCRIPTION COLUMN ####################
listings_svd <- cbind(listings_with_exits[,6], listings_vif)
names(listings_svd)[1] = 'description'

corp = corpus(listings_svd$description)

Dfm <- dfm(corp, 
           remove = stopwords("english"),
           remove_punct=TRUE,
           remove_numbers=TRUE,
           stem = T)
#Q1 top 25 features
topfeatures(Dfm, n=25)

stopwords <- c('denver', 'room', 'bedroom')
Dfm <- dfm_remove(Dfm,stopwords)

textplot_wordcloud(Dfm, max_words=150)

Dfm <- dfm_trim(Dfm ,min_termfreq=5, min_docfreq=3)
dim(Dfm)

tfidf <- dfm_tfidf(Dfm)

SVD <- textmodel_lsa(tfidf)
SVD$docs[]

combined_df <- cbind(listings_svd[,2:31], as.data.frame(SVD$docs))


set.seed(101)
trainIndex <- createDataPartition(combined_df$exited, p=0.7, list=FALSE, times=1)

df.train <- combined_df[trainIndex,]
df.valid <- combined_df[-trainIndex,]

ROSE_balanced_train <- ovun.sample(exited~., data=df.train, method='over')$data
summary(ROSE_balanced_train$exited)


balanced_LR <- train(exited~.,
                     data=ROSE_balanced_train,
                     method='glm',
                     family='binomial',
                     na.action=na.pass)
summary(balanced_LR)
balanced_LR_pred <- predict(balanced_LR, newdata = df.valid)
confusionMatrix(balanced_LR_pred, df.valid$exited, positive='1')

significant_df <- combined_df[, which(names(combined_df) %in% c('host_is_superhost', 'host_listings_count', 'room_type',
                                                                  'minimum_nights', 'maximum nights', 'availability_30', 'availability_90', 'number_of_reviews',
                                                                   'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin', 'exited',
                                                                'V1', 'V2', 'V4', 'V9'))]

trainIndex <- createDataPartition(significant_df$exited, p=0.7, list=FALSE, times=1)

df.train <- significant_df[trainIndex,]
df.valid <- significant_df[-trainIndex,]

ROSE_balanced_train <- ovun.sample(exited~., data=df.train, method='over')$data
summary(ROSE_balanced_train$exited)


balanced_LR <- train(exited~.,
                     data=ROSE_balanced_train,
                     method='glm',
                     family='binomial',
                     na.action=na.pass)
summary(balanced_LR)
balanced_LR_pred <- predict(balanced_LR, newdata = df.valid)
confusionMatrix(balanced_LR_pred, df.valid$exited, positive='1')



