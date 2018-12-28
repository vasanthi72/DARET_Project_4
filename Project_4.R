library(CARET)
library(caret)
library(MASS)
library(car)
library(pROC)
library(InformationValue)
library(C50)
library(mltools)
library(rpart)
library(randomForest)
library(ggplot2)
library(reshape2)
library(corrplot)
CancerData <- read.csv("D:/Abhyayanam/DARET/Projects/Project 4 - Disease Prediction/CancerData.csv", stringsAsFactors=FALSE)
View(CancerData)
CanDat <- CancerData
sum(is.na(CanDat))
str(CanDat)
nrow(CanDat)
ncol(CanDat)
colnames(CanDat)
summary(CanDat$X)
CanDat <- CanDat[,-c(33)]
colnames(CanDat)
ncol(CanDat)
df <- CanDat
df <- df[,-(1)]
colnames(df)
df.m <- melt(df, id.var = "diagnosis")
p <- ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=diagnosis))
p + facet_wrap( ~ variable, scales="free")
qntradius_mean <- quantile(CanDat$radius_mean, probs=c(.25, .75))
qnttexture_mean <- quantile(CanDat$texture_mean, probs=c(.25, .75))
qntperimeter_mean <- quantile(CanDat$perimeter_mean, probs=c(.25, .75))
qntarea_mean <- quantile(CanDat$area_mean, probs=c(.25, .75))
qntsmoothness_mean <- quantile(CanDat$smoothness_mean, probs=c(.25, .75))
qntcompactness_mean <- quantile(CanDat$compactness_mean, probs=c(.25, .75))
qntconcavity_mean <- quantile(CanDat$concavity_mean, probs=c(.25, .75))
qntconcave.points_mean <- quantile(CanDat$concave.points_mean, probs=c(.25, .75))
qntsymmetry_mean <- quantile(CanDat$symmetry_mean, probs=c(.25, .75))
qntfractal_dimension_mean <- quantile(CanDat$fractal_dimension_mean, probs=c(.25, .75))
qntradius_se <- quantile(CanDat$radius_se, probs=c(.25, .75))
qnttexture_se <- quantile(CanDat$texture_se, probs=c(.25, .75))
qntperimeter_se <- quantile(CanDat$perimeter_se, probs=c(.25, .75))
qntarea_se <- quantile(CanDat$area_se, probs=c(.25, .75))
qntsmoothness_se <- quantile(CanDat$smoothness_se, probs=c(.25, .75))
qntcompactness_se <- quantile(CanDat$compactness_se, probs=c(.25, .75))
qntconcavity_se <- quantile(CanDat$concavity_se, probs=c(.25, .75))
qntconcave.points_se <- quantile(CanDat$concave.points_se, probs=c(.25, .75))
qntsymmetry_se <- quantile(CanDat$symmetry_se, probs=c(.25, .75))
qntfractal_dimension_se <- quantile(CanDat$fractal_dimension_se, probs=c(.25, .75))
qntradius_worst <- quantile(CanDat$radius_worst, probs=c(.25, .75))
qnttexture_worst <- quantile(CanDat$texture_worst, probs=c(.25, .75))
qntperimeter_worst <- quantile(CanDat$perimeter_worst, probs=c(.25, .75))
qntarea_worst <- quantile(CanDat$area_worst, probs=c(.25, .75))
qntsmoothness_worst <- quantile(CanDat$smoothness_worst, probs=c(.25, .75))
qntcompactness_worst <- quantile(CanDat$compactness_worst, probs=c(.25, .75))
qntconcavity_worst <- quantile(CanDat$concavity_worst, probs=c(.25, .75))
qntconcave.points_worst <- quantile(CanDat$concave.points_worst, probs=c(.25, .75))
qntsymmetry_worst <- quantile(CanDat$symmetry_worst, probs=c(.25,.75))
qntfractal_dimension_worst <- quantile(CanDat$fractal_dimension_worst, probs=c(.25, .75))
Hradius_mean <- 1.5 * IQR(CanDat$radius_mean);Hradius_se <- 1.5 * IQR(CanDat$radius_se); Hradius_worst <- 1.5 * IQR(CanDat$radius_worst);
Htexture_mean <- 1.5 * IQR(CanDat$texture_mean);Htexture_se <- 1.5 * IQR(CanDat$texture_se); Htexture_worst <- 1.5 * IQR(CanDat$texture_worst);
Hperimeter_mean <- 1.5 * IQR(CanDat$perimeter_mean);Hperimeter_se <- 1.5 * IQR(CanDat$perimeter_se); Hperimeter_worst <- 1.5 * IQR(CanDat$perimeter_worst);
Harea_mean <- 1.5 * IQR(CanDat$area_mean);Harea_se <- 1.5 * IQR(CanDat$area_se); Harea_worst <- 1.5 * IQR(CanDat$area_worst);
Hsmoothness_mean <- 1.5 * IQR(CanDat$smoothness_mean);Hsmoothness_se <- 1.5 * IQR(CanDat$smoothness_se); Hsmoothness_worst <- 1.5 * IQR(CanDat$smoothness_worst);
Hcompactness_mean <- 1.5 * IQR(CanDat$compactness_mean);Hcompactness_se <- 1.5 * IQR(CanDat$compactness_se); Hcompactness_worst <- 1.5 * IQR(CanDat$compactness_worst);
Hconcavity_mean <- 1.5 * IQR(CanDat$concavity_mean);Hconcavity_se <- 1.5 * IQR(CanDat$concavity_se); Hconcavity_worst <- 1.5 * IQR(CanDat$concavity_worst);
Hconcave.points_mean <- 1.5 * IQR(CanDat$concave.points_mean);Hconcave.points_se <- 1.5 * IQR(CanDat$concave.points_se); Hconcave.points_worst <- 1.5 * IQR(CanDat$concave.points_worst);
Hsymmetry_mean <- 1.5 * IQR(CanDat$symmetry_mean);Hsymmetry_se <- 1.5 * IQR(CanDat$symmetry_se); Hsymmetry_worst <- 1.5 * IQR(CanDat$symmetry_worst);
Hfractal_dimension_mean <- 1.5 * IQR(CanDat$fractal_dimension_mean);Hfractal_dimension_se <- 1.5 * IQR(CanDat$fractal_dimension_se); Hfractal_dimension_worst <- 1.5 * IQR(CanDat$fractal_dimension_worst);
outCanDat <- CanDat
outCanDat$radius_mean[outCanDat$radius_mean < (qntradius_mean[1] - Hradius_mean)] <- qntradius_mean[1]; outCanDat$radius_mean[outCanDat$radius_mean > (qntradius_mean[2] + Hradius_mean)] <- qntradius_mean[2]
outCanDat$radius_se[outCanDat$radius_se < (qntradius_se[1] - Hradius_se)] <- qntradius_se[1]; outCanDat$radius_se[outCanDat$radius_se > (qntradius_se[2] + Hradius_se)] <- qntradius_se[2]
outCanDat$radius_worst[outCanDat$radius_worst < (qntradius_worst[1] - Hradius_worst)] <- qntradius_worst[1]; outCanDat$radius_worst[outCanDat$radius_worst > (qntradius_worst[2] + Hradius_worst)] <- qntradius_worst[2]
outCanDat$texture_mean[outCanDat$texture_mean < (qnttexture_mean[1] - Htexture_mean)] <- qnttexture_mean[1]; outCanDat$texture_mean[outCanDat$texture_mean > (qnttexture_mean[2] + Htexture_mean)] <- qnttexture_mean[2];
outCanDat$texture_se[outCanDat$texture_se < (qnttexture_se[1] - Htexture_se)] <- qnttexture_se[1];
outCanDat$texture_se[outCanDat$texture_se > (qnttexture_se[2] + Htexture_se)] <- qnttexture_se[2];
outCanDat$texture_worst[outCanDat$texture_worst < (qnttexture_worst[1] - Htexture_worst)] <- qnttexture_worst[1]; outCanDat$texture_worst[outCanDat$texture_worst > (qnttexture_worst[2] + Htexture_worst)] <- qnttexture_worst[2]
outCanDat$perimeter_mean[outCanDat$perimeter_mean < (qntperimeter_mean[1] - Hperimeter_mean)] <- qntperimeter_mean[1]; outCanDat$perimeter_mean[outCanDat$perimeter_mean > (qntperimeter_mean[2] + Hperimeter_mean)] <- qntperimeter_mean[2];
outCanDat$perimeter_se[outCanDat$perimeter_se < (qntperimeter_se[1] - Hperimeter_se)] <- qntperimeter_se[1];
outCanDat$perimeter_se[outCanDat$perimeter_se > (qntperimeter_se[2] + Hperimeter_se)] <- qntperimeter_se[2];
outCanDat$perimeter_worst[outCanDat$perimeter_worst < (qntperimeter_worst[1] - Hperimeter_worst)] <- qntperimeter_worst[1]; outCanDat$perimeter_worst[outCanDat$perimeter_worst > (qntperimeter_worst[2] + Hperimeter_worst)] <- qntperimeter_worst[2]
outCanDat$area_mean[outCanDat$area_mean < (qntarea_mean[1] - Harea_mean)] <- qntarea_mean[1]; outCanDat$area_mean[outCanDat$area_mean > (qntarea_mean[2] + Harea_mean)] <- qntarea_mean[2];
outCanDat$area_se[outCanDat$area_se < (qntarea_se[1] - Harea_se)] <- qntarea_se[1];
outCanDat$area_se[outCanDat$area_se > (qntarea_se[2] + Harea_se)] <- qntarea_se[2];
outCanDat$area_worst[outCanDat$area_worst < (qntarea_worst[1] - Harea_worst)] <- qntarea_worst[1]; outCanDat$area_worst[outCanDat$area_worst > (qntarea_worst[2] + Harea_worst)] <- qntarea_worst[2]
outCanDat$smoothness_mean[outCanDat$smoothness_mean < (qntsmoothness_mean[1] - Hsmoothness_mean)] <- qntsmoothness_mean[1]; outCanDat$smoothness_mean[outCanDat$smoothness_mean > (qntsmoothness_mean[2] + Hsmoothness_mean)] <- qntsmoothness_mean[2]; outCanDat$smoothness_se[outCanDat$smoothness_se < (qntsmoothness_se[1] - Hsmoothness_se)] <- qntsmoothness_se[1];
outCanDat$smoothness_se[outCanDat$smoothness_se > (qntsmoothness_se[2] + Hsmoothness_se)] <- qntsmoothness_se[2];
outCanDat$smoothness_worst[outCanDat$smoothness_worst < (qntsmoothness_worst[1] - Hsmoothness_worst)] <- qntsmoothness_worst[1]; outCanDat$smoothness_worst[outCanDat$smoothness_worst > (qntsmoothness_worst[2] + Hsmoothness_worst)] <- qntsmoothness_worst[2]
outCanDat$compactness_mean[outCanDat$compactness_mean < (qntcompactness_mean[1] - Hcompactness_mean)] <- qntcompactness_mean[1]; outCanDat$compactness_mean[outCanDat$compactness_mean > (qntcompactness_mean[2] + Hcompactness_mean)] <- qntcompactness_mean[2];
outCanDat$compactness_se[outCanDat$compactness_se < (qntcompactness_se[1] - Hcompactness_se)] <- qntcompactness_se[1];
outCanDat$compactness_se[outCanDat$compactness_se > (qntcompactness_se[2] + Hcompactness_se)] <- qntcompactness_se[2];
outCanDat$compactness_worst[outCanDat$compactness_worst < (qntcompactness_worst[1] - Hcompactness_worst)] <- qntcompactness_worst[1]; outCanDat$compactness_worst[outCanDat$compactness_worst > (qntcompactness_worst[2] + Hcompactness_worst)] <- qntcompactness_worst[2]
outCanDat$concavity_mean[outCanDat$concavity_mean < (qntconcavity_mean[1] - Hconcavity_mean)] <- qntconcavity_mean[1]; outCanDat$concavity_mean[outCanDat$concavity_mean > (qntconcavity_mean[2] + Hconcavity_mean)] <- qntconcavity_mean[2];
outCanDat$concavity_se[outCanDat$concavity_se < (qntconcavity_se[1] - Hconcavity_se)] <- qntconcavity_se[1];
outCanDat$concavity_se[outCanDat$concavity_se > (qntconcavity_se[2] + Hconcavity_se)] <- qntconcavity_se[2];
outCanDat$concavity_worst[outCanDat$concavity_worst < (qntconcavity_worst[1] - Hconcavity_worst)] <- qntconcavity_worst[1]; outCanDat$concavity_worst[outCanDat$concavity_worst > (qntconcavity_worst[2] + Hconcavity_worst)] <- qntconcavity_worst[2]
outCanDat$concave.points_mean[outCanDat$concave.points_mean < (qntconcave.points_mean[1] - Hconcave.points_mean)] <- qntconcave.points_mean[1]; outCanDat$concave.points_mean[outCanDat$concave.points_mean > (qntconcave.points_mean[2] + Hconcave.points_mean)] <- qntconcave.points_mean[2];
outCanDat$concave.points_se[outCanDat$concave.points_se < (qntconcave.points_se[1] - Hconcave.points_se)] <- qntconcave.points_se[1];
outCanDat$concave.points_se[outCanDat$concave.points_se > (qntconcave.points_se[2] + Hconcave.points_se)] <- qntconcave.points_se[2];
outCanDat$concave.points_worst[outCanDat$concave.points_worst < (qntconcave.points_worst[1] - Hconcave.points_worst)] <- qntconcave.points_worst[1]; outCanDat$concave.points_worst[outCanDat$concave.points_worst > (qntconcave.points_worst[2] + Hconcave.points_worst)] <- qntconcave.points_worst[2]
outCanDat$symmetry_mean[outCanDat$symmetry_mean < (qntsymmetry_mean[1] - Hsymmetry_mean)] <- qntsymmetry_mean[1]; outCanDat$symmetry_mean[outCanDat$symmetry_mean > (qntsymmetry_mean[2] + Hsymmetry_mean)] <- qntsymmetry_mean[2];
outCanDat$symmetry_se[outCanDat$symmetry_se < (qntsymmetry_se[1] - Hsymmetry_se)] <- qntsymmetry_se[1];
outCanDat$symmetry_se[outCanDat$symmetry_se > (qntsymmetry_se[2] + Hsymmetry_se)] <- qntsymmetry_se[2];
outCanDat$symmetry_worst[outCanDat$symmetry_worst < (qntsymmetry_worst[1] - Hsymmetry_worst)] <- qntsymmetry_worst[1]; outCanDat$symmetry_worst[outCanDat$symmetry_worst > (qntsymmetry_worst[2] + Hsymmetry_worst)] <- qntsymmetry_worst[2]
outCanDat$fractal_dimension_mean[outCanDat$fractal_dimension_mean < (qntfractal_dimension_mean[1] - Hfractal_dimension_mean)] <- qntfractal_dimension_mean[1]; outCanDat$fractal_dimension_mean[outCanDat$fractal_dimension_mean > (qntfractal_dimension_mean[2] + Hfractal_dimension_mean)] <- qntfractal_dimension_mean[2];
outCanDat$fractal_dimension_se[outCanDat$fractal_dimension_se < (qntfractal_dimension_se[1] - Hfractal_dimension_se)] <- qntfractal_dimension_se[1];
outCanDat$fractal_dimension_se[outCanDat$fractal_dimension_se > (qntfractal_dimension_se[2] + Hfractal_dimension_se)] <- qntfractal_dimension_se[2];
outCanDat$fractal_dimension_worst[outCanDat$fractal_dimension_worst < (qntfractal_dimension_worst[1] - Hfractal_dimension_worst)] <- qntfractal_dimension_worst[1]; outCanDat$fractal_dimension_worst[outCanDat$fractal_dimension_worst > (qntfractal_dimension_worst[2] + Hfractal_dimension_worst)] <- qntfractal_dimension_worst[2]
colnames(outCanDat)
colnames(CanDat)
summary(CanDat)
summary(outCanDat)
cormat <- cor(outCanDat)
str(outCanDat)
cormat <- cor(outCanDat[,-c(1,2)])
round(cormat,2)
sum(is.na(outCanDat))
head(outCanDat,15)
tail(outCanDat,15)
corrplot(cormat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
set.seed(246)
smp_size <- floor(0.75 * nrow(outCanDat))
train_ind <- sample(seq_len(nrow(outCanDat)), size = smp_size)
outCanDattrain <- outCanDat[train_ind, ]
outCanDattest <- outCanDat[-train_ind, ]
colnames(outCanDattrain)
colnames(outCanDattest)
nrow(outCanDattrain)
nrow(outCanDattest)
nrow(outCanDat)
table(outCanDat$diagnosis)
table(outCanDattrain$diagnosis)
table(outCanDattest$diagnosis)
ncol(outCanDattrain)
ncol(outCanDattest)
ncol(outCanDat)
colnames(outCanDat)
outCanDat <- outCanDat[,-c(1)]
outCanDattrain <- outCanDattrain[,-c(1)]
outCanDattest <- outCanDattest[,-c(1)]
ncol(outCanDat)
colnames(outCanDat)
colnames(outCanDattrain)
colnames(outCanDattest)
logCantrain <- outCanDattrain
logCantrain$diag <- ifelse(logCantrain$diagnosis=='B',0,1)
table(logCantrain$diag)
logCantest <- outCanDattest
logCantest$diag <- ifelse(logCantest$diagnosis=='B',0,1)
table(logCantest$diag)
logCanDat <- outCanDat
logCanDat$diag <- ifelse(logCanDat$diagnosis=='B',0,1)
table(logCanDat$diag)
table(CanDat$diagnosis)
logCanDat <- logCanDat[,-c(1)]
logCantrain <- logCantrain[,-c(1)]
logCantest <- logCantest[,-c(1)]
logCanDat$diag <- as.factor(logCanDat$diag)
logCantrain$diag <- as.factor(logCantrain$diag)
logCantest$diag <- as.factor(logCantest$diag)
logmodel <- glm(diag~.,data = logCantrain, family = binomial(link = 'logit'),control = list (maxit=50))
steplogmodel <- stepAIC(logmodel,method='backward')
library(tree)
library(C50)
rfmodel1 <- tree(diagnosis~., data = CanDattrain)
summary(rfmodel1)
rfmodel1pred <- predict(rfmodel1, CanDattest)
library(mltools)
library(rpart)
rfmodel2 <- rpart(diagnosis~.,data=CanDattrain)
summary(rfmodel2)
rfmodel2pred <- predict(rfmodel2, CanDattest)
library(C50)
library(caret)
library(rpart)
library(randomForest)
train_control <- trainControl(method="cv", number=10)
rfmodel3 <- train(diagnosis~., data = CanDattrain, trContol = train_control,method ="rf")
rfmodel3pred <- predict(rfmodel3, CanDattest)
rfmodel3
rfmodel3$finalModel
table(CanDattrain$diagnosis)
varImp(rfmodel3)
confusionMatrix(CanDattest$diagnosis, rfmodel3pred)
table(rfmodel3pred, CanDattest$diagnosis)
model3data <- as.data.frame(cbind(CanDattest$diagnosis,rfmodel3pred))
colnames(model3data)[1] <- "diagnosis"
model3data$diagnosis <- ifelse(model3data$diagnosis =='B',1,2)
model3data$diagnosis <- as.numeric(model3data$diagnosis)
model3data$rfmodel3pred <- as.numeric(model3data$rfmodel3pred)
rocmodel3 <- roc(model3data$diagnosis, model3data$rfmodel3pred, data = model3data)
rocmodel3
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
seed <- 7
metric <- 'Accuracy'
colnames(CanDattrain)
mtry <- sqrt(ncol(CanDattrain[,-1]))
tunegrid <- expand.grid(.mtry = mtry)
rfmodel4 <- train(diagnosis~.,data = CanDattrain, method ="rf", metric = metric, tuneGrid = tunegrid, trControl = control)
rfmodel4pred <- predict(rfmodel4, CanDattest)
rfmodel4
rfmodel4$finalModel
table(CanDattrain$diagnosis)
varImp(rfmodel4)
confusionMatrix(CanDattest$diagnosis, rfmodel4pred)
table(rfmodel4pred, CanDattest$diagnosis)
model4data <- as.data.frame(cbind(CanDattest$diagnosis,rfmodel4pred))
colnames(model4data)[1] <- "diagnosis"
model4data$diagnosis <- ifelse(model4data$diagnosis =='B',1,2)
model4data$diagnosis <- as.numeric(model4data$diagnosis)
model4data$rfmodel4pred <- as.numeric(model4data$rfmodel4pred)
rocmodel4 <- roc(model4data$diagnosis, model4data$rfmodel4pred, data = model4data)
rocmodel4
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'random')
set.seed(seed)
mtry <- sqrt(ncol(CanDattrain[,-1]))
rfmodel5 <- train(diagnosis~.,data = CanDattrain, method = "rf", metric = metric, tuneLength = 15, trControl = control)
rfmodel5pred <- predict(rfmodel5, CanDattest)
rfmodel5
rfmodel3$finalModel
table(CanDattrain$diagnosis)
varImp(rfmodel5)
confusionMatrix(CanDattest$diagnosis, rfmodel5pred)
table(rfmodel5pred, CanDattest$diagnosis)
model5data <- as.data.frame(cbind(CanDattest$diagnosis,rfmodel5pred))
colnames(model5data)[1] <- "diagnosis"
model5data$diagnosis <- ifelse(model5data$diagnosis =='B',1,2)
model5data$diagnosis <- as.numeric(model5data$diagnosis)
model5data$rfmodel5pred <- as.numeric(model5data$rfmodel5pred)
rocmodel5 <- roc(model5data$diagnosis, model5data$rfmodel5pred, data = model5data)
rocmodel5
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = 'grid')
rfmodel6 <- train(diagnosis~.,data = CanDattrain, method ='gbm', metric = metric, trControl = control)
rfmodel6pred <- predict(rfmodel6, CanDattest)
rfmodel6
rfmodel6$finalModel
table(CanDattrain$diagnosis)
varImp(rfmodel6)
confusionMatrix(CanDattest$diagnosis, rfmodel6pred)
table(rfmodel6pred, CanDattest$diagnosis)
model6data <- as.data.frame(cbind(CanDattest$diagnosis,rfmodel6pred))
colnames(model6data)[1] <- "diagnosis"
model6data$diagnosis <- ifelse(model6data$diagnosis =='B',1,2)
model6data$diagnosis <- as.numeric(model6data$diagnosis)
model6data$rfmodel6pred <- as.numeric(model6data$rfmodel6pred)
rocmodel6 <- roc(model6data$diagnosis, model6data$rfmodel6pred, data = model6data)
rocmodel6
Accuracy <- function(mat) {
Accuracy <- ((mat[1,1]+mat[2,2])/(mat[3,3]))
return(Accuracy)
}
RecallorSensitivity <- function(mat) {
RS <- (mat[1,1]/mat[3,1])
return(RS)
}
Specificity <- function(mat) {
Spec <- (mat[2,2]/mat[3,2])
return(Spec)
}
Precision <- function(mat) {
Prec <- (mat[1,1]/mat[1,3])
return(Prec)
}
CM3 <- table(rfmodel3pred, CanDattest$diagnosis)
CM3 <- as.matrix(CM3)
CM3 <- cbind(CM3,RowTotal=rowSums(CM3))
CM3 <- rbind(CM3, ColTotal=colSums(CM3))
CM4 <- table(rfmodel4pred, CanDattest$diagnosis)
CM4 <- as.matrix(CM4)
CM4 <- cbind(CM4,RowTotal=rowSums(CM4))
CM4 <- rbind(CM4, ColTotal=colSums(CM4))
CM5 <- table(rfmodel5pred, CanDattest$diagnosis)
CM5 <- as.matrix(CM5)
CM5 <- cbind(CM5,RowTotal=rowSums(CM5))
CM5 <- rbind(CM5, ColTotal=colSums(CM5))
CM6 <- table(rfmodel6pred, CanDattest$diagnosis)
CM6 <- as.matrix(CM6)
CM6 <- cbind(CM6,RowTotal=rowSums(CM6))
CM6 <- rbind(CM6, ColTotal=colSums(CM6))
Accuracy(CM3)
RecallorSensitivity(CM3)
Specificity(CM3)
Precision(CM3)
Accuracy(CM4)
RecallorSensitivity(CM4)
Specificity(CM4)
Precision(CM4)
Accuracy(CM5)
RecallorSensitivity(CM5)
Specificity(CM5)
Precision(CM5)
Accuracy(CM6)
RecallorSensitivity(CM6)
Specificity(CM6)
Precision(CM6)
