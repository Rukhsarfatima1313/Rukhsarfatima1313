datar<-read.csv("smoking_dataset.csv")
str(datar)
summary(d)

datarr<-na.omit(datar)

library(ggplot2)
ggplot(datar,aes(y=Gtp))+geom_boxplot()

IQR_Gtp<-IQR(datar$Gtp)
IQR_Gtp
lower_bound<-quantile(datar$Gtp,0.25)-1.2*IQR_Gtp
upper_bound<-quantile(datar$Gtp,0.75)+1.2*IQR_Gtp
x<-datar[datar$Gtp>=lower_bound&datar$Gtp<=upper_bound,]
library(ggplot2)
boxplot(Gtp ~ smoking, data = x,
        main = "Boxplot of Gtp by Smoking Status",
        xlab = "Smoking Status (0: Non-Smoker, 1: Smoker)",
        ylab = "Gtp")


library(ggplot2)

ggplot(x,aes(x=hemoglobin))+geom_histogram()


mean(x$Gtp)
median(x$Gtp)
sd(x$Gtp)
var(x$Gtp)

ggplot(datar, aes(x = factor(tartar))) +
  geom_bar(fill = "black") +
  labs(title = "Bar Plot for tartar", x = "Tartar status", y = "Frequency")


barplot(table(datar$tartar, datar$smoking), beside = TRUE, col = c( "yellow","brown"), legend = TRUE)


contingency_table<-table(datar$smoking,datar$tartar)
print(contingency_table)


str(datar)
summary(datar)
datar$gender<-as.factor(datar$gender)
datar$hearing.left.<-as.factor(datar$hearing.left.)
datar$hearing.right.<-as.factor(datar$hearing.right.)
datar$Urine.protein<-as.factor(datar$Urine.protein)
datar$oral<-as.factor(datar$oral)
datar$dental.caries<-as.factor(datar$dental.caries)
datar$tartar<-as.factor(datar$tartar)
datar$smoking<-as.factor(datar$smoking)
summary(datar)
str(datar)
datarr<-na.omit(datar)

library(dplyr)
vars_to_check<-c("height.cm.","systolic","triglyceride","hemoglobin","serum.creatinine","ALT","Gtp")

remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 3 * IQR_val
  upper_bound <- Q3 + 3 * IQR_val
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

datar_no_outliers <- datar
datar_no_outliers[vars_to_check] <- lapply(datar_no_outliers[vars_to_check], remove_outliers)
datar_no_outliers <- na.omit(datar_no_outliers)



summary(datar_no_outliers)
str(datar_no_outliers)

set.seed(123)
sample_index <- sample(1:nrow(datar_no_outliers), 0.7 * nrow(datar_no_outliers))
train_datar <- datar_no_outliers[sample_index, ]
test_datar <- datar_no_outliers[-sample_index, ]
str(train_datar)

logistic_model<-glm(smoking~height.cm.+gender+height.cm.+systolic+triglyceride+hemoglobin+serum.creatinine+ALT+Gtp+dental.caries+tartar,data = train_datar,family = binomial())
summary(logistic_model)

predictions <- predict(logistic_model, newdata = test_datar, type = "response") 
predicted_classes <- ifelse(predictions > 0.5, 1, 0) 
conf_matrix <- table(predicted_classes, test_datar$smoking) 
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)



