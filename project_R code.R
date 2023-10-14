# Identify campaign targets using UCI bank marketing data

library(sqldf)
library(lattice)  # Lattice Graphics
library(vcd)  # Visualizing Categorical Data
library(ROCR)  # Visualizing Classifier Performance


bank_all <- read.csv("Desktop/bank.csv", sep = ";")
str(bank_all)
summary(bank_all)

table(bank_all$previous)

# select those never contacted before
bank <- subset(bank_all, subset=(previous==0), select=
                 c("response", "age", "balance", "job", "marital", "education", "default", "housing", "loan")
)

str(bank)
summary(bank)


#################### examine the relationship between DV and IVs
histogram(~ age | response, data=bank, type="density", xlab="Age", layout=c(1,2))
histogram(~ balance | response, data=bank, type="density", xlab="Balance", layout=c(1,2))
sqldf("select response, avg(age), avg(balance) from bank group by response") 

table(bank$job, bank$response)
mosaic(~ job + response, data=bank, labeling= labeling_border(rot_labels = c(0,0,0,-20), just_labels = c("right", "center", "center", "right")), highlighting="response", highlighting_fill=c("darkblue", "lightgreen"))

table(bank$marital, bank$response)
mosaic(~ marital + response, data=bank, highlighting="response", highlighting_fill=c("darkblue", "lightgreen"))

table(bank$education, bank$response)
mosaic(~ education + response, data=bank, highlighting="response", highlighting_fill=c("darkblue", "lightgreen"))

table(bank$default, bank$response)
mosaic(~ default + response, data=bank, highlighting="response", highlighting_fill=c("darkblue", "lightgreen"))

table(bank$housing, bank$response)
mosaic(~ housing + response, data=bank, highlighting="response", highlighting_fill=c("darkblue", "lightgreen"))

table(bank$loan, bank$response)
mosaic(~ loan + response, data=bank, highlighting="response", highlighting_fill=c("darkblue", "lightgreen"))


#################### logistic regression
bank$observed_response <- ifelse(bank$response=='yes', 1, 0)

bank_spec <- {observed_response ~ age + job + education + marital + default + balance + housing + loan}
bank_fit <- glm(bank_spec, family=binomial, data=bank)
summary(bank_fit)

bank$predicted_prob <- predict.glm(bank_fit, type="response") 

densityplot(~ predicted_prob | response, data=bank, layout=c(1,2),
            plot.points="n", col="darkblue", xlab="Predicted Response Probability"
) 

sqldf("select response, avg(predicted_prob) from bank group by response") 


# use of a 0.50 cutoff
bank$predicted_response <- ifelse(bank$predicted_prob > 0.5, 1, 0)
bank$predicted_response <- factor(bank$predicted_response, levels=c(0,1), labels=c("NO","YES"))  
confusion_matrix <- table(bank$predicted_response, bank$response)
cat("\nConfusion Matrix (rows=Predicted; columns=Actual\n")
confusion_matrix
accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix)
cat("\nAccuracy: ", accuracy, "\n")


# try a lower cutoff of 0.10
bank$predicted_response <- ifelse(bank$predicted_prob > 0.1, 1, 0)
bank$predicted_response <- factor(bank$predicted_response, levels=c(0,1), labels=c("NO","YES"))  
confusion_matrix <- table(bank$predicted_response, bank$response)
cat("\nConfusion Matrix (rows=Predicted; columns=Actual\n")
confusion_matrix
accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix)
cat("\nAccuracy: ", accuracy, "\n")

mosaic( ~ predicted_response + response, data=bank,
        labeling_args = list(set_varnames=c(predicted_response="Predicted", response="Actual")),
        highlighting = c("response"), highlighting_fill = c("lightgreen", "darkblue", "darkblue", "lightgreen")
)


#################### benefit/cost analysis
c <- 10
r <- 100

t <- seq(0, 0.35, 0.05)
k <- length(t)

A <- data.frame(TN=t, FN=t, FP=t, TP=t)
for (i in 1:k) {
  A$TN[i] <- sum(bank$predicted_prob <= t[i] & bank$response=="no")
  A$FN[i] <- sum(bank$predicted_prob <= t[i] & bank$response=="yes")
  A$FP[i] <- sum(bank$predicted_prob > t[i] & bank$response=="no")
  A$TP[i] <- sum(bank$predicted_prob > t[i] & bank$response=="yes")
}

A$cost <- c * (A$TP + A$FP)
A$revenue <- A$TP * r
A$profit <- A$revenue - A$cost
A$ROI <- A$profit / A$cost

plot(t, A$cost, type='l', lty=2, lwd=2, col="red", 
     xlab="Cutoff", ylab="Revenue / Cost / Profit",
     xlim=c(0,0.35), ylim=c(-2000, 40000))
lines(t, A$revenue, type='l', lty=2, lwd=2, col="green")
lines(t, A$profit, type='l', lty=1, lwd=2, col="blue")
legend("topright", legend=c("Profit", "Revenue", "Cost"), lty=c(1, 2, 2), lwd=2, col=c("blue", "green", "red"))


#################### ROC curve
bank_pred <- prediction(bank$predicted_prob, bank$response)
bank_roc <- performance(bank_pred, "tpr", "fpr")
bank_auc <- as.numeric(performance(bank_pred, "auc")@y.values)

plot(bank_roc, col="blue", xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(c(0,1))
legend("bottomright", legend=paste("AUC = ",round(bank_auc, digits=3)))


#################### lift curve
bank_pred <- prediction(bank$predicted_prob, bank$response)
bank_lift <- performance(bank_pred , "lift", "rpp")
plot(bank_lift, col="blue", xlab="Proportion Targeted", ylab="Lift over Baseline Rate")


