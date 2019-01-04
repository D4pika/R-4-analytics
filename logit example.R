# Load in dataset
library(ISLR)
data(Default, package="ISLR")
head(Default)

# income by balance colored by defaulters
#install.packages("ggplot2")
library(ggplot2)
p1 <- ggplot(Default, aes(x=balance, y=income)) + geom_point(aes(color = default))
p1 <- p1 + ggtitle("Income & Balance colored by Default")
p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold"))
p1 <- p1 + theme(axis.title.x = element_text(face="bold", size=15),
                 axis.text.x  = element_text(size=12, color="black"),
                 axis.title.y = element_text(face="bold", size=15),
                 axis.text.y  = element_text(size=12, color="black")
                 )
p1 <- p1 + scale_y_continuous(breaks=seq(0,80000,10000))
p1 <- p1 + scale_x_continuous(breaks=seq(0,3000,500))
p1

# Side-by-side boxplot of balance vs default group
p2 <- ggplot(Default, aes(x=default, y=balance, fill=default)) + geom_boxplot() 
p2 <- p2 + geom_boxplot() + ggtitle("Card Balance by Default Group")
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold"))
p2 <- p2 + theme(axis.title.x = element_text(face="bold", size=15),
                 axis.text.x  = element_text(size=12, color="black"),
                 axis.title.y = element_text(face="bold", size=15),
                 axis.text.y  = element_text(size=12, color="black")
)
p2 <- p2 + scale_y_continuous(breaks=seq(0,3500,500))
p2

# Side-by-side boxplot of income vs default group
p3 <- ggplot(Default, aes(x=default, y=income, fill=default)) 
p3 <- p3 + geom_boxplot() + ggtitle("Income by Default Group")
p3 <- p3 + theme(plot.title = element_text(lineheight=.8, face="bold"))
p3 <- p3 + theme(axis.title.x = element_text(face="bold", size=15),
                 axis.text.x  = element_text(size=12, color="black"),
                 axis.title.y = element_text(face="bold", size=15),
                 axis.text.y  = element_text(size=12, color="black")
)
p3 <- p3 + scale_y_continuous(breaks=seq(0,80000,10000))
p3
#multiplot(p2, p3, cols=2)

# create a barplot showing distribution of classes
No <- table(Default$default)["No"][[1]]
Yes <- table(Default$default)["Yes"][[1]]
Total <- No + Yes
barplot(table(Default$default), xlab="Class (Y)", ylab="Count",
        main="Barplot of Class Labels", ylim=range(0,Total))
    text(.65, No*.9, paste0(No/Total*100,"%"))
    text(1.9, Yes*.9, paste0(Yes/Total*100,"%"))

################################################################################
# Model 1: default ~ balance
################################################################################
# fit a logit class model on just balance
glm.fit <- glm(default ~ balance, data=Default, family=binomial)
summary(glm.fit)
# what are the predicted probabilities
p_hat <- round(predict(glm.fit, type="response"),4)
# what are the predicted classes
y_hat <- ifelse(p_hat>0.50, 1, 0)
# what are the predicted classes
y <- ifelse(Default[,"default"]=="Yes", 1, 0)
# maybe put the response in a data.frame with the prob and class prediction
results = data.frame(y, y_hat, p_hat)
colnames(results) = c("y", "y_hat", "p_hat")

# Look at the distribution of defaults versus non-defaulters
p4 <- ggplot(results, aes(p_hat)) + xlim(0,1) + ylim(0,10) + geom_histogram(aes(y= ..density..)
             , binwidth=0.05, colour="blue",size=I(.5), alpha= I(1/5), linetype="solid"
             , fill="red")
p4 <- p4 +  geom_density(aes(y= ..density..), colour="blue",size=I(.5), alpha= I(1/3)
                 , linetype="solid", fill="red")
p4 <- p4 + facet_grid(y ~ .)
p4 <- p4 + geom_text(x=0.65, y=5, label="misclassification point = 0.50", color="black"
              , size=I(5), alpha= I(1), angle=0, family="Gill Sans MT", fontface="plain"
              , hjust=0.5,	vjust=0.5, lineheight=0.8)
p4 <- p4 + geom_vline(xintercept =0.50, color="black", size=I(1.5), alpha= I(1)
                      , linetype="solid")
p4 <- p4 + labs(title = "Probability of Default"
                , y="Density of Default(1) vs. Not Default(0)")
p4

################################################################################
# Model 2: default ~ student
################################################################################
# fit a logit model on student
glm.stud <- glm(default ~ student, data=Default, family=binomial)
summary(glm.stud)

#probability of default for a student
exp(predict(glm.stud, data.frame(student=c("Yes"))))/
    (1+(exp(predict(glm.stud, data.frame(student=c("Yes"))))))
#0.04313859

# this is the same as the previous line
exp(-3.50413+(0.40489*1))/(1+(exp(-3.50413+(0.40489*1))))
#0.04313862

#probability of default for not a student
exp(predict(glm.stud, data.frame(student=c("No"))))/
    (1+(exp(predict(glm.stud, data.frame(student=c("No"))))))
#0.02919501

################################################################################
# Model 3: default ~ balance + income + student
################################################################################
# Multiple Logistic Regression
# fit a logit model with all variables
glm.mult <- glm(default ~ balance + income + student
                , data=Default, family=binomial)
summary(glm.mult)

# Example 1 and 2
ex1 <- data.frame(1500, 40000, "Yes")
names(ex1) <- c("balance","income","student")
ex2 <- data.frame(1500, 40000, "No")
names(ex2) <- c("balance","income","student")
exp(predict(glm.mult, ex1))/(1+exp(predict(glm.mult, ex1)))
exp(predict(glm.mult, ex2))/(1+exp(predict(glm.mult, ex2)))

# Obtain the odds ratios and 95% Wald CIs
betas <- data.frame(cbind(round(summary(glm.mult)$ coefficients[,1],5),
                    round(exp(summary(glm.mult)$ coefficients[,1]),5),
                    round(exp(confint.default(glm.mult)),5)
                    ))
names(betas) <- c("logOR","OddsRatio","WaldCI2.5","WaldCI97.5")
betas 
