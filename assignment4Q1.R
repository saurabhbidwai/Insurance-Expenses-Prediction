#Q.1
#a
insu=read.csv(file = "insurance.csv",stringsAsFactors = TRUE)

str(insu)

summary(insu$expenses)

hist(insu$expenses)

table(insu$region)

#b
cor1=cbind(insu$age,insu$bmi,insu$children,insu$expenses)
cor(cor1)
#or cor(insu[c("age","bmi","children","expenses")])

#c
plot(insu)

pairs(insu[c("age","bmi","children","expenses")])

#d
install.packages("psych")
library(psych)

pairs.panels(insu[c("age","bmi","children","expenses")])


#e
ind=sample((1:nrow(insu)),round(0.70*nrow(insu)))
train6=insu[ind,]
test6=insu[-ind,]


#f
insu$age2=insu$age^2
insu$bmi30=ifelse(insu$bmi>=30,1,0)

head(insu)

fit3=lm(expenses ~ age+factor(sex)+bmi+children+factor(smoker)+factor(region), data = insu)#0.7494

fit31=lm(expenses ~ age+sex+bmi+children+smoker+region, data = insu)#0.7494

fit32=lm(formula = expenses ~ age + bmi + children + factor(smoker) + factor(region), data = insu)#0.7496

fit33=lm(formula = expenses~ age+age2+ children+ bmi+ sex+ bmi30*smoker + region,data=insu)#0.8653
summary(fit33)

fit34=lm(formula = expenses ~age2 + children + bmi + sex + bmi30 + smoker + region + bmi30*smoker, data = insu)#0.8654
summary(fit34)

step(fit34)

vif(fit34)

out=predict(fit34,test6)

error=out-test6["expenses"]

rmse=sqrt(mean(error^2))
rmse

