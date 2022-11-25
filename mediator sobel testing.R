mydata<-read.csv(file.choose())
names(mydata)

library(psych)
describe(mydata)
#pearson
mycor=corr.test(mydata)
lowerMat(mycor$r,digits=3)
#p-value test
lowerMat(mycor$p,digits=4)

med.m1=lm(mydata$forum_posts~mydata$total_view,data=mydata)
summary(med.m1)

med.m2=lm(mydata$grade~mydata$total_view,data=mydata)
summary(med.m2)

med.m3=lm(mydata$grade~mydata$forum_posts+mydata$total_view,data=mydata)
summary(med.m3)

#Sobel test
coef.m1=summary(med.m1)$coefficients
coef.m1
a=coef.m1[2,1]
a
sa=coef.m1[2,2]
sa

coef.m3=summary(med.m3)$coefficients
coef.m3
b=coef.m3[3,1]
b
sb=coef.m3[3,2]
sb

ab=a*b
ab

z=ab/sqrt(a^2*sb^2+b^2*sa^2)
z

#p-value
p=pnorm(-abs(z))*2
p

