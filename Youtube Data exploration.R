getwd()
mydata<-read.csv("CAvideos.csv",header = TRUE,sep = ",")
summary(mydata)
summary(lm(mydata$views~mydata$likes+mydata$dislikes+mydata$comment_count+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
summary(lm(mydata$views~mydata$likes,data=mydata))3
summary(lm(mydata$views~mydata$likes+mydata$dislikes,data=mydata))
summary(lm(mydata$views~mydata$likes+mydata$dislikes+mydata$comment_count,data=mydata))

#it seems like all the value are significant with all have P value less than 5%.Therefore we reject the nUll

#non linear test

like2<-mydata$likes^2
like3<-mydata$likes^3
like4<-mydata$likes^4
dislike2<-mydata$dislikes^2
dislike3<-mydata$dislikes^3
dislike4<-mydata$dislikes^4
comment<-mydata$comment_count^2
comment3<-mydata$comment_count^3
comment4<-mydata$comment_count^4

#non linear test to the power 2
summary(lm(mydata$views~mydata$likes+like2+mydata$dislikes+dislike2+mydata$comment_count+comment+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))

# base on the result we can see that all P value has is less than 5%, we reject the null. All variable are significant 

# non linear test to the power of 3
summary(lm(mydata$views~mydata$likes+like2+like3+mydata$dislikes+dislike2+dislike3+mydata$comment_count+comment+comment+comment3+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
#base on the result, it seems that we should drop the variable like 2

# Ftest
unrestrictedva<-(lm(mydata$views~mydata$likes+like2+like3+mydata$dislikes+dislike2+dislike3+mydata$comment_count+comment+comment3+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
restriectedva<-(lm(mydata$views~mydata$likes+like3+mydata$dislikes+dislike2+dislike3+mydata$comment_count+comment+comment3+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
anova(unrestrictedva,restriectedva)
#base on the result, the F stat is 1.124 and the o value is 0.2891, we fail to reject the null, the variable is insignificant

#non linear test to the power 4
summary(lm(mydata$views~mydata$likes+like2+like3+like4+mydata$dislikes+dislike2+dislike3+dislike4+mydata$comment_count+comment+comment3+comment4+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
# base in the result, al lthe variable is significant,we reject the null

#estimate log lin model

summary(lm(log(mydata$views)~mydata$likes+mydata$dislikes+mydata$comment_count+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
# it estimate that there is an increase of 0,0006865% in view for each like,0,0003294% foe dislike and a decrease in view 0f 0,001429% for each comment

#interaction term
 # we now estimate if there a dummy continuous interaction term between the dummy ( comments_diabled) and the number of like

Comdis_like<-mydata$comments_disabled*mydata$likes
Comdis_like2<-mydata$comments_disabled*like2
Comdis_like3<-mydata$comments_disabled*like3
Comdis_like4<-mydata$comments_disabled*like4

summary(lm(mydata$views~mydata$likes+like2+like3+like4+Comdis_like+Comdis_like2+Comdis_like3+Comdis_like4+mydata$dislikes+dislike2+dislike3+dislike4+mydata$comment_count+comment+comment3+comment4+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))

# F test
unrestriectedva1<-(lm(mydata$views~mydata$likes+like2+like3+like4+Comdis_like+Comdis_like2+Comdis_like3+Comdis_like4+mydata$dislikes+dislike2+dislike3+dislike4+mydata$comment_count+comment+comment3+comment4+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
restriectedva1<-(lm(mydata$views~mydata$likes+like2+like3+like4+mydata$dislikes+dislike2+dislike3+dislike4+mydata$comment_count+comment+comment3+comment4+mydata$comments_disabled+mydata$ratings_disabled,data=mydata))
anova(unrestriectedva1,restriectedva1)
# base in the result we can reject the null hypothesis and comment disable has an effect on the number of like
      