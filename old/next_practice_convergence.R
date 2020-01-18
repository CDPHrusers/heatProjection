#Load nlme
library(lme4)

#Create data frame
set.seed(6)
df=data.frame(y=c(1:5+runif(5,0,1),runif(5,0,1)),
              x=rep(1:5,2),randef=rep(letters[1:2],each=5))

#Build model
mod=lmer(y~x+(1|randef),data=df)

#Create random draws from data frame to update model using a loop
set.seed(9)
df1=df[sample(nrow(df),replace=T),];df1=df1[order(df1[,"x"]),]
df2=df; df2$y=1 #Create bunk dataframe to throw back an error
df3=df[sample(nrow(df),replace=T),];df3=df3[order(df3[,"x"]),] 
df.list=list(df1,df2,df3)

#Create list to store updated models
mod.list=list()
#Update models using for loop and store in the list
for(i in seq_along(df.list)) {
  mod2=try(update(mod,data=df.list[[i]]),TRUE)
  if(isTRUE(class(mod2)=="try-error")) { next } else { mod.list[i]=mod2 } }


mod2=try(update(model,data=dlist[[i]],TRUE))
if(isTRUE(class(mod2)=="try-error")) { next } else  {mod.list[i]=mod2 } }
