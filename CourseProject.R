data(mtcars)

names(mtcars)
str(mtcars)

#try to find first correlations
pairs(mtcars, panel=panel.smooth, main="mtcars data")


summary(lm(am ~ ., data = mtcars))
fit <- lm(am ~ ., data = mtcars)
plot(predict(fit), resid(fit))
dfbetas(fit)
anova(fit) #anova reveals variance relation between values

summary(lm(mpg ~ am*. -1, data = mtcars))
fit2 <- lm(mpg ~ am*., data = mtcars)
plot(predict(fit2), resid(fit2))
anova(fit2) #anova reveals variance relation between values

#Relevant columns: mpg, am
#[, 9]         am	 Transmission (0 = automatic, 1 = manual)

#Try to identify necessary fields
#Adding 1 value
comparative <- function(model, exception){
        
        print(coef(model))
        for (i in 2:11){
                relevants <- c()
                if (!(colnames(mtcars)[i] %in% exception)){
                        print("----------------------------------------------------------------------------------")
                        print(colnames(mtcars)[i])
                        model.test <- with(mtcars, lm(mpg ~ am + mtcars[,i]))
                        print(coef(model.test))
                        print(anova(model, model.test))
                        if (anova(model, model.test)[2,6]< 0.05) {
                                print (paste(colnames(mtcars)[i], " is relevant"))
                                relevans <- rbind(relevants, colnames(mtcars)[i])
                                
                        }
                        else
                                print (paste(colnames(mtcars)[i], " is NOT relevant"))
                }
                
        }
        print(relevants)
}

model <- with(mtcars, lm(mpg ~ am))
comparative(model, c("am"))

anova(model, model.Disp)[2,6]
anova(model, model.Cyl)
anova(model, model.Hp)
anova(model, model.Drat) #Pr(>F) 0.0107 *
anova(model, model.Weight)
anova(model, model.Qsec)
anova(model, model.Gear) #Not interesting P > 0.9651



plot(mtcars$am, mtcars$mpg, pch=19)
lines(mtcars$am,model$fitted, col="black", lwd=3) 


modelAutomatic <- with(mtcars, lm(mpg[mtcars$am ==0] ~ am[mtcars$am ==0]))
modelManual <- with(mtcars, lm(mpg[mtcars$am ==1] ~ am[mtcars$am ==1]))

plot(mtcars$am, mtcars$mpg, pch=19)
points(mtcars$am, mtcars$mpg, pch=19, col=((mtcars$am==0)*1+1))
lines(mtcars$am[mtcars$am ==0],modelAutomatic$fitted, col="black", lwd=3) 
lines(mtcars$am[mtcars$am ==1],modelManual$fitted, col="red", lwd=3)

pairs(mtcars)



automatic <- mtcars[mtcars$am==0,]
automatic.Model <- lm(mpg ~ cyl, data = automatic)
summary(automatic.Model)
plot(automatic.Model)

manual <- mtcars[mtcars$am==1,]
manual.Model <- lm(mpg ~ cyl, data = manual)
summary(manual.Model)

mean(automatic$mpg)
summary(automatic$cyl)
str(automatic$cyl)

mean(manual$mpg)
summary(automatic$cyl)