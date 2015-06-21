####http://www.jerrydallal.com/lhsp/reginter.htm


init <- function(){
        #Load data
        data(mtcars)
        
        ?mtcars
        
#         [, 1]  mpg	 Miles/(US) gallon
#         [, 2]	 cyl	 Number of cylinders
#         [, 3]	 disp	 Displacement (cu.in.)
#         [, 4]	 hp	 Gross horsepower
#         [, 5]	 drat	 Rear axle ratio
#         [, 6]	 wt	 Weight (lb/1000)
#         [, 7]	 qsec	 1/4 mile time
#         [, 8]	 vs	 V/S
#         [, 9]	 am	 Transmission (0 = automatic, 1 = manual)
#         [,10]	 gear	 Number of forward gears
#         [,11]	 carb	 Number of carburetors
        
        
        #Show colnames of mtcars
        names(mtcars)
        
        #Show structure of mtcars
        str(mtcars)
        
        #try to find first correlations
        pairs(mtcars, panel=panel.smooth, main="mtcars data")        
        
        
        library(dplyr)
        
        with(mtcars,aggregate(mpg, list(c(am)), mean))
        
        
        
        #Create initial model mpg vs. am
        model <<- with(mtcars, lm(mpg ~ am))   
        with(mtcars,aggregate(mpg, list(c(am)), mean))
        summary(model)$coef[1,1]
        summary(model)$coef[1,1]+summary(model)$coef[2,1]
        
        print("The mean of mpg is higher in vehicles with manual transmission")
        summary(model)$coef        

        

        summary(model)
        
        
        dfbetas(model)
        plot(predict(model), resid(model))
        #Do comparative
        comparative(model, c("am"), 0.001)
}

#Try to identify necessary fields
#Adding 1 value
comparative <- function(model, exception, range){
        relevants <- c("")
        print(coef(model))
        for (i in 2:11){
                if (!(colnames(mtcars)[i] %in% exception)){
                        print("----------------------------------------------------------------------------------")
                        print(colnames(mtcars)[i])
                        model.test <- with(mtcars, lm(mpg ~ am + mtcars[,i]))
                        print(coef(model.test))
                        print(anova(model, model.test)) #anova reveals variance relation between models
                        if (anova(model, model.test)[2,6]< range) {
                                print (paste(colnames(mtcars)[i], " is relevant"))
                                relevants <- cbind(relevants, colnames(mtcars)[i])
                                
                        }
                        else
                                print (paste(colnames(mtcars)[i], " is NOT relevant"))
                }
                
        }
        print(paste("Relevant to interact with ",exception," are these:",relevants))
}

step(lm(mpg~., data=mtcars))

summary(lm(formula = mpg ~ wt + qsec + am, data = mtcars))

with(mtcars, plot(mpg ~ .))

library(car)
model.Incremental <- lm(mpg ~ . - disp, data = mtcars)
vif(model.Incremental)

library(car)
model.Incremental <- lm(mpg ~ . - disp - cyl - hp, data = mtcars)
vif(model.Incremental)

library(car)
model.Incremental <- lm(mpg ~ . - disp - cyl - hp - wt, data = mtcars)
vif(model.Incremental)

library(car)
model.Incremental <- lm(mpg ~ . - disp - cyl - hp - wt - gear, data = mtcars)
vif(model.Incremental)

library(car)
model.Incremental <- lm(mpg ~ . - disp - cyl - hp - wt - gear - vs, data = mtcars)
vif(model.Incremental)

library(car)
model.Incremental <- lm(mpg ~ . - disp - cyl - hp - wt - gear - vs, data = mtcars)
vif(model.Incremental)


plot(mtcars)
