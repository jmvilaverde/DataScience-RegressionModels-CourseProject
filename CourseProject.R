####http://www.jerrydallal.com/lhsp/reginter.htm


init <- function(){
        #Load data
        data(mtcars)
        
        ?mtcars
        
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






