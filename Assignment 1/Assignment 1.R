rm(list=ls())
library(glmnet)
# Split and Prepare Data
# --------------------------------------------------
# Split the Data Into Training and Testing Data
# (50/50):
tecator <- read.csv("tecator.csv")
set.seed(12345)
n = nrow(tecator)
id = sample(1:n, floor(n * 0.5))
train = tecator[id, ]
test = tecator[-id, ]

#=====Fit Linear Regression=====
lm_tecator <- lm(formula=Fat~.-Protein-Moisture-Sample,data=train)
summary(lm_tecator)
train_fitvalues <- lm_tecator$fitted.values
test_predict <- predict(lm_tecator, test)

MSE_train <- mean(lm_tecator$residuals^2)
MSE_train

MSE_test <- mean((test$Fat-test_predict)^2)
MSE_test
`#over fit


#=====Lasso=====
  #Make argument in the format that glmnet can use
  x_name <- colnames(tecator)[-1]
  x_name <- x_name[-102]
  x_name <- x_name[-102]
  x_name <- x_name[-101]
  x <- data.matrix(train[, x_name])
  y <- train$Fat
  #Train Lasso 
  lasso_tecator <- glmnet(x=x,y=y, alpha = 1)
  lasso_tecator
  plot(lasso_tecator,xvar="lambda")
  
  #pick lamda for 3 features
  coef_matrix <- as.matrix(coef(lasso_tecator))
  coef_matrix <- coef_matrix!=0
  coef_matrix <- colSums(coef_matrix)
  lamda_index <- which(coef_matrix == 4) # we use 4 because there is alway intercept with in the matric
  lasso_tecator$lambda[lamda_index] # the lamda for 3 features
  
  
  
#=====Ridge======


ridge_tecator <- glmnet(x, y, alpha = 0)
plot(ridge_tecator,xvar="lambda",label = TRUE)
  

#=====CV lasso=====
cv_lasso_tecator <- cv.glmnet(x=x,y=y, alpha = 1)
cv_lasso_tecator
plot(cv_lasso_tecator)


best_lambda <- cv_lasso_tecator$lambda.min
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
variables <- coef(best_model)
variables <- as.vector(variables!= 0)
coef_index <- which(match(variables,TRUE) == 1)
coef_index <- coef_index[-1]
coef_index
rownames(coef(best_model))[coef_index]


variables
coef(best_model)
predict(best_model, s = best_lambda, newx = data.matrix(train[, x_name]))

y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq



#====lasso example=====
#define response variable
y1 <- mtcars$hp

#define matrix of predictor variables
x1 <- data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x1, y1, alpha = 1)

#find optimal lambda value that minimizes test MSE
cv_lasso_tecator <- cv.glmnet(x=x,y=y, alpha = 1)

plot(cv_lasso_tecator)

best_lambda <- cv_lasso_tecator$lambda.min
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
variables <- coef(best_model)
variables <- as.vector(variables!= 0)
coef_index <- which(match(variables,TRUE) == 1)
coef_index <- coef_index[-1]

lambda_get_select <- rownames(coef(best_model))[coef_index]

predict(best_model,test)

#produce plot of test MSE by lambda value
plot(cv_model) 

best_cv_model <- glmnet(x1, y1, alpha = 1, lambda = best_cv_lambda)
coef(best_model)

#define new observation
new = matrix(c(24, 2.5, 3.5, 18.5), nrow=1, ncol=4) 

#use lasso regression model to predict response value
predict(best_cv_model, s = best_cv_lambda, newx = new)


y_predicted <- predict(best_cv_model, s = best_cv_lambda, newx = x1)

#find SST and SSE
sst <- sum((y1 - mean(y))^2)
sse <- sum((y_predicted - y1)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq


predict(ridge_tecator,train)

