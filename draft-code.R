library(tidyverse)
library(glmnet)
library(tree)
library(caret)



# A1 ----------------------------------------------------------------------


tecator <- read.csv("./data/tecator.csv")

tecator %>% glimpse()

n <- nrow(tecator)
set.seed(12345) 
id <- sample(1:n, floor(n * 0.5)) 
train_data <- tecator[id, ] 
test_data <- tecator[-id, ] 


## 1-1

lm_model <- lm(Fat~ . - Sample - Protein - Moisture, train_data)

y_hat_train <- lm_model$fitted.values
MSE_train <- mean(lm_model$residuals^2)
MSE_train
y_hat_test <- predict(lm_model, test_data)
MSE_test <- mean((y_hat_test - test_data$Fat)^2)
MSE_test
# the model overfitted on train dataset


# 1-2

# Cost + L1 norm of coefs

# 1-3
features <- as.matrix(train_data[,-c(1, 102, 103, 104)])
lasso_model <- glmnet(x = features, y = train_data$Fat, family = "gaussian", alpha = 1)

plot(lasso_model, "lambda", TRUE)


fig_df <- data.frame(t(as.matrix(coef(lasso_model))))
fig <- fig_df %>% 
  mutate(lambda = log(lasso_model$lambda)) %>%
  reshape2::melt("lambda") %>%
  filter(variable != "X.Intercept.") %>%
  ggplot(aes(lambda, value, group = variable, color = variable)) +geom_line() + labs(x = "log(lambda)", y = "Coefficient")
fig + guides(color="none")
# plotly::ggplotly(fig)

# NO CASE WITH 3 FEATURES! except if we ignore intercept

idx <- which(((fig_df != 0) %>% rowSums()) == 4)
lasso_model$lambda[idx]


# 1-4

features <- as.matrix(train_data[,-c(1, 102, 103, 104)])
ridge_model <- glmnet(x = features, y = train_data$Fat, family = "gaussian", alpha = 0)

plot(ridge_model, "lambda", TRUE)


fig_df <- data.frame(t(as.matrix(coef(ridge_model))))
fig <- fig_df %>% mutate(lambda = log(ridge_model$lambda)) %>% reshape2::melt("lambda") %>% filter(variable != "X.Intercept.") %>% 
  ggplot(aes(lambda, value, group = variable, color = variable)) +geom_line()+ labs(x = "log(lambda)", y = "Coefficient")
fig + guides(color="none")
# plotly::ggplotly(fig)

# NO CASE WITH 3 FEATURES!

idx <- which(((fig_df != 0) %>% rowSums()) == 4)
ridge_model$lambda[idx]


# 1-5

cv_model <- cv.glmnet(x = features, y = train_data$Fat, alpha = 1)

cv_model$lambda.min
coef(cv_model, s = "lambda.min") # 10 (with intercept)


final_model <- glmnet(x = features, y = train_data$Fat, alpha = 1, lambda = cv_model$lambda.min)

y_hat <- predict(final_model, as.matrix(test_data[-c(1, 102, 103, 104)]))
plot(test_data$Fat, y_hat)

mean((test_data$Fat- y_hat)^2)





# A2 ----------------------------------------------------------------------

# 2-1
bank_full <- read.csv2("./data/bank-full.csv")
bank_full <- bank_full %>%
  select(!duration) %>%
  mutate(across(where(is.character), as.factor))

n <- nrow(bank_full)
set.seed(12345)
train_idx <- sample(seq_len(n), floor(n * 0.4))
train_data <- bank_full[train_idx, ]
remainder_idx <- setdiff(seq_len(n), train_idx)
set.seed(12345)
valid_idx <- sample(remainder_idx, floor(n * 0.3))
valid_data <- bank_full[valid_idx, ]
test_idx <- setdiff(remainder_idx, valid_idx)
test_data <- bank_full[test_idx, ]



# 2-2 
#a
tree_default <- tree(y~., train_data)
y_hat_train <- predict(tree_default, train_data, type = "class")
cm_train <- table(train_data$y, y_hat_train)
1- (sum(diag(cm_train))/nrow(train_data))
# mean(y_hat_train != train_data$y)
y_hat_train <- predict(tree_default, valid_data, type = "class")
cm_valid <- table(valid_data$y, y_hat_train)
1- (sum(diag(cm_valid))/nrow(valid_data))
plot(tree_default)
text(tree_default)
summary(tree_default)$size
#b
tree_minsize <- tree(y~., train_data, control = tree.control(nrow(train_data), minsize = 7000))
y_hat_train <- predict(tree_minsize, train_data, type = "class")
cm_train <- table(train_data$y, y_hat_train)
1- (sum(diag(cm_train))/nrow(train_data))
# mean(y_hat_train != train_data$y)
y_hat_train <- predict(tree_minsize, valid_data, type = "class")
cm_valid <- table(valid_data$y, y_hat_train)
1- (sum(diag(cm_valid))/nrow(valid_data))
plot(tree_minsize)
text(tree_minsize, pretty = 0)
summary(tree_minsize)$size

#c
  tree_mindev <- tree(y~., train_data, control = tree.control(nrow(train_data), mindev = 0.0005))
  y_hat_train <- predict(tree_mindev, train_data, type = "class")
  cm_train <- table(train_data$y, y_hat_train)
  1- (sum(diag(cm_train))/nrow(train_data))
  # mean(y_hat_train != train_data$y)
  y_hat_train <- predict(tree_mindev, valid_data, type = "class")
  cm_valid <- table(valid_data$y, y_hat_train)
  1- (sum(diag(cm_valid))/nrow(valid_data))
  
  plot(tree_mindev)
  # text(tree_mindev)
  summary(tree_mindev)$size


# 2-3

train_dev <- vector("numeric", 50)
valid_dev <- vector("numeric", 50)

for(num_leaves in 2:50){
  prune_tree <- prune.tree(tree_mindev, best = num_leaves)
  valid_tree <- predict(prune_tree, newdata = valid_data, type = "tree")
  train_dev[num_leaves] <- deviance(prune_tree)
  valid_dev[num_leaves] <- deviance(valid_tree)
}

data.frame("num_of_leaves" = 2:50, "train" = train_dev[2:50], "valid" = valid_dev[2:50]) %>%
  ggplot() +
  geom_line(aes(num_of_leaves, train, color = "Train Deviance")) +
  geom_point(aes(num_of_leaves, train, color = "Train Deviance")) +
  geom_line(aes(num_of_leaves, valid, color = "Validation Deviance")) +
  geom_point(aes(num_of_leaves, valid, color = "Validation Deviance"))


best_num_leaves <- which.min(valid_dev[2:50]) + 1

optimal_tree <- prune.tree(tree_mindev, best = best_num_leaves)
optimal_tree
plot(optimal_tree, type = "uniform")
text(optimal_tree)

# 2-4
y_hat <- predict(optimal_tree, newdata = test_data, type = "class")

cm_test <- table(test_data$y, y_hat)
sum(diag(cm_test))/nrow(test_data)

TP <- cm_test[2, 2]
FP <- cm_test[1, 2]
FN <- cm_test[2, 1]

F1_score <- TP / (TP + 0.5 * (FP + FN))
F1_score

# 2-5

# optim_tree_loss <- tree(y~., train_data, weights = c(0,5,1,0))
optim_tree_loss <- prune.tree(tree_mindev, best = 21, loss = matrix(c(0, 5, 1, 0), nrow = 2))
# y_hat <- predict(optim_tree_loss, newdata = test_data, type = "class")

# train_dev <- vector("numeric", 50)
# valid_dev <- vector("numeric", 50)
# 
# for(num_leaves in 2:50){
#   prune_tree <- prune.tree(tree_mindev, best = num_leaves, loss = matrix(c(100,1,5,10), nrow = 2))
#   valid_tree <- predict(prune_tree, newdata = valid_data, type = "tree")
#   train_dev[num_leaves] <- deviance(prune_tree)
#   valid_dev[num_leaves] <- deviance(valid_tree)
# }
# 
# data.frame("num_of_leaves" = 2:50, "train" = train_dev[2:50], "valid" = valid_dev[2:50]) %>%
#   ggplot() +
#   geom_line(aes(num_of_leaves, train, color = "Train Deviance")) +
#   geom_point(aes(num_of_leaves, train, color = "Train Deviance")) +
#   geom_line(aes(num_of_leaves, valid, color = "Validation Deviance")) +
#   geom_point(aes(num_of_leaves, valid, color = "Validation Deviance"))
# 
# 
# best_num_leaves <- which.min(valid_dev[2:50])
# 

# cm_test <- table(test_data$y, y_hat)
# sum(diag(cm_test))/nrow(test_data)
# 
# TP <- cm_test[2, 2]
# FP <- cm_test[1, 2]
# FN <- cm_test[2, 1]
# 
# F1_score <- TP / (TP + 0.5 * (FP + FN))
# F1_score

#working
y_hat <- predict(optimal_tree, newdata = test_data)

y_hat <- as.factor(ifelse(y_hat[,2] / y_hat[,1] > 1/5, 1, 0))

#second way
# bestI <- apply(y_hat %*% matrix(c(0, 5, 1, 0), nrow = 2), MARGIN=1, FUN = which.min)
# y_hat <- levels(test_data$y)[bestI]

cm_test <- table(test_data$y, y_hat)
sum(diag(cm_test))/nrow(test_data)

TP <- cm_test[2, 2]
FP <- cm_test[1, 2]
FN <- cm_test[2, 1]

F1_score <- TP / (TP + 0.5 * (FP + FN))
F1_score

# 2-6
y_hat_prob_tree <- predict(optimal_tree, test_data)
y_hat_prob_tree <- y_hat_prob_tree[, "yes"]

glm_model <- glm(y~., family = "binomial",  data = train_data)
y_hat_prob_glm <- predict(glm_model, test_data, type = "response")

pis <- seq(0.05, 0.95, by =0.05)

ROC_tree <- matrix(nrow = 19, ncol = 2)
ROC_glm <- matrix(nrow = 19, ncol = 2)

for(i in seq_along(pis)){
  
  y_hat_tree <- factor(ifelse(y_hat_prob_tree > pis[i], "yes", "no"), levels = c("no", "yes"))
  cm_test <- table(test_data$y, y_hat_tree)
  TP <- cm_test[2, 2]
  FP <- cm_test[1, 2]
  ROC_tree[i, 1] <- FP / sum(cm_test[1,])
  ROC_tree[i, 2] <- TP / sum(cm_test[2,])
  
  y_hat_glm <- factor(ifelse(y_hat_prob_glm > pis[i], "yes", "no"), levels = c("no", "yes"))
  cm_test <- table(test_data$y, y_hat_glm)
  TP <- cm_test[2, 2]
  FP <- cm_test[1, 2]
  ROC_glm[i, 1] <- FP / sum(cm_test[1,])
  ROC_glm[i, 2] <- TP / sum(cm_test[2,])
  
}
plot(ROC_tree, type = "l", col = "blue")
lines(ROC_glm, col = "red")


ROC_tree <- matrix(nrow = 19, ncol = 2)
ROC_glm <- matrix(nrow = 19, ncol = 2)

for(i in seq_along(pis)){
  
  y_hat_tree <- factor(ifelse(y_hat_prob_tree > pis[i], "yes", "no"), levels = c("no", "yes"))
  cm_test <- table(test_data$y, y_hat_tree)
  TP <- cm_test[2, 2]
  FP <- cm_test[1, 2]
  ROC_tree[i, 1] <- TP / sum(cm_test[2,])
  ROC_tree[i, 2] <- TP / (TP + FP)
  
  y_hat_glm <- factor(ifelse(y_hat_prob_glm > pis[i], "yes", "no"), levels = c("no", "yes"))
  cm_test <- table(test_data$y, y_hat_glm)
  TP <- cm_test[2, 2]
  FP <- cm_test[1, 2]
  ROC_glm[i, 1] <- TP / sum(cm_test[2,])
  ROC_glm[i, 2] <- TP / (TP + FP)
  
}
plot(ROC_tree, type = "l", col = "blue", ylim = c(0, 0.8), xlim = c(0, 1))
lines(ROC_glm, col = "red")


# A3 ----------------------------------------------------------------------

# 3-1

communities <- read.csv("./data/communities.csv")

X_matrix <- communities[1:100]

scaler <- preProcess(X_matrix)
X_matrix <- predict(scaler, X_matrix)
X_matrix <- scale(X_matrix)
cov_matrix <- cov(X_matrix)

eigen_results <- eigen(cov_matrix)

lambdas <- eigen_results$values

var_explained <- cumsum(lambdas/sum(lambdas) *100)

which(var_explained >= 95)[1]

lambdas[1:2]


# 3-2

PCA_results <- princomp(X_matrix)
first_comp <- PCA_results$loadings[, 1]
plot(first_comp)
sort(abs(PCA_results$loadings[,1]), decreasing = T)[1:5]



# fig_data <-
data.frame(
  PC1 = PCA_results$scores[, 1],
  PC2 = PCA_results$scores[, 2],
  ViolentCrimesPerPop = communities$ViolentCrimesPerPop) %>% 
  ggplot() +
  geom_point(aes(PC1, PC2, color = ViolentCrimesPerPop))

# 3-3
n <- nrow(communities)
set.seed(12345) 
id <- sample(1:n, floor( n * 0.5)) 
train_data <- communities[id, ] 
test_data <- communities[-id, ]

scaler <- preProcess(train_data[,-101])
train_data <- predict(scaler, train_data)
test_data <- predict(scaler, test_data)


lm_model <- lm(ViolentCrimesPerPop~., train_data)
lm_model <- lm(ViolentCrimesPerPop~ . - 1, train_data)

train_error <- mean(lm_model$residuals^2)
y_hat <- predict(lm_model, test_data)
test_error <- mean((y_hat - test_data$ViolentCrimesPerPop)^2)


# 3-4

train_error <- list()
test_error <- list()
counter <- 0
optim_function <- function(x, y, new_data){
  train_error <- list()
  test_error <- list()
  counter <- 0
  cost_function <- function(theta) {
    x <- as.matrix(x)
    y <- as.matrix(y)
    new_data <- as.matrix(new_data)
    counter <<- counter + 1
    mse_train <- mean((y - x %*% theta)^2)
    train_error[[counter]] <<- mse_train
    test_error[[counter]] <<-  mean((new_data[,101] - new_data[,1:100] %*% theta)^2)
    return(mse_train)
  }
}

cost_with_trace <- optim_function( x = train_data[1:100], y = train_data$ViolentCrimesPerPop, test_data)
result <- optim(rep(0, 100),  cost_with_trace, method = "BFGS")
environment(cost_with_trace)$counter
environment(cost_with_trace)$train_error %>% unlist() %>% plot()
plot(log(unlist(environment(cost_with_trace)$train_error)[-(1:500)]), type = "l")
plot(log(unlist(environment(cost_with_trace)$test_error)[-(1:500)]), type = "l")



train_error <- list()
test_error <- list()
counter <- 0

cost_function <- function(theta, x, y, x_new, y_new) {
  counter <<- counter + 1
  mse_train <- mean((y - x %*% theta)^2)
  train_error[[counter]] <<- mse_train
  test_error[[counter]] <<-  mean((y_new - x_new %*% theta)^2)
  return(mse_train)
}
result <- optim(rep(0, 100), cost_function, x = as.matrix(train_data[1:100]),
                y = train_data$ViolentCrimesPerPop,
                x_new = as.matrix(test_data[1:100]),
                y_new = test_data$ViolentCrimesPerPop, method = "BFGS")

train_error <- unlist(train_error)
test_error <- unlist(test_error)
plot(train_error[-(1:2000)], type ="l")
plot(test_error[-(1:2000)], type ="l")

best_it <- which.min(test_error)
train_error[best_it]
test_error[best_it]
