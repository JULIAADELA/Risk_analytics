library(readxl)
bankloan <- read_excel("C:/Users/ADMIN/Downloads/bankloans.xlsx")
View(bankloan)
library(tidyverse)
library(dplyr)

summary(bankloan)

#### 1. xem biến trạng thái defualt
data <- bankloan %>% dplyr::rename(default = default)
table(data$default)
round(prop.table(table(data$default)),3)

#### 3. Check and remove missing value
# check NA
colSums(is.na(data))
# xoa bo cac gia tri missing
data.rm <- na.omit(data)
sum(is.na(data.rm))


#### 2. Exploratory Data Analysis
# Data Overview
ggplot(data.rm  , aes(default, fill = default)) + geom_bar() +theme(legend.position = 'none')

barplot(table(data.rm$ed), main = "ed")

par(mfrow = c(2,3))
boxplot(data.rm$ed, main = "ed")
boxplot(data.rm$income, main = "income")
boxplot(data.rm$employ, main = "employ")
boxplot(data.rm$default, main = "default")





#### 4. Chia tập dữ liệu train và test:
# train 70% - test 30%
set.seed(2000)
ind <- sample(2, nrow(data.rm), replace = TRUE, prob = c(0.7, 0.3))

train.data <- data.rm[ind==1,]
train.data

test.data <- data.rm[ind==2,]
test.data

#### 5. Sử dụng mô hình Logit trong chấm điểm tín dụng:
# * Tính chỉ số IV của các biến trên tập train.data
library('Information')
library('ROSE')


# Tính chỉ số IV của các biến trên tập train.data
IV <- Information::create_infotables(data = data.rm, y = "default", parallel = FALSE)
print(IV$Summary)

# Loại các biến có IV nhỏ hơn 0.02
vars_removed <- IV$Summary %>% as.data.frame %>%
subset(IV < 0.02) %>% pull(1)
vars_removed
train.data_removed<- train.data %>% dplyr::select(-all_of(vars_removed))
train.data_removed
####   dừng tại đây để nhập 1: 6. Bin các biến theo WOE:
library(scorecard)
bins <- woebin(train.data_removed, y = "default")
woebin_plot(bins)

# chuyen du lieu sang WOE:
train.data_woe <- woebin_ply(train.data_removed, bins)
head(train.data_woe)

#### 7. Chạy mô hình logit cho tập train cho dữ liệu ban đầu
logit.model <- glm(default ~., family = binomial(link = 'logit'), data = train.data_removed)
summary(logit.model)

#### 8. Loc bien theo stepwise
train.step <- step(logit.model, direction = "backward", trace = 0)
summary(train.step)

#### 9. Kiểm tra mô hình trên tập train cho mô hình logit với dữ liệu ban đầu
train.prob <- predict(train.step, type = "response")
train.pred <- ifelse(train.prob > .5, "1", "0")
table(train.pred, train.data$default)


#### 10. Thực hiện mô hình trên tập test cho mô hình logit với dữ liệu ban đầu
logit.pred.prob<- predict(train.step, test.data, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
test.data$default <- as.factor(test.data$default)
### validation
caret::confusionMatrix(logit.pred, test.data$default, positive = "1")     ###1: default, 0: non_default

#### 11. Chạy mô hình logit cho tập train cho dữ liệu đã binning theo WOE
logit.model_woe <- glm(default ~., family = binomial(link = 'logit'), data = train.data_woe)
summary(logit.model_woe)

#### 12. Loc bien theo stepwise
logit.model.step_woe <- step(logit.model_woe, direction = "backward", trace = 0)
summary(logit.model.step_woe)

#### 13.Kiểm tra mô hình trên tập train cho mô hình logit với dữ liệu đã binning theo WOE
train.prob <- predict(logit.model.step_woe, type = "response")
train.pred <- ifelse(train.prob > .5, '1','0')
table(train.pred, train.data_removed$default)

#### 14. Thực hiện mô hình trên tập test:
test.data_woe <- woebin_ply(test.data, bins)
# Thực hiện mô hình trên tập test:
logit.pred.prob_woe <- predict(logit.model.step_woe, test.data_woe, type = 'response')
logit.pred_woe <- as.factor(ifelse(logit.pred.prob_woe > 0.5, 1, 0))
test.data_woe$default <- as.factor(test.data_woe$default)
### validation
caret::confusionMatrix(logit.pred_woe, test.data_woe$default, positive = "1")




#### 15. Thực hiện tính score:
# Calculate scorecard scores for variables based on the results from woebin and glm: 
my_card <- scorecard(bins, train.step, points0 = 700, odds0 = 1/4, pdo = 50)
head(my_card)

#### SHOW RESULT
# Calculate scorecard scores
z<-log(logit.pred.prob_woe/(1-logit.pred.prob_woe))
head(z,10)

# Scalling: Credit score = offset + factor * log(odds)
# Tại mức điểm 100 tương ứng với odds=50:1 và 120 tưng ứng với odds = 100:1
# factor = 20/ln(2)   = 28,85390082
# offset = 700-28,854*ln(1/50) = 
n=12
# score = (0.5*0.15+1/12)*28.854+offset/12
credit_score <-28.85-12.87*z

hist(credit_score)
head(credit_score,10)
head(z,10)
write.csv(credit_score,"credit_score.csv")
write.csv(z,"z_score.csv")







library(outliers)
chisq.out.test(data.rm$default)

test <- grubbs.test(data.rm$default)
test

test <- grubbs.test(data.rm$default, opposite = TRUE)
test

test <- grubbs.test(data.rm$ed, opposite = TRUE)
test

test <- dixon.test(data.rm$ed,
                   opposite = TRUE)
test
grubbs.test(data.rm$ed,opposite = TRUE)

grubbs.test(data.rm$income,opposite = TRUE)
grubbs.test(data.rm$employ,opposite = TRUE)


library(ROCR)
as.data.frame(IV$Tables$ed ) %>% arrange(desc(as.numeric(.$WOE)))
as.data.frame(IV$Tables$age) %>% arrange(desc(as.numeric(.$WOE)))
as.data.frame(IV$Tables$employ ) %>% arrange(desc(as.numeric(.$WOE)))


model <- train(LOST_NUMERIC~ fage+fed+femploy+fincome+faddress+fcreddebt+fothdebt,
               data = train.data, method ='glm',family = "binomial")
p <- predict(model, newdata = train.data_woe)
pr <- prediction(p,train.data_woe$LOST_NUMERIC)
prf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
