PlayList -> player
InjuryRecord -> injury
head(player)
head(injury)
library(dplyr)

injury.1 <- injury %>%
  # 첫 번째 행을 추출
  slice(1) %>%
  # 행을 벡터로 변환하여 열 이름으로 사용
  unlist() %>%
  # 원래 데이터프레임에 적용
  { colnames(injury) <- .; injury[-1, ] }
head(injury.1)

merge_pi <- merge(player, injury.1, by = "PlayerKey")
table(merge_pi$PlayerKey)
head(merge_pi)
ncol(merge_pi)
nrow(merge_pi)
table(merge_pi$BodyPart)
head(merge_pi)
View(merge_pi)

head(merge_pi)
df <- merge_pi[c(-1, -2, -3, -5, -6, -9, -14, -15, -16, -19, -20, -21, -22)]
head(df)
ncol(df)
df <- data.frame(df)
df$RosterPosition <- as.numeric(factor((df$RosterPosition)))
df$StadiumType <- as.numeric(factor((df$StadiumType)))
df$FieldType <- as.numeric(factor((df$FieldType)))
df$Weather <- as.numeric(factor((df$Weather)))
df$PlayType <- as.numeric(factor((df$PlayType)))
df$Position <- as.numeric(factor((df$Position)))
df$BodyPart <- as.numeric(factor((df$BodyPart)))
df$Surface <- as.numeric(factor((df$Surface)))
head(df)

df_s <- scale(df[-8])
head(df_s)
set.seed(614)
sel <- sample(1:nrow(df), 0.8*nrow(df))
train <- df_s[sel, ]
test <- df_s[-sel, ]
train.label <- df[sel, ]
test.label <- df[-sel, ]
nrow(train)
nrow(test)

train <- data.frame(train)
test <- data.frame(test)
colnames(train)
colnames(test)
train
train$BodyPart <- factor(train.label$BodyPart)
test$BodyPart <- factor(test.label$BodyPart)
install.packages('ranger')
install.packages("vip")
library(ranger)
library(vip)
rf <- ranger(BodyPart~. ,data = train, importance="impurity")

pred <- predict(rf, test)
pred <- data.frame(pred)

vip(rf)

library(caret)
confusionMatrix(pred$prediction, factor(test$BodyPart))

# accuracy = 0.8433, kappa= 0.71
  
ratio<-table(merge_pi$BodyPart, merge_pi$Position)  
table(ratio)
