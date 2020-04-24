library(tidyverse)
library(broom)
library(car)
library(MASS)
dat <- read.csv("dataset_Facebook.csv", sep = ";")
str(dat)
dat1 <- dat[,c(1:7,15)]
str(dat1)
par(mfrow = c(2,4))
for(i in 1:7){
       plot(dat1[,i],dat1[,8])
}
#畫圖，用type分類來看pages.total.like跟lifetime.people.的關係
ggplot(dat1, aes(x=dat1[,1], y=dat1[,8],color=Type))+
  geom_point(shape=1)+
  scale_color_hue(l=50)+
  geom_smooth(method=lm, se=FALSE)

#利用category分組求和
datc1 <- dat1 %>%
  group_by(Category) %>%
  summarise(Total = n())
ggplot(datc1, aes(x=Category, y=Total))+
  geom_bar(stat="identity")+
  geom_text(stat="identity", aes(label=Total), vjust=1, color=I("pink"), size=6)

ggplot(dat1, aes(x=Category, fill=Type))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..)))+
  ylab("")

lm <- lm(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post~.,data = dat1)
summary(lm)

vif(lm)#通常找VIF<10

#檢查相關係數，當相關係數過高時，可能有共線性的問題
cor(dat1[,c(-2,-7,-8)])

#stepwise method篩選變數
stepAIC(lm, direction = "both")$anova
#由結果得知應選擇Page.total.likes, Post Weekday, Category, Post.Hour

lm1 <- lm(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post~.,
          data = dat1[,c(-2,-4,-7)])
#使用到的參數有Page.total.likes, Post Weekday, Category, Post.Hour
#y為Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post
#利用上面4個參數所配置的線性迴歸較能對應y值
summary(lm1)$coefficients

stepAIC(lm1, direction = "both")$anova
#由stepwise method 檢查得知lm1的參數皆為需要的

plot(hatvalues(lm1))
#Leverage statistics槓桿值

summary(lm1)
#adjusted R-squared低，不是很好的模型
#p-value較高，參數顯著性低
#或許可以加入發文內容、好友人數等參數來建立更適合的模型