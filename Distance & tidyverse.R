#Homework1#
#1....
n <- nrow(iris)
D <- matrix(0,n,n)
data <- iris[,-5]
for(i in 2:n){
  for(j in 1:(i-1)){
    D[i,j] <- (sum((data[i,]-data[j,])^2))^0.5
    D[j,i] <- D[i,j]
    diag(D)=0
  }
}
#2.....
#(1)
library(tidyverse)
watch.table <- read_csv("watch_table.csv")
user.table <- read_csv("user_table.csv")
drama.table <- read_csv("drama_table.csv")
watch.table %>% left_join(user.table) ->full.table
drama.table %>% left_join(full.table) ->full.table

#(2)
full.table %>%
  + group_by(drama_name,gender)%>%
  + summarise(count = n())
#使用ios的用戶平均年齡：27.9歲

#(3)
full.table %>%
  + filter(device == "iOS") %>%
  + summarise(平均年齡= mean(age))
#使用ios的用戶性別：女性11位、男性6位

#(4)
full.table %>%
  + filter(location == "Taipei" & gender == "male") %>%
  + group_by(device) %>%
  + summarise(ave_payment = mean(payment))
#台北男性平均payment:Android、Chrome、IE＝0, ios = 0.5

full.table %>%
  + filter(location == "Taipei" & gender == "male") %>%
  + group_by(drama_name) %>%
  + summarise(觀看次數 = n())
#台北男性各drama觀看次數：
#Bromance: 2
#Descendants of the sun: 5
#Form 5 to 9: 6
#She was pretty: 1
#The Flame's Daughter: 3