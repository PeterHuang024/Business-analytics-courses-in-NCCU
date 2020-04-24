library(readr)
#1#
data <- read_csv("Womens Clothing E-Commerce Reviews.csv")
library(tm)

#必須要先將資料index中的空格去掉
data_rec <- subset(data, Recommended.IND==1)
data_unrec <- subset(data, Recommended.IND==0)

## Make a vector source and a corpus
x_rec=Corpus(VectorSource(data_rec$Review.Text))
x_unrec=Corpus(VectorSource(data_unrec$Review.Text))

##Clean text
x_rec=tm_map(x_rec, tolower) #convert to lower case
x_rec=tm_map(x_rec, removePunctuation) #remove punctuation
x_unrec=tm_map(x_unrec, tolower) #convert to lower case
x_unrec=tm_map(x_unrec, removePunctuation) #remove punctuation

#Remove stopwords
x_rec=tm_map(x_rec, removeWords, stopwords("english"))
x_unrec=tm_map(x_unrec, removeWords, stopwords("english"))

library(SnowballC)
x_rec=tm_map(x_rec, stemDocument)
x_unrec=tm_map(x_unrec, stemDocument)

x_rec_tdm <- TermDocumentMatrix(x_rec)
x_unrec_tdm <- TermDocumentMatrix(x_unrec)

# Convert TDM to matrix
review_m_rec <- as.matrix(x_rec_tdm)
review_m_unrec <- as.matrix(x_unrec_tdm)

# Sum rows and frequency data frame
freq_df_rec <- rowSums(review_m_rec)
freq_df_unrec <- rowSums(review_m_unrec)

# Sort term_frequency in descending order
freq_df_rec <- sort(freq_df_rec, decreasing = T)
freq_df_unrec <- sort(freq_df_unrec, decreasing = T)

# View the top 10 most common words
freq_df_rec[1:10]
freq_df_unrec[1:10]

#畫出recommend和unrecommend在文字上使用的barplot
par(mfrow = c(1,2)) 
barplot(freq_df_rec[1:15], col = "royalblue", las = 2,main="top 15 most common words from recommender")
barplot(freq_df_unrec[1:15], col = "green", las = 2,main="top 15 most common words from unrecommender")
#可以發現對recommend的資料來說最常出現的是"dress","love","fit","size","wear"...
#可以發現對unrecommend的資料來說最常出現的是"look","dress","like","too","fit"...
#可以發現兩者其實有許多文字重複

freq_df_rec <- data.frame(term = names(freq_df_rec),
                      num = freq_df_rec)
freq_df_unrec <- data.frame(term = names(freq_df_unrec),
                          num = freq_df_unrec)
#畫出文字雲
library(wordcloud2)
par(mfrow = c(1,2)) 
wordcloud2(freq_df_rec,size=0.5)#recommend
wordcloud2(freq_df_unrec,size=0.5)#unrecommend

x_rec_tdm2 <- removeSparseTerms(x_rec_tdm, sparse = 0.9)
mydata_rec <- as.data.frame(as.matrix(x_rec_tdm2))
hc_rec <- hclust(d = dist(mydata_rec, method = "maximum"), method = "complete")
plot(hc_rec)#觀察recommend用詞之間的關聯性：像love,color很靠近，comfort,work一組

x_unrec_tdm2 <- removeSparseTerms(x_unrec_tdm, sparse = 0.9)
mydata_unrec <- as.data.frame(as.matrix(x_unrec_tdm2))
hc_unrec <- hclust(d = dist(mydata_unrec, method = "maximum"), method = "complete")
plot(hc_unrec)#觀察unrecommend用詞之間的關聯性：像nice,return很靠近，fir,larg一組


#2#
text <-c("我們中國的漢字
落筆成畫留下五千年的歷史
         讓世界都認識
         我們中國的漢字
         一撇一捺都是故事
         
         跪舉火把虔誠像道光
         四方田地落穀成倉
         古人象形聲意辨惡良
         魃魈魁鬾魑魅魍魎
         Wu 又双叒叕
         Wu 火炎焱燚
         Wu 水沝淼㵘
         㙓𨰻
         
         煢煢孑立 沆瀣一氣
         踽踽獨行 醍醐灌頂
         綿綿瓜瓞 奉為圭臬
         龍行龘龘 犄角旮旯
         娉婷裊娜 涕泗滂沱
         呶呶不休 不稂不莠
         卬
         咄嗟 蹀躞 耄耋 饕餮
         囹圄 蘡薁 覬覦 齟齬
         狖軛鼯軒 怙惡不悛
         其靁虺虺 腌臢孑孓
         陟罰臧否 針砭時弊
         鱗次櫛比 一張一翕
         
         我們中國的漢字
         落筆成畫留下五千年的歷史
         讓世界都認識
         我們中國的漢字
         一撇一捺都是故事
         
         現在全世界各地
         到處有中國字
         黃皮膚的人驕傲地把頭抬起
         我們中國的漢字
         一平一仄譜寫成詩
         
         煢煢孑立 沆瀣一氣
         踽踽獨行 醍醐灌頂
         綿綿瓜瓞 奉為圭臬
         龍行龘龘 犄角旮旯
         娉婷裊娜 涕泗滂沱
         呶呶不休 不稂不莠
         
         咄嗟 蹀躞 耄耋 饕餮
         囹圄 蘡薁 覬覦 齟齬
         狖軛鼯軒 怙惡不悛
         其靁虺虺 腌臢孑孓
         陟罰臧否 針砭時弊
         鱗次櫛比 一張一翕
         
         我們中國的漢字
         落筆成畫留下五千年的歷史
         讓世界都認識
         我們中國的漢字
         一撇一捺都是故事
         
         現在全世界各地
         到處有中國字
         黃皮膚的人驕傲地把頭抬起
         我們中國的漢字
         一平一仄譜寫成詩
         優美旋律自宮商角徵羽
         眾人皆說成之於語故成語
         ")

library(devtools)
library("jiebaR")
cc<-worker()
cc[text]

keep <- c("中國的","其靁","虺虺","狖軛","五千年的","中國字","譜寫成詩","都是",
          "黃皮膚的","蘡薁","黃皮膚的人","魃魈魁鬾","㙓𨰻","宮商角徵羽","說成",
          "瓜瓞","龘龘","一翕","落穀") #不想拆分開的字
new_user_word(cc, keep) #增加字定義的辭彙
cc[text]

count <-freq(cc[text])  #can also use table(cc[text])
count

str(count)

newd = data.frame(count)
head(newd[order(newd$freq,decreasing = TRUE),],20)
newdd = newd[order(newd$freq,decreasing = TRUE),]

library('wordcloud2')
wordcloud2(newdd,color="random-light",backgroundColor="black",shape='cardioid')