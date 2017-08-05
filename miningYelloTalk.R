##SETUP
setwd("C:/Users/Wutdy/Desktop/datasci/yell/twit")
library(twitteR)
library(ROAuth)

consumer_key = "46BcJBMylYijUHO7XEFiScCJk"
consumer_secret = "eiXO2Jlx2vs1ZtLUUkYF4xU2z24ZDc5boe14zZqPd1SA9NDQDz"
access_token = "77724019-ce5bMI4aE4hYJc5FCluL5xjOmWYXKKR6SGbWKHAXX"
access_secret = "JY0YrViLx4KOITSnSEG4PvxRXIq8j02l17q6Jao15nX4z"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="C:/Users/Wutdy/Desktop/datasci/yell/twit/socialmediacacert.pem")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

cred = OAuthFactory$new(consumerKey=consumer_key, consumerSecret=consumer_secret,
                        requestURL='https://api.twitter.com/oauth/request_token',
                        accessURL='https://api.twitter.com/oauth/access_token',
                        authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

yellotalk.data= searchTwitter("#yellotalk")

rate.limit = getCurRateLimitInfo(c("lists"))
##conversion from list to data frame
yelldata.df = do.call(rbind, lapply(yellotalk.data, as.data.frame))
write.csv(bigdata.df, "C:/Users/Wutdy/Desktop/datasci/yell/twit/yellotalkdata.csv")

##package to create doc-term
library("tm")

yell_list = sapply(yellotalk, function(x) x$getText())
yell_corpus = Corpus(VectorSource(yell_list))
yell_corpus = tm_map(yell_corpus, content_transformer(stringi::stri_trans_tolower))
yell_corpus = tm_map(yell_corpus, removePunctuation)
yell_corpus = tm_map(yell_corpus, 
                        function(x)removeWords(x, stopwords()))

library(wordcloud)
wordcloud(yell_corpus)
yell.tdm = TermDocumentMatrix(yell_corpus)
yell.tdm

#identify terms used at least 10 times
findFreqTerms(yell.tdm, lowfreq = 50)
#find association "people"
findAssocs(yell.tdm, 'buu', 0.50)

#fing another association by Clustering
##remove sparse terms from tdm then convert to data frame
yell2.tdm = removeSparseTerms(yell.tdm, sparse = 0.92)
yell2.df = as.data.frame(as.matrix(yell2.tdm))
##scale the data
yell2.df.scale = scale(yell2.df)
##create distance matrix
yell.dist = dist(yell2.df.scale, method = "euclidean")
#cluster
yell.fit = hclust(yell.dist, method = "ward.D2")
#plot picture
plot(yell.fit, main="Cluster - YelloTalk")
#try k=5
groups = cutree(yell.fit, k=4)
##tell me clustering
rect.hclust(yell.fit, k=4, border = "blue")