#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
install.packages("ROAuth")
library.path <- .libPaths()
library("RCurl", lib.loc = library.path)
library("ROAuth")
install.packages(c('ROAuth','RCurl'))
require('ROAuth')
require('RCurl')
library('RCurl')
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("nH209VGgm8IWM4NCpECFLISFO", # Consumer Key (API Key)
                    "l5RuexnivulKkR064JN6dOFY9f1gRhyCx0D4usfjIIf79VdzCm", #Consumer Secret (API Secret)
                    "720626396-y1SsMdlek0LbeTkjgu1Widx21oGPG2NUAtHIKfVo",  # Access Token
                    "6zjRfiNQ42F6TEtUbUB6H6vyHedFFQvX719I8t8hncNFQ")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('ShashiTharoor', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweetshashi.csv")

getwd()
# 
handleTweets <- searchTwitter('DataScience', n = 10000)
# handleTweetsDF <- twListToDF(handleTweets)
# dim(handleTweetsDF)
# View(handleTweetsDF)
# #handleTweetsMessages <- unique(handleTweetsDF$text)
# #handleTweetsMessages <- as.data.frame(handleTweetsMessages)
# #write.csv(handleTweetsDF, "TefalHandleTweets.csv")
# 
library(rtweet)
