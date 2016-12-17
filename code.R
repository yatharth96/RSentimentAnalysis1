library(twitteR)
library(httr)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)

consumer_key = "TiSNgNuLqwJ6zizM6uuC1ggOg"
access_token = "443542132-SM0FVcmbabPyTFmf8Fi0BfCCfV5Nbyblm39hT2sB"
consumer_token = "qrwe5VNUevtrU0t9lvlv0z6w9J97PNosHQ9O8Orn9yFf0EdsYZ"
acess_secret = "3fEcym0sP3UtnhkhIVvgEGf5zbC646S9B1dDSaFL9doi2"

setup_twitter_oauth(consumer_key,consumer_token,access_token,acess_secret)


trump = searchTwitter("Donald Trump+@realDonaldTrump",n = 400 ,lang = "en")
clinton = searchTwitter("Hillary Clinton + @HillaryClinton",n = 400 ,lang = "en")
trumpdf = twListToDF(trump)
clintondf = twListToDF(clinton)

trumpdf = select(trumpdf,text)
clintondf = select(clintondf,text)

trump_unique = as.data.frame(trumpdf[!duplicated(trumpdf),],stringsAsFactors = F)
clinton_unique = as.data.frame(clintondf[!duplicated(clintondf),],stringsAsFactors = F)
names(trump_unique) =  c("text")
names(clinton_unique) =  c("text")


trump_unique$text = gsub("(@|$|#)\\w+","",trump_unique$text)
trump_unique$text = gsub("http\\S+","",trump_unique$text)
trump_unique$text = gsub("RT","",trump_unique$text)
trump_unique$text = gsub("[[:punct:]]","",trump_unique$text)
trump_unique$text = gsub("\\d+\\w+","",trump_unique$text)
#trump_unique$text = gsub("[.,/-_'-]","",trump_unique$text)

clinton_unique$text = gsub("(@|$|#)\\w+","",clinton_unique$text)
clinton_unique$text = gsub("http\\S+","",clinton_unique$text)
clinton_unique$text = gsub("RT","",clinton_unique$text)
clinton_unique$text = gsub("[[:punct:]]","",clinton_unique$text)
clinton_unique$text = gsub("\\d+\\w+","",clinton_unique$text)
#clinton_unique$text = gsub("[.,/-_'-]","",clinton_unique$text)


## Sentiment Analysis
# Trump
trump_unique$language = "en"
trump_unique$id = seq.int(nrow(trump_unique))
trump_request = trump_unique[c(2,3,1)]
trump_size = nrow(trump_request)

trump_request_json = toJSON(list(documents = trump_request))
response = POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment",
                body = trump_request_json,
                add_headers(.headers = c("Content-Type"="application/json",
                "Ocp-Apim-Subscription-Key"="5155d7c8316641f9986d24dc562da8df")))
content_trump = content(response)
output_trump = data.frame(matrix(unlist(content_trump),nrow = trump_size,byrow = T),stringsAsFactors = F)
names(output_trump) = c("Sentiment_Score","ID")
output_trump$tweets = as.factor("Trump")
output_trump$Sentiment_Score = as.numeric(output_trump$Sentiment_Score)


# Clinton
clinton_unique$language = "en"
clinton_unique$id = seq.int(nrow(clinton_unique))
clinton_request = clinton_unique[c(2,3,1)]
clinton_size = nrow(clinton_request)

clinton_request_json = toJSON(list(documents = clinton_request))
response = POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment",
                body = clinton_request_json,
                add_headers(.headers = c("Content-Type"="application/json",
                                         "Ocp-Apim-Subscription-Key"="5155d7c8316641f9986d24dc562da8df")))
content_clinton = content(response)
output_clinton = data.frame(matrix(unlist(content_clinton),nrow = clinton_size,byrow = T),stringsAsFactors = F)
names(output_clinton) = c("Sentiment_Score","ID")
output_clinton$tweets = as.factor("Clinton")
output_clinton$Sentiment_Score  = as.numeric(output_clinton$Sentiment_Score)


## Plotting results
final = rbind(output_trump,output_clinton)
g = ggplot(final,aes(tweets,Sentiment_Score)) + 
    geom_boxplot(aes(colour = tweets)) + 
    geom_jitter(position = position_jitter(width = 0.3),colour = "grey ", alpha = 0.4) +
    labs(title = "Trump vs Clinton Tweets Analysis") + 
    labs(x = "Candidate", y = "Sentimental Score of tweets")
print(g)

## Summary of sentimental scores
# Trump
summary(output_trump$Sentiment_Score)

# Clinton
summary(output_clinton$Sentiment_Score)





