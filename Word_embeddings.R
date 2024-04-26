#Skip-gram
load("~/Desktop")
library(word2vec)
skip_gram <- word2vec(x = reviews$review_precessed, type = "skip-gram") #Already pre-processed
summary(skip_gram)[1:20]

#Predicting close words

war <- predict(skip_gram, "war", type = "nearest", top_n = 5)
print(war)
marvel <- predict(skip_gram, "marvel", type = "nearest", top_n = 5)
marvel
dark <- predict(skip_gram, "dark", type = "nearest", top_n = 5)
dark
toy <- predict(skip_gram, "toy", type = "nearest", top_n = 5)
toy
family <- predict(skip_gram, "famili", type = "nearest", top_n = 5) #Stemmed
family

#Generate new embeddings
all_words <- predict(skip_gram, summary(skip_gram), type = "embedding")
war_family <- (all_words ["war",] + all_words["famili",])
war_family <- predict(skip_gram, war_family, type = "nearest", top_n = 5)
war_family
marvel_dc <- (all_words ["marvel",] + all_words["dc",])
marvel_dc <- predict(skip_gram, marvel_dc, type = "nearest", top_n = 5)
marvel_dc


#Combining multiple embeddings
war_family_girlfriend <- (all_words ["war",] + all_words["famili",]+all_words["girlfriend",])
war_family_girlfriend <- predict(skip_gram, war_family_girlfriend, type = "nearest", top_n = 5)
war_family_girlfriend
marvel_dc_superhero <- (all_words ["marvel",] + all_words["dc",] - all_words["superhero",])
marvel_dc_superhero <- predict(skip_gram, marvel_dc_superhero, type = "nearest", top_n = 5)
marvel_dc_superhero


#Document-level polarity predictions
document_embeddings <- doc2vec(skip_gram, reviews$review_precessed)
dependent <- reviews$polarity
indepdent <- document_embeddings
model <- lm(dependent ~ ., data = as.data.frame(indepdent))
summary(model)
