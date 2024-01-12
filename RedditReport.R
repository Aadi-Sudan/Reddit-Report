library(dplyr)
library(readr)
library(plotly)

setwd("/Users/aadisudan/Downloads")
getwd()

sarcasticcommentsDf <- read_csv("train-balanced-sarcasm.csv")

sarcasticsummaryDf <- sarcasticcommentsDf %>%
  group_by(subreddit, label) %>%
    summarize(
      numOfEntries = n(),
      score = mean(score)
) %>%
  mutate(
    adjscore = score*pmin(0.5 *(1.2)^(numOfEntries), 20)
  )

averageAdjFactor = mean(pmin(0.5 *(1.2)^(sarcasticsummaryDf$numOfEntries), 20))
sarcasticsummaryDf = sarcasticsummaryDf %>%
  mutate(
    adjscore = adjscore/averageAdjFactor
  )

sarcasticFreq <- sarcasticsummaryDf%>%
  filter(label == 1) %>%
  arrange(desc(numOfEntries))

nonsarcasticFreq <- sarcasticsummaryDf%>%
  filter(label == 0) %>%
  arrange(desc(numOfEntries))

sarcasticHigh <- sarcasticsummaryDf %>%
  filter(label == 1) %>%
  arrange(desc(score))
highestsarcasticDf <- ungroup(sarcasticHigh)[1:25, ]

sarcasticLow <- sarcasticsummaryDf %>%
  filter(label == 1) %>%
  arrange(score)
lowestsarcasticDf <- ungroup(sarcasticLow)[1:25, ]

notsarcasticHigh <- sarcasticsummaryDf %>%
  filter(label == 0) %>%
  arrange(desc(score))
highestnotsarcasticDf <- ungroup(notsarcasticHigh)[1:25, ]

notsarcasticLow <- sarcasticsummaryDf %>%
  filter(label == 0) %>%
  arrange(score)
lowestnotsarcasticDf <- ungroup(notsarcasticLow)[1:25, ]

highestsarcastic <- plot_ly(
  x = highestsarcasticDf$subreddit,
  y = highestsarcasticDf$adjscore,
  type = "bar"
) %>%
  layout(title = "Top 25 Scores for Sarcastic Reddit Comments")
highestsarcastic

lowestsarcastic <- plot_ly(
  x = lowestsarcasticDf$subreddit,
  y = lowestsarcasticDf$adjscore,
  type = "bar"
)%>%
  layout(title = "Bottom 25 Scores for Sarcastic Reddit Comments")
lowestsarcastic

highestnotsarcastic <- plot_ly(
  x = highestnotsarcasticDf$subreddit,
  y = highestnotsarcasticDf$adjscore,
  type = "bar"
)%>%
  layout(title = "Top 25 Scores for Nonsarcastic Reddit Comments")
highestnotsarcastic

lowestnotsarcastic <- plot_ly(
  x = lowestnotsarcasticDf$subreddit,
  y = lowestnotsarcasticDf$adjscore,
  type = "bar"
)%>%
  layout(title = "Bottom 25 Scores for Nonsarcastic Reddit Comments")
lowestnotsarcastic

View(sarcasticFreq)
View(nonsarcasticFreq)
