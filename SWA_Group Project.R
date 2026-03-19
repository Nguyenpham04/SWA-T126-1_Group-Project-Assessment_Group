library(tidyverse) 
library(RedditExtractoR)
#https://youtu.be/Snm0Azfi_hc,how to get reddit API 
#Question 1
#1A
urls <- find_thread_urls(
  subreddit = "climatechange",
  sort_by = "top",
  period = "year"
)
view(urls)
comment <- get_thread_content(urls$url)
view(comment$comments)
view(comment$threads)

write.csv(comment$threads,"reddit_threads.csv")
threads <- read.csv("reddit_threads.csv")
#1B

top_3_threads <- threads %>%
  arrange(desc(upvotes)) %>%
  slice(1:3)

top_3_threads
top_3_threads[, c("title", "text")]
