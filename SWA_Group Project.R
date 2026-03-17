library(tidyverse) 
library(RedditExtractoR)
#https://youtu.be/Snm0Azfi_hc,how to get reddit API 
urls <- find_thread_urls(
  subreddit = "climate change",
  sort_by = "top",
  period = "year"
)
view(urls)
comment <- get_thread_content(urls$url)
view(comment$comments)
view(comment$threads)

write.csv(comment$threads,"reddit_threads.csv")
write.csv(comment$comments, "reddit_comments.csv")

top_3_threads <- comment$threads %>%
  arrange(desc(score)) %>%
  slice(1:3)

top_3_threads
