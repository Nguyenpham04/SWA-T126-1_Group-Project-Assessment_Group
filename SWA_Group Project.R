library(tidyverse) 
library(RedditExtractoR)
library(tidytext)
#https://youtu.be/Snm0Azfi_hc,how to get reddit API 
#Question 1
#1A
urls <- find_thread_urls(
  subreddit = "climatechange",
  sort_by = "top",
  period = "year")

view(urls)

comment <- get_thread_content(urls$url)
view(comment$comments)
view(comment$threads)

write.csv(comment$threads,"reddit_threads.csv")
write.csv(comment$comments,"reddit_comments.csv")
threads <- read.csv("reddit_threads.csv",stringsAsFactors = FALSE)
comments <-read.csv("reddit_comments.csv",stringsAsFactors = FALSE)
#1B

top_3_threads <- threads %>%
  arrange(desc(upvotes)) %>%
  slice(1:3)

top_3_threads
top_3_threads[c("title", "text")]

#1C
# t lưu thành 2 files khác nhau nên dùng left join để có comments nhé, cái này t chỉ dùng dữ liệu của 3 threads nhiều upvote nhấ
merged_data <- top_3_threads %>%
  left_join(comments %>% select(comment, url), by = "url")

view(merged_data)

#clean data
merged_data <- merged_data %>%
  filter(!is.na(comment)) %>%                     # bỏ các dòng NA
  mutate(comment = tolower(comment)) %>%          # chữ thường
  mutate(comment = gsub("[[:punct:]]", " ", comment)) # bỏ dấu câu

view(merged_data)

bigrams <-  merged_data %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2)

bigrams <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

data("stop_words")
bigrams <- bigrams %>%
  filter(!is.na(word1), !is.na(word2)) %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!grepl("http|www", word1),
         !grepl("http|www", word2)) %>%
  filter(!grepl("[0-9]", word1),
         !grepl("[0-9]", word2)) %>%
  filter(word1 != "na", word2 != "na") %>%

bigrams <- bigrams %>%
  mutate(word1 = wordStem(word1),
         word2 = wordStem(word2))

bigrams <- bigrams %>%
  unite(bigram, word1, word2, sep = " ")

view(bigrams)

#count top 15 comments each thread

bigram_counts <- bigrams %>%
  count(url, bigram, sort = TRUE)
bigram_counts

top_bigrams <- bigram_counts %>%
  group_by(url) %>%
  slice_max(order_by = n, n = 15) 

top_bigrams <- top_bigrams %>% ungroup()

view(top_bigrams)

#plot bigram 

threads <- unique(as.character(top_bigrams$url))

for (i in seq_along(threads)) {
  t <- threads[i]   # đảm bảo chỉ 1 giá trị
  
  data_plot <- top_bigrams[top_bigrams$url == t, ]
  
  print(
    ggplot(data_plot, aes(x = reorder(bigram, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Top 15 Bigrams - Thread", i),
        x = "Bigram",
        y = "Frequency"
      ) +
      theme_minimal()
  )
}
