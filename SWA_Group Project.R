library(tidyverse) 
library(RedditExtractoR)
library(tidytext)
library(SnowballC)
library(tm)
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

top_3_threads[c("title", "text")]
#1C
# t lưu thành 2 files khác nhau nên dùng left join để có comments nhé, cái này t chỉ dùng dữ liệu của 3 threads nhiều upvote nhấ
merged_data <- top_3_threads %>%
  left_join(comments %>% select(comment, url), by = "url")

view(merged_data)

#clean data
data("stop_words")
custom_stop <- c("im", "dont", "youre", "ive", "amp", "thats", "cant","don t","i m","it s","isn t","i m", "in the","in a")

#bigrams
bigrams <- merged_data %>%
  filter(!is.na(comment), comment != "") %>%   
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  mutate(across(c(word1, word2), trimws)) %>%
  filter(word1 != "", word2 != "") %>%
  mutate(across(c(word1, word2), wordStem)) %>%
  filter(!word1 %in% c(stop_words$word, custom_stop),
         !word2 %in% c(stop_words$word, custom_stop)) %>%
  filter(!grepl("http|www|[0-9]", word1),
         !grepl("http|www|[0-9]", word2)) %>%
  unite(bigram, word1, word2, sep = " ")

view(bigrams)

#count top 15 comments each thread
bigram_counts <- bigrams %>%
  count(url, bigram, sort = TRUE)

top_bigrams <- bigram_counts %>%
  group_by(url) %>%
  slice_max(order_by = n, n = 15, with_ties = FALSE) %>%
  ungroup()

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

#Question 2
#2A
view(merged_data)
cleaned_data <- merged_data %>% 
  filter(!is.na(comment), comment != "") %>%
  mutate(comment = trimws(comment)) %>%
  filter(!grepl("http|www", comment)) %>%
  filter(!grepl("[0-9]", comment)) %>%
  filter(!comment %in% c(stop_words$word, custom_stop))
view(cleaned_data)

corpus <- VCorpus(VectorSource(cleaned_data$comment))
dtm <- DocumentTermMatrix(corpus)
tf_idf <- weightTfIdf(dtm)
mat <- as.matrix(tf_idf)
str(mat)
#2B
wcss <- vector()
for (k in 1:10) {
  km <- kmeans(mat, centers = k, nstart = 25)
  wcss[k] <- km$tot.withinss
}

plot(1:10, wcss, type = "b",
     main = "Elbow Method",
     xlab = "Number of clusters (k)",
     ylab = "WCSS")
#2C
all_comments <- cleaned_data %>%
  mutate(label = case_when(
    url == top_3_threads$url[1] ~ "A",
    url == top_3_threads$url[2] ~ "B",
    url == top_3_threads$url[3] ~ "C"
  ))
view(all_comments)

#2D
# PCA for dimension reduction
pca <- prcomp(mat, scale. = TRUE)
ncomp <- min(ncol(pca$x), 50)
mat_pca <- pca$x[, 1:ncomp]
# Run kmeans with chosen k (say k=3 from elbow)
set.seed(123)
km <- kmeans(mat_pca, centers = 3, nstart = 25)
km
#2E
plot_data <- data.frame(
  PC1 = mat_pca[,1],
  PC2 = mat_pca[,2],
  cluster = factor(km$cluster),
  label = all_comments$label[1:nrow(mat_pca)]  # match row count
)

ggplot(plot_data, aes(x = PC1, y = PC2,
                      color = cluster, shape = label)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "K-means Clustering vs Ground Truth",
       x = "PC1", y = "PC2")

# Question 3 Regression Analysis
#3A
threads <- read.csv("reddit_threads.csv")
head(threads)

#3B
plot(threads$score, threads$comments,
     main = "Relationship Between Upvotes and Comments",
     xlab = "Score (Upvotes)",
     ylab = "Number of Comments",
     col = "lightblue",
     pch = 16)

plot(log(threads$score + 1), log(threads$comments + 1),
     main = "Relationship Between Upvotes and Comments (Log Scale)",
     xlab = "Score (Upvotes)",
     ylab = "Number of Comments",
     col = "lightpink", 
     pch = 16)

#3C
model <- lm(comments ~ score, data = threads)
summary(model)
