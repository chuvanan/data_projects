


library("Rfacebook")
library("anytime")
library("dplyr")

my_oauth <- fbOAuth(app_id = "1463623273653549",
                    app_secret = "ca7f450acc28a5a8710f7d56957c952e")

save(my_oauth, file = "my_oauth")
load("my_oauth")

## group_data <- getGroup(group_id = "472656846222343", token = my_oauth, n = 1000)

group_data <- group_data[, c("from_name", "message", "created_time",
                             "type", "likes_count", "comments_count", "shares_count")]
fb <- group_data

fb$created_time <- anytime(fb$created_time)
fb$date <- substr(fb$created_time, 1, 10)

posts_by_date <- table(fb$date, dnn = "date")
posts_by_date <- as.data.frame.table(posts_by_date,
                                     responseName = "n",
                                     stringsAsFactors = F)
posts_by_date$date <- as.Date(posts_by_date$date)

plot(posts_by_date$date, cumsum(posts_by_date$n), type = "b")

active_fbers <- fb %>%
  group_by(from_name) %>%
  summarise(n_posts = n(),
            n_likes = sum(likes_count),
            n_cmts = sum(comments_count),
            n_shares = sum(shares_count)) %>%
  mutate(likes_per_post = n_likes/n_posts,
         cmts_per_post = n_cmts/n_posts,
         shares_per_post = n_shares/n_posts) %>%
  ungroup()

plot(sqrt(active_fbers$likes_per_post), sqrt(active_fbers$cmts_per_post),
     cex = active_fbers$shares_per_post,
     xpd = T, bty = "n", ann = F, axes = F)
axis(1)
axis(2)

