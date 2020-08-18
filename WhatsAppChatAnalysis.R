library("rwhatsapp")
library("jpeg")
library("stringr")

chat <- rwa_read("F:\\Datasets\\whatsapp_chat.txt")
chat <- na.omit(chat)
chat <- chat[chat$author != "+91 87544 05449", ]
chat <- chat[chat$author != "Kalaivani", ]

chat$author <-str_replace_all(chat$author, "Usha Vijay", "User1")
chat$author <-str_replace_all(chat$author, "Asha", "User2")
chat$author <-str_replace_all(chat$author, "Kalaivani UK", "User3")
chat$author <-str_replace_all(chat$author, "Rama", "User4")



setwd("F://R Projects")
library("dplyr")
library("ggplot2"); 
library("lubridate")



jpeg("charts//whatsappanalysis//monthly.jpg")
chat %>%
  mutate(day = month(time, label = TRUE)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_col(fill = c("#5b59d6")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 12, ),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 10, ),
        axis.text.x=element_text(size=10, hjust=0.95,vjust=0.5),
        axis.text.y=element_text(size=10,hjust=1),
        axis.title.x=element_text(size=9, face="bold", hjust=0.5,vjust=-1),
        axis.title.y=element_text(size=9, face="bold", hjust=0.5,vjust=2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ylab("") + xlab("") +
  ggtitle("Messages per Month")
dev.off()



jpeg("charts//whatsappanalysis//day_week.jpg")
chat %>%
  mutate(day = weekdays(as.POSIXct(time))) %>%
  count(day) %>%
  ggplot(aes(x = reorder(day, n), y = n)) +
  geom_col(fill = c("#5b59d6")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 12, ),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 10, ),
        axis.text.x=element_text(size=10, hjust=0.95,vjust=0.5),
        axis.text.y=element_text(size=10,hjust=1),
        axis.title.x=element_text(size=9, face="bold", hjust=0.5,vjust=-1),
        axis.title.y=element_text(size=9, face="bold", hjust=0.5,vjust=2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ylab("") + xlab("") +
  ggtitle("Messages per Day of Week")
dev.off()


chat %>% 
  group_by(author)%>%
  mutate(n = str_count(text)) %>%
  summarise(n = max(n)) %>%
  filter(n > 100 & n < 300)

jpeg("charts//whatsappanalysis//wn_messages.jpg")
chat %>%
  mutate(cnt = str_count(text)) %>%
  filter(cnt > 100 & cnt < 300) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_col(fill = c("#5b59d6")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 12, ),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 10, ),
        axis.text.x=element_text(size=10, hjust=0.95,vjust=0.5),
        axis.text.y=element_text(size=10,hjust=1),
        axis.title.x=element_text(size=9, face="bold", hjust=0.5,vjust=-1),
        axis.title.y=element_text(size=9, face="bold", hjust=0.5,vjust=2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Highest Number of long messages")
dev.off()

library("tidyr")
library("ggimage")
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

png("charts//whatsappanalysis//wn_emojis.png")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
dev.off()


library("tidytext")
library("stopwords")
library("qdapDictionaries")

is.word  <- function(x) x %in% DICTIONARY$word

to_remove <- c(stopwords(language = "en"),
               "media",
               "omitted",
               "i",
               "the",
               "u",
               "ll",
               "yeah",
               "ya",
               "ok",
               "can",
               "em","ma","like","much","also","even","mama",
               "adha","cant","wont","dint","tell","told","know"
                )

List <- strsplit(chat$text, " ")
chat_words <- data.frame(author=rep(chat$author, sapply(List, length)), word=unlist(List))
chat_clean_words <- chat_words[which(is.word(chat_words$word)),]


#png("charts//whatsappanalysis//wn_words.png")
chat_clean_words %>%
 # unnest_tokens(input = text,
#                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  filter(nchar(as.character(word)) > 3) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 12, ),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 10, ),
        axis.text.x=element_text(size=10, hjust=0.95,vjust=0.5),
        axis.text.y=element_text(size=10,hjust=1),
        axis.title.x=element_text(size=9, face="bold", hjust=0.5,vjust=-1),
        axis.title.y=element_text(size=9, face="bold", hjust=0.5,vjust=2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")
dev.off()



png("charts//whatsappanalysis//wn_words_tf_idf.png")
chat_clean_words %>%
  #unnest_tokens(input = text,
 #               output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  filter(nchar(as.character(word)) > 4) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 5, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words by author")
dev.off()