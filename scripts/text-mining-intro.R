library(tidytext)
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

text_df

text_df %>%
  unnest_tokens(word, text)
# We’ve now split each row so that there is one token (word) in each row of the new data frame

library(janeaustenr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books
# Now that the data is in one-word-per-row format, we can manipulate it with tidy tools like dplyr. We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join()
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)
# We can also use dplyr’s count() to find the most common words in all the books as a whole.
tidy_books %>%
  count(word, sort = TRUE) 

library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()