library(tidyverse)
library(tidytext)
library(ggwordcloud)

# Set your filepath here
directory <- "~/Mirror/2024-2025/DAP 2/final"
bill_export <- read_csv(paste0(directory, "/MSP_congress.csv"), skip = 2)

bill_summary_text <- bill_export |> 
  filter(!is.na(`Latest Summary`)) |> 
  pull(`Latest Summary`) |> 
  paste(collapse = " ")

# str_remove_all to remove html notation, then create df for tokenization
bill_summary_text_clean <- bill_summary_text |> 
  str_remove_all("<[^>]+>") |> 
  data.frame() |> 
  rename(text = str_remove_all.bill_summary_text............)

# Create tokens, remove stop words and other non-substantive terms, get counts
bill_summary_tokens <- bill_summary_text_clean |> 
  unnest_tokens(word_tokens, text, token = "words") |> 
  anti_join(stop_words, by = c("word_tokens" = "word"))

remove_terms <- c("sec", "secretary", "act", "requires", "division", "title", "subtitle", "provide", "related", "including", "amends", "directs", "establish", "appropriations", "provisions", 0:5000)

bill_summary_tokens <- bill_summary_tokens|> 
  filter(! word_tokens %in% remove_terms) |> 
  group_by(word_tokens) |> 
  summarise(n = n()) |>
  arrange(desc(n), .by_group=FALSE)

# Generate word cloud
set.seed(111)

wordcloud <- ggwordcloud(bill_summary_tokens$word_tokens,
            bill_summary_tokens$n,
            max.words = 30,
            random.order = FALSE,
            colors = scales::hue_pal()(3))

print(wordcloud)

wordcloud |> ggsave(filename="textanalysis_wordcloud.png",
                    dpi = 300)
