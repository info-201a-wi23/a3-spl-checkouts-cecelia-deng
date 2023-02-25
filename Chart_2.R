library(dplyr)
library(ggplot2)

Data_books_sum <- Data_books %>%
  group_by(MaterialType, CheckoutMonth) %>%
  summarise(total_checkouts = sum(Checkouts))

Data_VBEAA <- Data_books_sum[Data_books_sum$MaterialType %in% c("VIDEODISC", "BOOK", "EBOOK", "AUDIOBOOK", "AUDIOBOOK"),]

bar_chart <- ggplot(Data_VBEAA, aes(x = CheckoutMonth, y = total_checkouts)) +
  geom_bar(stat = "identity", aes(fill = MaterialType)) +
  labs(x = "Month", y = "Total checkouts", title = "The trend of Monthly checkouts by material types") +
  scale_fill_brewer(palette = "Purples")

bar_chart
