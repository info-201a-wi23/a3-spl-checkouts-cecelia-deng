library(dplyr)
library(ggplot2)

Data_books <- read.csv("Library Dataset (Cleaned).csv")

Data_books_sum <- Data_books %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarise(total_checkouts = sum(Checkouts))

Data_VBEAA <- Data_books_sum[Data_books_sum$MaterialType %in% c("VIDEODISC", "BOOK", "EBOOK", "AUDIOBOOK", "AUDIOBOOK"),]

line_chart2 <- ggplot(Data_VBEAA, aes(x=CheckoutYear, y=total_checkouts, color = MaterialType)) +
  geom_line() +
  xlab("Checkout Year") +
  ylab("Total Number Of Checkouts") +
  ggtitle("Change in Materials' Checkouts over Time from 2017-2023") +
  labs(color = "MaterialType") +
  scale_fill_brewer(palette = "Reds")

line_chart2

