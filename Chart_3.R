library("ggplot2")
library("dplyr")
library("scales")

# load data
Data_books <- read.csv("Library Dataset (Cleaned).csv")
data_usageclass <- Data_books %>%
  group_by(UsageClass, CheckoutYear) %>%
  summarise(total_checkouts = sum(Checkouts))

Data_DP <- data_usageclass[data_usageclass$UsageClass %in% c("Physical", "Digital"),]

# create line chart
line_chart3 <- ggplot(Data_DP, aes(x=CheckoutYear, y=total_checkouts, color = UsageClass)) +
  geom_line() +
  xlab("Checkout Year") +
  ylab("Amount of Checkouts") +
  ggtitle("Changes in physical/digital checkouts of items over time from 2017-2023.") +
  labs(color = "UsageClass") +
  scale_fill_brewer(palette = "Blues")

line_chart3




