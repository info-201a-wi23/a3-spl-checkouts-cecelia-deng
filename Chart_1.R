library(dplyr)
library(ggplot2)


# load data
Data_books <- read.csv("Library Dataset (Cleaned).csv")

Data_books_subject <- Data_books %>%
  group_by(Subjects, CheckoutYear) %>%
  summarise(total_checkouts = sum(Checkouts))

Data_S <- Data_books_subject[Data_books_subject$Subjects %in% c("Books and reading Juvenile fiction",	
"Rock music 2011 2020, Rock music", "Fiction, Literature, Romance"),]

# create line chart
line_chart <- ggplot(Data_S, aes(x=CheckoutYear, y=total_checkouts, color = Subjects)) +
  geom_line() +
  xlab("Checkout Year") +
  ylab("Number of Checkouts") +
  ggtitle("Change in Number of Subject Checkouts over Time from 2017-2023") +
  labs(color = "Subjects") +
  scale_fill_brewer(palette = "Blues")


line_chart

