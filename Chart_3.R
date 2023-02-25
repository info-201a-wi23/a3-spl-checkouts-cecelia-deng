library("ggplot2")
library("dplyr")
library("scales")

# load data
Data_books <- read.csv("Library Dataset (Cleaned).csv")

data_usageclass <- Data_books %>%
  group_by(type = UsageClass) %>%
  summarise(checkouts = sum(Checkouts))

total_checkouts = sum(data_usageclass$checkouts, na.rm = TRUE)

data_usageclass <- data_usageclass %>%
  mutate(percentage_checkouts = (checkouts / total_checkouts))

pie_chart <- ggplot(data = data_usageclass, aes(x = "", y = percentage_checkouts, fill = type)) +
  labs(title = "Digital and physical as a percentage of the total number of checkouts", fill = "Usageclass Type") +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_brewer(palette = "Purples")

pie_chart
