# Packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Questions: 
# - "What is the average number of checkouts for each Usageclass(Digital and Physical) ?"
# - "How has the number of print book, ebooks and audiobook checkouts changed over time?" 
# - "What is the average number of checkouts for items in each year?"
# - "Which item type (e.g. BOOK, EBOOK, VIDEODISC) has the highest number of checkouts?"
# - "Are there any seasonal trends in item checkouts, and if so, which items are most affected?"

# Extract columns
Data_books <- read.csv("Library Dataset (Cleaned).csv")

Data_books_subject <- Data_books %>%
  group_by(Subjects, CheckoutYear) %>%
  summarise(total_checkouts = sum(Checkouts))

Data_S <- Data_books_subject[Data_books_subject$Subjects %in% c("Books and reading Juvenile fiction", "Rock music 2011 2020, Rock music", "Fiction, Literature, Romance"),]


head(Data_books)

unique_column <- unique(Data_books$MaterialTy)
write.csv(unique_column, "MATERIAL_TYPE.csv", row.names = FALSE)

# Factor for categorical variables
## MATERIAL TYPE 
mt_type <- read.csv('MATERIAL_TYPE.csv')
#Data_books <- left_join(Data_books, mt_type) #MATERIAL_TYPE -> mt_type

# Average number of checkouts for each item
## avg_checkouts
average_checkouts_by_item <- Data_books %>%
  group_by(MaterialType) %>%
  summarize(avg_checkouts = mean(Checkouts))

# Load data, show the total number do checkouts.
Data_books_usage <- Data_books %>%
  group_by(UsageClass, CheckoutYear) %>%
  summarise(total_checkouts = sum(Checkouts))

# Average checkouts for each items per year
## avg_checkouts_by_year
avg_checkouts_by_year <- aggregate(Checkouts ~ CheckoutYear, data = Data_books, FUN = mean)

cat("Average number of checkouts per year:")
print(avg_checkouts_by_year)

# Find the item with the total number of checkouts
## checkouts_by_type
checkouts_by_type <- 
  aggregate(Checkouts ~ MaterialType, data = Data_books, sum)

# Comparison of items with the highest checkout
## item_type_max_checkout
item_type_max_checkout <- 
  checkouts_by_type$MaterialType[which.max(checkouts_by_type$Checkouts)]

# Print highest item name 
print(item_type_max_checkout)

# make a new colunm for the content of MaterialType and CheckoutMonth
## Data_books_sum
Data_books_sum <- Data_books %>%
  group_by(MaterialType, CheckoutMonth) %>%
  summarise(total_checkouts = sum(Checkouts))

# Choose the content which needed 
## Data_VBEAA
Data_VBEAA <- Data_books_sum[Data_books_sum$MaterialType %in% c("VIDEODISC", "BOOK", "EBOOK", "AUDIOBOOK", "AUDIOBOOK"),]

# make a new colunm for the content of UsageClass and Checkouts
## data_usageclass
data_usageclass <- Data_books %>%
  group_by(type = UsageClass) %>%
  summarise(checkouts = sum(Checkouts))

# caculate the totoal checkouts number 
## total_checkouts
total_checkouts = sum(data_usageclass$checkouts, na.rm = TRUE)

# mutate a new one 
## data_usageclass
data_usageclass <- data_usageclass %>%
  mutate(percentage_checkouts = (checkouts / total_checkouts))





