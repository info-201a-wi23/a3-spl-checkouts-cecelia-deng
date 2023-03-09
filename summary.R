# Packages
library(dplyr)
library(tidyr)
library(ggplot2)

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

# Average number of checkouts for Little Women
LW_avg_checkout <- Data_books %>%
  filter(Title == "Little Women") %>%
  summarize(mean_checkout = mean(Checkouts)) %>%
  pull(mean_checkout)
LW_avg_checkout

# Average number of checkouts for Jane Eyre
JE_avg_checkout <- Data_books %>%
  filter(Title == "Jane Eyre") %>%
  summarize(mean_checkout = mean(Checkouts)) %>%
  pull(mean_checkout)
JE_avg_checkout

# Group the data by year and Title of Jane Eyre, then find the maximum checkout number for each group
JE_max_checkouts <- Data_books %>%
  filter(Title == "Jane Eyre") %>%
  group_by(CheckoutYear, Title) %>%
  summarize(max_checkout = max(Checkouts))
jane_eyre_checkouts <- subset(JE_max_checkouts, Title == "Jane Eyre")
JE_max_index <- which.max(jane_eyre_checkouts$max_checkout)
JE_max_year <- jane_eyre_checkouts$CheckoutYear[JE_max_index]
JE_max_year

# Group the data by year and Title of Little Women, then find the maximum checkout number for each group
LW_max_checkouts <- Data_books %>%
  filter(Title == "Little Women") %>%
  group_by(CheckoutYear, Title) %>%
  summarize(max_checkout = max(Checkouts))
little_women_checkouts <- subset(LW_max_checkouts, Title == "Little Women")
LM_max_index <- which.max(little_women_checkouts$max_checkout)
LM_max_year <- little_women_checkouts$CheckoutYear[LM_max_index]
LM_max_year

# Average number of checkouts for EBOOK
EB_avg_checkout <- Data_books %>%
  filter(MaterialType == "EBOOK") %>%
  summarize(mean_checkout = mean(Checkouts)) %>%
  pull(mean_checkout)
EB_avg_checkout

# Group the data by year and EBOOK, then find the maximum checkout number for each group
EB_max_checkouts <- Data_books %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(max_checkout = max(Checkouts))
ebook_checkouts <- subset(EB_max_checkouts, MaterialType == "EBOOK")
EB_max_index <- which.max(ebook_checkouts$max_checkout)
EB_max_year <- ebook_checkouts$CheckoutYear[EB_max_index]
EB_max_year

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

# make a new column for the content of UsageClass and Checkouts
## data_usageclass
data_usageclass <- Data_books %>%
  group_by(type = UsageClass) %>%
  summarise(checkouts = sum(Checkouts))

# calculate the total checkouts number 
## total_checkouts
total_checkouts = sum(data_usageclass$checkouts, na.rm = TRUE)

# mutate a new one 
## data_usageclass
data_usageclass <- data_usageclass %>%
  mutate(percentage_checkouts = (checkouts / total_checkouts))





