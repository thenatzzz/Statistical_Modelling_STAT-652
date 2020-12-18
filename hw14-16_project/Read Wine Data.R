### Activate the dplyr package
library(dplyr)

### Read-in dataset
data.in = read.csv("Datasets/Wine Quality.csv")

### Use select to extract only the variables we want and filter
### to remove the sulphates outlier. %>% is called the pipe
### operator. It allows you to string together multiple dplyr
### functions with less typing, but you don't need to know about it.
### You can ask me in my Q&A sessions if you're curious.
data = data.in %>%
  select(residual.sugar, density, pH, sulphates, alcohol) %>%
  filter(sulphates < 1.4)


### Rename residual.sugar to sugar
colnames(data)[1] = "sugar"