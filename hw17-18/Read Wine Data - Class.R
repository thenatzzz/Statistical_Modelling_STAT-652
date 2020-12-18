### Activate the dplyr package
library(dplyr)

### Read-in dataset
data.in = read.csv("Datasets/Wine Quality.csv")

### Use transmute to rename variables, recode quality, and remove
### variables we aren't interested in. The case_when() function lets
### us divide a variable into groups and have different outputs
### across groups. You don't need to know the details.
data = data.in %>%
  transmute(quality = case_when(quality < 6 ~ "low",
    quality == 6 ~ "med",
    quality > 6 ~ "high"),
    sugar = residual.sugar, density = density, pH = pH,
    sulphates = sulphates, chlor = chlorides,
    alcohol = alcohol) %>%
  filter(sulphates < 1.4, pH < 3.85, chlor <= 0.3)

