### Activate the dplyr package
library(dplyr)

### Read-in dataset
data.in = read.csv("Wine Quality.csv")


### Use transmute to rename variables, recode quality, and remove
### variables we aren't interested in. The case_when() function lets
### us divide a variable into groups and have different outputs
### across groups. You don't need to know the details.
data = data.in %>%
  transmute(fix.acid = fixed.acidity, vol.acid = volatile.acidity,
    cit.acid = citric.acid, sugar = residual.sugar, chlor = chlorides,
    f.SO2 = free.sulfur.dioxide, t.SO2 = total.sulfur.dioxide, 
    density = density, pH = pH,
    sulphates = sulphates, type = type, 
    quality = case_when(quality < 6 ~ "low",
      quality == 6 ~ "med",
      quality > 6 ~ "high"),
    alcohol = alcohol) %>%
  filter(vol.acid < 1.2, chlor < 0.3, pH < 3.9, sulphates < 1.4,
    f.SO2 < 110, t.SO2 < 300)

