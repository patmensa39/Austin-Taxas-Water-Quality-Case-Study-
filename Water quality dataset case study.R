# Austin Water Quality Case Study

# Load in the libraries that we'll need
pacman::p_load(tidyverse, lubridate, stringr)

### loading the dataset 

water <- read_csv("http://594442.youcanlearnit.net/austinwater.csv")

### viewing the data
glimpse(water)

attach(water)

### selecting only the required columns 

water <- water %>% 
  select(SITE_NAME, SITE_TYPE, SAMPLE_DATE, PARAM_TYPE, PARAMETER, RESULT, UNIT)
glimpse(water)

### The is the need to rename them to make everything look simple 
water <- water %>% 
  rename(SiteName = SITE_NAME, SiteType = SITE_TYPE, SampleDate = SAMPLE_DATE,
         ParameterType = PARAM_TYPE, Parameter = PARAMETER, Result = RESULT, 
         Unit = UNIT)
glimpse(water)

### Pulling up the Parameter Name 
unique(water$Parameter)

### This would not work so i will try to look for the parameter with PH
water %>% filter(str_detect(Parameter, 'PH')) %>% select(Parameter) %>%
  select(Parameter) %>% unique()

### this also did not work so we look at the unique values in parameter type
unique(water$ParameterType)

### we can find  "Alkalinity/Hardness/pH" and "Conventionals" so we have to look 
### into the data to see if we have parameter type with "Alkalinity/Hardness/pH" 
### and "Conventionals"

water %>% filter(ParameterType == "Alkalinity/Hardness/pH" |
                  ParameterType == "Conventionals")


### this looks ok so we make it a new variable 
filtered_water <- water %>% filter(ParameterType == "Alkalinity/Hardness/pH" |
                                     ParameterType == "Conventionals")
glimpse(filtered_water)

### we look at the parameters involved in this dataset
unique(filtered_water$Parameter)

### we can see PH and water temperature so we filter them in the parameter column 

filtered_water <- water %>% 
  filter(Parameter == "PH" | Parameter == "WATER TEMPERATURE")
glimpse(filtered_water)

#### Looking at the summary of the data 
summary(filtered_water)

### Sitetype and a few other variables seems to be a factors so we have to deal with them 

filtered_water <- filtered_water %>% 
  mutate(SiteType = as.factor(SiteType), 
         Parameter = as.factor(ParameterType), 
         Parameter = as.factor(Parameter), 
         Unit = as.factor(Unit))
summary(filtered_water)

### the dates also seems like a string here so we have to deal with that too
### lets have a look at 

filtered_water$SampleDate

### this is true so we have to use the lubridate function to handle it 

filtered_water <- filtered_water %>%
  mutate(SampleDate = mdy_hms(SampleDate))

### lets have a look at it 
summary(filtered_water)

### Working with the units, we dont measure water by Feet or milligram per liter
### so we need to look for these values 

filtered_water %>% filter(Unit == "Feet")
### This will probably be Deg. Fahrenheit and the person did a mistake here 
### so we will have to recode it to Deg. Fahrenheit
filtered_water <- filtered_water %>%
  mutate(Unit = recode(Unit, "Feet" = "Deg. Fahrenheit"))

summary(filtered_water)
### we see that this has taken care of that

### Working with the units, we dont measure water in milligram per liter
### so we need to look for these values and remove them from the dataset 

filtered_water %>% filter(Unit == "MG/L")

### they are seven and we need to remove them 
filtered_water <- filtered_water %>% 
  filter(!Unit =="MG/L")

summary(filtered_water)

### you can see it there but it shows 0. we can get rid of it entirely with this code 
filtered_water <- filtered_water %>% 
  mutate(Unit = droplevels(Unit))

summary(filtered_water)
attach(filtered_water)
### Identifying and removing outliers '
### we draw a scatterplot to see
ggplot(data = filtered_water, mapping = aes(x = SampleDate, y = Result)) +
  geom_point()

### we can see that there is one of the resullts above 1 million so we have to 
### look into that 
filter(filtered_water, Result > 1000000)

### we see that there is no situation we measure water temperature above million 
### so don't know how to correct it so i will just remove that (outlier)

filtered_water <- filtered_water %>%
  filter(Result < 1000000)

summary(filtered_water)
### we see that there is no situation we measure water temperature above 1000 
### so I will filter all results below or equal to 1000
filtered_water <- filtered_water %>%
  filter(Result <= 1000)

summary(filtered_water)

### we draw a scatterplot to see
ggplot(data = filtered_water, mapping = aes(x = SampleDate, y = Result)) +
  geom_point()

### we can also draw a boxplot 
ggplot(data = filtered_water, mapping = aes(x = Unit, y = Result)) + 
  geom_boxplot()

### we realized that 60 degrees is too much for a water temperature
###so it will probably be fahrenheit so will have to deal with that 
filtered_water <- filtered_water %>%
  mutate(Unit = as.character(Unit)) %>%
  mutate(Unit = ifelse(Unit == "Deg. Celsius" & Result > 60, "Deg. Fahrenheit", Unit)) %>%
  mutate(Unit = as.factor(Unit))

### we can now draw a boxplot 
ggplot(data = filtered_water, mapping = aes(x = Unit, y = Result)) + 
  geom_boxplot()

### We have to convert the temperature from Fahrenheit to Celcius 

fahrenheit <- which(filtered_water$Unit == "Deg. Fahrenheit")

filtered_water$Result[fahrenheit] <- (filtered_water$Result[fahrenheit]-32) * (5/9)
filtered_water$Unit[fahrenheit] <- 'Deg. Celsius'

### we can now draw a boxplot 
ggplot(data = filtered_water, mapping = aes(x = Unit, y = Result)) + 
  geom_boxplot()

summary(filtered_water)

### you can see from the unit that the Deg. Fahreneheit is still the 
### so we have to get rid of it 

filtered_water$Unit <- droplevels(filtered_water$Unit)

summary(filtered_water)

### Widening the water quality dataset 
### now that we are done with the most of the work, 
### we have only some few columns and work with them 

filtered_water <- filtered_water %>%
  select(-ParameterType, -Unit)
summary(filtered_water)

filtered_water_wide <- pivot_wider(filtered_water, names_from = Parameter, 
                                   values_from = Result)

### this produces error because there are duplicated values 
dupe_check <- filtered_water[-5]
duplicated(dupe_check)

### i want to identify the row numbers 
which(duplicated(dupe_check))
dupes <- which(duplicated(dupe_check))

### you can look for an expert to understand why these duplicate values exits 
### I will remove these duplicates 
filtered_water <- filtered_water[-dupes,]

### we will now widening the data 
filtered_water_wide <- pivot_wider(filtered_water, names_from = Parameter, 
                                   values_from = Result)

filtered_water_wide


filtered_water_wide <-filtered_water_wide %>% 
  rename(PH = 'Alkalinity/Hardness/pH',Temperature = 'Conventionals')
filtered_water_wide

summary(filtered_water_wide)





