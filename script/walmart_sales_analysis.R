#1. Package Load / Installation / Options ----
library(tidyverse)
library(lubridate)
library(caret)

#cause what layman likes scientific notation
options(scipen = 99999)

#2. Load data ----
walmart_sales <- read_csv('raw data/Walmart_Store_sales.csv')

#3. Examine / Tidy data ----
summary(walmart_sales)
head(walmart_sales)

#removing header capitalization
colnames(walmart_sales) <- tolower(colnames(walmart_sales))
colnames(walmart_sales)

any(is.na(walmart_sales))
class(walmart_sales$date)

#changing date to ISO 8601 standard
walmart_sales$date <- dmy(walmart_sales$date)

#Saving tidied csv
write.csv(walmart_sales, 'clean data/clean_walmart_sales.csv')

ggplot(walmart_sales, aes(date, weekly_sales)) + facet_wrap(~store) + geom_line()

pattern_exceptions <- c(3,5,17, 28, 30, 33, 36, 37, 38, 42,43,44)

ggplot(walmart_sales %>% filter(store %in% pattern_exceptions), aes(date, weekly_sales)) + facet_wrap(~store) + geom_line()

#4. Addressing basic Kaggle tasks ----

#4a. Highest sales ----
print('Which store has the most sales?')
highest_wkly_sales <- max(walmart_sales$weekly_sales)

highest_walmart_sales<- walmart_sales %>% filter(weekly_sales == highest_wkly_sales)

paste0('Walmart Store #',highest_walmart_sales$store, ' reported the highest weekly sales on ', highest_walmart_sales$date,'. In that location, the temperature was ', highest_walmart_sales$temperature, ' degrees farenheit, the local CPI was ', highest_walmart_sales$cpi, ', and the local unemployment rate was ', highest_walmart_sales$unemployment, '%. Their sales were ', prettyNum(round(highest_walmart_sales$weekly_sales), big.mark = ','), ' USD.')

highest_walmart_sales_agg <- walmart_sales %>% group_by(store) %>% summarise(sales = sum(weekly_sales)) %>% arrange(desc(sales))

paste0('Overall, from the February 5, 2010 to October 26, 2012 Store # ', highest_walmart_sales_agg$store[1],' had the most sales with an aggregate sales figure of ', prettyNum(round(highest_walmart_sales_agg$sales[1]), big.mark = ','),' USD.')

#4b. highest std. dev ----
sdev_walmart_sales <- walmart_sales %>% group_by(store) %>% summarize(sdev = sd(weekly_sales)) %>% arrange(desc(sdev))

paste0('Store # ', sdev_walmart_sales$store[1], ' has the highest standard deviation of their weekly sales. The standard deviation is ', prettyNum(round(sdev_walmart_sales$sdev[1]), big.mark = ','), ' USD.')

ggplot(walmart_sales %>% filter(store == 14), aes(date, weekly_sales)) + geom_line()

#4c. Coefficient of standard deviation to the mean.----

sdev_walmart_sales <- walmart_sales %>% group_by(store) %>% summarize(sdev = sd(weekly_sales) , avg = mean(weekly_sales), coef_var = (sdev/avg)*100)

print('Although store #14 has the highest numerical standard deviation of all the stores, when viewed proportionally to the mean, store # 35 actually has the highest proportional standard deviation. Meaning that from their relative means, store #35 has more or larger proportional variance from the mean than store #14.')

#4d. Good Quarterly growth rate in Q3 2012 ----

Q2_q3<- c('may','june','july','august','september','october') #just for me to remember
quarterly_sales <- 
  walmart_sales %>% 
  filter(date >= '2012-05-01', 
         date < '2012-11-01') %>% 
  mutate(quarter = ifelse(date < '2012-08-01', 
                          'Q2' , 'Q3')) %>% 
  group_by(store, quarter) %>% 
  summarize(sales = sum(weekly_sales)) %>% 
  pivot_wider(id_cols = 'store' , 
              names_from = 'quarter', 
              values_from = 'sales') %>% 
  mutate(Q3_growth = ((Q3-Q2)/Q2)*100, 
         store = as.factor(store))

quarterly_sales %>% 
  filter(Q3_growth > 0) %>% 
  ggplot(aes(store, Q3_growth, 
             color = store, fill = store)) + 
  geom_col() 

print('Only a few stores had positive growth rate from Q2 to Q3 2012 (going by the Walmart fiscal calendar). Of these stores, Store #39 had the highest growth rate of 2.73%.')

q3_vector <- 
  which(between(walmart_sales$date, 
                as.Date('2011-08-01') , 
                as.Date('2011-11-01'))) %>% 
  append(which(between(walmart_sales$date, 
                       as.Date('2012-08-01') , 
                       as.Date('2012-11-01'))))

q3_annual <- 
  walmart_sales[q3_vector,] %>% 
  group_by(store, year = year(date)) %>% 
  summarize(q3_sales = sum(weekly_sales)) %>% 
  pivot_wider(id_cols = 'store', 
              names_from = 'year' , 
              values_from = 'q3_sales') %>% 
  mutate(annual_growth = (`2012` - `2011`)/`2011` *100, 
         store = as.factor(store)) %>% 
  arrange(desc(annual_growth)) 

q3_annual %>% 
  filter(annual_growth > 0) %>% 
  ggplot(aes(store, annual_growth,
             fill = store, color = store)) +
  geom_col()

q3_annual[1:2,]



print('On an annual rate, the stores had much better growth performance between Q3 2011 and Q3 2012. Both store 18 and 44 had growth rates around 12%.')

#4e. Holiday impact on sales ----


#initializing vectors as to the specific holiday sales dates as per Walmart
superbowl <- as.Date(c('2010-02-12','2011-02-11','2012-02-10','2008-02-13'))
labour_day <- as.Date(c('2010-09-10','2011-09-09','2012-09-07','2013-09-06'))
thanksgiving <- as.Date(c('2010-11-26','2011-11-25','2012-11-23','2013-11-29'))
christmas <- as.Date(c('2010-12-31','2011-12-30','2012-12-28','2013-12-27'))

walmart_sales <- walmart_sales %>%  mutate(holiday = 
      case_when(date %in% superbowl ~ 'superbowl', 
                date %in% labour_day ~ 'labour day',
                date %in% thanksgiving ~ 'thanksgiving',
                date %in% christmas ~ 'christmas'))

getSeason <- function(x){
  
  winter <- seq(as.Date('2009-12-21'), as.Date('2010-03-20'), by = 'days') %>% append(seq(as.Date('2010-12-21'), as.Date('2011-03-20'), by = 'days')) %>% append(seq(as.Date('2011-12-21'), as.Date('2012-03-20'), by = 'days'))
  
  fall <- unique(walmart_sales$date[!walmart_sales$date %in% winter] %>% subset(grepl('^.{4}-09.|^.{4}-10.|^.{4}-11.|^.{4}-12.', .)) %>% subset(. >= as.Date('2010-09-22'))) %>% .[-c(14,15,16, 30, 31, 32)]
  
spring <- unique(walmart_sales$date[!walmart_sales$date %in% winter %>% append(fall)] %>% subset(grepl('^.{4}-03.|^.{4}-04.|^.{4}-05.|^.{4}-06.' , .))) %>% .[-c(14, 28, 42, 43)]

summer <- unique(walmart_sales$date) %>% subset(!. %in% (spring %>% append(fall) %>% append(winter)))

ifelse(x %in% fall, 'Fall', 
       ifelse(x %in% winter, 'Winter',
              ifelse(x %in% summer, 'Summer', 'Spring')))

}


walmart_sales %>% 
  group_by(date, holiday) %>% 
  summarize(weekly_sales = mean(weekly_sales)) %>% 
  ggplot(aes(date, weekly_sales, 
             fill = holiday, color = holiday)) + 
  geom_point()

  
avg_holiday_sales <- 
  walmart_sales %>% 
  filter(holiday_flag ==1) %>%
  mutate(season = getSeason(date)) %>% 
  group_by(holiday, season) %>% 
  summarize(avg_sales = mean(weekly_sales))

walmart_sales %>% 
  filter(holiday_flag == 0) %>% 
  mutate(season = getSeason(date)) %>% 
  group_by(season) %>% 
  summarize(avg_season_sales = mean(weekly_sales)) %>% 
  ggplot(aes(season, avg_season_sales, 
             fill = season, color = season)) + 
  geom_col() + 
  geom_point(data = avg_holiday_sales, 
             aes(season, avg_sales, 
                 shape = holiday), 
             color = 'black') + 
  coord_cartesian(ylim = c(900000, 1500000))

walmart_sales %>% 
  filter(holiday_flag == 0) %>%
  mutate(season = getSeason(date)) %>% 
  group_by(season) %>% 
  summarize(avg_season_sales = mean(weekly_sales)) %>% 
  ggplot(aes(season, avg_season_sales, 
             fill = season, color = season)) + 
  geom_col() + 
  geom_point(data = avg_holiday_sales, 
             aes(season, avg_sales, shape = holiday), color = 'black') + 
  coord_cartesian(ylim = c(900000, 1500000))


print('Accounting for season in the traditional calendar sense and removing any sales related to promotional holiday events, we can see that Winter is the highest selling season, followed by Fall, Spring, and Summer last. In regards to the Walmart holiday events, Thanksgiving on average has the best performance compared to the regular non-holiday season it falls within. Labor Day and the Superbowl also slightly outperform the average sales for the seasons they fall within. Only Christmas does not sell higher on average than the aggregate average for Winter. Given the high sales points leading up to the Christmas event, my guess is that Walmart does not do any markdowns/promotional holiday events leading up to Christmas. As of Christmas day they have markdowns to get rid of excess holiday inventory.')

#4f. Monthly and Semester View ----

walmart_sales <- 
  walmart_sales %>% 
  mutate(month = month(date, label = TRUE))

walmart_sales %>% 
  group_by(month) %>% 
  summarize(avg_sales = mean(weekly_sales)) %>% 
  ggplot(aes(month, avg_sales)) + 
  geom_col() + 
  ggtitle('Average sales per month of data provided')

walmart_sales %>%
  mutate(date = floor_date(date, unit = 'month')) %>%
  group_by(date, month) %>%
  summarize(weekly_sales = sum(weekly_sales)) %>%
  ggplot(aes(date, weekly_sales, fill = month)) + 
  geom_col() + 
  ggtitle('Total sales per month') + 
  scale_x_date(breaks = '3 months') + 
  theme(axis.text.x = element_text(angle = -90, vjust = -0.05)) + 
  scale_fill_discrete()

walmart_sales %>%
  mutate(date = floor_date(date, unit = 'month')) %>%
  group_by(date, month) %>%
  summarize(weekly_sales = sum(weekly_sales)) %>%
  ggplot(aes(date, weekly_sales)) + geom_col() + ggtitle('Total sales per month') + facet_wrap(~month, scales = 'free') + scale_x_date() + theme(axis.text.x = element_text(angle = -90, vjust = -0.05), legend.position = 'none') + scale_fill_discrete()

print('We can view the sales per month on an average rate, but it may be unreliable to form any insights from being that from our data there are not the same amounts of sales data for each month. Likewise, depending on when the sales were recorded, some months may have 5 weekly sales entries as opposed to 4. A chronological month view gives us insight into the sales of these 45 stores as a sum from month to month. We can observe that December is consistently the highest performing month just as January is the lowest performing month. Other months have some variability in their performance. The facet view helps us speak to this, as we can more clearly see that months like February remain relatively consistent in their sales overall, but we begin to see performance changes across our three years in the months of: March, April, June, July, August, September, and October. The most concerning of course being those that have seen a decrease in sales from previous years (April, July, September, October). Changes in sales could possibly be due to factors we have already measured in our dataset. In the next section, we will observe how much (if at all) temperature, CPI, unemployment, and fuel price affect sales.')

#A new function to plot and observe sales month by select stores
plot_monthly_sales <- function(store_num_vector = c(1,3,5)){
walmart_sales %>%
  mutate(date = floor_date(date, unit = 'month')) %>%
  group_by(store, date, month) %>%
  summarize(weekly_sales = sum(weekly_sales)) %>%
  filter(store %in% store_num_vector) %>%
  ggplot(aes(date, weekly_sales, fill = month)) + geom_col() + ggtitle('Total sales per month') + facet_wrap(~store) + scale_x_date() + theme(axis.text.x = element_text(angle = -90, vjust = -0.05)) + scale_fill_discrete()
}

plot_monthly_sales()

walmart_sales <- 
  walmart_sales %>%
  mutate(season_year = getSeason(date)) %>%
  mutate(season_year = if_else(season_year %in% c('Fall','Spring','Summer'), 
                               paste0(season_year, year(date)), season_year)) %>%
  arrange(season_year,date) %>%
  mutate(season_year = 
           if_else(!grepl('^W.', season_year), 
                   season_year, 
                   if_else(month == 'Dec', 
                           paste0(season_year, year(date)), 
                           paste0(season_year,as.numeric(year(date)-1))))) 

walmart_sales_semester_agg<- 
  walmart_sales %>%
  group_by(season_year, store) %>%
  summarize(weekly_sales = sum(weekly_sales)) %>%
  pivot_wider(names_from = 'season_year', values_from = 'weekly_sales') %>% 
  mutate(FW2009 = Winter2009, 
         FW2010 = Fall2010+ Winter2010, 
         FW2011 = Fall2011+Winter2011, 
         FW2012 = Fall2012,
         SS2010 = Spring2010+Summer2010, 
         SS2011=Spring2011+Summer2011, 
         SS2012=Spring2012+Summer2012) %>%
  select(store, FW2009:SS2012) %>%
  pivot_longer('FW2009':'SS2012', names_to = 'semester')

walmart_sales_semester_agg$semester <- factor(walmart_sales_semester_agg$semester, levels = c(walmart_sales_semester_agg$semester[c(1,5,2,6,3,7,4)]))


walmart_sales_semester_agg %>%
  group_by(semester) %>%
  summarize(value = sum(value))%>% 
  arrange(semester) %>% 
  filter(semester %in% c('FW2010','SS2011','FW2011','SS2012')) %>%
  ggplot(aes(semester, value)) + geom_col() + ggtitle('Sales by Semester')+  coord_cartesian(ylim = c(1100000000, 1300000000))

#a new function to plot and observe semester sales by select stores
plot_semester_sales_store <- function(store_num_vector = c(1,3,5)){
walmart_sales_semester_agg %>% 
  filter(store %in% store_num_vector, semester %in% c('FW2010', 'SS2011','FW2011','SS2012')) %>%
    ggplot(aes(semester, value)) + 
    geom_col() + 
    facet_wrap(~store) + 
    theme(axis.text.x =element_text(angle = -45, vjust = -0.5)) + 
    labs(title = 'Sales by semester and store')+
    xlab('Semester')+
    ylab('Sales')
}
  
plot_semester_sales_store()

print('We should avoid making any conclusions to sales patterns viewed by semester being that we do not have full reporting of every season. We are only given sales data starting from the near end of FW2009, and ending at the relative start of Fall 2012, meaning we only have two full years of data to compare semesters. Assuming we look at a semester year beginning in the Fall and End with Summer, like an academic semester calendar, then our two full years begin with FW2010 and end with SS2012. In FW2010, the total sales across stores was 1.237 B USD. Then Walmart seemed to take a sizeable hit in sales as their sales figure dropped to 1.195 B USD. Then it rose to comparable highs of 1.269 and 1.270 B in the subsequent semesters.')

#5. Statistical analysis and models of store 1 ----
# 
# For Store 1 – Build prediction models to forecast demand
# 
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# 
# Change dates into days by creating new variable.


store1_all_dates <- walmart_sales %>% 
  mutate(value = 1) %>%
  spread(holiday, value, fill = 0) %>% #this line one-hot encodes the holiday variable 
  filter(store ==1) %>%
  select(date, weekly_sales, temperature, fuel_price, cpi, unemployment, christmas, `labour day` , superbowl, thanksgiving) %>%
  mutate(day_of_year = yday(date)) %>% #this changes the date into a numeric integer based on day of the year
  arrange(date)
  
full_date_model <- lm(log(weekly_sales) ~ temperature + cpi + fuel_price +  unemployment + christmas + `labour day` + superbowl + thanksgiving + day_of_year + I(temperature^2) + I(cpi^2) + I(fuel_price^2) + I(unemployment^2),  data = store1_all_dates)

summary(full_date_model)

print('This linear model accounts for 34.9% of the variability of sales. I have put weekly sales as a dependent variable into log form for better readability. The coefficients for all independent variables explains the effect of % change in sales given an increase of unit for that independent variable. Multivariate regression is not a causal model, this is only to imply the inherent mathematical relationship observed within data provided to our model. Our most significant values from the perspective of a low p-value are temperature, christmas, thanksgiving, and day of year. All other variables have >5% chance of affecting sales due to random chance and therefore should be avoided as deeming statistically significant. I will describe below the variables and my hypothesis as to how they affect our dependent variable.
      
Temperature: For store 1, the observed temperatures range between 35 and 91 degrees farenheit. At large, I would consider that the relationship between temperature and sales is a closed parabola. At either end of extremely cold or hot sales will take a hit as a threshold to comfortably commute and shop will have been passed. However, we must consider temperature in the context of the model for a specific store. The relationship between temperature and sales will change for each store based on their climate. In regards to store 1, our model posits a negative coefficient (-0.02% decrease in sales per increase in 1 degree farenheit). However, this could be caused by correlation between time of year and sales: profit driving holidays and cultural periods are in the winter (Thanksgiving, period before Christmas, Halloween) and less so in the Summer. This explains the observed relationship between temperature and sales.
      
CPI: This is a measurement of inflation in the economy. The higher the CPI, the more expensive things are, generally. With each increase of 1 for our CPI, our model assumes a 0.5% increase in sales. This is not to mean that there is an actual increase of sales being made. If our CPI is higher, then in general there is more inflation, resulting in higher costs at large. At higher CPI, Walmart products could cost more, thus resulting in "higher" sales. 

Fuel Price: Fuel Price is highly related to CPI, but it has its own seasonality. There is likely some degree of multi-collinearity. I do not believe this to have a significant impact on sales. Will remove from our next model.
      
Unemployment: I would hypothesize that increased rates of unemployment would result in a decrease in sales. Our model says otherwise, but this coefficient of 1.6% increase has a p-value of 61.4% This relationship has a pretty decent chance of being due to randomness.
      
All the holiday variables are to capture our possible holiday based outliers and factor them into our model. These are cultural sales events that would not be able to be adequately modeled otherwise. For example, the "Thanksgiving" holiday is actually Black Friday, but no other variables available would have been able to inform our model that on a random Friday in November sales will increase by about 22% for no apparent reason.
      
Day of year: This is a new variable made to put date into a measure of where it occurs within the year. From the first day of the year to the last, there should be a visible positive relationshp given that New Years day seems to mark some of the lowest sales in the year and the weeks leading up to Christmas represent some of the highest. Our model interprets a coefficient of 0.02% increase in sales for each day.
      
This model gives us a framework to forecast sales based on our independent variables. Unfortunately, given that we have used all of our data set to build the model, we have no observations left to test the model on. In the next code blocks, a new model is devised based around iterative fine-tuning between a training/test split of the whole dataset.')


#training df
 store1_train <- walmart_sales %>% 
  mutate(value = 1) %>%
  spread(holiday, value, fill = 0) %>% #this line one-hot encodes the holiday variable 
  filter(store ==1) %>%
  select(date, weekly_sales, temperature, fuel_price, cpi, unemployment, christmas, `labour day` , superbowl, thanksgiving) %>%
  mutate(day_of_year = yday(date)) %>% #this changes the date into a numeric integer based on day of the year
  filter(str_detect(date, regex('^2011.*$'))) #this filters the data to only dates in 2011

#test df
store1_test <- walmart_sales %>% 
  mutate(value = 1) %>%
  spread(holiday, value, fill = 0) %>% #this line one hot encodes the holiday variable 
  filter(store ==1) %>%
  select(date, weekly_sales, temperature, fuel_price, cpi, unemployment, christmas, `labour day` , superbowl, thanksgiving) %>%
  mutate(day_of_year = yday(date)) %>% #this changes the date into a numeric integer based on day of the year
  filter(!str_detect(date, regex('^2011.*$'))) #this filters the data to dates not in 2011

#our new model
single_year_test_model <- lm(log(weekly_sales) ~ unemployment + I(temperature^2)+ christmas + `labour day` + superbowl + thanksgiving ,  data = store1_train)

summary(single_year_test_model)

store1_test <- store1_test %>%
  mutate(prediction = predict(single_year_test_model, newdata = store1_test))

ggplot(store1_test, aes(date, weekly_sales)) + geom_line() + geom_point(aes(date, exp(prediction), color = 'prediction'), color = 'red') + ggtitle('Forecast of Sales for Store 1') + labs(subtitle = 'For 2010 and 2012')

print('The best model I could construct given our data has an R-squared of ~0.221, meaning it explains about 22.1% of the variability of weekly sales. Despite this low value, it does appear to graphically represent our test data as best as it can while being built from disparate training data, albeit it does predict on the low end for 2012. By numbers alone, our first model built from the full data set appears to be "more accurate," but as we have no new data to test on we will never know if this is actual model accuracy or overfitting. Meanwhile, our training model perform "worse" judging by r-squared, but we can see when applied to our test data set appears to represent the variability of sales with some degree of accuracy. 

One less ideal act I took within this model is selectively choosing the range of dates to train the model on. To make sure the model took into account the seasonality of our independent variables and all holidays, I made sure there was one full year range of data to be trained on. Because 2011 is the only continuous year of dates available, I had no choice but to train from this range. I will develop one more model in which I will iterate between years 2010, 2011, and 2012 to create the range of dates for the model. This way, seasonality and holidays can be observed in a still pseudo-random method.')

#initializing counts for a for loop
x<- 1
y<- 54
z<- 107

#initializing empty vectors to later filter by index
num_vec1 <- vector('numeric', 18L)
num_vec2 <-  vector('numeric', 17L)
num_vec3 <-  vector('numeric', 17L)

for (i in 1:18){
  num_vec1[i] <- x
  x<- x+3
}

for(i in 1:17){
  num_vec2[i] <- y
  y<- y+3
}

for(i in 1:17){
  num_vec3[i] <- z
  z <- z+3
}

count <- 1
dates_vec <- vector('numeric', 52L)
for (i in 1:18){
  dates_vec[count] <- num_vec1[i]
  dates_vec[count+1] <- num_vec2[i]
  dates_vec[count+2] <- num_vec3[i]
  count <- count + 3}

#the correct index values in the correct order in which we want to sample observations
dates_vec <- dates_vec[1:52]

store1_all_dates$date[dates_vec] # several dates are NA since 2012 does not go up that high. We will replace with its 2010 or 2011 comparable

dates_vec[c(42,45,48,51)]
dates_vec[42] <- 42
dates_vec[45] <- 97
dates_vec[48] <- 48
dates_vec[51] <- 103

rndm_date_test_model <- lm(log(weekly_sales) ~ unemployment + temperature + cpi + fuel_price + christmas + `labour day` + superbowl + thanksgiving + day_of_year, data = store1_all_dates[dates_vec,])


rndm_dates_removed_store_1 <- store1_all_dates[-dates_vec,]

rndm_dates_removed_store_1 <- rndm_dates_removed_store_1 %>%
  mutate(prediction = predict(rndm_date_test_model, newdata = rndm_dates_removed_store_1))

summary(rndm_date_test_model)

ggplot(rndm_dates_removed_store_1, aes(date, weekly_sales)) + geom_line() + geom_point(aes(y = exp(prediction)), color = 'red') + ggtitle('Forecast of sales for Walmart Store 1') + ylab('Weekly Sales $') + labs(subtitle = 'Prediction in red')

print("This looks like our best performing model yet! One annoying thing is that it does not capture the huge spike in sales the week ending before Christmas. Even though this is not considered a holiday event from Walmart's perspective, we should use our domain knowledge of there being last minute christmas gift shopping during this week")

store1_all_dates <- store1_all_dates %>% mutate(before_xmas = if_else(grepl(regex('^.{4}-12-24|^.{4}-12-23|^.{4}-12-22|^.{4}-12-21|^.{4}-12-20|^.{4}-12-19|^.{4}-12-18'), date), 1, 0))

rndm_date_test_model <- lm(log(weekly_sales) ~ unemployment + temperature + cpi + fuel_price + christmas + `labour day` + superbowl + thanksgiving + before_xmas + day_of_year, data = store1_all_dates[dates_vec,])

rndm_dates_removed_store_1 <- store1_all_dates[-dates_vec,]

rndm_dates_removed_store_1 <- rndm_dates_removed_store_1 %>%
  mutate(prediction = predict(rndm_date_test_model, newdata = rndm_dates_removed_store_1))

summary(rndm_date_test_model)

ggplot(rndm_dates_removed_store_1, aes(date, weekly_sales)) + geom_line() + geom_point(aes(y = exp(prediction)), color = 'red') + ggtitle('Forecast of sales for Walmart Store 1') + ylab('Weekly Sales $') + labs(subtitle = 'Prediction in red')

print('So much better!')
