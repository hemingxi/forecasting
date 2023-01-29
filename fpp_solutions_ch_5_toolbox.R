############################## Q01 ##############################

# Produce forecasts for the following series using whichever of NAIVE(y), SNAIVE(y) or RW(y ~ drift()) is more appropriate in each case:
#   
# Australian Population (global_economy)
# Bricks (aus_production)
# NSW Lambs (aus_livestock)
# Household wealth (hh_budget).
# Australian takeaway food turnover (aus_retail).

#Aus Population 



aus_ec <- global_economy %>%
  filter(Country == "Australia") 
  
aus_ec

aus_ec %>% 
  autoplot(Population) +
  labs(y = "Population", title = "Population of Australia")

aus_ec_train <- aus_ec %>%
  filter_index("1960" ~ "2010")
aus_ec_test <- aus_ec %>%
  filter_index("2011" ~ .)

aus_ec_train_fit <- aus_ec_train %>% 
  model(RW(Population ~ drift()))

count(aus_ec)
count(aus_ec_train)

aus_ec_fc <- aus_ec_train_fit %>% forecast(h = 7)

aus_ec_fc

aus_ec_fc %>%
  autoplot(aus_ec_train, level = NULL) +
   autolayer(aus_ec_test, Population, colour = "black") +
  labs(
    y = "Population",
    title = "Forecasts for Australian Population"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

#bricks

bricks <- aus_production %>%
  select(Bricks) %>%
  filter_index("1970 Q1" ~ "2005 Q2")
  

bricks

bricks %>% 
  autoplot(Bricks) +
  labs(y = "Brick Production", title = "Yr Qtr")

bricks_train <- bricks %>%
  filter_index("1970 Q1" ~ "2002 Q4")
bricks_test <- bricks %>%
  filter_index("2002 Q4" ~ .)

count(bricks_test)

bricks_train_fit <- bricks_train %>% 
  model(SNAIVE(Bricks ~ lag("year")))

bricks_fc <- bricks_train_fit %>% forecast(h = 7)

bricks_fc

bricks_fc %>%
  autoplot(bricks, level = NULL) +
  autolayer(bricks_test, Bricks, colour = "black") +
  labs(
    y = "Brick Production",
    title = "Forecasts for Australian Brick Production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


#NSW Lambs

aus_livestock

aus_livestock %>% distinct(Animal)
aus_livestock %>% distinct(State)
aus_livestock %>% distinct(Month)
aus_livestock %>% distinct(Month) %>% arrange(desc(Month))

lambs <- aus_livestock %>%
  select(Bricks) %>%
  filter_index("1970 Q1" ~ "2005 Q2")

lambs <- aus_livestock %>%
  filter(Animal == "Lambs" & State == "New South Wales") 

lambs

lambs %>% 
  autoplot(Count) +
  labs(y = "Lamb Count", title = "Month")

lambs_train <- lambs %>%
  filter_index("2010 Jan" ~ "2017 Dec")
lambs_test <- lambs %>%
  filter_index("2018 Jan" ~ .)

count(lambs_test)

lambs_train_fit <- lambs_train %>% 
  model(SNAIVE(Count))

lambs_fc <- lambs_train_fit %>% forecast(h = 12)

lambs_fc

lambs_fc %>%
  autoplot(lambs_train, level = NULL) +
  autolayer(lambs_test, Count, colour = "black") +
  labs(
    y = "Lamb Production",
    title = "Forecasts for Australian Lamb Production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


#household wealth
hh_budget

hh_budget %>% distinct(Country)
hh_budget %>% distinct(Year)
hh_budget %>% distinct(Year) %>% arrange(desc(Year))

#1995 to 2016

aus_wealth <- hh_budget %>%
  filter(Country == "Australia")

aus_wealth %>% autoplot(Wealth)

hh_budget %>% autoplot(Wealth)

aus_wealth_train <- aus_wealth %>%
  filter_index("1995" ~ "2013")
aus_wealth_test <- aus_wealth %>%
  filter_index("2014" ~ .)

count(aus_wealth_test) #3 forecast periods

aus_wealth_train_fit <- aus_wealth_train %>% 
  model(RW(Wealth ~ drift())) #this creates a mable - a model table)

aus_wealth_train_fit

aus_wealth_fc <- aus_wealth_train_fit %>% forecast(h = 3) #this creates the forecast tsiblle

aus_wealth_fc


aus_wealth_fc %>%
  autoplot(aus_wealth_train, level = NULL) +
  autolayer(aus_wealth_test, Wealth, colour = "black") +
  labs(
    y = "HH Wealth",
    title = "Forecasts for Australian HH Wealth"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


############################## Q02 ##############################

gafa_stock

gafa_stock %>% distinct(Symbol)

#create a new index using trading days

fb <- gafa_stock %>% 
  filter(Symbol == "FB") %>% 
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)


fb %>% autoplot(Close)

fb_2015 <- fb %>% filter(year(Date) == 2015)
fb_2015 %>% autoplot(Close)

fb_fit <- fb_2015 %>%
  model(Drift = NAIVE(Close ~ drift()))

fb_fit

fb_jan_2016 <- fb %>% 
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

fb_fc <- fb_fit %>% 
  forecast(new_data = fb_jan_2016)

fb_fc


fb_fc %>% 
  autoplot(fb_2015, level = NULL)+
  autolayer(fb_jan_2016, Close, colour = "black") +
  labs(y="$US",
       title = "FB daily close",
       subtitle = "Jan 2016 to Jan 2016"
       ) +
  guides(colour = guide_legend(title = "Forecast"))



num_last_row <- nrow(fb_2015)

fb_2015[1,]$Close
fb_2015[num_last_row,]$Close

rise = fb_2015[num_last_row,]$Close -fb_2015[1,]$Close

run = fb_2015[num_last_row,]$day - fb_2015[1,]$day

slope = rise/run

slope

#starting point - close = 78.4, day = 253

x_0 <- fb_2015[1,]$day
y_0 <- fb_2015[1,]$Close

x_0
y_0

fb_jan_2016_pred <- fb_jan_2016 %>% 
  mutate(drift_fc = y_0 + (day-x_0)*slope)

fb_jan_2016_pred %>% 
  autoplot(drift_fc)



fb_fc %>% 
  autoplot() +
  autolayer(fb_jan_2016_pred, drift_fc, colour = "red")



############################## Q03 ##############################


# Extract data of interest
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production

# Define and estimate a model
fit <- recent_production %>% model(SNAIVE(Beer))

fit

# Look at the residuals
recent_production %>% autoplot(Beer)
fit %>% gg_tsresiduals()


# Look a some forecasts
fit %>% forecast() %>% autoplot(recent_production)

#residuals look like there's constant variance over time

#histogram of residuals show that they are more likely to be negative
#which means there's a downward trend in the data that we can account for in a better model
#and our forecast is currently biasned

#SNAIVE has significant auto correlations at t=4, meaning if same quarter last year was high, 
#this qtr is likely to be slightly lower
#meaning we can improve on this model

############################## Q04 ##############################

aus_production

aus_production %>% autoplot(Bricks)

fit <- aus_production %>% model(SNAIVE(Bricks))
fit %>% gg_tsresiduals()
fit %>% forecast() %>% autoplot(aus_production)
#residuals do not have constant variance over time

#histogram shows an upward trend that SNAIVE is not accounting for

#there is significant negative autocorrelation peaking at time 8 and significant positive peaking at t=20

global_economy

global_economy %>% distinct(Country)

aus_exports <- global_economy %>% 
  filter(Country == "Australia")

aus_exports %>% autoplot(Exports)

fit <- aus_exports %>% model(NAIVE(Exports))
fit %>% gg_tsresiduals()
fit %>% forecast() %>% autoplot(aus_exports)

#residuals have slightly increased variance after 2000
#histogram shows centered residuals around 0, probably roughly normal?
#there's not significant autocorrelation, except a little bit at time 0
#so this is a decent model 


############################## Q05 ##############################

aus_livestock

aus_vic <- aus_livestock %>% 
  filter(State == "Victoria")

aus_vic

fit <- aus_vic %>% model(SNAIVE(Count))
fit %>% filter(Animal =="Calves") %>%  gg_tsresiduals()
fit %>% forecast() %>%autoplot(aus_vic)

#yes these are reasonable benchmarks for this series

############################## Q06 ##############################


# Good forecast methods should have normally distributed residuals.
# False, we can still produce good forecasts without normal residuals

# A model with small residuals will give good forecasts.
# Yes, that indicates lower forecast error

# The best measure of forecast accuracy is MAPE.
# no, it requires that 0 be meaningful, the MASE would be better, comparing the method with a benchmark method

# If your model doesn’t forecast well, you should make it more complicated.
# False, poor forecasts in the testing data could indicate overfitting. If that's the case we'd want to decrease the parameters specified

# Always choose the model with the best forecast accuracy as measured on the test set.
# No, in that case you're "overusing" your test set and could be overfitting your data on that


############################## Q07 ##############################

aus_retail
?aus_retail

set.seed(123456789)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries

myseries_train <- myseries %>%
  filter(year(Month) < 2011)

myseries %>% autoplot(Turnover) +
  autolayer(myseries_train, Turnover, color = "red")

fit <- myseries_train %>% 
  model(SNAIVE(Turnover))

fit %>%  gg_tsresiduals()

#the residuals have a significant positive tail
#the residuals are also autocorrelated

fc <- fit %>%  forecast(new_data = anti_join(myseries, myseries_train))
fc
fc %>% autoplot(myseries)

fit
fit %>% accuracy() #on training data - in sample training accuracy
fc
fc %>% accuracy(myseries) # on testing data - use a forecast object and provide the actual values to compare

############################## Q08 ##############################
#skip

############################## Q09 ##############################
#skip

############################## Q10 ##############################
#skip

############################## Q11 ##############################

aus_production %>% autoplot(Bricks)

#check how many NAs in each column
colSums(is.na(aus_production))

aus_brick <- aus_production %>% 
  select(Quarter, Bricks)

aus_brick

#check which periods have NAs
aus_brick %>% filter(is.na(Bricks))

#filter for only the period that has data
aus_brick <- aus_brick %>% 
  filter_index( . ~ "2005 Q2")

colSums(is.na(aus_brick))

aus_brick_model <- aus_brick %>%
  model(
    STL(Bricks ~ trend() +
                 season())) #use a changing seasonal window, instead of window = "periodic"
  
aus_brick_model

aus_brick_model %>% 
  components() %>%
  autoplot()

# part c - use a naive method to produce forecasts of the sadj data

?components

aus_brick_model %>% 
  components() %>% 
  select(Quarter, season_adjust) %>% 
  autoplot

components(aus_brick_model) %>%
  as_tsibble() %>%
  autoplot(Bricks, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Bricks",
       title = "Bricks")

aus_brick_season_adj <- components(aus_brick_model) %>%
  as_tsibble() %>% 
  select(-.model)

aus_brick_season_adj


aus_brick_sadj_model <- aus_brick_season_adj %>% 
  model(NAIVE(season_adjust))

aus_brick_sadj_model %>% 
  forecast() %>% 
  autoplot(aus_brick_season_adj)

# part d - fit a decomposition model - and get the re-seasonalized forecasts

fit_dcmp <- aus_brick %>% 
  model(stlf = decomposition_model(
    STL(Bricks ~ trend() +
          season()),
    NAIVE(season_adjust)
  ))


fit_dcmp %>% 
  forecast() %>% 
  autoplot(aus_brick)

# part e - check residuals

fit_dcmp %>% gg_tsresiduals()

#residuals are auto correlated, meaning there is information in the model that we are still missing

# part f - repeat with robust decomp

fit_dcmp <- aus_brick %>% 
  model(stlf = decomposition_model(
    STL(Bricks ~ trend() +
          season(), robust=TRUE),
    NAIVE(season_adjust)
  ))


fit_dcmp %>% 
  forecast() %>% 
  autoplot(aus_brick)

fit_dcmp %>% gg_tsresiduals()

# part g - compare stl vs snaive

aus_brick %>%
  as_dataframe() %>% 
  select(Quarter) %>% 
  arrange(desc(Quarter))

aus_brick_test <- aus_brick %>% 
  filter_index("2003 Q3" ~ "2005 Q2")

aus_brick_train <- aus_brick %>% 
  anti_join(aus_brick_test)

nrow(aus_brick)
nrow(aus_brick_test)
nrow(aus_brick_train)

#fit SNAIVE

#fit decomp

aus_brick_fit <- aus_brick_train %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() +
            season(), robust=TRUE),
      NAIVE(season_adjust)),
    s_naive = SNAIVE(Bricks)
  )

aus_brick_fit

fc <- aus_brick_fit %>% 
  forecast(new_data=aus_brick_test)

fc %>% autoplot(aus_brick)

fc %>% accuracy(aus_brick)

#the decomposition model has a lower MASE, MAPE, so it is a better model

#model code:

#the residuals have a significant positive tail
#the residuals are also autocorrelated

fc <- fit %>%  forecast(new_data = anti_join(myseries, myseries_train))
fc
fc %>% autoplot(myseries)

fit
fit %>% accuracy() #on training data - in sample training accuracy
fc
fc %>% accuracy(myseries) # on testing data - use a forecast object and provide the actual values to compare

#no it does not make much of a difference
#robust makes it so that outliers do not affect our trend/cycle that much

# part g - repeat with robust decomp

# Testing code - ignore#
# aus_production %>%
#   select(everything()) %>%  # replace to your needs
#   summarise_all(~ sum(is.na(.)))
# 
# us_retail_employment <- us_employment %>%
#   filter(year(Month) >= 1990, Title == "Retail Trade")
# 
# us_retail_employment
# us_retail_employment %>% distinct(Series_ID)
# 
# dcmp <- us_retail_employment %>%
#   model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
#   components() %>%
#   select(-.model)
# 
# 
# dcmp %>%
#   model(NAIVE(season_adjust)) %>%
#   forecast() %>%
#   autoplot(dcmp) +
#   labs(y = "Number of people",
#        title = "US retail employment")


############################## Q12 ##############################



















