############################## Q1 ##############################
global_economy

list_of_countries <- global_economy %>% distinct(Country)

countries_to_keep = c("Canada", "Japan", "United States", "Australia", "China")

global_ec_2 <- global_economy %>% 
  filter(Country %in% countries_to_keep) %>% 
  mutate(gdp_per_capita = GDP/Population)

global_ec_2 %>%  autoplot(gdp_per_capita)
global_ec_2 %>%  autoplot(gdp_per_capita/CPI)

############################## Q2 ##############################

#per capita and inflation adjustment for gdp

#calendar transformation
aus_livestock_adj <- aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers" & State == "Victoria") %>% 
  mutate(count_per_day = Count/days_in_month(Month))

aus_livestock_adj %>% 
  autoplot(count_per_day)

aus_livestock_adj %>% 
  autoplot(Count)

#join population and do a per capita demand?
#or is that too complicated... and it's only 3 years, so probably not intended
vic_elec
vic_elec %>% autoplot()


#box cox
aus_production %>% autoplot(Gas)

lambda <- aus_production %>%
  features(Gas, features = guerrero) %>% #this chooses a lambda for us
  pull(lambda_guerrero)

lambda

aus_production %>%
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))


############################## Q3 ##############################
canadian_gas %>% autoplot()
#we want to use box cox when the variance increases as the variable increases
#but the maximum variance seems to be in the middle


############################## Q4 ##############################

aus_retail
?aus_retail

set.seed(123456789)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries %>% autoplot(Turnover)

lambda <- myseries %>%
  features(Turnover, features = guerrero) %>% #this chooses a lambda for us
  pull(lambda_guerrero)

lambda

myseries %>%
  autoplot(box_cox(Turnover, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed Turnover with $\\lambda$ = ",
         round(lambda,2))))

############################## Q5 ##############################


aus_production %>% autoplot(Tobacco)

lambda <- aus_production %>%
  features(Tobacco, features = guerrero) %>% #this chooses a lambda for us
  pull(lambda_guerrero)

lambda

aus_production %>%
  autoplot(box_cox(Tobacco, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed Tobacco Production with $\\lambda$ = ",
         round(lambda,2))))

ansett_mel_syd <- ansett %>% 
  filter(Airports == "MEL-SYD" & Class == "Economy") 

ansett_mel_syd %>%   
  autoplot(Passengers)

############################## Q7 ##############################
gas <- tail(aus_production, 5*4) %>% select(Gas)

gas %>% autoplot()

head(gas)

gas_decomp <- gas %>%
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) %>%
  components() 
  
gas_decomp %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition of total
                  gas production")


#c - yes there is a quarterly component where gas production peaks in Q3 of each year

# plot the seasonally adjusted data

head(gas_decomp)

gas_decomp %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Gas Production (petajoules)",
       title = "Gas Production") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

# create an outlier
gas_outlier <- gas
gas_outlier$Gas[7] <- gas_outlier$Gas[7] +300
gas_outlier$Gas 


gas_outlier_decomp <- gas_outlier %>%
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) %>%
  components() 

gas_outlier_decomp %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Gas Production (petajoules)",
       title = "Gas Production") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

#outlier has big impact on both the trend and seaonsally adjusted data in that one period
#outlier also messes up the aseonal adjustment (the seasonal component)

#impact of outlier at end of data

length(gas_outlier$Gas)

gas_outlier <- gas
gas_outlier$Gas[19] <- gas_outlier$Gas[19] +300
gas_outlier$Gas 


gas_outlier_decomp <- gas_outlier %>%
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) %>%
  components() 

gas_outlier_decomp %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Gas Production (petajoules)",
       title = "Gas Production") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

# not sure what the difference is, seems to compress the trend and the saonsal adjustment together and depress the seasonal compnent


############################## Q8 ##############################

aus_retail
?aus_retail

set.seed(123456789)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries


x11_dcmp <- myseries %>%
  model(x11 = X_13ARIMA_SEATS(Turnover ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of retail turnover using X-11.")

#the seasonal effects are going down over time instead of up
#the residual is also going down over time



############################## Q9 ##############################

# the seasonal component is increasing slightly over time
# there is a consistent upward trend that has leveled off for a few years after 1990, but has picked back up
# the graph is usually quite stable, but there is a significant dip in the residual after 1990, particuarly in march
# yes the 1991 recession is visible in the residual


############################## Q10 ##############################

canadian_gas
?canadian_gas

canadian_gas %>% autoplot(Volume)
canadian_gas %>% gg_subseries(Volume)
canadian_gas %>% gg_season(Volume, labels = "both")

#the seasonal pattern starts out quite steady, usually slightly higher in the winter montsh and lower in the summer months
# but evolves to have a very sgrongstrong dip in the summer months
# and then changes again have a significant dip in certain months such as feb 

canadian_gas %>%
  model(
    STL(Volume ~ trend()+
          season(period = 12),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

#trend window is the number of consecutive observations to be used when estimating the trend-cycle
#season window is the number of consecutive years to be used in estimating each value in the seasonal component

canadian_gas %>%
  model(
    STL(Volume ~ trend()+
          season(period = 12),
        robust = TRUE)) %>%
  components() -> canadian_gas_model


canadian_gas_model %>% gg_season(season_12)
#the seasonal component gets stronger around the 1980s
#in more recent years it's gotten quite seasonal with dips in Feb, Jun, and Sep

canadian_gas_model %>% autoplot(season_adjust)

x11_dcmp <- canadian_gas %>%
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of Canadian Gas using X-11.")

#the residual component is much larger in the X-11 method, especially in the earlier years
#this is likely because the shape of the seasonal component is not allowed to change over time


