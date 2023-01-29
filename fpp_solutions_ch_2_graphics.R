library(data.table)
library(fpp3)
library(readr)
library(USgas)
library(scales)
library(readxl)


melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

# gafa_stick - historical storck prices for google, amazon, facebook, and apple
# PBS - monthly medicare prescription - # of scripts and costs in $AUD
# vic_elec - half hourly electricity demand for Victoria
# pelt - Hudson's Bay Company trading records for Hare and Lynx furs

############################## Q1 ##############################

autoplot(gafa_stock) #daily
autoplot(PBS) #monthly
autoplot(vic_elec) # every 30 mins
autoplot(pelt) #yearly

head(gafa_stock)

# does not work because time element is required
# gafa_stock %>% 
  # filter(Symbol == "AAPL") %>%
  # select(Symbol, Date, Close) %>%
  # group_by(Symbol) %>%
  # summarize(MaxClose = max(Close)) %>%
  # filter(Close == MaxClose) -> max_aapl


############################## Q2 ##############################
gafa_stock %>%
  as_tibble %>%
  group_by(Symbol) %>%
  summarize(max_close = max(Close)) -> max_close_data

max_close_data %>% 
  inner_join(gafa_stock, by="Symbol") %>% 
  filter(max_close == Close)


#find indices
attr(gafa_stock, "index")
attr(gafa_stock, "key")
index_var(gafa_stock)
key_vars(gafa_stock)

############################## Q3 ##############################
#https://bit.ly/fpptute1
dir()
tute1 <- readr::read_csv("./Data/tute1.csv")
View(tute1) 

#convert to tsibble
mytimeseries <- tute1 %>% 
  mutate(Quarter = yearquarter(Quarter)) %>%  #convert from text to monthly time oject using yearmonth
  as_tsibble(index = Quarter)



#pivot longer makes the dataset in long format
names(mytimeseries)

mytimeseries_long <- mytimeseries %>% 
  pivot_longer(-Quarter) #you select all the variables that you want to stack (not the keys)
# -quarter means all but one

mytimeseries %>%
pivot_longer(-Quarter) %>% 
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line()+
  facet_grid(name ~ ., scales="free_y")

############################## Q4 ##############################

view(us_total)

#keys allow us to store multiple time series in a single object

us_total_gas_ts <- us_total %>% 
  as_tsibble(key=state, index=year)

new_england_states <-c("Maine", "Vermont", "New Hampshire", "Massachusetts", 
                       "Connecticut", "Rhode Island")

us_total_gas_ts %>% distinct(state) 
  
us_total_gas_ts %>% 
  filter(state %in% new_england_states) %>% 
  ggplot(aes(x=year, y=y, color=state))+
  labs(y="Gas Consumption")+
  geom_line()+
  scale_y_continuous(label=comma)+
  facet_grid(state ~ ., scales="free_y")


############################## Q5 ##############################
#https://bit.ly/fpptourism

tourism_my <- readxl::read_excel("./Data/tourism.xlsx")


mytimeseries <- tourism_my %>% 
  mutate(Quarter = yearquarter(Quarter)) %>%  #convert from text to monthly time oject using yearmonth
  as_tsibble(index = Quarter, key=c(Region, State, Purpose))

mytimeseries
tourism

mytimeseries %>% distinct(Region)
mytimeseries %>% distinct(State)
mytimeseries %>% distinct(Purpose)


# gafa_stock %>%
#   as_tibble %>%
#   group_by(Symbol) %>%
#   summarize(max_close = max(Close)) -> max_close_data

avg_trips<- mytimeseries %>% 
  as_tibble %>%
  group_by(Region, Purpose) %>%
  summarize(average_trips = mean(Trips))

max_trips <- avg_trips  %>% 
  ungroup() %>% 
  summarize(max_trips = max(average_trips))
 
max_avg_trips <- max_trips$max_trips

region_max_avg_trips <- avg_trips %>% 
  filter(average_trips==max_avg_trips)

mytimeseries %>% 
  inner_join(region_max_avg_trips, by=c("Region", "Purpose"))

mytimeseries %>% 
  group_by(across(-c(Purpose, Region, Trips))) %>% group_vars()


trips_by_state <- mytimeseries %>% 
  group_by(across(c(Quarter, State))) %>% 
  summarize(Trips = sum(Trips))

############################## Q6 ##############################

autoplot(aus_production, vars(Bricks))

autoplot(pelt, vars(Lynx))

autoplot(gafa_stock, vars(Close))

autoplot(vic_elec, (Demand))

?aus_production
?pelt
?gafa_stock
?vic_elec

autoplot(vic_elec, (Demand))+
  labs(title = "Victoria Total Electricity Demand",
       subtitle = "in MW",
       y = "Electricity Demand (MW)")


############################## Q7 ##############################

aus_arrivals
autoplot(aus_arrivals)

aus_arrivals %>% 
  filter(Origin=="US") %>% 
  gg_season(Arrivals, labels="both")
  # facet_grid(Origin ~ ., scales = "free_y")

#japan visitors going down over time
#uk has strong seasonaal patter in the winter months (summer in aus)
#NZ does not show this pattern and is even throughout the year
#peak in Q3 holidays and winter holidays in Q4?
#US has a significant jmp for 2000 for olympics
#similar to UK in that there's more visitors in the winter in US, but not as much as UK

aus_arrivals %>% 
  gg_subseries(Arrivals)

#US has a weird bump in Q3 just afer 1990 that doensèt occur for other countries or other quartesr in that same year


# mytimeseries %>%
#   pivot_longer(-Quarter) %>%
#   ggplot(aes(x = Quarter, y = value, colour = name)) +
#   geom_line() +
#   facet_grid(name ~ ., scales = "free_y")


############################## Q8 ##############################
aus_retail
?aus_retail

set.seed(123456789)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries

#autoplot(), gg_season(), gg_subseries(), gg_lag(),
# ACF() %>% autoplot()

# Can you spot any seasonality, cyclicity and trend? What do you learn about the series?

myseries %>% autoplot(Turnover)
myseries %>% gg_season(Turnover)
myseries %>% gg_subseries(Turnover)
myseries %>% ACF(Turnover) %>% autoplot()

#remove dec to see the cycle better
myseries %>%
  mutate(Turnover=ifelse(month(Month )%in% c(11, 12),NA,Turnover)) %>% 
  autoplot()

#remove or only look at dec to see the cycle better
myseries %>%
  filter(month(Month )%in% c( 12)) %>% 
  autoplot()
  

#seaonsality - highest turnover leading up to dec
#trend - going up over time
#cycle - there's a downward drop towards the more recent years
#increases up to 1990, then there's apause, then increases again up to 2010 and starts dropping until going up again
#there's significant auto correlation at all months, but highest at 12 and 6


############################## Q9 ##############################

us_emp_private <- us_employment %>% 
  filter(Title == "Total Private") 

us_emp_private

?autoplot

us_emp_private %>% autoplot(Employed)
us_emp_private %>% gg_season(Employed)
us_emp_private %>% gg_subseries(Employed)
us_emp_private %>% ACF(Employed) %>% autoplot()
us_emp_private %>% gg_lag()

us_emp_private %>% autoplot(aes_string("Employed"))

#make a function to make plotting things out easier.
#one way is to feed in the var_to_plot as a string "Employed" and to use aes_string
#but I can't get that to work.
#this seems to be giving a solution
# https://www.r-bloggers.com/2019/07/bang-bang-how-to-program-with-dplyr/
# it talks about enquo so that the user doens't need to "quo()" the input
# and about !! which is called bang bang to unquote the input
# there's also some stuff with eval and substitute() that I don't really understand, and didn't work
# but this is good for now!

plot_four_graphs <- function(input_data, var_to_plot) {
  var_to_plot <- enquo(var_to_plot)
  input_data %>% autoplot(!!var_to_plot) %>% print()
  input_data %>% gg_season(!!var_to_plot) %>% print()
  input_data %>% gg_subseries(!!var_to_plot) %>% print()
  input_data %>% ACF(!!var_to_plot) %>% autoplot() %>% print()
}

# plot_four_graphs(us_emp_private, us_emp_private$Employed)
plot_four_graphs(us_emp_private, Employed)

us_gasoline
plot_four_graphs(us_gasoline, Barrels)

pelt

############################## Trying to Read large CSV, selecting vars ##############################
## write mtcars to file
write.csv(mtcars, row.names = FALSE, quote = FALSE, file = "./Data/mtcars.csv")
## now read only the columns beginning with 'd'
# DT <- fread("./Data/mtcars.csv", select = grep("^d", names(fread("mtcars.csv", nrow = 0L))))
DT <- fread("./Data/mtcars.csv", nrow = 0L)
names(DT)

selected_vars <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am" )
DT <- fread("./Data/mtcars.csv", select = selected_vars)

## have a look

############################## Q11 ##############################

aus_livestock %>% 
  distinct(Animal)

pigs_livestock <- aus_livestock %>% 
  filter(Animal == "Pigs", State=="Victoria", year(Month) %in% c(1990:1995))

pigs_livestock %>% autoplot(Count)
pigs_livestock %>% ACF(Count) %>% autoplot()

############################## Q12 ##############################

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog %>% autoplot(Close) + labs(title="After re-indexing")

gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>% 
  autoplot(Close) +
  labs(title="Before re-indexing")

dgoog %>% autoplot(diff)
dgoog %>% ACF(diff) %>% autoplot




