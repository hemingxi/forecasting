rm()
############################## Q01 ##############################
#Plot the series with the highest mean, and the series with the lowest standard deviation.

PBS
?PBS

PBS_features <- PBS %>%
  features(Cost, list(mean = mean, sd = sd))

PBS_features %>% 
  arrange(desc(mean))

PBS_features %>% 
  arrange(sd)
  
PBS %>% 
  filter(Concession=="General" & Type=="Co-payments" & ATC1=="M" & ATC2=="M02") %>%
  autoplot(Cost)


PBS %>% 
  filter(Concession=="Concessional" & Type=="Co-payments" & ATC1=="C" & ATC2=="C10") %>%
  autoplot(Cost)


############################## Q02 ##############################
#Use GGally::ggpairs() to look at the relationships between the STL-based features 
#for the holiday series in the tourism data. Change seasonal_peak_year and seasonal_trough_year to factors, 
#as shown in Figure 4.3. Which is the peak quarter for holidays in each state?

tourism

holiday <- tourism %>%
  filter(Purpose =="Holiday")

holiday_features <- holiday %>% 
  features(Trips, feat_stl)

holiday_features

library(glue)


holiday_features %>% 
  select_at(vars(contains("season"), State)) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = State), cardinality_threshold=NULL)



############################## Q03 ##############################

PBS 
?PBS

PBS_features <- PBS %>%
  features(Cost, feature_set(pkgs = "feasts"))

PBS_features %>% 
  distinct(ATC2)

PBS_features


# is.finite( PBS_features )

# has_NA <- function(x) {
#   rl <- rle(is.na(x))$values
#   any(rl)
# }
# 
# PBS_features %>%
#   select_if(~ !has_NA(.x))

PBS_features_drop_na <- PBS_features %>%
  drop_na()

PBS_features
PBS_features_drop_na


library(broom)
pcs_PBS_features <- PBS_features_drop_na %>%
  select(-Concession, -Type, -ATC1, -ATC2) %>%
  prcomp(scale = TRUE) %>%
  augment(PBS_features_drop_na)

pcs_PBS_features

pcs_PBS_features %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Concession )) +
  geom_point() +
  theme(aspect.ratio = 1)

pcs_PBS_features %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Type  )) +
  geom_point() +
  theme(aspect.ratio = 1)

pcs_PBS_features %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = ATC1  )) +
  geom_point() +
  theme(aspect.ratio = 1)

# The first principal component (.fittedPC1) is the linear combination of the features
# which explains the most variation in the data. 
# The second principal component (.fittedPC2) is the linear combination which 
# explains the next most variation in the data, 
# while being uncorrelated with the first principal component.


outliers <- pcs_PBS_features %>%
  filter(.fittedPC2 < -10) %>%
  select(Concession, Type, ATC1, ATC2, .fittedPC1, .fittedPC2)
outliers

outliers %>% 
  left 

PBS

outliers %>%
  left_join(PBS, by = c("Concession", "Type", "ATC1", "ATC2")) %>%
  mutate(
    Series = glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}",
                  .sep = "\n\n")
  ) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PBS")

#first of all there are series with no SD, which we have removed
#these series have most of its points at 0 and a huge spike
