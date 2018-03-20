
table(temporaryAccomodationDataDF$Tenancy_Stop_Reason)


ta_df <- temporaryAccomodationDataDF[c(1, 2, 8, 9,11)] %>% unique()


ggplot(temporaryAccomodationDataDF, aes(x = MAP_EAST, y = MAP_NORTH)) + geom_point() +
 geom_hex() + geom_density_2d()


dmy(temporaryAccomodationDataDF$Date_of_Birth)

hist(ta_df$Time_in_TA / 365)
boxplot(ta_df$Time_in_TA / 365)
