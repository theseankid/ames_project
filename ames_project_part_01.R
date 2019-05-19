# Set-up and Importing Data
library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(GGally)
library(AmesHousing)

ames <- make_ames()
dim(ames)
sapply(ames,class)

# exploring data
# lets import packages for visualization and data exploration


summary(ames$Sale_Price)
g1 <- ggplot(ames, aes(Sale_Price)) + geom_density(kernel = "gaussian") +
  geom_vline(aes(xintercept =  mean(ames$Sale_Price)), col = "red") +
  geom_vline(aes(xintercept =  median(ames$Sale_Price)), col = "blue") +
  ggtitle("Mean and Kernel density plot of Sale Price") +
  scale_x_continuous(labels = dollar)
g2 <- ggplot(ames, aes(sample = Sale_Price)) +
  stat_qq() + stat_qq_line(color = "red") +
  scale_y_continuous(labels = dollar) +
  ggtitle("QQ-plot for Sale Price")
grid.arrange(g1, g2, nrow=2)

# The red line represents sitting on a normal distribution, which this is clearly far away from. A standard trick is to do the log transformation of the data
# I have a feeling the irregularity has something to do with location or lot features so I will color the next qqplot by zoning
ames <- ames %>% mutate(log10_SP = log10(Sale_Price))
g1 <- ggplot(ames, aes(log(Sale_Price))) + geom_density(kernel = "gaussian") +
  geom_vline(aes(xintercept = mean(log(ames$Sale_Price))), col = "red") +
  geom_vline(aes(xintercept = median(log(ames$Sale_Price))), col = "blue") + 
  ggtitle("Mean and Kernel density plot of log10 of Sale Price")

make_qq <- function(dd, x) {
  dd<-dd[order(dd[[x]]), ]
  dd$qq <- qnorm(ppoints(nrow(dd)))
  dd
}
g2 <- ggplot(make_qq(ames, "log10_SP")) + 
  geom_point(aes(x=qq, y=log10_SP, color=MS_Zoning)) + 
  labs(x="Theoretical",y="Observed") +
  stat_qq_line(aes(sample = ames$log10_SP), color = "red") +
  ggtitle("QQ-plot for Log10 Sale_Price colored by Zoning Classification")
grid.arrange(g1, g2, nrow=2)

# I knew I was onto something, we are getting somewhere! Taking the log helps unskew the data, and we see the irregularities don't come from the majority of the dataset
# lets explore some of the other locational features and their distributions



# plot location versus Sale Price
ggplot() + geom_point(data=ames, aes(x=Longitude,y=Latitude), size = 1, alpha = .5) +
  stat_summary_2d(data=ames, aes(x = Longitude, y = Latitude, z = Sale_Price), alpha=.5) +
  scale_fill_gradient(name = "Sale_Price", low = "green", high = "red", labels = dollar) +
  ggtitle("Plot of Location vs Avg. Sale Price")

# it's clear that the average sale price varies with location

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_density2d(alpha = .75, color = "red") + 
    geom_point(alpha = .25, size = 1) + 
    geom_smooth(method=lm, color="blue", alpha = .5, ...)
  p
}
pairs_col1 <- c("Longitude", "Latitude", "log10_SP")
ggpairs(ames, columns = pairs_col1,lower = list(continuous = my_fn))

# the above plots show a few things
# longitude and latitude are not strongly correlated
# linear relationships don't fit all that well
# there is spatial irregularity in sale price
# some more exploratory analysis of sale price and locational features


# Zoning
table(ames$MS_Zoning)
# since we are focusing on Housing Prices, lets drop the Agricultural, Commercial, and Industrial Sales
# we will filter out the non-residential observations, check that they were moved properly by checking the dataset size, and then remove the extra factor levels
idx_Zoning <- grepl("Res", ames$MS_Zoning)
sum(idx_Zoning)
ames <- ames[idx_Zoning,]
ames$MS_Zoning <- droplevels(ames$MS_Zoning)
table(ames$MS_Zoning)

ames %>% group_by(MS_Zoning) %>% summarize(
  count = n(), mean = mean(Sale_Price),median = median(Sale_Price), min = min(Sale_Price), max = max(Sale_Price))
ggplot(ames, aes(x = Longitude, y = Latitude, col=MS_Zoning)) + geom_point()
ggplot(ames, aes(x = MS_Zoning, y = Sale_Price)) + 
  geom_boxplot(width = 0.3) + 
  scale_y_log10(labels=dollar)+
  coord_trans(y = "log10") + 
  xlab("Zoning") + 
  ylab("Sale Price") +
  coord_flip()

# one approach is to combine medium and high density

#Neighborhood
ggplot(ames, aes(x = Longitude, y = Latitude, col = Neighborhood)) + geom_point() +
  guides(col = guide_legend(ncol = 1))#, byrow = TRUE))
table(ames$Neighborhood)
ggplot(ames, aes(x = Neighborhood, y = Sale_Price)) + 
  geom_boxplot(width = .3) + 
  scale_y_log10(labels=dollar)+
  #coord_trans(y = "log10") + 
  xlab("Neighborhood") + 
  ylab("Sale Price") + coord_flip()

# Condition_1
ggplot(ames, aes(x = Longitude, y = Latitude, col=Condition_1)) + geom_point()
table(ames$Condition_1)
ggplot(ames, aes(x = Condition_1, y = Sale_Price)) + 
  geom_boxplot(width = .3) + 
  scale_y_log10(labels=dollar)+
  coord_trans(y = "log10") + 
  xlab("Condition_1") + 
  ylab("Sale Price") +
  coord_flip()

# the means vary quite a bit with condition, however like with neighborhood, the classes are very unbalanced
# but this confirms our suspicion, price varies with location

# Now we will test if locational features explain significant variance in sale price, if so, they will be valuable predictors

lm1 <- lm(log10(Sale_Price) ~ MS_Zoning, data = ames)
lm2 <- lm(log10(Sale_Price) ~ Neighborhood, data = ames)
lm3 <- lm(log10(Sale_Price) ~ Condition_1, data = ames)
anova(lm1)
anova(lm2)
anova(lm3)
lm4 <- lm(log10(Sale_Price)~ 1, data = ames)
lm5 <- lm(log10(Sale_Price)~ Longitude , data = ames)
lm6 <- lm(log10(Sale_Price)~ Latitude, data = ames)
lm7 <- lm(log10(Sale_Price)~ Longitude + Latitude, data = ames)
anova(lm4,lm5,lm6,lm7)

# based on the anova tests location matters, so we will use it. good thing our intuition was right.

# now for more data exploration

# not every house has a porch or deck but it is a desirable feature
sum(ames$Wood_Deck_SF == 0) 
sum(ames$Open_Porch_SF == 0)
sum(ames$Enclosed_Porch == 0)
sum(ames$Three_season_porch == 0)
sum(ames$Screen_Porch == 0)

# I will combine porch and deck area into one variable because they are somewhat sparse
ames <- ames %>% mutate(Porch_Deck_SF = Wood_Deck_SF + Open_Porch_SF + Enclosed_Porch + Three_season_porch + Screen_Porch)
sum(ames$Porch_Deck_SF ==0)

# now w will look at the continuous features and see if we need to engineer any features
pairs_col2 <- c("log10_SP","Total_Bsmt_SF", "Gr_Liv_Area", "Garage_Area", "Porch_Deck_SF","Lot_Area", "Lot_Frontage")
ggpairs(ames, columns = pairs_col2,lower = list(continuous = my_fn))

ggplot(ames, aes(x=Year_Built, y = Sale_Price)) + geom_point(aes(alpha = .25), show.legend = FALSE) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2)) + scale_y_log10(labels=dollar)

# there doesn't seem to be much nonlinearity, so we will leave these features alone
# year built seems to be fairly well behaved
# also note, these features are pretty right skewed, however... 
# since they are the independent variables/predictors we don't have to worry so much about their distributions

# now for the discrete features

table(ames$Roof_Matl)

#ames %>% group_by(Roof_Matl) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
#ames %>% group_by(Roof_Style) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Foundation) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Lot_Shape) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
#ames %>% group_by(Lot_Config) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
#ames %>% group_by(Land_Slope) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Bldg_Type) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
#ames %>% group_by(Overall_Cond) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))

ames %>% group_by(Fireplaces) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Heating) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Central_Air) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))

#ames %>% group_by(Garage_Cond) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Garage_Cars) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Garage_Type) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
#ames %>% group_by(Fence) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))

ames %>% group_by(Full_Bath) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Half_Bath) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Bedroom_AbvGr) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(Kitchen_AbvGr) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))
ames %>% group_by(TotRms_AbvGrd) %>% summarize(count = n(), mean = mean(Sale_Price), median = median(Sale_Price))

ggplot(ames, aes(x = Full_Bath, y = Sale_Price)) + 
  geom_boxplot(width = .3) + 
  scale_y_log10(labels=dollar)+
  ylab("Sale Price") + coord_flip()

# I left out variables talking about quality and condition for two main reasons:
# 1) they have lots of levels, so they are computationally expensive to compute
# 2) other characteristics of the home tell us about the quality (like size, number of rooms, etc)

# drop bedrooms and kitchens, tot rooms takes care of both 


Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
  Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
  Central_Air + Fireplaces + Longitude + Latitude