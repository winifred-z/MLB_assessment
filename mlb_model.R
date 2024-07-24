library(readr)
library(fastDummies)
library(dplyr)
library(data.table)
library(broom)
library(stringr)
library(ggplot2)
library(caret)

#df<-readr::read_tsv("C:/Users/19256/Downloads/assessment_data.tsv")
#df_no_NA <- df[!is.na(df$tickets_listed),]
#View(df_no_NA)

#group by taxonomy, venue
df_no_NA %>%
  group_by(taxonomy)%>%
  summarise_at(vars(tickets_listed, mean_listing_price), list(Mean = mean, SD = sd))

df_no_NA %>%
  group_by(venue_name)%>%
  summarise_at(vars(tickets_listed, mean_listing_price), list(Mean = mean, SD = sd))

data <- df_no_NA %>% group_by(venue_name) %>% summarize(mean_price = mean(mean_listing_price))
ggplot(data, aes(x=venue_name, y=mean_price)) + geom_bar(stat = "identity")

#MLB-only model
ticket_model <- df_no_NA[df_no_NA$venue_name == "Dodger Stadium",]

#days between listing date and 8/01, probably rename this
ticket_model$days_before_listing = as.Date("2017-08-01")- ticket_model$listing_date
ticket_model$event_date <- as.Date(ticket_model$event_datetime)
ticket_model$days_before_event = as.Date("2017-08-01")- ticket_model$event_date
ticket_model$day_of_week <- weekdays(ticket_model$event_date)
View(ticket_model)

#Grouping Variables

#on a weekend?
weekend_days <- c("Friday", "Saturday", "Sunday") #Friday included
ticket_model$weekend <- ifelse(ticket_model$day_of_week %in% weekend_days, 1, 0)

#Won 50%+ games in 2016
over_50 <- c("San Francisco Giants", "Washington Nationals", "New York Mets", 
             "Kansas City Royals") 
ticket_model$above_50 <- ifelse(ticket_model$performer_1 %in% over_50|
                                  ticket_model$performer_2 %in% over_50, 1, 0)
#in National League, not American
nat_league <- c("San Francisco Giants", "Washington Nationals", "New York Mets", 
                "Arizona Diamondbacks", "Atlanta Braves", "Cincinnati Reds", 
                "Colorado Rockies", "Milwaukee Brewers",
                "San Diego Padres")
ticket_model$is_national <- ifelse(ticket_model$performer_1 %in% nat_league|
                                     ticket_model$performer_2 %in% nat_league, 1, 0)
#on the West Coast
west_teams <- c("Arizona Diamondbacks", "San Diego Padres", "San Francisco Giants",
                "Los Angeles Angels","Colorado Rockies")
ticket_model$west_coast <- ifelse(ticket_model$performer_1 %in% west_teams|
                                    ticket_model$performer_2 %in% west_teams, 1, 0)
#Central team
central_teams <- c("Chicago White Sox", "Cincinnati Reds", "Kansas City Royals",
                   "Milwaukee Brewers", "Minnesota Twins")
ticket_model$central <- ifelse(ticket_model$performer_1 %in% central_teams|
                                 ticket_model$performer_2 %in% central_teams, 1, 0)
#East coast
east_teams <- c("New York Mets", "Atlanta Braves", "New York Mets",
                   "Washington Nationals")
ticket_model$east_coast <- ifelse(ticket_model$performer_1 %in% east_teams|
                                 ticket_model$performer_2 %in% east_teams, 1, 0)


View(ticket_model)
ticket_model_1 <- ticket_model[, c("tickets_listed", "days_before_listing", "days_before_event", 
                                   "weekend", "above_50", "is_national", "west_coast", 
                                   "central", "east_coast")]

#bar chart:
bar_data <- ticket_model %>% group_by(weekend) %>% summarize(mean_tickets = mean(tickets_listed))
ggplot(bar_data, aes(x=weekend, y=mean_tickets)) + geom_bar(stat = "identity")


#multiple regression:

#modeling tickets listed
fit_all = lm(tickets_listed ~., data = ticket_model_1)
#summary(fit_all)
#formula(fit_all)
model_1 = lm(tickets_listed ~ 1, data = ticket_model_1)
#head(ticket_model_1)
#summary(model_1)
step_model = step(model_1, direction = "forward", scope = formula(fit_all))
summary(step_model)

#modeling mean listing price
ticket_model_2 <- ticket_model[, c("mean_listing_price", "days_before_listing", "days_before_event", "weekend", "above_50",
                                   "is_national", "west_coast", "central", "east_coast")]
fit_all_2 = lm(mean_listing_price ~., data = ticket_model_2)
model_2 = lm(mean_listing_price ~ 1, data = ticket_model_2)
step_model_2 = step(model_2, direction = "forward", scope = formula(fit_all_2))
summary(step_model_2)

#plotting:
ggplt1 <- ggplot(ticket_model,aes(x=days_before_event,y=tickets_listed))+ geom_point()+ theme_classic()
ggplt1+geom_smooth(method=lm,se=FALSE,fullrange=TRUE) 

ggplt2 <- ggplot(ticket_model_2,aes(x=days_before_event,y=mean_listing_price))+ geom_point()+ theme_classic()
ggplt2+geom_smooth(method=lm,se=FALSE,fullrange=TRUE)

#testing models:
set.seed(123)
random_sample <- createDataPartition(ticket_model_1$tickets_listed, p = 0.8, list = FALSE)
mlb_ticket_model = lm(formula = tickets_listed ~ days_before_listing + days_before_event + 
                        central + above_50 + is_national, data = ticket_model_1)

