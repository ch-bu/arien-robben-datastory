library(tidyverse)
library(dbplyr)
library(RSQLite)
library(rvest)
library(lubridate)

# https://datahub.io/collections/football
# https://www.jokecamp.com/blog/guide-to-football-and-soccer-data-and-apis/


# ******************* European Soccer Database ******************************
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "database.sqlite")

team <- dbGetQuery(con,"Select * from Team") %>% 
  as.tibble

team_attributes <- dbGetQuery(con,"Select * from Team_Attributes") %>% 
  as.tibble

league <- dbGetQuery(con,"Select * from League") %>% 
  as.tibble

bundesliga <- league %>% 
  filter(name == "Germany 1. Bundesliga") %>% 
  {.$id}

player <- dbGetQuery(con, "Select * From Player") %>% as.tibble
player_attributes <- dbGetQuery(con, "Select * From Player_Attributes") %>% 
  as.tibble

match <- dbGetQuery(con, "Select * From Match") %>% as.tibble



# *************** Transfermarkt *********************************************
# TODO: Alle Spiele der Vereine aus dem Netzen rausnehmen,
# zu der Zeit als Robben gespielt hat

# Vergleiche zu anderen Spielern anstellen.
# Z.B. Ronaldo, Messi, Linksaußenspieler

# Bilder 

injuries <- read_csv("data/injuries.csv")
goals <- read_csv("data/goals.csv") %>% 
  mutate(
    date = dmy(date),
    team = team %>% as.factor %>% 
      fct_relevel("FC Groningen", "PSV Eindhoven",
                  "FC Chelsea", "Real Madrid",
                  "FC Bayern München")
  )

leistungsdaten <- read_csv("data/leistungsdaten.csv")


# How many goals per season?
goals %>% 
  count(season) %>% 
  ggplot(aes(x = season, y = n)) + 
  geom_area(fill = "pink", aes(group = 1), 
            color = "black")

# How many goals per team?
goals %>% 
  count(team) %>% 
  ggplot(aes(team, n)) +
  geom_col()

# On which date did robben score? 
goals %>% 
  ggplot(aes(date, team)) + 
  geom_jitter()

# Who are the best provides for robben?
goals %>% 
  count(provider, sort = TRUE)





