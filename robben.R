library(tidyverse)
library(dbplyr)
library(RSQLite)
library(rvest)
library(ggrepel)
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


(all_games <- read_csv("data/all_games_teams.csv") %>% 
  select(-X4) %>% 
  mutate(
    date = date %>% str_remove("^.{2}\\. ") %>% dmy
 ))

injuries <- read_csv("data/injuries.csv") %>% 
  pivot_longer(cols = c(from, to), 
               names_to = "from_to", values_to = "date")


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
            color = "black") +
  labs(
    title = "Number of goals per season"
  )

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


# In which games did Robben score? 
goals_in_all_games <- all_games %>% 
  select(date, team, season) %>% 
  left_join(
    goals %>% count(date, team, 
                    sort = TRUE, name = "goal"), 
    by = c("date", "team")
  ) %>% 
  arrange(date) %>% 
  mutate(
    goal = case_when(
      is.na(goal) ~ 0,
      is.integer(goal) ~ as.double(goal)
    ) %>% as.factor,
    play_week = date %>% week,
    day = yday(date),
    cum_goal = goal %>% as.character 
       %>% as.integer %>% cumsum
  )

# How many goals per week and season?
goals_in_all_games %>% 
  ggplot(aes(play_week, y = season)) +
  geom_bin2d(aes(fill = goal), color = "black")

# How many goals per day? 
goals_in_all_games %>% 
  ggplot(aes(day, y = season)) +
  geom_bin2d(aes(fill = goal), color = "black")

# Cumulative goals
# TODO: Scorer Punkte visualisieren
# TODO: Events dazu machen
# TODO: d3 Links zu Artikeln einfügen

events <- tibble(
  date = c(
    as.Date("2013-05-25"),
    as.Date("2012-05-19"),
    as.Date("2019-03-05")
    ),
  cum_goal = c(
    131,
    118,
    206
    ),
  event = c(
    "Robben gewinnt die\nChampions League",
    "Drama Dahoim",
    "Eine Wadenverletzung zwingt ihn\nzu 51 Tagen Pause"
    )
)

goals_in_all_games %>% 
  ggplot(aes(date, cum_goal)) +
  geom_area(alpha = .5, aes(fill = team)) +
  geom_line(color = "black") +
  geom_text_repel(data = events, aes(label = event),
                  min.segment.length = 0.8,
                  nudge_x = -250,
                  nudge_y = 40) +
  labs(
    title = "Cumulative goals"
  )
























