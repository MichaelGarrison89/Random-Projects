

library("stringr")
library("dplyr")
library("tidyr")
library("ggplot2")




lines       <- read.csv("Lines.csv", header = TRUE, 
                        stringsAsFactors = FALSE) %>%
               mutate(team = str_sub(team, 2, -2)) %>%
               mutate(over.return = 
                        ifelse(over.odds < 0, 
                               100^2 / -over.odds, over.odds)) %>%
               mutate(under.return =
                        ifelse(under.odds < 0, 
                               100^2 / -under.odds, under.odds))


projections <- read.csv("Projections Dummy.csv", header = FALSE) %>%
               rename(team = V1, wins = V2, division = V3) %>%
               mutate(quality = wins / 16) %>%
               select(team, quality, division)

schedule    <- read.csv("Schedule.csv", header = TRUE) %>%
               mutate(game.code = row_number()) %>%
               left_join(select(projections, -quality), 
                         by = c("Home" = "team")) %>%
               rename(home.division = division) %>%
               left_join(select(projections, -quality), 
                         by = c("Away" = "team")) %>%
               rename(away.division = division) %>%
               mutate(divisional.game = 
                        ifelse(home.division == away.division, 1 ,0)) %>%
               mutate(home.conference = str_sub(home.division, end = 3)) %>%
               mutate(away.conference = str_sub(away.division, end = 3)) %>%
               mutate(conference.game = 
                      ifelse(home.conference == away.conference, 1, 0)) %>%
               group_by(Home, away.division) %>%
               mutate(n = n()) %>%
               mutate(strength.game = ifelse(n == 1, 1, 0)) %>%
               select(-n) %>%
               ungroup()



iterations <- 50000
set.seed(10)

sims <- data.frame(game.code = rep(seq(1, 256), iterations), 
                   trial = rep(1:iterations, each = 256))

sims <-
  sims %>% 
  left_join(schedule, by = c("game.code" = "game.code")) 


sims <-
  sims %>%
  left_join(projections, by = c("Home" = "team")) %>%
  rename(home.projection = quality)

sims <-
  sims %>%
  left_join(projections, by = c("Away" = "team")) %>%
  rename(away.projection = quality) %>%
  mutate(home.randomness = rnorm(nrow(sims), mean = 0, sd = 2 / 16)) %>%
  mutate(away.randomness = rnorm(nrow(sims), mean = 0, sd = 2 / 16)) %>%
  mutate(home.quality = home.projection + home.randomness) %>%
  mutate(away.quality = away.projection + away.randomness) %>%
  mutate(home.win.prob = (home.quality * (1 - away.quality)) / 
           (home.quality + away.quality - 2 * home.quality * away.quality) +
           home.field.advantage) %>%
  mutate(draw = runif(nrow(sims))) %>%
  mutate(winner = ifelse(home.win.prob >= draw, as.character(Home), 
                                                as.character(Away))) %>%
  mutate(home.win = ifelse(home.win.prob >= draw, 1, 0))


raw.output <-
  sims %>%
  mutate(home.opponent = as.character(Away)) %>%
  mutate(away.opponent = as.character(Home)) %>%
  gather("home.away", "team", Home, Away) %>%
  mutate(opponent = ifelse(home.away == "Home", 
                           home.opponent, away.opponent)) %>%
  mutate(win = ifelse(home.away == "Home", home.win, 1 - home.win)) %>%
  mutate(team.division = ifelse(home.away == "Home", 
                                as.character(home.division), 
                                as.character(away.division))) %>%
  mutate(team.conference = ifelse(home.away == "Home",
                                  as.character(home.conference),
                                  as.character(away.conference))) %>%
  select(game.code, trial, home.away, team, team.division, team.conference,
         win, opponent, divisional.game, conference.game, strength.game)




outcome.summary <-
  raw.output %>%
  group_by(trial, team) %>%
  summarize(wins = sum(win)) %>%
  group_by(team) %>%
  mutate(trials = n()) %>%
  group_by(wins, team) %>%
  summarize(results = mean(n() / trials)) %>%
  spread(key = wins, value = results)


betting.table <-
  
  outcome.summary %>%
  gather("wins", "probability", -team) %>%
  left_join(lines, by = c("team" = "team")) %>%
  select(team, over.under, wins, probability) %>%
  mutate(prob = ifelse(is.na(probability), 0, probability)) %>%
  mutate(wins = as.numeric(as.character(wins))) %>%
  mutate(prob.over = ifelse(wins > over.under, prob, 0)) %>%
  mutate(prob.under = ifelse(wins < over.under, prob, 0)) %>%
  group_by(team, over.under) %>%
  summarize(prob.over = sum(prob.over), prob.under = sum(prob.under)) %>%
  left_join(lines, by = c("team" = "team", "over.under" = "over.under")) %>%
  mutate(expected.return.over = 
           prob.over * over.return - prob.under * 100) %>%
  mutate(expected.return.under = 
           prob.under * under.return - prob.over * 100) %>%
  mutate(expected.return.over = 
           round(expected.return.over, 3)) %>%
  mutate(expected.return.under = 
           round(expected.return.under, 3))



ordered.table <-
betting.table %>%
  select(team, over.under, expected.return.over, 
         expected.return.under, prob.over, prob.under) %>%
  rename(over = expected.return.over) %>%
  rename(under = expected.return.under) %>%
  gather("bet", "expected.return", over, under) %>%
  mutate(prob.win = ifelse(bet == "over", prob.over, prob.under)) %>%
  select(team, over.under, bet, prob.win, expected.return) %>%
  arrange(desc(expected.return))


# This section is where we find the division winners for each iteration.
# Our division.results data.frame starts out as a list of all the teams that
# matched the maximum win total in their division. We will winnow down this
# data frame.

division.results <-
  raw.output %>%
  mutate(div.win = win * divisional.game) %>%
  mutate(conf.win = win * conference.game) %>%
  mutate(strength.win = win * strength.game) %>%
  group_by(trial, team.division, team) %>%
  summarize(wins = sum(win), div.wins = sum(div.win),
            conf.wins = sum(conf.win), str.wins = sum(strength.win)) %>%
  group_by(trial, team.division) %>%
  mutate(max = max(wins)) %>%
  filter(wins == max) %>%
  mutate(n = n())

# We'll count all the divisions that are only represented once in a given
# trial, which would mean there was no tie to win the division.

division.winners <-
  division.results %>%
  filter(n == 1)

division.results <-
  division.results %>%
  filter(n != 1) 

# Here we start accounting for tiebreakers. First, the head-to-heads.

# This is a data.frame of all the team/trial combinations that got two wins 
# against any other team. We semi-join it with the division results frame,
# because we only want to give credit to teams that won twice against a 
# fellow division leader. The we semi-join it again wtih the division results
# frame, because we only care if a team got beat twice by a fellow division
# leader.

head.to.head <- 
  raw.output %>%
  select(-game.code, -home.away) %>%
  filter(divisional.game == 1) %>%
  group_by(trial, team, opponent) %>%
  summarize(wins = sum(win)) %>%
  filter(wins == 2) %>%
  select(-wins) %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "opponent" = "team")) %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "team"     = "team"))



# This data frame shows all the teams that were not beaten twice by any other
# team. Those teams can't yet be eliminated.

not.beaten.twice <-
  division.results %>%
  anti_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  group_by(trial, team.division) %>%
  mutate(n = n())
  
beaten.twice <-
  division.results %>%
  semi_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  anti_join(not.beaten.twice, by = c("trial" = "trial",
                                     "team.division" = "team.division"))

first.break.win <- filter(not.beaten.twice, n == 1)

division.winners <-
  division.winners %>%
  bind_rows(first.break.win)

division.results <-
  not.beaten.twice %>%
  filter(n != 1) %>%
  bind_rows(beaten.twice)


first.break.win  <- NULL
beaten.twice     <- NULL
not.beaten.twice <- NULL

# Now we'll do the second tie-break, which is just division wins.
# We filter division.results so that only the teams that have the most wins in
# their division are left.

division.results <-
  division.results %>%
  group_by(trial, team.division) %>%
  mutate(max.div.wins = max(div.wins)) %>%
  filter(div.wins == max.div.wins) 

# Then our second.break frame pulls out any divisions that now only have one
# team.

second.break <-
  division.results %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-max.div.wins)

division.winners <-
  division.winners %>%
  bind_rows(second.break)

division.results <-
  division.results %>%
  anti_join(second.break, by = c("trial" = "trial",
                                "team.division" = "team.division")) %>%
  mutate(n = n())

second.break <- NULL


# In some instances, the second tie-break will eliminate the third team
# in a 3-way tie, so we need to repeat the first tiebreaker.


head.to.head <-
  head.to.head %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "opponent" = "team")) %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "team"     = "team"))


not.beaten.twice <-
  division.results %>%
  anti_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  group_by(trial, team.division) %>%
  mutate(n = n())

beaten.twice <-
  division.results %>%
  semi_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  anti_join(not.beaten.twice, by = c("trial" = "trial",
                                     "team.division" = "team.division"))

first.break.win <- 
  filter(not.beaten.twice, n == 1) %>%
  select(-max.div.wins)

division.winners <-
  division.winners %>%
  bind_rows(first.break.win)

division.results <-
  not.beaten.twice %>%
  filter(n != 1) %>%
  bind_rows(beaten.twice) %>%
  select(-max.div.wins)


first.break.win  <- NULL
beaten.twice     <- NULL
not.beaten.twice <- NULL


# The next tie-breaker is how teams performed against common opponents. 
# Within a division, all opponents will be common except for the strength of
# schedule match-ups. Therefore, if 2+ teams are tied, whichever did best
# against its strength match-ups must have done worst against common opponents
# and can be eliminated.

division.results <-
  division.results %>%
  group_by(trial, team.division) %>%
  mutate(min.sos.wins = min(str.wins)) %>%
  filter(str.wins == min.sos.wins)

str.break.win <-
  division.results %>%
  group_by(trial, team.division) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-min.sos.wins)

division.results <-
  division.results %>%
  anti_join(str.break.win, by = c("trial" = "trial",
                                  "team.division" = "team.division")) %>%
  select(-min.sos.wins)

division.winners <- bind_rows(division.winners, str.break.win)

str.break.win <- NULL

# And now we loop back through the first tiebreaker again in case we
# eliminated any 3-way ties.

head.to.head <-
  head.to.head %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "opponent" = "team")) %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "team"     = "team"))


not.beaten.twice <-
  division.results %>%
  anti_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  group_by(trial, team.division) %>%
  mutate(n = n())

beaten.twice <-
  division.results %>%
  semi_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  anti_join(not.beaten.twice, by = c("trial" = "trial",
                                     "team.division" = "team.division"))

first.break.win <- 
  filter(not.beaten.twice, n == 1)

division.winners <-
  division.winners %>%
  bind_rows(first.break.win)

division.results <-
  not.beaten.twice %>%
  filter(n != 1) %>%
  bind_rows(beaten.twice)


first.break.win  <- NULL
beaten.twice     <- NULL
not.beaten.twice <- NULL

# And now we'll do the conference games tie-break.

division.results <-
  division.results %>%
  group_by(trial, team.division) %>%
  mutate(max.conf.wins = max(conf.wins)) %>%
  filter(conf.wins == max.conf.wins)

conf.break.win <-
  division.results %>%
  group_by(trial, team.division) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-max.conf.wins)

division.results <-
  division.results %>%
  anti_join(conf.break.win, by = c("trial" = "trial",
                                  "team.division" = "team.division")) %>%
  select(-max.conf.wins)

division.winners <- bind_rows(division.winners, conf.break.win)

conf.break.win <- NULL

# One final trip through the head-to-head tiebreaks to find any remaining teams
# we can pick off.

head.to.head <-
  head.to.head %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "opponent" = "team")) %>%
  semi_join(division.results, by = c("trial"    = "trial",
                                     "team"     = "team"))


not.beaten.twice <-
  division.results %>%
  anti_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  group_by(trial, team.division) %>%
  mutate(n = n())

beaten.twice <-
  division.results %>%
  semi_join(head.to.head, by = c("trial" = "trial",
                                 "team"  = "opponent")) %>%
  anti_join(not.beaten.twice, by = c("trial" = "trial",
                                     "team.division" = "team.division"))

first.break.win <- 
  filter(not.beaten.twice, n == 1)

division.winners <-
  division.winners %>%
  bind_rows(first.break.win)

division.results <-
  not.beaten.twice %>%
  filter(n != 1) %>%
  bind_rows(beaten.twice)


first.break.win  <- NULL
beaten.twice     <- NULL
not.beaten.twice <- NULL

# I should do a third tie-break here, but I'll just use a random number.
# At this point, with >95% of the trials & divisions accounted for, I'll just
# let it be random. In the future, it'll be worth to try the next tie-breaker,
# which is record in common games.

rand.n <- nrow(division.results)

division.results <-
  division.results %>%
  ungroup() %>%
  mutate(random = runif(rand.n)) %>%
  group_by(trial, team.division) %>%
  mutate(max.rand = max(random)) %>%
  filter(random == max.rand)

division.winners <-
  division.winners %>%
  bind_rows(division.results)

division.results <- NULL

# check
nrow(division.winners) == 8 * iterations



betting.table.plus <-
  division.winners %>%
  group_by(team) %>%
  summarize(div.winner = n() / (iterations)) %>%
  right_join(betting.table, by = c("team" = "team")) %>%
  left_join(div.lines, by = c("team" = "team")) %>%
  mutate(div.return = odds * 100 * div.winner - 100 * (1 - div.winner))

ordered.table.plus <-
  betting.table.plus %>%
  select(team, div.winner, div.return) %>%
  arrange(desc(div.return)) %>%
  rename(expected.return = div.return) %>%
  rename(prob.win = div.winner) %>%
  mutate(bet = "division win")
  
output <- bind_rows(ordered.table, ordered.table.plus) %>%
  arrange(desc(expected.return))


outcome.summary %>%
  gather("wins", "prob", -team) %>%
  ggplot(aes(x = wins, y = prob)) +
  facet_wrap(~team, nrow = 8) +
  geom_bar(stat = "identity", width = 0.5, fill = "dodgerblue") +
  theme_minimal() 




result.team <- "Vikings"

o.u <-
lines %>% 
  filter(team == result.team) %>%
  select(over.under) %>%
  as.numeric(.)



outcome.summary %>%
  filter(team == result.team) %>%
  gather("wins", "prob", -team) %>%
  ggplot(aes(x = wins, y = prob)) +
  geom_bar(stat = "identity", width = 0.5, fill = "dodgerblue") +
  theme_minimal() +
  ggtitle(paste(result.team, "Win Totals")) +
  labs(x = "Wins", y = "% of Results") +
  geom_vline(x = o.u + 1, color = "red", size = 1.5, linetype = "dashed")


outcome.summary %>% filter(team == result.team) %>%
  gather("wins", "prob", -team) %>%
  tbl_df()

schedule %>% 
  filter(Home == result.team | Away == result.team) %>% 
  select(Home, Away)
