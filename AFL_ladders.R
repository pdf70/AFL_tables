# Filename: "AFL_ladders.R"

# Reads in data from wikipedia of history of all VFL / AFL ladders at season-end
# Note that the format of the input data may change as people change wikipedia entries

# Team colours sourced from https://sportsfancovers.com/australian-football-league-color-codes/
# or from https://imagecolorpicker.com/en.

# Wikipedia data should be checked against afltables final ladder positions

# Retrieve previous work from:
setwd(output_path) 
load(file = "afl_ladders_raw.Rdata")     # list - "tables"
load(file="afl_ladders.Rdata")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Set directory paths
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(lubridate)
library(tidyverse)
library(scales)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From 1897 to 2023
seasons = c(seq(1897, 2023, by = 1))

no_teams_finals = c(4, rep(8,3), rep(4,71), rep(5,19), rep(6,3), rep(8,length(seasons) - 97))

# Note: need to update this line each year to value of table number in wikipedia for latest season
wiki_table_no = c(rep(2,4), 4, 20, rep(21,3), rep(18,2), rep(20,37), 22, rep(21, 4), rep(20, 18),
                  rep(22,2), rep(24,29), rep(23,5), rep(1,4), 10, rep(3,15))
wiki_table_no[which(seasons %in% c(2000, 2022))] = 2
wiki_table_no[which(seasons %in% c(1916, 1992))] = 14
wiki_table_no[which(seasons == 1918)] = 16
wiki_table_no[which(seasons == 1917)] = 17
wiki_table_no[which(seasons %in% c(1942, 1943, 2002))] = 18
wiki_table_no[which(seasons %in% c(1925, 1982))] = 19
wiki_table_no[which(seasons == 1991)] = 20
wiki_table_no[which(seasons %in% c(1952, 1978, 1993))] = 21
wiki_table_no[which(seasons == 1987)] = 22
wiki_table_no[which(seasons %in% c(1973, 1979, 1981, 1983, 1985, 1997))] = 23
wiki_table_no[which(seasons == 1990)] = 25

wiki_name = c(rep("_VFL_season", 93), rep("_AFL_season", length(seasons) - 93))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph_afl = function(team_abbrev) {
  data_for_graph = afl_ladders %>% 
    filter(abbrev == team_abbrev)
  
  max_teams_in_season = max(data_for_graph$count_teams)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_end)
  max_yr = max(data_for_graph$yr_end)
  graph_range = max_yr - min_yr
  
  league_name = case_when(                           
    min_yr >= 1990 ~ "AFL",
    max_yr < 1990 ~ "VFL",
    TRUE ~ "VFL/AFL")
  
  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 1907.5, seq(1914.5, 1918.5, by = 1), 1924.5, 1941.5, 1943.5, 1986.5, 1990.5, 1994.5, 2010.5, 2011.5), 
                     xend = c(1907.5, seq(1914.5, 1918.5, by = 1), 1924.5, 1941.5, 1943.5, 1986.5, 1990.5, 1994.5, 2010.5, 2011.5, 2099.5),
                     ystart = c(rep(18,15)), yend = c(8, 10, 9, 4, 6, 8, 9, 12, 11, 12, 14, 15, 16, 17, 18))
  x_intercepts = data_for_graph$yr_end[(data_for_graph$yr_end %% ifelse(graph_range > 60, 10, 5)) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_end, y = Pos, group=afl_stint)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(premiers), size = as.factor(premiers))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$premiers_colour[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE) +
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", data_for_graph$chart_name[1], "in", league_name, "from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams (start from 1901, as every team made finals for 3 years prior)
    {if(min_yr<1972)geom_segment(aes(x = max(1900.5,min_yr), xend = min(max_yr,1971.5), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1972)&(max_yr>1972))geom_segment(aes(x = 1971.5, xend = 1971.5, y = 4.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1972)geom_segment(aes(x = 1971.5, xend = 1990.5, y = 5.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1991)&(max_yr>1991))geom_segment(aes(x = 1990.5, xend = 1990.5, y = 5.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1991)geom_segment(aes(x = 1990.5, xend = 1993.5, y = 6.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1994)&(max_yr>1994))geom_segment(aes(x = 1993.5, xend = 1993.5, y = 6.5, yend = 8.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1994)geom_segment(aes(x = max(1993.5,min_yr), xend = max(yr_end), y = 8.5, yend = 8.5), linetype="dotted", colour = "black", linewidth = 1)}
  
  graph_1
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in external data
# read all league ladders in one loop
# to read a league ladder manually, see code at bottom, read_html("https://en.wikipedia.org/wiki/1979_VFL_season")
tables = list()
for (j in 1:104) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]]  %>% # added to my list
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

for (j in 105:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]]  %>% # added to my list
    select(Pos:Pts) %>%
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

# Start from here if have pre-loaded data
# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in 1:length(seasons)) {
  header_fmt1 = colnames(tables[[j]])
  headers_all = rbind(header_fmt1, headers_all)
}
for (j in 46) {
  header_fmt2 = colnames(tables[[j]])
  headers_all = rbind(header_fmt2, headers_all)
}
for (j in 105) {
  header_fmt3 = colnames(tables[[j]])
  headers_all = rbind(header_fmt3, headers_all)
}

header_fmt1 = colnames(tables[[1]]) %>%
  str_replace("\\#","Pos")
header_fmt2 = colnames(tables[[46]]) %>%
  str_replace("\\#","Pos")
header_fmt3 = colnames(tables[[105]])

for (j in 1:length(seasons)) {  
  colnames(tables[[j]]) = header_fmt1
}
for (j in 46) {
colnames(tables[[j]]) = header_fmt2              # exception - 1942 season
}
for (j in 105:length(seasons)) {
  colnames(tables[[j]]) = header_fmt3              # extra column added from 2001 season
}

# convert from list to data frame
tables_all_fmt1 = do.call(rbind, lapply(tables[c(1:45, 47:104)], as.data.frame))
tables_all_fmt2 = do.call(rbind, lapply(tables[46], as.data.frame))
tables_all_fmt3 = do.call(rbind, lapply(tables[c(105:length(seasons))], as.data.frame))

tables_all_fmt1_adj = tables_all_fmt1 %>%
  mutate(B = 0) %>%
  select(Pos:D, B, PF:season) %>%
  rename("Pld" = "P", "perc" = "%")

tables_all_fmt2_adj = tables_all_fmt2 %>%
  rename("Pld" = "P", "perc" = "%")

tables_all_fmt3_adj = tables_all_fmt3 %>%
  mutate(B = 0) %>%
  select(Pos:D, B, PF:season) %>%
  rename("perc" = "PP")

tables_all = rbind(tables_all_fmt1_adj, tables_all_fmt2_adj, tables_all_fmt3_adj) %>%
    arrange(season_no, Pos)

# read in premiers & runners-up
table_premiers_clean = read_html("https://en.wikipedia.org/wiki/List_of_VFL/AFL_premiers") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_other = table_premiers_clean[[2]] %>%
  select(c(1:2, 4)) %>%
  filter(Season %in% seasons) %>%
  rename("Runners_up" = "Runners-up") %>%
  mutate(Season = as.numeric(Season))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
afl_teams = read_csv("afl_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
afl_ladders = tables_all %>% 
  mutate(Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         premiers = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(P)", 1, 0),
         minor_premiers = ifelse(Pos == 1, 1, 0),
         Team = str_replace(Team, " \\(P\\)", ""),            # to get consistency in team name
         Pts = as.numeric(str_replace(Pts, "\\[.*\\]", "")),
         B = ifelse(season == 1943, 1, B),
         pts_per_win = 4,
         pts_per_draw = 2,
         pts_deducted = Pts - (pts_per_win * (W + B) + pts_per_draw * D),
         pts_awarded = ifelse(season == 2015 & pts_deducted == 2, 2, 0),
         pts_deducted = pts_deducted - pts_awarded,
         max_avail_pts = (Pld + B) * pts_per_win,
         pts_achieved_perc = Pts / max_avail_pts,
         points_diff = PF - PA,
         raw_perc = PF / PA * 100,              # Note R rounds 0.5 down to 0
         perc_check = raw_perc - perc,
         points_per_game = round(PF / Pld, 1),
         yr_end = as.numeric(season)) %>%
  group_by(season) %>%
  mutate(count_teams = n(),
         wooden_spoon = ifelse(Pos == max(Pos), 1, 0),
         wooden_spoon = ifelse(premiers == 1, 0, wooden_spoon),
         wooden_spoon = ifelse(yr_end == 1916 & Pos == max(Pos) - 1, 1, wooden_spoon)) %>%
  ungroup() %>%
  select(Pos:premiers, minor_premiers, count_teams, wooden_spoon, pts_per_win:yr_end)

# Create a table of team names, including history & past team name changes
teams = as_tibble(unique(afl_ladders$Team))
colnames(teams) = c("previous_name")
teams = teams %>% 
  mutate(current_name = previous_name) %>%
  mutate(current_name = case_when(                            # to get consistency of team names
    previous_name == "South Melbourne" ~ "Sydney",
    previous_name == "Swans" ~ "Sydney",
    previous_name == "Footscray" ~ "Western Bulldogs",
    previous_name == "Kangaroos" ~ "North Melbourne",
    previous_name == "Brisbane" ~ "Brisbane Lions",
    TRUE ~ current_name))

teams_all = left_join(teams, afl_teams, by = c("current_name" = "current_name"))

# Repeat data cleansing for team name in premiers list
table_other_adj = table_other %>% 
  mutate(Premiers = str_replace(Premiers, "\\(.*?\\)", ""),
         Premiers = trimws(Premiers),
         Premiers = str_replace_all(Premiers, "[:digit:]", ""),
         premiers_clean = Premiers,
         Runners_up = str_replace(Runners_up, "\\(.*?\\)", ""),
         Runners_up = trimws(Runners_up)) %>%
  distinct() %>%
  mutate(Runners_up_clean = case_when(
    Runners_up == "South Melbourne" ~ "Sydney",
    Runners_up == "Footscray" ~ "Western Bulldogs",
    TRUE ~ Runners_up),
    no_teams_finals)

afl_ladders_all = left_join(afl_ladders, teams_all, by = c("Team" = "previous_name"))

# Add additional information of previous season's finishing position
afl_ladders = afl_ladders_all %>%
  left_join(table_other_adj, by = c("season" = "Season")) %>%
  arrange(current_name, season_no) %>%
  mutate(finals = ifelse(Pos <= no_teams_finals, 1, 0),
         runners_up = ifelse(current_name == Runners_up_clean, 1, 0),
         gf_years = ifelse(season %in% c(1897, 1924), 0, 1),
         grand_finalist = (premiers + runners_up) * gf_years,
         prev_pos = ifelse(current_name == lag(current_name), lag(Pos), NA)) %>%
  mutate(next_pos = ifelse(current_name == lead(current_name), lead(Pos), NA)) %>%
  arrange(season_no, Pos) %>%
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(Pos - prev_pos)),
         pos_abs_diff = abs(pos_diff)) %>%
  group_by(current_name) %>%
  mutate(cum_premiers = cumsum(premiers),
         streak_premiers = c(ave(c(0, premiers), cumsum(c(0, premiers) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_premiers = c(ave(c(0, premiers), cumsum(c(0, premiers) > 0), FUN = seq_along) - 1)[-1],
         cum_runners_up = cumsum(runners_up),
         streak_runners_up = c(ave(c(0, runners_up), cumsum(c(0, runners_up) == 0), FUN = seq_along) - 1)[-1],
         cum_minor_premiers = cumsum(minor_premiers),
         streak_minor_premiers = c(ave(c(0, minor_premiers), cumsum(c(0, minor_premiers) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_minor_premiers = c(ave(c(0, minor_premiers), cumsum(c(0, minor_premiers) > 0), FUN = seq_along) - 1)[-1],
         cum_finals = cumsum(finals),
         streak_finals = c(ave(c(0, finals), cumsum(c(0, finals) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_finals = c(ave(c(0, finals), cumsum(c(0, finals) > 0), FUN = seq_along) - 1)[-1],
         cum_grand_finals = cumsum(grand_finalist),
         streak_grand_finals = c(ave(c(0, grand_finalist), cumsum(c(0, grand_finalist) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_grand_finals = c(ave(c(0, grand_finalist), cumsum(c(0, grand_finalist) > 0), FUN = seq_along) - 1)[-1]) %>%
  ungroup() %>%
  mutate(row_number = row_number(),
         exited_yr_1 = case_when(
           abbrev == "GEE" ~ 1915,
           abbrev == "SYD" ~ 1915,
           abbrev == "ESS" ~ 1915,
           abbrev == "STK" ~ 1915,
           abbrev == "MEL" ~ 1915,
           TRUE ~ 2099),
         exited_yr_2 = case_when(
           abbrev == "GEE" ~ 1941,
           TRUE ~ 2099),
         afl_stint = case_when(
           yr_end > exited_yr_2 ~ 3,
           yr_end > exited_yr_1 ~ 2,
           TRUE ~ 1))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of VFL/AFL tables data
# Make all-time ladder
afl_all_time_ladder = group_by(afl_ladders, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_L = sum(L),
            Total_D = sum(D),
            Total_B = sum(B),
            Total_adj = sum(pts_awarded),
            Total_Ded = sum(pts_deducted),
            Total_PF = sum(PF),
            Total_PA = sum(PA),
            Total_perc = round(sum(PF) / sum(PA) * 100, 1),
            Total_Pts = sum(Pts),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            count_premiers = sum(premiers),
            count_runners_up = sum(runners_up),
            count_minor_premiers = sum(minor_premiers),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            count_4th = sum(Pos == 4),
            best = min(Pos),
            count_spoon = sum(wooden_spoon),
            count_gf = sum(grand_finalist),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_perc), desc(Total_PF))

# premiers by final position
premiers = filter(afl_ladders, premiers == 1)
premiers_by_Pos = group_by(premiers, Pos) %>%
  summarise(count = n())

# totals by season
season_totals = group_by(afl_ladders, season) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_L = sum(L),
            Total_D = sum(D),
            Total_PF = sum(PF),
            Total_PA = sum(PA),
            Total_perc = round(sum(PF) / sum(PA) * 100, 1),
            Total_Pts = sum(Pts),
            max_ave_points_scored_team = max(points_per_game),
            min_ave_points_scored_team = min(points_per_game)) %>%
  mutate(ave_points_scored_team = round(Total_PF / Total_Pld, 1))

title_race_totals = group_by(afl_ladders, season, yr_end) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_perc_1 = sum(perc[Pos == 1]),
            Total_perc_2 = sum(perc[Pos == 2]),
            Total_PF_1 = sum(perc[Pos == 1]),
            Total_PF_2 = sum(perc[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_perc = Total_perc_1 - Total_perc_2,
         margin_PF = Total_PF_1 - Total_PF_2)

# totals by club
club_records = group_by(afl_ladders, current_name) %>%
  summarise(highest_PF = max(PF),
            lowest_PF = min(PF),
            highest_PA = max(PA),
            lowest_PA = min(PA),
            highest_perc = max(perc),
            lowest_perc = min(perc),
            highest_Pts = max(Pts),
            lowest_Pts = min(Pts))

team_streaks = group_by(afl_ladders, current_name) %>%
  summarise(count = n(),
            max_streak_premiers = max(streak_premiers),
            max_streak_missed_premiers = max(streak_missed_premiers),
            max_streak_runners_up = max(streak_runners_up),
            streak_minor_premiers = max(streak_minor_premiers),
            max_streak_missed_minor_premiers = max(streak_missed_minor_premiers),
            max_streak_finals = max(streak_finals),
            max_streak_missed_finals = max(streak_missed_finals),
            max_streak_grand_finals = max(streak_grand_finals),
            max_streak_missed_grand_finals = max(streak_missed_grand_finals)) %>%
  arrange(current_name)

# Records for each team in a season
highest_PF_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "highest_PF" = "PF")) %>%
  select(current_name, highest_PF, Pld, season)

lowest_PF_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "lowest_PF" = "PF")) %>%
  select(current_name, lowest_PF, Pld, season)

highest_PA_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "highest_PA" = "PA")) %>%
  select(current_name, highest_PA, Pld, season)

lowest_PA_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "lowest_PA" = "PA")) %>%
  select(current_name, lowest_PA, Pld, season)

highest_perc_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "highest_perc" = "perc")) %>%
  select(current_name, highest_perc, Pld, season)

lowest_perc_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "lowest_perc" = "perc")) %>%
  select(current_name, lowest_perc, Pld, season)

highest_Pts_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "highest_Pts" = "Pts")) %>%
  select(current_name, highest_Pts, Pld, season)

lowest_Pts_team = club_records %>%
  left_join(afl_ladders, by = c("current_name" = "current_name",
                               "lowest_Pts" = "Pts")) %>%
  select(current_name, lowest_Pts, Pld, season)

# Records for a single season - not adjusted for no. of games
# most & least points
most_pts_season = arrange(afl_ladders, desc(Pts), desc(perc)) %>%
  select(season, Team, Pld, Pts, perc)
head(most_pts_season, 5)

least_pts_season = arrange(afl_ladders, Pts, perc) %>%
  select(season, Team, Pld, Pts, perc)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(afl_ladders, desc(W)) %>%
  select(season, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(afl_ladders, W) %>%
  select(season, Team, Pld, W) %>%
  filter(W == 0)
least_wins_season

# most & least losses
most_losses_season = arrange(afl_ladders, desc(L)) %>%
  select(season, Team, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(afl_ladders, L) %>%
  select(season, Team, Pld, L)
head(least_losses_season, 5)

# most draws
most_draws_season = arrange(afl_ladders, desc(D)) %>%
  select(season, Team, Pld, D) 
head(most_draws_season, 5)

# most & least points scored
most_points_season = arrange(afl_ladders, desc(PF)) %>%
  select(season, Team, Pld, PF)
head(most_points_season, 5)

least_points_season = arrange(afl_ladders, PF) %>%
  select(season, Team, Pld, PF) 
head(least_points_season, 5)

# most & least points conceded
most_points_against_season = arrange(afl_ladders, desc(PA)) %>%
  select(season, Team, Pld, PA)
head(most_points_against_season, 5)

least_points_against_season = arrange(afl_ladders, PA) %>%
  select(season, Team, Pld, PA)
head(least_points_against_season, 5)

# best & worst percentage
best_perc_season = arrange(afl_ladders, desc(perc)) %>%
  select(season, Team, Pld, perc)
head(best_perc_season, 5)

worst_perc_season = arrange(afl_ladders, perc) %>%
  select(season, Team, Pld, perc)
head(worst_perc_season, 5)

# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(afl_ladders, desc(pts_achieved_perc)) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(afl_ladders, pts_achieved_perc) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)

# most points to not win the league
most_pts_not_premiers_season = arrange(afl_ladders, desc(Pts)) %>%
  filter(premiers == 0) %>%
  select(season, Team, Pld, Pts) 
head(most_pts_not_premiers_season, 5)

# least points to win the league
least_pts_premiers_season = arrange(afl_ladders, Pts) %>%
  filter(premiers == 1) %>%
  select(season, Team, Pld, Pts)
head(least_pts_premiers_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_perc), desc(margin_PF)) %>%
  left_join(afl_ladders, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_perc)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_perc, margin_PF) %>%
  left_join(afl_ladders, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_perc)
head(least_winning_margin_season, 5)

# highest movement in final position
highest_mvmt_up_season = arrange(afl_ladders, desc(pos_diff)) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(afl_ladders, pos_diff) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_down_season, 5)


# lowest position to premiers in one season
prev_pos_premiers = afl_ladders %>%
  filter(premiers == 1) %>%
  select(season, Team, prev_pos) %>%
  arrange(desc(prev_pos), season)
head(prev_pos_premiers, 5)

# lowest position after being premiers in one season
next_pos_premiers = afl_ladders %>%
  filter(premiers == 1) %>%
  select(season, Team, next_pos) %>%
  arrange(desc(next_pos), season)
head(next_pos_premiers, 5)


# volatility of position from year to year
pos_changes = afl_ladders %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# Longest streaks
longest_streaks_premiers = arrange(afl_ladders, desc(streak_premiers)) %>%
  select(season, Team, streak_premiers)
head(longest_streaks_premiers, 5)

longest_streaks_missed_premiers = arrange(afl_ladders, desc(streak_missed_premiers)) %>%
  select(season, Team, streak_missed_premiers)
head(longest_streaks_missed_premiers, 5)

longest_streaks_runners_up = arrange(afl_ladders, desc(streak_runners_up)) %>%
  select(season, Team, streak_runners_up)
head(longest_streaks_runners_up, 5)

longest_streaks_minor_premiers = arrange(afl_ladders, desc(streak_minor_premiers)) %>%
  select(season, Team, streak_minor_premiers)
head(longest_streaks_minor_premiers, 5)

longest_streaks_missed_minor_premiers = arrange(afl_ladders, desc(streak_missed_minor_premiers)) %>%
  select(season, Team, streak_missed_minor_premiers)
head(longest_streaks_missed_minor_premiers, 5)

longest_streaks_finals = arrange(afl_ladders, desc(streak_finals)) %>%
  select(season, Team, streak_finals)
head(longest_streaks_finals, 5)

longest_streaks_missed_finals = arrange(afl_ladders, desc(streak_missed_finals)) %>%
  select(season, Team, streak_missed_finals)
head(longest_streaks_missed_finals, 5)

longest_streaks_grand_finals = arrange(afl_ladders, desc(streak_grand_finals)) %>%
  select(season, Team, streak_grand_finals)
head(longest_streaks_grand_finals, 5)

longest_streaks_missed_grand_finals = arrange(afl_ladders, desc(streak_missed_grand_finals)) %>%
  select(season, Team, streak_missed_grand_finals)
head(longest_streaks_missed_grand_finals, 5)


# no. of teams in finals
finals_teams = afl_ladders %>% 
#  filter(str_detect(tolower(Qualification), pattern = "finals") |
#           str_detect(tolower(Qualification), pattern = "play-offs")) %>% 
  group_by(season, yr_end) %>% 
  summarise(finals_teams = max(Pos))

# list of all team abbreviations
teams_unique = unique(afl_ladders$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = afl_ladders %>% 
  filter(!Pts == (pts_per_win * (W + B) + pts_per_draw * D))    

error_check_pld = afl_ladders %>%
  filter(!Pld == (W + D + L))

error_check_results = season_totals %>%
  filter(!Total_W == Total_L)

error_check_pd_season = season_totals %>%
  filter(!(Total_PF == Total_PA))

error_check_perc = afl_ladders %>%
  filter(!(abs(perc_check) < 0.1))

error_check_pos = group_by(afl_ladders, season) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))

error_sorted_pos = afl_ladders %>%
  arrange(season_no, desc(Pts), desc(perc), desc(PF)) %>%
  mutate(sorted_row_number = row_number(),
         row_no_diff = row_number - sorted_row_number) %>%
  filter(!(row_no_diff == 0))

check_identical_pos = afl_ladders %>%
  group_by(season_no, Pts, perc, PF) %>%
  summarise(count_seasons = n()) %>%
  filter(count_seasons > 1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph_afl("CAR")   # Carlton
make_graph_afl("COL")   # Collingwood
make_graph_afl("ESS")   # Essendon
make_graph_afl("FIT")   # Fitzroy
make_graph_afl("GEE")   # Geelong
make_graph_afl("MEL")   # Melbourne
make_graph_afl("SYD")   # Sydney Swans
make_graph_afl("STK")   # St. Kilda
make_graph_afl("RIC")   # Richmond
make_graph_afl("UNI")   # Melbourne University
make_graph_afl("WBD")   # Western Bulldogs
make_graph_afl("HAW")   # Hawthorn
make_graph_afl("NME")   # North Melbourne
make_graph_afl("BRB")   # Brisbane Bears
make_graph_afl("WCE")   # West Coast Eagles
# new teams from AFL era 1990s -
make_graph_afl("ADE")   # Adelaide Crows
make_graph_afl("FRE")   # Fremantle Dockers
make_graph_afl("BRL")   # Brisbane Lions
make_graph_afl("POR")   # Port Adelaide
make_graph_afl("GOL")   # Gold Coast Suns
make_graph_afl("GWS")   # Greater Western Sydney Giants


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(afl_all_time_ladder) <- gsub(x = names(afl_all_time_ladder), pattern = "_", replacement = " ") 

setwd(output_path)
save(tables, file = "afl_ladders_raw.Rdata")
save(afl_ladders, file = "afl_ladders.Rdata")
write.csv(afl_ladders, file = "afl_ladders_full.csv")
write.csv(afl_all_time_ladder, file = "afl_all_time_ladder.csv")
setwd(path) 

# export single graph
#setwd(output_path)
#ggsave("graph_ggsave.pdf")
#setwd(path)

# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph_afl(teams_unique[i])
  setwd(output_path)
  #  ggsave(paste("graph_afl_", teams_unique[i], ".pdf", sep=""))
  ggsave(paste("performance_chart_afl_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_afl_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:
# Check streaks for gaps - e.g. years when there was no Grand Final
# Graph of season_totals data - ave_points_scored_team, min & max

# Future:
# NMFC chart not put on wikipedia, as there was already one there, only up to 2009 but includes VFA era.
# Start on AFLW when there is enough data for meaningful charts
# Tasmania Devils from 2028


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
# read one league ladder manually
table = read_html("https://en.wikipedia.org/wiki/2023_AFL_season")
tables_all <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all[[3]]
table_yyyymm
