# Setup and Libraries -----------------------------------------------------


options(java.parameters="-Xmx3g")
library(gganimate)
library(transformr)
library(sf)
library(sfheaders)
library(concaveman)
library(ggforce)
library(sp)
library(mgcv)
library(stats)
library(xgboost)
library(caret)
library(bartMachine)
library(ggridges)
library(lubridate)
library(RColorBrewer)
library(grDevices)
library(igraph)
library(nflreadr)
library(tidyverse) 
library(teamcolors) 
library(ggimage)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(DT)
library(DescTools)
library(gt)
library(formattable)

# Import Data -------------------------------------------------------------


setwd("C:/Users/sethl/OneDrive/Excel Files/Big-Data-Bowl-2022")

w1 <- read.csv("week1.csv")
w2 <- read.csv("week2.csv")
w3 <- read.csv("week3.csv")
w4 <- read.csv("week4.csv")
w5 <- read.csv("week5.csv")
w6 <- read.csv("week6.csv")
w7 <- read.csv("week7.csv")
w8 <- read.csv("week8.csv")

df <- w1  %>% 
  full_join(w2) %>%
  full_join(w3) %>%
  full_join(w4)  %>%
  full_join(w5)  %>%
  full_join(w6)  %>%
  full_join(w7)  %>%
  full_join(w8)

df <- df %>% 
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160 / 3 - y, y),
         dir = ifelse(playDirection == "left", dir + 180, dir),
         dir = ifelse(dir > 360, dir - 360, dir),
         o = ifelse(playDirection == "left", o + 180, o),
         o = ifelse(o > 360, o - 360, o))

df <- df %>% 
  left_join(nfl_colors, by = c("team" = "Code"))

df <- df %>% 
  mutate(primary = if_else(is.na(primary) == 1, "#8b4513", primary))

games <- read.csv("games.csv")

plays <- read.csv("plays.csv")

players <- read.csv("players.csv")

pff <- read.csv("pffScoutingData.csv")




# Filtering -----------------------------------------------------


framesInt <- df %>% 
  mutate(is_start = as.numeric(event %in% c("autoevent_ballsnap", "ball_snap")),
         is_end = as.numeric(event %in% c("fumble", "handoff", "lateral",
                                          "autoevent_passforward", "pass_forward",
                                          "qb_sack", "qb_strip_sack", "run"))) %>% 
  group_by(gameId, playId) %>% 
  mutate(any_start = any(is_start == 1),
         any_end = any(is_end == 1)) %>%
  filter(any_start, any_end) %>%
  summarize(start_frame = frameId[which(is_start == 1)[1]],
            end_frame = frameId[which(is_end == 1 & frameId > start_frame)[1]], .groups = "drop")

play_block_rush <- pff %>% 
  filter(pff_role %in% c("Pass Block", "Pass Rush")) %>%
  dplyr::select(gameId, playId, nflId, pff_role, pff_positionLinedUp) %>%
  mutate(pff_role = str_remove(pff_role, "Pass "))

pff_network <- pff %>% 
  filter(pff_role %in% c("Pass Block", "Pass Rush", "Pass")) %>%
  dplyr::select(gameId, playId, nflId, pff_role, pff_positionLinedUp) 


#Objective and Feature Engineering ----
# Area Generation ---------------------------------------------------------

workDF1 <- df  %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Block") %>%
  group_by(gameId, playId, frameId)  %>% 
  mutate(extray = x[pff_positionLinedUp == "C"]-8,
         extray2 = x[pff_positionLinedUp == "C"]-8,
         extrax = max(y),
         extrax2 = min(y)) %>% 
  group_by(gameId, playId) %>% 
  mutate(Time = frameId - min(frameId) + 1, 
         WithinDown = (Time/25),
         extray = head(extray, 1),
         extray2 = head(extray2, 1)) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(extray = case_when(Time <= 30 ~ head(extray, 1),
                            T ~ max(x[y == extrax])),
         extray2 = case_when(Time <= 30 ~ head(extray2, 1),
                             T ~ max(x[y == extrax2]))) %>% 
  group_by() 


workDF3 <- workDF1 %>% 
  mutate(points = matrix(c(extray, extrax),ncol = 2))%>% 
  group_by(gameId, playId, frameId) %>% 
  slice_head(n = 1) %>% 
  select(points)

workDF4 <- workDF1 %>% 
  mutate(points = matrix(c(extray2, extrax2),ncol = 2)) %>% 
  group_by(gameId, playId, frameId) %>% 
  slice_head(n = 1)%>% 
  select(points)


workDF2 <- workDF1 %>% 
  mutate(points = matrix(c(x, y),ncol = 2)) %>% 
  full_join(workDF3) %>% 
  full_join(workDF4) %>% 
  mutate(points2 = sf_point(points)) %>% 
  group_by(gameId, playId, frameId) %>% 
  arrange(x) %>% 
  arrange(gameId, playId, frameId) %>% 
  mutate(poly = concaveman(points2)) %>%  
  mutate(area = st_area(poly$polygons),
         areaperperson = area/n())


# Area from Model ---------------------------------------------------------

ggp <- ggplot(workDF2 , aes(x = frameId, y = areaperperson)) + 
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = 31)+
  geom_vline(xintercept = 36)+
  scale_x_continuous(breaks = seq(5,100,5))


ggp_data <- ggplot_build(ggp)$data[[2]] 
ggp_fit <- ggplot(ggp_data, aes(x, y)) +  
  geom_line()
ggp_data2 <- ggplot_build(ggp_fit)$data[[1]] 

time <- data.frame(frameId = seq(1, 190,1))

fit <- gam(formula = areaperperson ~ s(frameId,bs = "cs"), data = workDF2)

model <- ggp_data2 %>% 
  select(x, y) %>% 
  mutate(frameId = round(x, 0),
         areaperperson = y) %>% 
  group_by(frameId) %>% 
  slice_head(n = 1) %>%
  select(frameId, areaperperson)


time <- time %>%  
  left_join(model)

imputed_y <- predict(fit, time[is.na(time$areaperperson), ])
time$areaperperson[is.na(time$areaperperson)] <- imputed_y


AreaModel <- workDF2 %>%  
  left_join(time, by = c("frameId")) %>% 
  group_by(gameId, playId) %>% 
  mutate(Time = frameId - min(frameId) + 1,
         WithinDown = (Time/25)) %>%  
  group_by() %>% 
  mutate(areaperDiff = areaperperson.x - areaperperson.y,
         Value = case_when(Time < 25 ~ areaperDiff / log(Time + 1, base = 26),
                           T ~ areaperDiff / WithinDown)) %>% 
  filter(!is.na(team)) %>% 
  group_by(gameId, playId, team) %>% 
  summarise(DiffTotal = sum(unique(Value), na.rm = T))



# Blocking Win/Loss -------------------------------------------------------


workDF5 <- workDF2 %>% 
  group_by(gameId, playId, frameId) %>% 
  summarise(Polygon = head(poly, 1))


dfBlock1 <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Rush") %>%
  group_by(gameId, playId) %>% 
  mutate(Time = frameId - min(frameId) + 1,
         points = matrix(c(x, y),ncol = 2),
         points2 = sf_point(points)) %>% 
  left_join(workDF5, by = c("gameId", "playId", "frameId"))  

dfBlock2 <- dfBlock1 %>% 
  group_by(gameId, playId, frameId, nflId) %>% 
  mutate(Inside = as.integer(st_contains(Polygon$polygons, points2$geometry)),
         Inside = if_else(is.na(Inside) == 1, 0, 1),
         Inside = case_when(Time < 5 ~ 0,
                            Time >= 5 & Inside == 1 ~ 1,
                            T ~ 0),
         WithinDown = (Time/25),
         Value = case_when(Time < 25 ~ Inside / WithinDown,
                           T ~ Inside / log(Time, 25)))%>% 
  group_by(gameId, playId) %>%
  arrange(Time) %>%
  filter(Inside == 1) %>%
  summarise(Value = sum(Value, na.rm = T))




# PoQIT Full Model--------------------------------------------------------------

FullQOP <- AreaModel %>% 
  left_join(dfBlock2) %>% 
  group_by() %>% 
  mutate(Value = if_else(is.na(Value) == 1, 0, Value),
         PsrPR = percent_rank(Value))

QualityOfPocket <- FullQOP %>% 
  group_by() %>% 
  mutate(Max = max(DiffTotal, na.rm = T)) %>% 
  group_by(gameId, playId) %>% 
  summarise(QualityOfPocket = DiffTotal - (Max*2)*PsrPR) 



# Feature Engineering ---------------------------------------


FEDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Block" | pff_role == "Rush") %>%
  select(!primary) %>% 
  select(!secondary)


BlitzDF <- FEDF %>% 
  group_by(gameId, playId) %>% 
  mutate(Cy = y[pff_positionLinedUp == "C" & frameId == start_frame]) %>% 
  filter(pff_role == "Rush") %>% 
  group_by(gameId, playId, nflId) %>% 
  slice_head(n = 1) %>% 
  mutate(Rushers = n(),
         Left = case_when(y >= Cy ~ 1,
                          T ~ 0),
         Right = case_when(y < Cy ~ 1,
                           T ~ 0)) %>% 
  group_by(gameId, playId) %>% 
  summarise(Rushers = sum(Rushers),
            RushLeft = sum(Left),
            RushRight = sum(Right),
            AvgY = mean(y))


defensedf <- df %>% 
  left_join(plays) %>% 
  select(gameId, playId, defensiveTeam) %>% 
  distinct()

offensedf <- df %>% 
  left_join(plays) %>% 
  select(gameId, playId, possessionTeam) %>% 
  distinct()


DeepestDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  left_join(defensedf, by = c("gameId", "playId")) %>% 
  filter(frameId == start_frame) %>% 
  group_by(gameId, playId) %>% 
  mutate(Startx = x[pff_positionLinedUp == "C"]) %>% 
  group_by(gameId, playId, defensiveTeam, nflId) %>%
  filter(team == defensiveTeam) %>% 
  group_by(gameId, playId, defensiveTeam) %>% 
  arrange(desc(x))  %>% 
  slice_head(n = 1) %>% 
  group_by(gameId, playId) %>% 
  summarise(Deepest = x - Startx)


MotionDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(offensedf, by = c("gameId", "playId")) %>% 
  group_by(gameId, playId) %>% 
  filter(team == possessionTeam) %>% 
  mutate(PreSnapMotion = if_else(frameId < start_frame &
                                   s >= 1.5, 1, 0),
         MotionAtSnap = if_else(frameId == start_frame &
                                  s >= 1.5, 1, 0)) %>% 
  summarise(PreSnapMotion = (if_else(sum(PreSnapMotion) >= 1, 1, 0)),
            MotionAtSnap = (if_else(sum(MotionAtSnap) >= 1, 1, 0)))

SlidesDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(offensedf, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  group_by(gameId, playId) %>% 
  filter(team == possessionTeam) %>% 
  filter(!is.na(pff_role), pff_role == "Pass Block") %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= start_frame + 3)  %>% 
  group_by(gameId, playId, nflId, jerseyNumber, pff_positionLinedUp) %>% 
  mutate(AvgDir = mean(dir, na.rm = T),
         IOL = case_when(pff_positionLinedUp == "C" |
                           pff_positionLinedUp == "LG" |
                           pff_positionLinedUp == "RG"  ~ "IOL",
                         T ~ "EDGE")) %>% 
  group_by(gameId, playId, IOL) %>% 
  mutate(AvgDirIOL = mean(dir, na.rm = T),
         Right = case_when(AvgDirIOL >= 90 & AvgDirIOL < 270 ~ 1,
                           T ~ 0),
         Left = case_when(AvgDirIOL >= 90 & AvgDirIOL < 270 ~ 0,
                          T ~ 1)) %>% 
  group_by(gameId, playId, nflId) %>% 
  slice_head(n = 1) %>% 
  group_by(gameId, playId) %>% 
  summarise(Slide = (case_when(sum(Right) >= 3 ~ 1,
                               sum(Left) >= 3 ~ 0)),
            AvgDir = mean(AvgDir),
            AvgDirIOL = mean(AvgDirIOL)) 



OLDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(offensedf, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  group_by(gameId, playId) %>% 
  filter(team == possessionTeam) %>% 
  filter(!is.na(pff_role), pff_role == "Pass Block",
         pff_positionLinedUp == "LT" |
           pff_positionLinedUp == "LG" |
           pff_positionLinedUp == "C" |
           pff_positionLinedUp == "RG" |
           pff_positionLinedUp == "RT" ) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>%
  group_by(nflId) %>% 
  mutate(AvgSpeed = mean(s, na.rm = T),
         AvgAccel = mean(a, na.rm = T)) %>% 
  group_by(gameId, playId) %>% 
  filter(frameId == start_frame) %>% 
  summarise(MaxWidth.x = max(y, na.rm = T) - min(y, na.rm = T),
            OLSpeed = mean(AvgSpeed),
            OLAccel = mean(AvgAccel))



DLDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(defensedf, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  group_by(gameId, playId) %>% 
  filter(team == defensiveTeam) %>% 
  filter(!is.na(pff_role), pff_role == "Pass Rush",
         pff_positionLinedUp == "NRT" |
           pff_positionLinedUp == "NT" |
           pff_positionLinedUp == "NLT" |
           pff_positionLinedUp == "LOLB" |
           pff_positionLinedUp == "LE"  |
           pff_positionLinedUp == "REO" |
           pff_positionLinedUp == "DLT" |
           pff_positionLinedUp == "DRT" |
           pff_positionLinedUp == "RE" |
           pff_positionLinedUp == "LEO" |
           pff_positionLinedUp == "ROLB"|
           pff_positionLinedUp == "LILB" |
           pff_positionLinedUp == "RILB"
  ) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>%
  mutate(Roundx = round(x, 0)) %>% 
  group_by(nflId) %>% 
  mutate(AvgSpeed = mean(s, na.rm = T),
         AvgAccel = mean(a, na.rm = T)) %>% 
  group_by(gameId, playId) %>% 
  filter(frameId == start_frame) %>% 
  summarise(DLStart = round(mean(Roundx, na.rm = T),0),
            DLWidth = max(y[Roundx - DLStart < 3], na.rm = T) - min(y[Roundx - DLStart < 3], na.rm = T),
            DLSpeed = mean(AvgSpeed),
            DLAccel = mean(AvgAccel)) 



StuntsDF <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role), pff_role == "Pass Rush",
         pff_positionLinedUp == "NRT" |
           pff_positionLinedUp == "NT" |
           pff_positionLinedUp == "NLT" |
           pff_positionLinedUp == "LOLB" |
           pff_positionLinedUp == "LE"  |
           pff_positionLinedUp == "REO" |
           pff_positionLinedUp == "DLT" |
           pff_positionLinedUp == "DRT" |
           pff_positionLinedUp == "RE" |
           pff_positionLinedUp == "LEO" |
           pff_positionLinedUp == "ROLB"|
           pff_positionLinedUp == "LILB" |
           pff_positionLinedUp == "RILB"
  ) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= start_frame + 5)  %>% 
  left_join(defensedf, by = c("gameId", "playId")) %>% 
  filter(team == defensiveTeam) %>% 
  group_by(gameId, playId, frameId)  %>% 
  arrange(y) %>% 
  mutate(Rushers = n(),
         Roundx = round(x, 0),
         DLStart = round(mean(Roundx, na.rm = T),0)) %>% 
  group_by(gameId, playId) %>% 
  mutate(DLStart = head(DLStart, 1)) %>% 
  group_by(gameId, playId, nflId) %>% 
  arrange(frameId) %>% 
  mutate(DL = Roundx - DLStart,
         DL = head(DL, 1)) %>% 
  filter(DL < 3) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(Diff = round(y - lead(y, n = 1), 0)) %>% 
  select(gameId, playId, nflId, jerseyNumber, frameId, Rushers, y, Diff) %>% 
  group_by(gameId, playId, nflId) %>% 
  arrange(frameId) %>% 
  mutate(Stunt = if_else((Diff <= 0 & lead(Diff, n = 1) >= 0) |
                           (Diff >= 0 & lead(Diff, n = 1) <= 0), 1, 0),
         Stunt = if_else(is.na(Stunt) == 1 & Diff == 0, 1, Stunt),
         Stunt = if_else(is.na(Stunt) == 1, 0, Stunt)) %>% 
  group_by(gameId, playId) %>% 
  summarise(Stunt = (if_else(sum(Stunt) >= 1, 1, 0)))



BlockTypes <- pff %>% 
  group_by(gameId, playId) %>% 
  mutate(BH = case_when(pff_blockType == "BH" ~ 1,
                        T ~ 0),
         CH = case_when(pff_blockType == "CH" ~ 1,
                        T ~ 0),
         CL = case_when(pff_blockType == "CL" ~ 1,
                        T ~ 0),
         NB = case_when(pff_blockType == "NB" ~ 1,
                        T ~ 0),
         PA = case_when(pff_blockType == "PA" ~ 1,
                        T ~ 0),
         PP = case_when(pff_blockType == "PP" ~ 1,
                        T ~ 0),
         PR = case_when(pff_blockType == "PR" ~ 1,
                        T ~ 0),
         PT = case_when(pff_blockType == "PT" ~ 1,
                        T ~ 0),
         SR = case_when(pff_blockType == "SR" ~ 1,
                        T ~ 0),
         SW = case_when(pff_blockType == "SW" ~ 1,
                        T ~ 0),
         UP = case_when(pff_blockType == "UP" ~ 1,
                        T ~ 0)) %>% 
  summarise(BH = (if_else(sum(BH) >= 1, 1, 0)),
            CH = (if_else(sum(CH) >= 1, 1, 0)),
            CL = (if_else(sum(CL) >= 1, 1, 0)),
            NB = (if_else(sum(NB) >= 1, 1, 0)),
            PA = (if_else(sum(PA) >= 1, 1, 0)),
            PP = (if_else(sum(PP) >= 1, 1, 0)),
            PR = (if_else(sum(PR) >= 1, 1, 0)),
            PT = (if_else(sum(PT) >= 1, 1, 0)),
            SR = (if_else(sum(SR) >= 1, 1, 0)),
            SW = (if_else(sum(SW) >= 1, 1, 0)),
            UP = (if_else(sum(UP) >= 1, 1, 0)),
            BackBlock = (if_else(sum(pff_backFieldBlock, na.rm = T) >= 1, 1, 0)))



playsDF <- plays[,-c(3, 7:23)] %>%
  mutate(dropBackType = if_else(is.na(dropBackType) == 1, "TRADITIONAL", dropBackType),
         DFBOX = mean(defendersInBox, na.rm = T),
         defendersInBox = if_else(is.na(defendersInBox) == 1, DFBOX, as.numeric(defendersInBox)),
         offenseFormation = if_else(is.na(offenseFormation) == 1, "SHOTGUN", offenseFormation),
  ) %>% 
  select(!DFBOX) %>% 
  pivot_wider(names_from = c("offenseFormation"), 
              values_from = c("offenseFormation")) %>% 
  pivot_wider(names_from = c("personnelO"), 
              values_from = c("personnelO"))  %>% 
  select(!"NA") %>% 
  pivot_wider(names_from = c("personnelD"), 
              values_from = c("personnelD")) %>% 
  select(!"NA") %>% 
  pivot_wider(names_from = c("dropBackType"), 
              values_from = c("dropBackType"))%>% 
  pivot_wider(names_from = c("pff_passCoverage"), 
              values_from = c("pff_passCoverage"))%>% 
  pivot_wider(names_from = c("pff_passCoverageType"), 
              values_from = c("pff_passCoverageType"))  



names <- names(playsDF[,9:97])

playsDF <- playsDF %>% 
  mutate_at(vars(all_of(names)), funs(if_else(is.na(.), 0, 1)))



TTT <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role), pff_role == "Pass",
         !is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame) %>% 
  group_by(gameId, playId, team) %>% 
  summarise(TimeToEnd = n()/10) %>% 
  group_by(gameId, team) %>% 
  arrange(playId) %>% 
  mutate(TimeToEnd1 = cummean(TimeToEnd),
         TimeToEnd2 = lag(TimeToEnd1, 1),
         TimeToEnd = if_else(is.na(TimeToEnd2) == 1, TimeToEnd1, TimeToEnd2)) %>% 
  group_by(gameId, playId) %>% 
  select(TimeToEnd)



pbp <- nflreadr::load_pbp(seasons = 2021)

pbpBDB <- pbp %>% 
  mutate(old_game_id = as.integer(old_game_id)) %>% 
  mutate(gameId = old_game_id, 
         playId = play_id,
         pass_oeMean = mean(pass_oe, na.rm = T),
         pass_oe = if_else(is.na(pass_oe) == 1, pass_oeMean, pass_oe),
         air_yards = if_else(is.na(air_yards) == 1, 0, air_yards),
  ) %>% 
  filter(!is.na(posteam)) %>% 
  group_by(gameId, posteam) %>% 
  arrange(playId) %>% 
  mutate(air_yards1 = cummean(air_yards),
         air_yards2 = lag(air_yards1, 1),
         air_yards = if_else(is.na(air_yards2) == 1, air_yards1, air_yards2)) %>% 
  group_by() %>% 
  select(gameId, playId, week, game_seconds_remaining, air_yards, 
         wp, spread_line, pass_oe) 

# Conversion to per play level ----

WideDF <- FEDF %>% 
  select(!team) %>% 
  group_by(gameId, playId) %>% 
  summarise() %>% 
  left_join(OLDF, by = c("gameId", "playId")) %>% 
  left_join(SlidesDF, by = c("gameId", "playId"))  %>% 
  left_join(DLDF, by = c("gameId", "playId")) %>% 
  left_join(StuntsDF, by = c("gameId", "playId")) %>% 
  left_join(MotionDF, by = c("gameId", "playId")) %>% 
  left_join(DeepestDF, by = c("gameId", "playId")) %>% 
  left_join(BlitzDF, by = c("gameId", "playId")) %>% 
  left_join(BlockTypes, by = c("gameId", "playId")) %>% 
  left_join(playsDF, by = c("gameId", "playId")) %>%
  left_join(pbpBDB, by = c("gameId", "playId")) %>%
  left_join(TTT, by = c("gameId", "playId")) %>%
  left_join(QualityOfPocket, by = c("gameId", "playId"))  %>% 
  group_by() %>% 
  select(!gameId) %>% 
  select(!playId) %>% 
  select(!MaxWidth.x) %>% 
  select(!AvgDir) %>% 
  select(!AvgDirIOL) %>% 
  select(!DLStart)

WideDF <- na.omit(WideDF)


#MODEL EVALUATION----
# XGBoost Modeling ----------------------------------------------------------------

train = WideDF %>% 
  filter(week <= 6) %>% 
  select(!week)

test = WideDF %>% 
  filter(week > 6)%>% 
  select(!week)



train_x = data.matrix(train[, -c(128)])
train_y = t(train[, 128])

test_x = data.matrix(test[, -c(128)])
test_y = t(test[, 128])

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 10000, print_every_n = 10, early_stopping_rounds = 500, maximize = F)

final = xgboost(data = xgb_train, max.depth = 3, nrounds = 33, verbose = 0)

pred_y = predict(final, xgb_test)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse  


# > mean((test_y - pred_y)^2) #mse
# [1] 40698.57
# > caret::MAE(test_y, pred_y) #mae
# [1] 172.0154
# > caret::RMSE(test_y, pred_y) #rmse  
# [1] 201.7389



importance_matrix <- xgb.importance(model = final)


ggplot(importance_matrix %>% 
         arrange(desc(Gain)) %>% 
         slice_head(n = 20), aes(x = Gain, y = reorder(Feature, Gain)))+
  geom_col(aes(fill = case_when(Gain > 0.12 ~ "#228b22",
                                Gain > 0.06 ~ "#003366",
                                Gain > 0.03 ~ "#2f847c",
                                T ~ "#a32638")))+
  theme_reach()+
  scale_fill_identity(aesthetics = c("fill", "color"))+
  labs(title = "XGBOOST Model Feature Importance",
       subtitle = "Traditional Pocket Types, Top 20 Features only",
       y = "Feature",
       x = "Gain (Importance)",
       caption = "@SethDataScience")


# BART --------------------------------------------------------------------


WideDFBart <- na.omit(WideDF) %>% 
  select_if(~ !is.character(.)) 


barttrain_x = WideDFBart %>% 
  filter(week <= 6) %>% 
  select(!week) %>% 
  select(!QualityOfPocket)

barttrain_y = WideDFBart %>% 
  filter(week <= 6)%>% 
  select(QualityOfPocket) 

barttest_x = WideDFBart %>% 
  filter(week > 6) %>% 
  select(!week) %>% 
  select(!QualityOfPocket)

barttest_y = WideDFBart %>% 
  filter(week > 6)%>% 
  select(QualityOfPocket)



bart_machine = bartMachine(barttrain_x, barttrain_y$QualityOfPocket)
summary(bart_machine)

bartVar <- investigate_var_importance(bart_machine, num_replicates_for_avg = 20)  

BartVarPlot <- data.frame(bartVar$avg_var_props[1:20]) 

BartVarPlot <- BartVarPlot %>% 
  arrange(desc(BartVarPlot[,1]))

ggplot(BartVarPlot, aes(y = reorder(rownames(BartVarPlot), BartVarPlot[,1]), x = BartVarPlot[,1]))+
  geom_col(aes(fill = case_when(BartVarPlot[,1] > 0.05 ~ "#228b22",
                                BartVarPlot[,1] > 0.03 ~ "#003366",
                                BartVarPlot[,1] > 0.02 ~ "#2f847c",
                                T ~ "#a32638")))+
  theme_reach()+
  scale_fill_identity(aesthetics = c("fill", "color"))+
  labs(title = "BART Model Variable Importance",
  subtitle = "Traditional Pocket types, Top 20 Variables only",
       y = "Features",
       x = "Inclusion Proportion",
  caption = "@SethDataScience")



rmse <- function(x, y) sqrt(mean((x - y)^2))
rsq <- function(x, y) summary(lm(y~x))$r.squared
y_pred <- predict(bart_machine, barttest_x)
paste('r2:', rsq(barttest_y$QualityOfPocket, y_pred))
paste('rmse:', rmse(barttest_y$QualityOfPocket, y_pred))
cor.test(barttest_y$QualityOfPocket, y_pred, method=c("pearson"))

# [1] "r2: 0.243311465707482"
# > paste('rmse:', rmse(barttest_y$QualityOfPocket, y_pred))
# [1] "rmse: 201.370731869129"
# > cor.test(barttest_y$QualityOfPocket, y_pred, method=c("pearson"))
# 
# Pearson's product-moment correlation
# 
# data:  barttest_y$QualityOfPocket and y_pred
# t = 24.995, df = 1943, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4588801 0.5261767
# sample estimates:
#       cor 
# 0.4932661 




# Comparison to EPA -------------------------------------------------------

RegEPA <- pbp %>% 
  mutate(old_game_id = as.integer(old_game_id)) %>%
  filter(week < 9) %>% 
  inner_join(QualityOfPocket, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down)) %>%
  group_by() %>%
  mutate(QOP = QualityOfPocket,
         QOPPR = percent_rank(QOP),
         epa1 = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                        epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
         epaPR = percent_rank(epa),
         epaPR1 = percent_rank(epa1),
         epaplus = (QOPPR),
         epaplus2 = (QOPPR*2 + epaPR)/3,
         epaplus3 = (QOPPR + epaPR)/2,
         epaplus4 = (QOPPR + epaPR*2)/3,
         epaplus21 = (QOPPR*2 + epaPR1)/3,
         epaplus31 = (QOPPR + epaPR1)/2,
         epaplus41 = (QOPPR + epaPR1*2)/3)  %>% 
  select(season, posteam, week, old_game_id, play_id, QOP, QOPPR, epa, epa1, epaPR, epaplus,
         epaplus2, epaplus3, epaplus4,
         epaplus21, epaplus31, epaplus41)

CheckCORRegEPA <- RegEPA %>%
  group_by(season, posteam) %>% 
  arrange(play_id) %>%  
  arrange(week) %>% 
  mutate(NextEPA = lead(epa, 1)) %>% 
  na.omit(NextEPA)  %>% 
  group_by() %>% 
  summarise(EPAcor = cor(epa, NextEPA),
            EPA1cor = cor(epa1, NextEPA),
            EPAPluscor = cor(epaplus, NextEPA),
            EPAPlus2cor = cor(epaplus2, NextEPA),
            EPAPlus3cor = cor(epaplus3, NextEPA),
            EPAPlus4cor = cor(epaplus4, NextEPA),
            EPAPlus21cor = cor(epaplus21, NextEPA),
            EPAPlus31cor = cor(epaplus31, NextEPA),
            EPAPlus41cor = cor(epaplus41, NextEPA),
            QOPtoEPAcor = cor(epa, QOP)
  ) 



RegEPAWeek <- pbp %>% 
  mutate(old_game_id = as.integer(old_game_id)) %>%
  filter(week < 9) %>% 
  inner_join(QualityOfPocket, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down)) %>%
  group_by() %>%
  mutate(QOP = QualityOfPocket,
         epa1 = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                        epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)))  %>%
  group_by(season, posteam, week) %>% 
  summarise(QOP = mean(QOP, na.rm = T),
            epa = mean(epa, na.rm = T),
            epa1 = mean(epa1, na.rm = T)) %>% 
  group_by() %>% 
  mutate(QOPPR = percent_rank(QOP),
         epaPR = percent_rank(epa),
         epaPR1 = percent_rank(epa1),
         epaplus = QOPPR,
         epaplus2 = (QOPPR*2 + epaPR)/3,
         epaplus3 = (QOPPR + epaPR)/2,
         epaplus4 = (QOPPR + epaPR*2)/3,
         epaplus21 = (QOPPR*2 + epaPR1)/3,
         epaplus31 = (QOPPR + epaPR1)/2,
         epaplus41 = (QOPPR + epaPR1*2)/3) %>% 
  select(season, posteam, week, QOP, QOPPR, epa, epa1, epaPR, epaplus,
         epaplus2, epaplus3, epaplus4,
         epaplus21, epaplus31, epaplus41)

CheckCORRegEPAWeek <- RegEPAWeek %>%
  group_by(season, posteam) %>%  
  arrange(week) %>%  
  mutate(NextEPA = lead(epa, 1)) %>% 
  na.omit(NextEPA) %>% 
  group_by() %>% 
  summarise(EPAcor = cor(epa, NextEPA),
            EPA1cor = cor(epa1, NextEPA),
            EPAPluscor = cor(epaplus, NextEPA),
            EPAPlus2cor = cor(epaplus2, NextEPA),
            EPAPlus3cor = cor(epaplus3, NextEPA),
            EPAPlus4cor = cor(epaplus4, NextEPA),
            EPAPlus21cor = cor(epaplus21, NextEPA),
            EPAPlus31cor = cor(epaplus31, NextEPA),
            EPAPlus41cor = cor(epaplus41, NextEPA),
            QOPtoEPAcor = cor(epa, QOP)
  ) 


QOPPLOT <- RegEPAWeek %>%
  group_by(season, posteam) %>%  
  arrange(week) %>%  
  mutate(NextEPA = lead(epa, 1)) %>% 
  na.omit(NextEPA) 


ggplot(QOPPLOT, aes(x = epaplus4, y = NextEPA))+
  geom_point()+
  geom_smooth(method = "lm", color = "red")+
  theme_reach()+
  labs(title = "Next Week's EPA output vs Composite EPA & PoQIT",
       subtitle = "Composite Metric is 2/3's Current Week EPA and 1/3 PoQIT, Correlation of 0.124",
       y = "Next Week's EPA",
       x = "Percentile of Composite EPA and PoQIT")

ggplot(QOPPLOT, aes(x = epa, y = NextEPA))+
  geom_point()+
  geom_smooth(method = "lm", color = "red")+
  theme_reach()+
  labs(title = "Next Week's EPA output vs Current EPA",
       subtitle = "Using just Current Week's EPA, there is very low correlation of 0.064",
       y = "Next Week's EPA",
       x = "Current Week's EPA")


lm <- lm(NextEPA ~ epaplus4, data = QOPPLOT)
summary(lm)


ggplot(QOPPLOT, aes(x = QOP))+
  geom_density(aes(fill = "#cb4154", color = "#a32638",  alpha = 0.7))+
  theme_reach()+
  scale_fill_identity(aesthetics = c("fill", "color"))+
  scale_linewidth()+
  labs(title = "Density Plot of Week-Level PoQIT",
       subtitle = "It appears to be a Normal Distribution, which is encouraging.
       This metric could easily be centered and scaled, but there is an interesting point to be made that the average measure is Negative.",
       y = "Density", 
       x = "Week-Level Average PoQIT")

ggplot(QOPPLOT, aes(x = epa))+
  geom_density(aes())


# Checking Variables of Importance ----------------------------------------

EPAFill <- pbp %>% 
  mutate(old_game_id = as.integer(old_game_id)) %>%
  filter(week < 9) %>%
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down)) %>%
  group_by(old_game_id, play_id) %>%
  summarise(epa = epa,
            epa1 = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                           epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)))



WideTeam <- FEDF %>%
  group_by(gameId, playId, team) %>% 
  summarise() %>% 
  left_join(OLDF, by = c("gameId", "playId"))  %>% 
  left_join(SlidesDF, by = c("gameId", "playId"))  %>% 
  left_join(DLDF, by = c("gameId", "playId")) %>% 
  left_join(StuntsDF, by = c("gameId", "playId")) %>% 
  left_join(MotionDF, by = c("gameId", "playId")) %>% 
  left_join(DeepestDF, by = c("gameId", "playId")) %>% 
  left_join(BlitzDF, by = c("gameId", "playId")) %>% 
  left_join(BlockTypes, by = c("gameId", "playId")) %>% 
  left_join(playsDF, by = c("gameId", "playId")) %>%
  left_join(pbpBDB, by = c("gameId", "playId")) %>%
  left_join(TTT, by = c("gameId", "playId")) %>% 
  left_join(EPAFill, by = c("gameId" = "old_game_id", "playId" = "play_id")) %>% 
  left_join(QualityOfPocket, by = c("gameId", "playId"))  %>% 
  group_by()


WideTeam <- na.omit(WideTeam)

WideTeamCor <- WideTeam %>% 
  mutate(PoQIT = QualityOfPocket) %>% 
  select(!gameId) %>% 
  select(!playId) %>% 
  select(!team) %>% 
  select(!week) %>% 
  select(!AvgDir) %>% 
  select(Rushers, BackBlock, pff_playAction, PoQIT)

WideTeamCor <- WideTeamCor[,-c(43:101)]

options(scipen = 999)
AllCor <- cor(WideTeamCor)
AllCor <- AllCor[,130]
view(AllCor)

AllCor <- as.data.frame(AllCor)

AllCor$Features <- rownames(AllCor)

AllCor <- AllCor %>% 
  select(Features, everything())

gt(AllCor)%>%
  tab_header(title = "Counter-Intuitive Features Correlation Table",
             subtitle = "Highlighting the Correlation from some of the confusing features")%>%
  data_color(columns = c(Rushers, BackBlock, pff_playAction, PoQIT),
             colors = scales::col_numeric(palette = colors, domain = NULL))  %>% 
  tab_source_note("Data: 2021 Weeks 1-8. | Viz: @SethDataScience")


TeamsAnalysis <- WideTeam %>% 
  left_join(offensedf, by = c("gameId", "playId")) %>%   
  filter(team == possessionTeam) %>% 
  select(!possessionTeam) %>% 
  group_by(team) %>% 
  summarise(Plays = n(),
            PoQIT = mean(QualityOfPocket),
            PassOE = mean(pass_oe),
            BackBlockRate = sum(BackBlock) / Plays,
            BlitzFacedRate = sum(if_else(Rushers >= 5, 1, 0))/Plays,
            BackBlockOE = BackBlockRate - BlitzFacedRate, 
            AverageRushersFaced = mean(Rushers),
            DLSpeedFaced = mean(DLSpeed))  %>% 
  mutate(Team = team) %>%
  select(Team, everything()) %>%
  select(!team) %>%
  select(!Plays) %>%
  mutate(PoQIT = round(PoQIT, 2),
         PassOE = round(PassOE, 2),
         BackBlockRate = as.numeric((round(BackBlockRate, 4)*100)),
         BlitzFacedRate = as.numeric((round(BlitzFacedRate, 4)*100)),
         BackBlockOE = round(BackBlockRate - BlitzFacedRate, 2),
         AverageRushersFaced = round(AverageRushersFaced, 2),
         DLSpeedFaced = round(DLSpeedFaced, 2)
  )  %>% 
  arrange(desc(PoQIT))


TeamsDefAnalysis <- WideTeam %>% 
  left_join(defensedf, by = c("gameId", "playId")) %>% 
  filter(team == defensiveTeam) %>% 
  select(!defensiveTeam) %>% 
  group_by(team) %>% 
  summarise(Plays = n(),
            PoQITAllowed = mean(QualityOfPocket),
            PassOEAgainst = mean(pass_oe),
            BackBlockAgainstRate = sum(BackBlock) / Plays,
            BlitzRate = sum(if_else(Rushers >= 5, 1, 0))/Plays,
            BackBlockOEAgainst = BackBlockAgainstRate - BlitzRate, 
            AverageRushers = mean(Rushers),
            DLSpeed = mean(DLSpeed))  %>%  
  mutate(Team = team) %>%
  select(Team, everything()) %>%
  select(!team) %>%
  select(!Plays) %>%
  mutate(PoQITAllowed = round(PoQITAllowed, 2),
         PassOEAgainst = round(PassOEAgainst, 2),
         BackBlockAgainstRate = as.numeric((round(BackBlockAgainstRate, 4)*100)),
         BlitzRate = as.numeric((round(BlitzRate, 4)*100)),
         BackBlockOEAgainst = round(BackBlockAgainstRate - BlitzRate, 2),
         AverageRushers = round(AverageRushers, 2),
         DLSpeed = round(DLSpeed, 2),
         )  %>% 
  arrange((PoQITAllowed)) 




toptable <- TeamsAnalysis[c(1:5,28:32),]

Bottomtable <- TeamsDefAnalysis[c(1:5,28:32),]

colors <- colorRamp(c("#FF0000", "#00FF00"))
invcolors <- colorRamp(c("#00FF00", "#FF0000"))


gtable <- Bottomtable

gtableplot <- gt(gtable) %>% 
  tab_header(title = "Team-level Analysis of PoQIT and its important features",
             subtitle = "Defense Top and Bottom 5 by PoQIT Allowed")%>%
  data_color(columns = c(PoQITAllowed, PassOEAgainst),
             colors = scales::col_numeric(palette = invcolors, domain = NULL)) %>% 
  data_color(columns = c(BackBlockAgainstRate, BlitzRate, BackBlockOEAgainst, DLSpeed, AverageRushers),
             colors = scales::col_numeric(palette = colors, domain = NULL)) %>% 
  tab_source_note("Data: 2021 Weeks 1-8. | Viz: @SethDataScience")


gtableplot



# Network Analysis --------------------------------------------------------

NFLIDs <- df  %>% 
 left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff_network, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame) %>% 
  select(pff_positionLinedUp) %>% 
  distinct()

NFLIDs <- as.vector(NFLIDs)

combinations <- apply(combn(NFLIDs$pff_positionLinedUp, 2), 2, c)

combinations_df <- t(combinations)
combinations_dft <- data.frame(combinations_df)

NFLCoords <- df  %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff_network, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame) %>% 
  select(gameId, playId, frameId, pff_positionLinedUp, nflId, x, y, s, dir)


Networkfill <- combinations_dft %>% 
  left_join(NFLCoords, by = c("X1" = "pff_positionLinedUp")) %>% 
  left_join(NFLCoords, by = c("X2" = "pff_positionLinedUp", "gameId", "playId", "frameId")) 

Network <- Networkfill %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(Dist = sqrt(((x.x - x.y)^2 + (y.x - y.y)^2)),
         SameDirMin = if_else(dir.x - 90 <= 0, dir.x - 90 + 360, dir.x - 90),
         SameDirMax =if_else(dir.x + 90 >= 360, dir.x + 90 - 360, dir.x + 90),
         SpeedDiff = case_when(between(dir.y, SameDirMin, SameDirMax)  &
                                 x.y >= x.x ~ s.y-s.x,
                               between(dir.y, SameDirMin, SameDirMax)  &
                                 x.y < x.x ~ s.x - s.y,
                               T ~ s.y + s.x),
         TimeToClose = Dist/SpeedDiff,
         OffQB = case_when((X1 == "LT" |
                              X1 == "LG" |
                              X1 == "C" |
                              X1 == "RG" |
                              X1 == "RT" |
                              X1 == "TE-R" |
                              X1 == "TE-L"|
                              X1 == "TE-iL" |
                              X1 == "TE-oL"|
                              X1 == "TE-oR"|
                              X1 == "HB" |
                              X1 == "HB-L"|
                              X1 == "HB-R" |
                              X1 == "FB-L"|
                              X1 == "HB-L" ) & X2 == "QB" ~ 1,
                           T ~ 0),
         QBOff = case_when((X2 == "LT" |
                              X2 == "LG" |
                              X2 == "C" |
                              X2 == "RG" |
                              X2 == "RT" |
                              X2 == "TE-R" |
                              X2 == "TE-L"|
                              X2 == "TE-iL" |
                              X2 == "TE-oL"|
                              X2 == "TE-oR"|
                              X2 == "HB" |
                              X2 == "HB-L"|
                              X2 == "HB-R" |
                              X2 == "FB-L"|
                              X2 == "HB-L" ) & X1 == "QB" ~ 1,
                           T ~ 0),
         DefQB = case_when((X1 == "NRT" |
                              X1 == "NT" |
                              X1 == "NLT" |
                              X1 == "LOLB" |
                              X1 == "LE"  |
                              X1 == "REO" |
                              X1 == "DLT" |
                              X1 == "DRT" |
                              X1 == "RE" |
                              X1 == "LEO" |
                              X1 == "ROLB"|
                              X1 == "LILB" |
                              X1 == "RILB") & X2 == "QB" ~ 1,
                           T ~ 0),
         QBDef = case_when((X2 == "NRT" |
                              X2 == "NT" |
                              X2 == "NLT" |
                              X2 == "LOLB" |
                              X2 == "LE"  |
                              X2 == "REO" |
                              X2 == "DLT" |
                              X2 == "DRT" |
                              X2 == "RE" |
                              X2 == "LEO" |
                              X2 == "ROLB"|
                              X2 == "LILB" |
                              X2 == "RILB") & X1 == "QB" ~ 1,
                           T ~ 0),
         DefOff = case_when((X1 == "NRT" |
                               X1 == "NT" |
                               X1 == "NLT" |
                               X1 == "LOLB" |
                               X1 == "LE"  |
                               X1 == "REO" |
                               X1 == "DLT" |
                               X1 == "DRT" |
                               X1 == "RE" |
                               X1 == "LEO" |
                               X1 == "ROLB"|
                               X1 == "LILB" |
                               X1 == "RILB") & 
                              (X2 == "LT" |
                                 X2 == "LG" |
                                 X2 == "C" |
                                 X2 == "RG" |
                                 X2 == "RT" |
                                 X2 == "TE-R" |
                                 X2 == "TE-L"|
                                 X2 == "TE-iL" |
                                 X2 == "TE-oL"|
                                 X2 == "TE-oR"|
                                 X2 == "HB" |
                                 X2 == "HB-L"|
                                 X2 == "HB-R" |
                                 X2 == "FB-L"|
                                 X2 == "HB-L" ) ~ 1,
                            T ~ 0),
         OffDef = case_when((X2 == "NRT" |
                               X2 == "NT" |
                               X2 == "NLT" |
                               X2 == "LOLB" |
                               X2 == "LE"  |
                               X2 == "REO" |
                               X2 == "DLT" |
                               X2 == "DRT" |
                               X2 == "RE" |
                               X2 == "LEO" |
                               X2 == "ROLB"|
                               X2 == "LILB" |
                               X2 == "RILB") &
                              (X1 == "LT" |
                                 X1 == "LG" |
                                 X1 == "C" |
                                 X1 == "RG" |
                                 X1 == "RT" |
                                 X1 == "TE-R" |
                                 X1 == "TE-L"|
                                 X1 == "TE-iL" |
                                 X1 == "TE-oL"|
                                 X1 == "TE-oR"|
                                 X1 == "HB" |
                                 X1 == "HB-L"|
                                 X1 == "HB-R" |
                                 X1 == "FB-L"|
                                 X1 == "HB-L" ) ~ 1,
                            T ~ 0)) %>% 
  group_by() %>% 
  mutate(TimeToClosePR = percent_rank(TimeToClose),
         TimeToClose2 = case_when(TimeToClose == Inf ~ 10000,
                                  TimeToClose <= 0 ~ 50/(TimeToClosePR+0.01),
                                  is.na(TimeToClose) == 1 ~ 10000,
                                  T ~ TimeToClose)
  )  %>% 
  filter((TimeToClose2 <= 1 & (DefQB == 1 | QBDef == 1)) |
           (TimeToClose2 >= 1 & (OffQB == 1 | QBOff == 1)) ) %>%
  select(X1, X2, TimeToClose2)



test <- graph_from_data_frame(Network, directed = F, vertices = NULL)


degree <-  as.data.frame(degree(test))


eigen <- eigen_centrality(test)
eigen <- as.data.frame(eigen$vector)

networkdist <- distances(test, weights = Network$TimeToClose2)

QBDist <- as.data.frame(networkdist[,10])


#DATA VIZ ----------------------------------------------------------------
# Field stuff -------------------------------------------------------------

SelectPlay <- plays %>%
  select(gameId, playId, playDescription) %>% 
  filter(gameId == 2021091300, playId == 3664)  %>% 
  left_join(df)

xmin <- 0
xmax <- 160/3
hash_right <- 38.35
hash_left <- 12
hash_width <- 3.3
ymin <- max(round(min(SelectPlay$x, 
                      na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(SelectPlay$x, 
                      na.rm = TRUE) + 10, -1), 120)
ymin = 35
ymax = 50
df_hash <- 
  expand.grid(x = c(0, 23.36667, 29.96667, xmax), 
              y = (10:110)) %>% 
  filter(!(floor(y %% 5) == 0), y < ymax, y > ymin)

field_base <- ggplot() +
  annotate("text", x = df_hash$x[df_hash$x < 55/2],
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) +
  annotate("text", x = df_hash$x[df_hash$x > 55/2],
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
  annotate("segment", x = xmin, y = seq(max(10, ymin), min(ymax, 110), by = 5),
           xend =  xmax, yend = seq(max(10, ymin), min(ymax, 110), by = 5)) +
  annotate("text", x = rep(hash_left, 11), y = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 270, size = 4) +
  annotate("text", x = rep((xmax - hash_left), 11), y = seq(10, 110, by = 10),
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 90, size = 4) +
  annotate("segment", x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin),
           xend = c(xmin, xmax, xmax, xmin), yend = c(ymax, ymax, ymin, ymin), color = "black")

# Viz of pocket -----------------------------------------------------------




workDFViz <- workDF2 %>% 
  inner_join(SelectPlay) %>% 
  group_by(gameId, playId, frameId) %>% 
  summarise(Polygon = head(poly, 1))


dfBlockViz <- df %>% 
  inner_join(SelectPlay) %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Rush") %>%
  group_by(gameId, playId) %>% 
  mutate(Time = frameId - min(frameId) + 1,
         points = matrix(c(x, y),ncol = 2),
         points2 = sf_point(points)) %>% 
  left_join(workDFViz, by = c("gameId", "playId", "frameId"))  %>% 
  group_by(gameId, playId, frameId, nflId) %>% 
  summarise(Inside = as.integer(st_contains(Polygon$polygons, points2$geometry)),
            Inside = if_else(is.na(Inside) == 1, 0, 1),
            Inside = case_when(Time < 5 ~ 0,
                               Time >= 5 & Inside == 1 ~ 1,
                               T ~ 0),
            WithinDown = (Time/25),
            Value = case_when(Time < 25 ~ Inside / WithinDown,
                              T ~ Inside / log(Time, 25)))



PlayViz1 <- df %>% 
  inner_join(SelectPlay) %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Block") %>%
  group_by(gameId, playId, frameId)  %>% 
  mutate(y1 = x,
         x1 = ((160/3) - y)) %>% 
  mutate(extray = y1[pff_positionLinedUp == "C"]-8,
         extray2 = y1[pff_positionLinedUp == "C"]-8,
         extrax = max(x1),
         extrax2 = min(x1)) %>% 
  group_by(gameId, playId) %>% 
  mutate(Time = frameId - min(frameId) + 1, 
         WithinDown = (Time/25),
         extray = head(extray, 1),
         extray2 = head(extray2, 1)) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(extray = case_when(Time <= 30 ~ head(extray, 1),
                            T ~ y1[x1 == extrax]),
         extray2 = case_when(Time <= 30 ~ head(extray2, 1),
                             T ~ y1[x1 == extrax2])) %>% 
  group_by() 


PlayViz2 <- PlayViz1 %>% 
  mutate(points = matrix(c(extrax, extray),ncol = 2))%>% 
  group_by(gameId, playId, frameId) %>% 
  slice_head(n = 1) %>% 
  select(points)

PlayViz3 <- PlayViz1 %>% 
  mutate(points = matrix(c(extrax2, extray2),ncol = 2)) %>% 
  group_by(gameId, playId, frameId) %>% 
  slice_head(n = 1)%>% 
  select(points)


PlayViz4 <- PlayViz1 %>% 
  mutate(points = matrix(c(x1, y1),ncol = 2)) %>% 
  full_join(PlayViz2) %>% 
  full_join(PlayViz3) %>% 
  mutate(points2 = sf_point(points)) %>% 
  group_by(gameId, playId, frameId) %>% 
  arrange(x1) %>% 
  arrange(gameId, playId, frameId) %>% 
  mutate(poly = concaveman(points2)) %>%  
  mutate(area = st_area(poly$polygons),
         areaperperson = area/n()) 


AreaModelPR <- workDF2 %>%
  left_join(time, by = c("frameId")) %>%
  group_by(gameId, playId) %>%
  filter(!is.na(team)) %>%
  mutate(Time = frameId - min(frameId) + 1,
         WithinDown = (Time/25)) %>%
  group_by() %>%
  mutate(areaperDiff = areaperperson.x - areaperperson.y,
         Value = case_when(Time < 25 ~ areaperDiff / log(Time + 1, base = 26),
                           T ~ areaperDiff / WithinDown)) %>%
  group_by(frameId) %>%
  mutate(AREAPR = percent_rank(areaperperson.x)) %>%
  group_by(gameId, playId, frameId) %>%
  slice_head(n = 1)  %>%
  group_by(gameId, playId) %>%
  summarise(frameId = frameId,
            Area = areaperperson.x,
            AREAPR = AREAPR)

AreaModelViz  <- AreaModelPR %>% 
inner_join(SelectPlay) 



PlayVizBlock <- df %>% 
inner_join(SelectPlay) %>%  
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Rush") %>% 
  mutate(frameId = as.numeric(frameId))




PlayViz <- PlayViz4 %>% 
  left_join(PlayVizBlock, by = c("gameId", "playId", "frameId")) %>% 
  left_join(dfBlockViz, by = c("gameId", "playId", "frameId","nflId.y" = "nflId")) %>% 
  left_join(AreaModelViz, by = c("gameId", "playId", "frameId")) %>% 
  group_by() %>% 
  mutate(colorCode = colors(AREAPR),
         COLOR = rgb(colorCode[,1], colorCode[,2], colorCode[,3], maxColorValue=255),
         RED = "#FF0000")




# Animated VIZ ---- 



play_animationVIZ <- field_base +
  geom_sf(data = PlayViz$poly$polygons,
          aes(fill = PlayViz$COLOR, alpha = 0.5,
              group = seq_along(PlayViz$frameId)))+
  geom_point(data = PlayViz, 
             aes(x = (xmax - y.x), y = x.x,
                 group = nflId.x, 
                 fill = primary.x,
                 color = primary.x,
                 size = 3,
                 alpha = 0.7)) +
  geom_text(data = PlayViz, 
            aes(x = (xmax-y.x), y = x.x, label = jerseyNumber.x), 
            color = "white", vjust = 0.36, size = 2.5) +
  geom_point(data = PlayViz,
             aes(x = (xmax - y.y), y = x.y,
                 group = nflId.y,
                 fill = if_else(PlayViz$Inside == 1, "red", primary.y),
                 color = if_else(PlayViz$Inside == 1, "red", primary.y),
                 size = 3),
             alpha = 0.7) +
  geom_text(data = PlayViz, 
            aes(x = (xmax-y.y), y = x.y, label = PlayViz$jerseyNumber.y), 
            color = "white", vjust = 0.36, size = 2.5) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  ylim(ymin, ymax)+
  cowplot::theme_nothing() + theme(plot.title = element_text(),
                                   plot.subtitle = element_text(),
                                   plot.caption = element_text()) +
  labs(title = "LV vs BAL 92nd Percentile PoQIT
       
       ",
       subtitle = "Percentile out of All plays, selecting to highlight only plays 
       with 4+ Rushers, outside of 2-minute situations.
       
       
       
       ",
       caption = "@SethDataScience")+
  transition_time(PlayViz$frameId) +
  ease_aes('linear') + NULL


ex_play_lengthVIZ <- length(unique(PlayViz$frameId))


animate(play_animationVIZ, fps = 10, nframe = ex_play_lengthVIZ)


# Weighted Area per Person ------------------------------------------------




ggplot(workDF2 %>%
         group_by(frameId) %>%
         summarise(area = mean(area),
                   areaper = mean(areaperperson),
                   count = n()), aes(x = frameId, y = areaper, size = count)) +
  geom_point()+
  geom_smooth(aes(color = "red"))+
  geom_vline(xintercept = 35)+
  scale_x_continuous(breaks = seq(5,100,5), limits = c(6,45))+
  scale_color_identity(aesthetics = c("color"))+
  labs(title = "Value of Weighted Area Per Person over Time",
       y = "Weighted Area Per Person",
       x = "Frame Within Play (1/10s per frame)",
       subtitle = "Sharp cutoff after 3 seconds (30 frames from start) because the pocket shape changes to just be the polygon of the players.")+
  theme_reach()