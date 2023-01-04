'''
Still to do:

-Network Analysis
--DataViz of Network Analysis

-Team/Player Level importance of top features
-Table of the Top Features and their cor
--Explanations of top features and their cor

-EPA Cor table/Viz
'''



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


setwd("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL/DataBowl/Big-Data-Bowl-2022/Big-Data-Bowl-2022")


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
  # Flip positional values
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




# Selected Plays ----------------------------------------------------------


# NE/NYJ 1st Percentile filter(gameId == 2021091906, playId == 3050) %>% 

# LV/BAL 92nd Percentile filter(gameId == 2021091300, playId == 3664) 

check <- QualityOfPocket %>% 
  group_by() %>% 
  mutate(QOPPR = percent_rank(QualityOfPocket)) %>% 
  filter(gameId == 2021091906, playId == 3050)


# Field Generation and Animation --------------------------------------------------------


Time <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role), pff_role == "Pass",
         !is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame) %>% 
  group_by(gameId, playId) %>% 
  summarise(Time = n())

BadPocket <- df %>%  
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(pff, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role), pff_role == "Pass",
         !is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame) %>% 
  filter(gameId == 2021091906, playId == 3050)



TopPlaysSearch <- pbp %>% 
  mutate(old_game_id = as.integer(old_game_id),
         qtr2 = seconds_to_period(quarter_seconds_remaining)) %>% 
  # filter(old_game_id == 2021091300) %>% 
  select(old_game_id, play_id, game_id, qtr, qtr2, quarter_seconds_remaining, down) %>% 
  inner_join(QualityOfPocket, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
  group_by() %>%
  mutate(QOPPR = percent_rank(QualityOfPocket)) %>% 
  left_join(BlitzDF, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>%  
  left_join(Time, by = c("old_game_id" = "gameId", "play_id" = "playId"))  %>%  left_join(playsDF, by = c("old_game_id" = "gameId", "play_id" = "playId"))  %>% 
  filter(pff_playAction == 0,
    quarter_seconds_remaining >= 120, Rushers == 4, between(Time, 10,30)
    ) %>%
  arrange((QualityOfPocket)) %>% 
  group_by() %>%
  slice_head(n = 6) 


SelectPlay <- plays %>%
  select(gameId, playId, playDescription) %>% 
  filter(gameId == 2021101708, playId == 2519) 

SelectPlay <- df %>%
  inner_join(SelectPlay, by = c("gameId", "playId")) 


# General field boundaries
xmin <- 0
xmax <- 160/3
hash_right <- 38.35
hash_left <- 12
hash_width <- 3.3
# Specific boundaries for a given play
ymin <- max(round(min(SelectPlay$x, 
                      na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(SelectPlay$x, 
                      na.rm = TRUE) + 10, -1), 120)
# Hash marks
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



play_animation <- field_base +
  geom_point(data = SelectPlay, 
             aes(x = (xmax - y), y = x,
                 shape = (team),
                 fill = primary,
                 group = nflId, 
                 size = (team),
                 color = primary),
             alpha = 0.7) +
  geom_text(data = SelectPlay, 
            aes(x = (xmax-y), y = x, label = jerseyNumber), 
            color = "white", vjust = 0.36, size = 3.5) +
  scale_size_manual(values = c(6,6,4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_color_identity(aesthetics = c("fill", "color"))+
  # scale_fill_manual(values = c(SelectPlay$primary,"brown", SelectPlay$primary), guide = FALSE) +
  # scale_color_manual(values = c(SelectPlay$primary,"brown", SelectPlay$primary), guide = FALSE) +
  ylim(ymin, ymax) + coord_fixed() +
  cowplot::theme_nothing() + theme(plot.title = element_text()) +
  transition_time(frameId) + ease_aes('linear') + NULL


ex_play_length <- length(unique(SelectPlay$frameId))


animate(play_animation, fps = 10, nframe = ex_play_length)


Stop Hitting Enter
# Filtering and Feature Engineering -----------------------------------------------------

#Selecting only the frames we are interested in working with - 
#Copied from Ron Yurko's code


framesInt <- df %>% 
  mutate(is_start = as.numeric(event %in% c("autoevent_ballsnap", "ball_snap")),
         # Now all of the options for the end of the ball carrier sequence:
         is_end = as.numeric(event %in% c("fumble", "handoff", "lateral",
                                          "autoevent_passforward", "pass_forward",
                                          "qb_sack", "qb_strip_sack", "run"))) %>% 
  group_by(gameId, playId) %>% 
  mutate(any_start = any(is_start == 1),
         any_end = any(is_end == 1)) %>%
  filter(any_start, any_end) %>%
  summarize(start_frame = frameId[which(is_start == 1)[1]],
            end_frame = frameId[which(is_end == 1 & frameId > start_frame)[1]], .groups = "drop")

#Generating the list of players who rushed or blocked on a play

play_block_rush <- pff %>% 
  filter(pff_role %in% c("Pass Block", "Pass Rush")) %>%
  dplyr::select(gameId, playId, nflId, pff_role, pff_positionLinedUp) %>%
  mutate(pff_role = str_remove(pff_role, "Pass "))

pff_network <- pff %>% 
  filter(pff_role %in% c("Pass Block", "Pass Rush", "Pass")) %>%
  dplyr::select(gameId, playId, nflId, pff_role, pff_positionLinedUp) 


# Area Generation ---------------------------------------------------------

#Defining the pocket area

workDF1 <- df  %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Block") %>%
  group_by(gameId, playId, frameId)  %>% 
  # These are the extra points at the back of the pocket to round out
  # the area the OL should be guarding (QB-independant)
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


#Having to create two new DFs to create a matrix of points to round
# out the pocket

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

#Actually generating the area of the pocket by creating a polygon
# and calculating the area from there. 
# Debated about using the cumulative average of the area, but chose
# to go with the raw area / # of blockers (as more blockers means
# more area inherently).

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


#The following lines of code are to calculate that smoothed curve
# line. Geom_smooth does a model to create that curve, and I don't
# feel like recreating it when it did it for me.

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
 #ggp_data2  #This is the values for my model based smooth area per person
# Adjusting the values to be tidier
 
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
 #plot(imputed_y)
 
 
# Joining the model data and taking the sum of the residuals from the
# model to the actual as a proxy for quality of the pocket.
 
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
# 
# 
#  #Defining the rushers for the whole play
#  
#  dfRush <- df %>% 
#    left_join(framesInt, by = c("gameId", "playId")) %>% 
#    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
#    filter(!is.na(pff_role)) %>%
#    filter(!is.na(start_frame), !is.na(end_frame),
#           frameId >= start_frame, frameId <= end_frame)  %>% 
#    filter(pff_role == "Rush") %>%
#    group_by(gameId, playId) %>% 
#    mutate(Time = frameId - min(frameId) + 1)
#  
#  #Defining the play and whether the blocker allows the rusher they 
#  #matched with to get past the perpendicular line between them and 
#  # a hypothetical QB. Also taking into account WHEN the rusher got 
#  #past as a measure of time during the play with pressure happening at 
#  # 2.5s being considered normal, earlier getting more weight, and later
#  # being downweighted as it is more expected.
#  
#  dfBlock <- df %>% 
#    left_join(framesInt, by = c("gameId", "playId")) %>% 
#    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
#    filter(!is.na(pff_role)) %>%
#    filter(!is.na(start_frame), !is.na(end_frame),
#           frameId >= start_frame, frameId <= end_frame)  %>% 
#    filter(pff_role == "Block") %>%
#    group_by(gameId, playId) %>% 
#    mutate(Time = frameId - min(frameId) + 1,
#           QBy = x[pff_positionLinedUp == "C" & Time == 1]-6,
#           QBx = y[pff_positionLinedUp == "C" & Time == 1],
#           slope = (QBy - x + 0.0000000001) / (QBx - y + 0.0000000001),
#           intercept = x - slope * y,
#           perpslope = -1/slope,
#           perpint = x - perpslope * y) %>% 
#    left_join(dfRush, by = c("gameId", "playId", "Time"))  %>% 
#    mutate(Past = ifelse(x.y < perpint + perpslope * y.y & Time >= 5, 1, 0),
#           WithinDown = (Time/25),
#           Value = case_when(Time < 25 ~ Past / WithinDown,
#                            T ~ Past / log(Time, 25)))  %>% 
#    group_by(gameId, playId) %>% 
#    arrange(Time) %>% 
#    filter(Past == 1) %>% 
#    summarise(Off = head(team.x,1),
#              Def = head(team.y,1),
#              Value = sum(Value))
#  
#  
#  dfRushtest <- df %>% 
#    filter(gameId == 2021091906, playId == 3050) %>% 
#    left_join(framesInt, by = c("gameId", "playId")) %>% 
#    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
#    filter(!is.na(pff_role)) %>%
#    filter(!is.na(start_frame), !is.na(end_frame),
#           frameId >= start_frame, frameId <= end_frame)  %>% 
#    filter(pff_role == "Rush") %>%
#    group_by(gameId, playId) %>% 
#    mutate(Time = frameId - min(frameId) + 1,
#           points = matrix(c(x, y),ncol = 2),
#           points2 = sf_point(points)) 
 
 workDF5 <- workDF2 %>% 
   # filter(gameId == 2021091906, playId == 3050) %>% 
   group_by(gameId, playId, frameId) %>% 
   summarise(Polygon = head(poly, 1))
   
 
 dfBlock1 <- df %>% 
   # filter(gameId == 2021091906, playId == 3050) %>% 
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
   # filter(gameId == 2021091906, playId == 3050) %>% 
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
 
 
 

# Full Combo --------------------------------------------------------------

FullQOP <- AreaModel %>% 
     left_join(dfBlock2) %>% 
     group_by() %>% 
     mutate(Value = if_else(is.na(Value) == 1, 0, Value),
            PsrPR = percent_rank(Value)
            #QOP = DiffTotal / (PsrPR + 0.000001)
            )
 
 QualityOfPocket <- FullQOP %>% 
   group_by() %>% 
   mutate(Max = max(DiffTotal, na.rm = T)) %>% 
   group_by(gameId, playId) %>% 
   summarise(
     #QualityOfPocket = QOP,
     # DiffTotal = DiffTotal,
     # PsrPR = PsrPR,
     # Check = (Max)*PsrPR,
     QualityOfPocket = DiffTotal - (Max*2)*PsrPR) 
 
 
 QOPEPA <- FullQOP %>% 
   group_by() %>% 
   mutate(Max = max(DiffTotal, na.rm = T)) %>% 
   group_by(gameId, playId) %>% 
   summarise(QOP = DiffTotal - (2*Max)*PsrPR) 
   
 write_rds(FullQOP, file = "Quality Of Pocket.rds")
 
# Next Round of Feature Engineering ---------------------------------------
# 
# smalldf <- workDF1 %>% 
#      distinct(gameId, playId) %>% 
#      sample_n(1000)
#      head()
     
   
   
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
    select(gameId, playId, game_seconds_remaining, air_yards, 
           wp, spread_line, pass_oe) 

# Converting into a single wide df on a per play level ####
#NFLID
  #OLDF, DLDF, SlidesDF, pffValue

#PlayID
  #StuntsDF, MotionDF, DeepestDF, BlitzDF, BlockTypes, playsDF, pbpBDB

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
  # 
  # WideDFSmall <- WideDF %>% 
  #   sample_n(1000)
  
  
  # 
  # pivot_wider(names_from = nflId, values_from = c(pff_blockType, pff_backFieldBlock, AvgSpeed.x, AvgAccel.x, MaxWidth.x, AvgDir, AvgDirIOL, Slide, AvgSpeed.y, AvgAccel.y, MaxWidth.y)) 

  

# XGBoost Modeling ----------------------------------------------------------------
 #make this example reproducible
  set.seed(0)
  
  #split into training (80%) and testing set (20%)
  parts = createDataPartition(WideDF$QualityOfPocket, p = .8, list = F)
  train = WideDF[parts, ]
  test = WideDF[-parts, ]
  
  #define predictor and response variables in training set
  train_x = data.matrix(train[, -c(128)])
  train_y = t(train[, 128])
  
  #define predictor and response variables in testing set
  test_x = data.matrix(test[, -c(128)])
  test_y = t(test[, 128])
  
  #define final training and testing sets
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)

  #define watchlist
  watchlist = list(train=xgb_train, test=xgb_test)
  
  #fit XGBoost model and display training and testing data at each round
  model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 1000, print_every_n = 10, early_stopping_rounds = 100, maximize = F)
  
  final = xgboost(data = xgb_train, max.depth = 3, nrounds = 32, verbose = 0)
  
  #use model to make predictions on test data
  pred_y = predict(final, xgb_test)
  
  #measure prediction accuracy
  mean((test_y - pred_y)^2) #mse
  caret::MAE(test_y, pred_y) #mae
  caret::RMSE(test_y, pred_y) #rmse  
  
  
  # mean((test_y - pred_y)^2) #mse
  # [1] 41905.77
  # >   caret::MAE(test_y, pred_y) #mae
  # [1] 173.6377
  # >   caret::RMSE(test_y, pred_y) #rmse  
  # [1] 204.709
  
  
  # Dtrain = xgb.DMatrix(
  #   data = as.matrix(WideDF[, -128]),
  #   label = WideDF$QualityOfPocket)
  # Dtest = xgb.DMatrix(
  #   data = as.matrix(WideDF[, 128]))
  # 
  # xgbcv = xgb.cv(data = Dtrain,
  #                max.depth = 3,
  #                nrounds = 1000,
  #                nfold = 5,
  #                print_every_n = 10,
  #                early_stopping_rounds = 100,
  #                maximize = F)
  # 
  # Stopping. Best iteration:
  #   [28]	train-rmse:176.124016+0.555481	test-rmse:184.956121+2.181484
  
  importance_matrix <- xgb.importance(model = final)
  
  xgb.plot.importance(importance_matrix)


# BART --------------------------------------------------------------------

  #FOR SOME REASON, YOU CAN'T HAVE COLUMNS AS FACTORS or CHARACTERS!!!!!
  #Consider making the factor/character columns into pivot_wider extra columns
  
  
  WideDFBart <- na.omit(WideDF) %>% 
    select_if(~ !is.character(.))
  
  WideDFTrain <- WideDFBart[, -128]
  WideDFTest <- WideDFBart$QualityOfPocket
  
  
  # Use k-fold cross-validation with the BART model
  results <- cv.bartMachine(WideDFTest, WideDFTrain, K = 10)
  
  # Extract the cross-validated predictions
  predictions <- results$pred
  
  
  
  #make this example reproducible
  set.seed(0)
  
  #split into training (80%) and testing set (20%)
  parts = createDataPartition(WideDFBart$QualityOfPocket, p = .8, list = F)
  barttrain_x = WideDFTrain[parts, ]
  barttrain_y = WideDFTest[parts]
  barttest_x = WideDFTrain[-parts,]
  barttest_y = WideDFTest[-parts]
  
  bart_machine = bartMachine(barttrain_x, barttrain_y)
  summary(bart_machine)
  
  rmse_by_num_trees(bart_machine, 
                    tree_list=c(seq(25, 75, by=5)),
                    num_replicates=3)
  
  bart_machine <- bartMachine(barttrain_x, barttrain_y, num_trees=60, seed=0)
  summary(bart_machine)
  bartVar <- investigate_var_importance(bart_machine, num_replicates_for_avg = 20)  
  
  BartVarPlot <- data.frame(bartVar$avg_var_props[1:20]) %>% 
    arrange(desc(BartVarPlot[,1]))
  
  ggplot(BartVarPlot, aes(y = reorder(rownames(BartVarPlot), BartVarPlot[,1]), x = BartVarPlot[,1]))+
    geom_col()+
    theme_reach()+
    labs(title = "BART Model Variable Importance (Top 10 Variables)",
         y = "Features",
         x = "Inclusion Proportion")
  
  plot_convergence_diagnostics(bart_machine)
  
  check_bart_error_assumptions(bart_machine)
  
  plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE)
  plot_y_vs_yhat(bart_machine, Xtest=barttest_x, ytest=barttest_y, prediction_intervals = TRUE)
  
  rmse <- function(x, y) sqrt(mean((x - y)^2))
  rsq <- function(x, y) summary(lm(y~x))$r.squared
  y_pred <- predict(bart_machine, barttest_x)
  paste('r2:', rsq(barttest_y, y_pred))
  paste('rmse:', rmse(barttest_y, y_pred))
  cor.test(barttest_y, y_pred, method=c("pearson"))
  
#   [1] "r2: 0.490629174935046"
#   >   paste('rmse:', rmse(barttest_y, y_pred))
#   [1] "rmse: 121.543045798169"
#   >   cor.test(barttest_y, y_pred, method=c("pearson"))
#   
#   Pearson's product-moment correlation
# 
# data:  barttest_y and y_pred
# t = 40.489, df = 1702, p-value < 0.00000000000000022
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6754289 0.7238588
# sample estimates:
#       cor 
# 0.7004493
  
  AHHHHHHHHHHHHHHHHHH
  
  
# Comparison to EPA -------------------------------------------------------
  
  RegEPA <- pbp %>% 
    mutate(old_game_id = as.integer(old_game_id)) %>%
    filter(week < 9) %>% 
    inner_join(QOPEPA, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
    filter(play == 1, epa != 0,
           !is.na(epa), !is.na(down)) %>%
    group_by() %>%
    mutate(QOP = QOP,
           QOPPR = percent_rank(QOP),
           epa1 = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                          epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
           epaPR = percent_rank(epa),
           epaplus = (QOPPR),
           epaplus2 = (QOPPR*2 + epaPR)/3,
           epaplus3 = (QOPPR + epaPR)/2,
           epaplus4 = (QOPPR + epaPR*2)/3)  %>% 
    select(season, posteam, week, old_game_id, play_id, QOP, QOPPR, epa, epa1, epaPR, epaplus,
           epaplus2, epaplus3, epaplus4)
  
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
              QOPtoEPAcor = cor(epa, QOP)
    ) 
  
  
  
  RegEPAWeek <- pbp %>% 
    mutate(old_game_id = as.integer(old_game_id)) %>%
    filter(week < 9) %>% 
    inner_join(QOPEPA, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
    filter(play == 1, epa != 0,
           !is.na(epa), !is.na(down)) %>%
    group_by() %>%
    mutate(QOP = QOP,
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
           epaplus4 = (QOPPR + epaPR*2)/3) %>% 
    select(season, posteam, week, QOP, QOPPR, epa, epa1, epaPR, epaplus,
           epaplus2, epaplus3, epaplus4)
  
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
              QOPtoEPAcor = cor(epa, QOP)
    ) 
  
  
  QOPPLOT <- RegEPAWeek %>%
    group_by(season, posteam) %>%  
    arrange(week) %>%  
    mutate(NextEPA = lead(epa, 1)) %>% 
    na.omit(NextEPA) 
  
  
  ggplot(QOPPLOT, aes(x = epaplus4, y = NextEPA))+
    geom_point()+
    geom_smooth()+
    geom_smooth(method = "lm", color = "red")
  
  ggplot(QOPPLOT, aes(x = epa, y = NextEPA))+
    geom_point()+
    geom_smooth()+
    geom_smooth(method = "lm", color = "red")
  
  lm <- lm(NextEPA ~ epaplus4, data = QOPPLOT)
  summary(lm)
  
  
  ggplot(QOPPLOT, aes(x = QOP))+
    geom_density(aes())
  
  ggplot(QOPPLOT, aes(x = epa))+
    geom_density(aes())
  
  # ggplot(QOPPLOT %>% 
  #          filter(between(epa, -0.5, 0.5),
  #                 between(QOP, -50, 50)),
  #                 aes(x = QOP, y = epa))+
  #   geom_point()+
  #   geom_smooth()+
  #   geom_smooth(method = "lm", color = "red")
  
  AHH
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
    left_join(offensedf, by = c("gameId", "playId")) %>% 
    filter(team == possessionTeam) %>% 
    select(!possessionTeam) %>% 
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
    group_by() %>% 
    select(!gameId) %>% 
    select(!playId) %>% 
    select(!MaxWidth.x) %>% 
    select(!AvgDir) %>% 
    select(!AvgDirIOL) %>% 
    select(!DLStart) %>% 
    select(!team)
  
  WideTeam <- na.omit(WideTeam)
  
  
  options(scipen = 999)
  AllCor <- cor(WideTeam)
  AllCor <- AllCor[,130]
  view(AllCor)
  
  
  
  ggplot(WideDF, aes(x = PA, y = QualityOfPocket))+
    geom_smooth(method = "lm")+
    geom_boxplot(aes(group = PA))
  
  
  
  TeamsAnalysis <- WideTeam %>% 
    group_by(team) %>% 
    summarise(Plays = n(),
              QOP = mean(QualityOfPocket),
              OLAccel = mean(OLAccel),
              OLSpeed = mean(OLSpeed),
              RolloutRate = (sum(SCRAMBLE_ROLLOUT_RIGHT) + sum(SCRAMBLE_ROLLOUT_LEFT)) / Plays,
              PARate = sum(pff_playAction)/Plays,
              BackBlockRate = sum(BackBlock) / Plays,
              DLAccel = mean(DLAccel),
              DLSpeed = mean(DLSpeed),
              StuntRate = sum(Stunt) / Plays
              )
  
  
  
  OLPlayerAnalysis <- df %>% 
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
    group_by(team, jerseyNumber, nflId) %>% 
    summarise(AvgSpeed = mean(s, na.rm = T),
              AvgAccel = mean(a, na.rm = T))
  
  
  
  
  DLPlayerAnalysis <- df %>% 
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
    group_by(team, jerseyNumber, nflId) %>% 
    summarise(AvgSpeed = mean(s, na.rm = T),
              AvgAccel = mean(a, na.rm = T))
  
  
  

# DATA VIZ ----------------------------------------------------------------
  # Viz of pocket -----------------------------------------------------------
  
  
  
  
  workDFViz <- workDF2 %>% 
    filter(gameId == 2021101708, playId == 2519) %>% 
    group_by(gameId, playId, frameId) %>% 
    summarise(Polygon = head(poly, 1))
  
  
  dfBlockViz <- df %>% 
    filter(gameId == 2021101708, playId == 2519)  %>% 
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
  
  
  
  # 
  # dfRushViz <- df %>% 
  #   filter(gameId == 2021091300, playId == 3664) %>% 
  #   left_join(framesInt, by = c("gameId", "playId")) %>% 
  #   left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  #   filter(!is.na(pff_role)) %>%
  #   filter(!is.na(start_frame), !is.na(end_frame),
  #          frameId >= start_frame, frameId <= end_frame)  %>% 
  #   filter(pff_role == "Rush") %>%
  #   group_by(gameId, playId) %>% 
  #   mutate(Time = frameId - min(frameId) + 1)
  # 
 
  # dfBlockViz <- df %>% 
  #   filter(gameId == 2021091300, playId == 3664) %>% 
  #   left_join(framesInt, by = c("gameId", "playId")) %>% 
  #   left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  #   filter(!is.na(pff_role)) %>%
  #   filter(!is.na(start_frame), !is.na(end_frame),
  #          frameId >= start_frame, frameId <= end_frame)  %>% 
  #   filter(pff_role == "Block") %>%
  #   group_by(gameId, playId) %>% 
  #   mutate(Time = frameId - min(frameId) + 1,
  #          QBy = x[pff_positionLinedUp == "C" & Time == 1]-6,
  #          QBx = y[pff_positionLinedUp == "C" & Time == 1],
  #          slope = (QBy - x + 0.0000000001) / (QBx - y + 0.0000000001),
  #          intercept = x - slope * y,
  #          perpslope = -1/slope,
  #          perpint = x - perpslope * y) %>% 
  #   left_join(dfRushViz, by = c("gameId", "playId", "Time"))  %>% 
  #   mutate(Past = ifelse(x.y < perpint + perpslope * y.y & Time >= 5, 1, 0),
  #          WithinDown = (Time/25),
  #          Value = case_when(Time < 25 ~ Past / WithinDown,
  #                            T ~ Past / log(Time, 25))) %>% 
  #   group_by(gameId, playId, frameId.y, nflId.y) %>% 
  #   summarise(Past = head(Past, 1))
  # 
  # 
  # 
  
  PlayViz1 <- df %>% 
    filter(gameId == 2021101708, playId == 2519)  %>% 
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
    
  # 
  # AreaModelPR <- workDF2 %>%  
  #   left_join(time, by = c("frameId")) %>% 
  #   group_by(gameId, playId) %>% 
  #   filter(!is.na(team)) %>% 
  #   mutate(Time = frameId - min(frameId) + 1,
  #          WithinDown = (Time/25)) %>%  
  #   group_by() %>% 
  #   mutate(areaperDiff = areaperperson.x - areaperperson.y,
  #          Value = case_when(Time < 25 ~ areaperDiff / log(Time + 1, base = 26),
  #                            T ~ areaperDiff / WithinDown)) %>% 
  #   group_by(frameId) %>% 
  #   mutate(AREAPR = percent_rank(areaperperson.x)) %>% 
  #   group_by(gameId, playId, frameId) %>% 
  #   slice_head(n = 1)  %>% 
  #   group_by(gameId, playId) %>% 
  #   summarise(frameId = frameId,
  #             Area = areaperperson.x,
  #             AREAPR = AREAPR) 
  
  AreaModelViz  <- AreaModelPR%>% 
    filter(gameId == 2021101708, playId == 2519) 
  
  
  
  PlayVizBlock <- df %>% 
    filter(gameId == 2021101708, playId == 2519)  %>% 
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
  
  
   #colors <- colorRamp(c("#FF0000", "#00FF00"))
  
  
A
# Field stuff -------------------------------------------------------------

  
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash_right <- 38.35
  hash_left <- 12
  hash_width <- 3.3
  # Specific boundaries for a given play
  # ymin <- max(round(min(PlayViz$x, 
  #                       na.rm = TRUE) - 10, -1), 0)
  # ymax <- min(round(max(PlayViz$x, 
  #                       na.rm = TRUE) + 10, -1), 120)
  ymin <- 15
  ymax <- 30
  # Hash marks
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
  
  # ggplot()+ 
  #   annotate("text", x = df_hash$x[df_hash$x < 55/2],
  #            y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) +
  #   annotate("text", x = df_hash$x[df_hash$x > 55/2],
  #            y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
  #   annotate("segment", x = xmin, y = seq(max(10, ymin), min(ymax, 110), by = 5),
  #            xend =  xmax, yend = seq(max(10, ymin), min(ymax, 110), by = 5)) +
  #   annotate("text", x = rep(hash_left, 11), y = seq(10, 110, by = 10),
  #            label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
  #            angle = 270, size = 4) +
  #   annotate("text", x = rep((xmax - hash_left), 11), y = seq(10, 110, by = 10),
  #            label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
  #            angle = 90, size = 4) +
  #   annotate("segment", x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin),
  #            xend = c(xmin, xmax, xmax, xmin), yend = c(ymax, ymax, ymin, ymin), color = "black")+
  #   ylim(ymin, ymax)+
  #   geom_sf(data = PlayViz$poly$polygons,
  #           aes(fill = PlayViz$area, alpha = 0.5))+
  #   geom_point(data = PlayViz, 
  #              aes(x = (xmax - y), y = x,
  #                  shape = (team),
  #                  fill = primary,
  #                  group = nflId, 
  #                  color = primary),
  #              alpha = 0.7)  +
  #   scale_size_manual(values = c(6, 4, 6), guide = FALSE) +
  #   scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  #   scale_color_identity(aesthetics = c("fill", "color")) +
  #   cowplot::theme_nothing() + theme(plot.title = element_text())
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
      cowplot::theme_nothing() + theme(plot.title = element_text()) +
      labs(title = "ARI vs CLE 9th Percentile Pocket",
           subtitle = "The Pocket Size is Smaller than Expected Throughout, gets compressed heavily, and allow 3 Defenders into the pocket very quickly.",
           caption = "Only highlighting plays with 4 Rushers, Non 2-minute/end of game situations, without play action.")+
      transition_time(PlayViz$frameId) +
      ease_aes('linear') + NULL
  
    
    ex_play_lengthVIZ <- length(unique(PlayViz$frameId))
    
    
    animate(play_animationVIZ, fps = 10, nframe = ex_play_lengthVIZ)
    
  
    
    
  A

  # #Viz for the area over the time of the play. There is a nice
  # # consistency to the data that I didn't expect for the first 3-3.5s.
  # # I graphed multiple iterations of it, but ended with area per person

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

  
# NETWORK ANALYSIS --------------------------------------------------------
  
  NFLIDs <- df  %>% 
    # filter(gameId == 2021091906) %>%
    left_join(framesInt, by = c("gameId", "playId")) %>% 
    left_join(pff_network, by = c("gameId", "playId", "nflId")) %>%
    filter(!is.na(pff_role)) %>%
    filter(!is.na(start_frame), !is.na(end_frame),
           frameId >= start_frame, frameId <= end_frame) %>% 
    select(pff_positionLinedUp) %>% 
    distinct()
  
  NFLIDs <- as.vector(NFLIDs)
  
  # Use the combn() function to get every combination of IDs
  combinations <- apply(combn(NFLIDs$pff_positionLinedUp, 2), 2, c)
  
  # Convert the combinations to a data frame
  combinations_df <- t(combinations)
  combinations_dft <- data.frame(combinations_df)
  
  # check <- combinations_dft %>% 
  #   filter(X1 == 53444 | X2 == 53444)
  
  NFLCoords <- df  %>% 
    # filter(gameId == 2021091906) %>%
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
  
  plot(test)
  
  
  degree <-  as.data.frame(degree(test))
  
  #betweenness(test)
  #cluster_louvain(test)
  
  
  eigen <- eigen_centrality(test)
  eigen <- as.data.frame(eigen$vector)
  
  networkdist <- distances(test, weights = Network$TimeToClose2)
  
  QBDist <- as.data.frame(networkdist[,10])
  
  
  NFLIDCheck <- df  %>% 
    filter(gameId == 2021091906, playId == 3050) %>% 
    left_join(framesInt, by = c("gameId", "playId")) %>% 
    left_join(pff_network, by = c("gameId", "playId", "nflId")) %>%
    filter(!is.na(pff_role)) %>%
    filter(!is.na(start_frame), !is.na(end_frame),
           frameId >= start_frame, frameId <= end_frame) %>% 
    select(nflId, team, pff_positionLinedUp, jerseyNumber) %>% 
    distinct()
  
  a
  # OLD Blocking Plot -----------------------------------------------------------
  
  
  dfBlockplot <- df %>% 
    left_join(framesInt, by = c("gameId", "playId")) %>% 
    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
    filter(!is.na(pff_role)) %>%
    filter(!is.na(start_frame), !is.na(end_frame),
           frameId >= start_frame, frameId <= end_frame)  %>% 
    filter(pff_role == "Block", gameId == 2021091300,
           playId == 3312) %>%
    group_by(gameId, playId) %>% 
    mutate(Time = frameId - min(frameId) + 1,
           QBy = x[pff_positionLinedUp == "C" & Time == 1]-6,
           QBx = y[pff_positionLinedUp == "C" & Time == 1],
           slope = (QBy - x + 0.0000000001) / (QBx - y + 0.0000000001),
           intercept = x - slope * y,
           perpslope = -1/slope,
           perpint = x - perpslope * y)  %>% 
    left_join(dfMatch, by = c("gameId", "playId", "nflId" = "nflId.x")) %>% 
    left_join(dfRush, by = c("gameId", "playId", "Time", "nflId.y" = "nflId")) %>% 
    mutate(Circleinside = ifelse((x.y - x.x)^2 + (y.y - y.x)^2 <= radius^2, 1, 0)) 
  
  
  #Debugging the plotting of the lines and circles for the geometry
  
  
  ggplot(dfBlockplot %>% 
           filter(Time == 15),
         aes(x = y.x, y = x.x)) +
    
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
             xend = c(xmin, xmax, xmax, xmin), yend = c(ymax, ymax, ymin, ymin), color = "black")+
    ylim(65,75) + coord_fixed() +
    xlim(xmin, xmax) +
    geom_point()+
    geom_point(aes(x = QBx, y = QBy))+
    geom_point(aes(x = y.y, y = x.y, color = "red"))+
    geom_circle(aes(x0 = (y.x), y0 = (x.x), r = radius,
                    color = ifelse(Circleinside == 1, "red", "green")))+
    scale_color_identity(aesthetics = c("color", "fill"))+
    theme_reach()
  # geom_abline(slope = dfBlockplot$slope[dfBlockplot$Time == 15], intercept = dfBlockplot$intercept[dfBlockplot$Time == 15])+
  # geom_abline(slope = dfBlockplot$perpslope[dfBlockplot$Time == 15], intercept = dfBlockplot$perpint[dfBlockplot$Time == 15])
  # 
  
  
  
  
  # I hate coding -----------------------------------------------------------
  radius <- 1.5
  
  #Defining the distinct rushers who are in the play at 1.5s
  
  dfRushMatch <- df %>% 
    left_join(framesInt, by = c("gameId", "playId")) %>% 
    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
    filter(!is.na(pff_role)) %>%
    filter(!is.na(start_frame), !is.na(end_frame),
           frameId >= start_frame, frameId <= end_frame)  %>% 
    filter(pff_role == "Rush", gameId == 2021091300) %>%
    group_by(gameId, playId) %>% 
    mutate(Time = frameId - min(frameId) + 1) %>% 
    filter(Time == 15)
  
  #defining the matchup between specific blockers and rushers in the
  #play based on a circle centered on the blocker at 1.5s into the play
  
  dfMatch <- df %>% 
    left_join(framesInt, by = c("gameId", "playId")) %>% 
    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
    filter(!is.na(pff_role)) %>%
    filter(!is.na(start_frame), !is.na(end_frame),
           frameId >= start_frame, frameId <= end_frame)  %>% 
    filter(pff_role == "Block", gameId == 2021091300) %>%
    group_by(gameId, playId) %>% 
    mutate(Time = frameId - min(frameId) + 1) %>% 
    filter(Time == 15) %>% 
    left_join(dfRushMatch, by = c("gameId", "playId", "Time")) %>% 
    mutate(Circleinside = ifelse((x.y - x.x)^2 + (y.y - y.x)^2 <= radius^2, 1, 0)) %>% 
    filter(Circleinside == 1) %>% 
    select(gameId, playId, nflId.x, nflId.y)
  
  
  dfBlock <- df %>% 
    left_join(framesInt, by = c("gameId", "playId")) %>% 
    left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
    filter(!is.na(pff_role)) %>%
    filter(!is.na(start_frame), !is.na(end_frame),
           frameId >= start_frame, frameId <= end_frame)  %>% 
    filter(pff_role == "Block", gameId == 2021091300) %>%
    group_by(gameId, playId) %>% 
    mutate(Time = frameId - min(frameId) + 1)  %>% 
    group_by(gameId, playId, frameId) %>% 
    summarise(poly = cbind(x, y))
  
  
  test <- dfRush %>% 
    group_by(gameId, playId, frameId, nflId) %>% 
    filter(playId == 55) %>% 
    summarise(point = cbind(x, y))  %>% 
    left_join(dfBlock, by = c("gameId", "playId", "frameId"))
  
  
  # test2 <- InPocket = pip2d(dfBlock$poly, point)
  
  
  polyfill <- workDF1 %>% 
    mutate(points = matrix(c(x, y),ncol = 2)) %>% 
    full_join(workDF3) %>% 
    full_join(workDF4) %>% 
    mutate(points2 = sf_point(points)) %>% 
    group_by(gameId, playId, frameId) %>% 
    arrange(x) %>% 
    arrange(gameId, playId, frameId) %>% 
    summarise(poly = concaveman(points2)) 
  
  testpointsx <- c(1,2,3,4,5,6)
  testpointsy <- c(6,7,8,9,7,6)
  
  points <- sf_point(matrix(c(testpointsx, testpointsy), ncol = 2))
  
  poly <- concaveman(points)
  
  x = c(3, 3, 3, 3, 3)
  y = c(2, 3, 4, 5, 6)
  
  point <- cbind(x, y)
  
  test <- cbind(testpointsx, testpointsy)
  
  
  InPocket <-  pip2d(test, point)
  
  
  # df_grouped <- with(StuntsDF, by(StuntsDF, list(gameId, playId), function(x) {
  #   points <- as.matrix(x[, c("x", "y")])
  #   distances <- dist(points)
  #   df_distances <- as.data.frame(as.matrix(distances))
  #   df_distances
  # }))
  
  
  # df_nearest <- with(StuntsDF, by(StuntsDF, list(gameId, playId), function(x) {
  #   # Create a matrix with the x and y values of the points in each group
  #   points <- as.matrix(x[, c("x", "y")])
  #   
  #   # Use the dist function to compute the Euclidean distance between the given point and each point in the group
  #   distances <- dist(points)
  #   
  #   # Find the row index of the minimum distance
  #   nearest_row <- which.min(distances[, 1])
  #   
  #   # Return the row with the nearest point
  #   x[nearest_row, ]
  # }))
  
  offoppadjWPBDB <- pbp %>% 
    mutate(old_game_id = as.integer(old_game_id)) %>% 
    inner_join(smalldf, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
    filter(play == 1, epa != 0,
           !is.na(epa), !is.na(down)) %>%
    mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                         epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1))) %>% 
    group_by(season, defteam, week, old_game_id) %>% 
    summarise(Pos = posteam,
              Def = defteam,
              Count = NROW(defteam),
              PassEPA = mean(if_else(pass == 1, epa, 0), na.rm = T)) %>% 
    distinct() %>% 
    group_by(season, defteam) %>% 
    summarise(Week = week,
              old_game_id = old_game_id,
              Pos = Pos,
              Def = Def,
              PassEPA = PassEPA,
              PosteamPassEPA = mean(PassEPA)) %>% 
    group_by(season,Def) %>% 
    summarise(Week = Week,
              old_game_id = old_game_id,
              Pos = Pos,
              PassEPA = PassEPA,
              PosteamPassEPA = PosteamPassEPA,
              DefteamPassEPA = mean(PassEPA)) %>% 
    group_by(season) %>% 
    summarise(Week = Week,
              old_game_id = old_game_id,
              Pos = Pos,
              Def = Def,
              PassEPA = PassEPA,
              PosteamPassEPA = PosteamPassEPA,
              DefteamPassEPA = DefteamPassEPA,
              LGPassEPA = mean(PassEPA),
              PassADJ = DefteamPassEPA - LGPassEPA,
              FinPassEPA = PassEPA - PassADJ) %>% 
    distinct() %>% 
    group_by(season, Week, old_game_id, Pos) %>% 
    summarise(FinPassEPA = mean(FinPassEPA))
  
  offQOPoppadjWPBDB <- pbp %>% 
    mutate(old_game_id = as.integer(old_game_id)) %>% 
    inner_join(smalldf, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
    left_join(FullQOP, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>% 
    filter(play == 1, epa != 0,
           !is.na(epa), !is.na(down)) %>%
    group_by() %>% 
    mutate(QOPPR = percent_rank(QOP)) %>% 
    mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                         epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
           epaPR = percent_rank(epa),
           epaplus = (QOPPR + epaPR*2)/3
    ) %>% 
    group_by(season, defteam, week, old_game_id) %>% 
    summarise(Pos = posteam,
              Def = defteam,
              Count = NROW(defteam),
              PassEPA = mean(epaplus, na.rm = T)) %>% 
    distinct() %>% 
    group_by(season, defteam) %>% 
    summarise(Week = week,
              old_game_id = old_game_id,
              Pos = Pos,
              Def = Def,
              PassEPA = PassEPA,
              PosteamPassEPA = mean(PassEPA)) %>% 
    group_by(season,Def) %>% 
    summarise(Week = Week,
              old_game_id = old_game_id,
              Pos = Pos,
              PassEPA = PassEPA,
              PosteamPassEPA = PosteamPassEPA,
              DefteamPassEPA = mean(PassEPA)) %>% 
    group_by(season) %>% 
    summarise(Week = Week,
              old_game_id = old_game_id,
              Pos = Pos,
              Def = Def,
              PassEPA = PassEPA,
              PosteamPassEPA = PosteamPassEPA,
              DefteamPassEPA = DefteamPassEPA,
              LGPassEPA = mean(PassEPA),
              PassADJ = DefteamPassEPA - LGPassEPA,
              FinPassEPA = PassEPA - PassADJ) %>% 
    distinct() %>% 
    group_by(season, Week, old_game_id, Pos) %>% 
    summarise(FinPassEPA = mean(FinPassEPA))
  
  
  
  
  # #define predictor and response variables in training set
  # barttrain_x = barttrain[, -c(48)]
  # barttrain_y = barttrain$QualityOfPocket
  # 
  # #define predictor and response variables in testing set
  # barttest_x = barttest[, -48]
  # barttest_y = barttest$QualityOfPocket
  
  # options(java.parameters="-Xmx5000m")
  
  
  # scale_fill_gradient(high="#00FF00", low="#FF0000")+