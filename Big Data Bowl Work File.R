
library(gganimate)
library(sf)
library(sfheaders)
library(concaveman)



# Import Data -------------------------------------------------------------



setwd("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL/DataBowl/Big-Data-Bowl-2022/Big-Data-Bowl-2022")

w1 <- read.csv("week1.csv")
w2 <- read.csv("week2.csv")
w3 <- read.csv("week3.csv")
w4 <- read.csv("week4.csv")
w5 <- read.csv("week5.csv")
w6 <- read.csv("week6.csv")
w7 <- read.csv("week7.csv")
w8 <- read.csv("week8.csv")

df <- w1  
  # full_join(w2) %>% 
  # full_join(w3) %>% 
  # full_join(w4)  %>% 
  # full_join(w5)  %>% 
  # full_join(w6)  %>% 
  # full_join(w7)  %>% 
  # full_join(w8) 

df <- df %>% 
  # Flip positional values
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160 / 3 - y, y),
         dir = ifelse(playDirection == "left", dir + 180, dir),
         dir = ifelse(dir > 360, dir - 360, dir),
         o = ifelse(playDirection == "left", o + 180, o),
         o = ifelse(o > 360, o - 360, o))

games <- read.csv("games.csv")

plays <- read.csv("plays.csv")

players <- read.csv("players.csv")

pff <- read.csv("pffScoutingData.csv")



# Field Generation and Animation --------------------------------------------------------

#Debugging to watch a play animated - Copied from Ron Yurko's code

SelectPlay <- plays %>%
  select(gameId, playId, playDescription) %>% 
  #sample_n(1)
  filter(gameId == 2021091300, playId == 3312)

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
                 shape = team, fill = team,
                 group = nflId, size = team, color = team),
             alpha = 0.7) +
  geom_text(data = SelectPlay, 
            aes(x = (xmax-y), y = x, label = jerseyNumber), 
            color = "white", vjust = 0.36, size = 3.5) +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#FFB612","brown", "#101820"), guide = FALSE) +
  scale_color_manual(values = c("#FFB612","brown", "#101820"), guide = FALSE) +
  ylim(ymin, ymax) + coord_fixed() +
  cowplot::theme_nothing() + theme(plot.title = element_text()) +
  transition_time(frameId) + ease_aes('linear') + NULL


ex_play_length <- length(unique(SelectPlay$frameId))


animate(play_animation, fps = 10, nframe = ex_play_length)



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


# Area Generation ---------------------------------------------------------

#Defining the pocket area

workDF1 <- df %>% 
  left_join(framesInt, by = c("gameId", "playId")) %>% 
  left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
  filter(!is.na(pff_role)) %>%
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame)  %>% 
  filter(pff_role == "Block", gameId == 2021091300) %>%
  group_by(gameId, playId, frameId)  %>% 
  # These are the extra points at the back of the pocket to round out
  # the area the OL should be guarding (QB-independant)
  mutate(extray = x[pff_positionLinedUp == "C"]-8,
         extrax = max(y),
         extrax2 = min(y)) %>% 
  group_by() 


#Having to create two new DFs to create a matrix of points to round
# out the pocket

workDF3 <- workDF1 %>% 
  mutate(points = matrix(c(extray, extrax),ncol = 2))%>% 
  group_by(gameId, playId, frameId) %>% 
  slice_head(n = 1) %>% 
  select(points)

workDF4 <- workDF1 %>% 
  mutate(points = matrix(c(extray, extrax2),ncol = 2)) %>% 
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
         areaperperson = area/n()) %>% 
  group_by(gameId, playId) %>% 
  mutate(cumarea = cummean(area))

#Viz for the pocket polygon

ggplot() + 
  geom_sf(data = head(workDF2$poly$polygons,1)) 

#Viz for the area over the time of the play. There is a nice
# consistency to the data that I didn't expect for the first 3-3.5s.
# I graphed multiple iterations of it, but ended with area per person

ggplot(workDF2 %>% 
         group_by(frameId) %>% 
         summarise(area = mean(area),
                   cumarea = mean(cumarea),
                   areaper = mean(areaperperson),
                   count = n()), aes(x = frameId, y = areaper, size = count)) + 
  geom_point()+
  geom_smooth(aes(color = "red"))+ 
  # geom_point(aes(y = area, color = "blue"))+
  # geom_smooth(aes(y = area, color = "blue"))+
  geom_vline(xintercept = 31)+
  scale_x_continuous(breaks = seq(5,100,5), limits = c(6,45))+
  scale_color_identity(aesthetics = c("color"))
  

#The following lines of code are to calculate that smoothed curve
# line. Geom_smooth does a model to create that curve, and I don't
# feel like recreating it when it did it for me.

ggp <- ggplot(workDF2 , aes(x = frameId, y = areaperperson, color = playId)) + 
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = 31)+
  geom_vline(xintercept = 36)+
  scale_x_continuous(breaks = seq(5,100,5))

 ggp_data <- ggplot_build(ggp)$data[[2]] 
 ggp_fit <- ggplot(ggp_data, aes(x, y)) +  
   geom_line()
 ggp_data2 <- ggplot_build(ggp_fit)$data[[1]] 
 ggp_data2  #This is the values for my model based smooth area per person

# Adjusting the values to be tidier
 
 model <- ggp_data2 %>% 
   select(x, y) %>% 
   mutate(frameId = round(x, 0),
          modely = y) %>% 
   group_by(frameId) %>% 
   slice_head(n = 1) %>%
   select(frameId, modely)
 
# Joining the model data and taking the sum of the residuals from the
# model to the actual as a proxy for quality of the pocket.
 
 AreaModel <- workDF2 %>% 
   left_join(model, by = c("frameId")) %>% 
   group_by() %>% 
   mutate(areaperDiff = areaperperson - modely) %>% 
   group_by(gameId, playId) %>% 
   mutate(DiffTotal = sum(unique(areaperDiff)))
 


# Blocking Win/Loss -------------------------------------------------------

radius <- 1.5
 
 #Defining the distinct rushers who are in the play at 1.5s
 
 dfRushMatch <- df %>% 
   left_join(framesInt, by = c("gameId", "playId")) %>% 
   left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
   filter(!is.na(pff_role)) %>%
   filter(!is.na(start_frame), !is.na(end_frame),
          frameId >= start_frame, frameId <= end_frame)  %>% 
   filter(pff_role == "Rush", gameId == 2021091300,
          playId == 3312) %>%
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
   filter(pff_role == "Block", gameId == 2021091300,
          playId == 3312) %>%
   group_by(gameId, playId) %>% 
   mutate(Time = frameId - min(frameId) + 1) %>% 
   filter(Time == 15) %>% 
   left_join(dfRushMatch, by = c("gameId", "playId", "Time")) %>% 
   mutate(Circleinside = ifelse((x.y - x.x)^2 + (y.y - y.x)^2 <= radius^2, 1, 0)) %>% 
   filter(Circleinside == 1) %>% 
   select(gameId, playId, nflId.x, nflId.y)
 
 #Defining the rushers for the whole play
 
 dfRush <- df %>% 
   left_join(framesInt, by = c("gameId", "playId")) %>% 
   left_join(play_block_rush, by = c("gameId", "playId", "nflId")) %>%
   filter(!is.na(pff_role)) %>%
   filter(!is.na(start_frame), !is.na(end_frame),
          frameId >= start_frame, frameId <= end_frame)  %>% 
   filter(pff_role == "Rush", gameId == 2021091300,
          playId == 3312) %>%
   group_by(gameId, playId) %>% 
   mutate(Time = frameId - min(frameId) + 1)
 
 #Defining the play and whether the blocker allows the rusher they 
 #matched with to get past the perpendicular line between them and 
 # a hypothetical QB. Also taking into account WHEN the rusher got 
 #past as a measure of time during the play with pressure happening at 
 # 2.5s being considered normal, earlier getting more weight, and later
 # being downweighted as it is more expected.
 
 dfBlock <- df %>% 
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
   mutate(Past = ifelse(x.y >= perpint + perpslope * y.y, 0, 1),
          WithinDown = (Time/25)) %>% 
   group_by(nflId) %>% 
   arrange(Time) %>% 
   filter(Past == 1) %>% 
   slice_head(n = 1) %>% 
   mutate(Value = Past/WithinDown)
 
   
 
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
   