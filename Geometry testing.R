library(ggforce)


test <- data.frame(matrix(c(3,7,7,8), ncol = 2, byrow = T)) %>% 
  mutate(QBx = 5,
         QBy = 1,
         radius = 2,
         testx = 4,
         testy = 8,
         slope = (QBy - X2 + 0.0000000001) / (QBx - X1 + 0.0000000001),
         intercept = X2 - slope * X1,
         perpslope = -1/slope,
         perpint = X2 - perpslope * X1,
         Circleinside = ifelse((testx - X1)^2 + (testy - X2)^2 <= radius^2,
                               1, 0),
         Past = ifelse(testy >= perpint + perpslope * testx, 0, 1))

ggplot(test, aes(x = X1, y = X2))+
  geom_point()+
  geom_point(aes(x = QBx, y = QBy))+
  geom_point(aes(x = testx, y = testy))+
  geom_circle(aes(x0 = X1, y0 = X2, r = radius, color = ifelse(Circleinside == 1, "green", "red")))+
  geom_abline(slope = test$slope, intercept = test$intercept)+
  geom_abline(slope = test$perpslope, intercept = test$perpint, color = ifelse(test$Past == 0, "green", "red"))+
  scale_y_continuous(breaks = seq(0,10,1), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10))+
  theme_reach()
