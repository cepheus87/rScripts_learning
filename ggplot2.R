http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization


#http://r4stats.com/examples/graphics-ggplot2/

library(ggplot2)

X <- cbind(1:10, 1:10 + rnorm(10))
X2 <- cbind(1:10, 1:10 + rnorm(10, sd = 2))

plot(X)

#quickplot()  == qplot()

qplot(X[,1])  # histogram

qplot(x = X[,1], y = X[,2])

qplot(x = X[,1], y = X[,2], xlim = c(0,10), ylim = c(0,10))

?qplot()

?ggplot()

df <- as.data.frame(X)   # have to be data frame
df  
df2 <- as.data.frame(X2)   
  
plot1 <- ggplot(df,
       aes(x = V1, y = V2)) +
#  geom_point()    # dziala samo
  geom_point(data = df, size = 5, col = "red") +
  labs(title = "Tytul", x = "some on x", y = "some on y" )


plot2 <-ggplot(df,
       aes(x = V1, y = V2)) +
  #  geom_point()    # dziala samo
  geom_line(data = df, size = 5, col = "red") +
  geom_line(data = df2) +
  labs(title = "Tytul", x = "some on x", y = "some on y" )

plot2 + theme(panel.background = element_rect(fill = "white", color = "black"))   # removing background and add borders of the plot panel

plot2 + theme(plot.background = element_rect(fill = "green", color = "black"))   # adding background color and add borders on the whole plot


plot2 + theme(plot.title = element_text(hjust = 0.5))   # moving title of the plot to the center



d<-data.frame(x=1:5, y1=1:5, y2=2:6)

ggplot(d, aes(x)) + 
  geom_line(aes(y=y1, colour="1")) + 
  geom_line(aes(y=y2, colour="2")) +
  scale_colour_manual(values=c("red", "blue"))


#### nie czaje na razie legendy
plot3 <-ggplot(df,
               aes(x = V1, y = V2)) +
  #  geom_point()    # dziala samo
  geom_line( col = "red", aes(y = df$V2, colour = "bla") ) +
  geom_line( col = "black", aes(y = df2$V2, colour = "abc") ) +
  labs(title = "Tytul", x = "some on x", y = "some on y" )

plot3



few boxplots:

https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
# bardzo dobry tutorial boxplotow

plot1 <- ggplot(data = sumDF,
                aes(x = testSample, y = accuracy) ) +     # okreslasz osie
  geom_boxplot(aes(fill = testSample))      # okreslasz po czym dzielsz boxploty


plot2 <- ggplot(data = sumDF,
                aes(x = percent, y = accuracy) ) +
  geom_boxplot(aes(fill = percent))

plot2

plot3 <- ggplot(data = sumDF,
                aes(x = percent, y = accuracy) ) +
  geom_boxplot(aes(fill = testSample))     #  w tym wypadku kazdy element osi x jest dodatkowo podzielony wg testSample

plot3


pointData <- data.frame(
  xname = pattVec,
  ypos = c(0.99, 0.99, 0.99),
  ptyname = c("20", "40", "60")
)

 plotAccuracy <- ggplot(data = sumDF,
                 aes(x = percent, y = accuracy) ) +
   geom_boxplot(aes(fill = testSample)) +
   geom_point(data  = pointData, mapping = aes(x = xname, y = ypos, shape = factor(ptyname)) )

   # rysuje wtedy pkt pod xname, wysokosci z ypos, a ksztalt pkt z ptyName