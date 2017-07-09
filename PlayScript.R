# Set working directory, load libraries and functions
setwd("~/Desktop/ForMorgan")
# change to the appropriate file path on your computer

# you may need to install some of these packages
library(ggplot2)
library(gridExtra)
library(png)
source("SprayChartReader.R")
source("som.field.R")
source("fielder_dist.R")


# Have to collect information from a picture to construct the baselines
# you can just run all of this code, don't worry about what it's doing
# the end result is just find the coordinates for our baselines 
cabreram09 = readPNG("CabreraMiguel09.png")

cabreram.r = cabreram09[,,1]
cabreram.g = cabreram09[,,2]
cabreram.b = cabreram09[,,3]

cabrera.rmelt = melt(cabreram.r, varnames = c("Y", "X"))
cabrera.gmelt = melt(cabreram.g, varnames = c("Y", "X"))
cabrera.bmelt = melt(cabreram.b, varnames = c("Y", "X"))

cabrera.melt = data.frame("X" = cabrera.rmelt$X, "Y" = cabrera.rmelt$Y, 
                          "R" = cabrera.rmelt$value, "G" = cabrera.gmelt$value, 
                          "B" = cabrera.bmelt$value)


# collect baseline coordinates
# color combo in row 1 is baseline and writing 
x.base = data.frame("X" = c())
y.base = data.frame("Y" = c())

for(i in 1:length(cabrera.melt$X)){
  if(cabrera.melt$R[i] == cabrera.melt$R[1] && 
     cabrera.melt$G[i] == cabrera.melt$G[1] &&
     cabrera.melt$B[i] == cabrera.melt$B[1]){
    x.base = rbind(x.base, cabrera.melt$X[i])
    y.base = rbind(y.base, cabrera.melt$Y[i])
  }
}

baselines = data.frame("X" = x.base$X1L, "Y" = 600 - y.base$X1L)
baselines = subset(baselines, baselines$Y <= 400)
baselines = subset(baselines, baselines$Y >= 25)
baselines = subset(baselines, baselines$X >= 10)
baselines = subset(baselines, baselines$X <= 595)

homeplate.y = min(baselines$Y)
homeplate.x = baselines$X[baselines$Y == homeplate.y][2]

leftpole.y = max(baselines$Y)
leftpole.x = baselines$X[baselines$Y == leftpole.y][1]

rightpole.y = max(baselines$Y)
rightpole.x = baselines$X[baselines$Y == rightpole.y][2]


second.x = homeplate.x
second.y = baselines$Y[baselines$X == second.x][2]

first.y = second.y - (second.y - homeplate.y)/2
first.x = baselines$X[baselines$Y == first.y][8]

third.y = first.y
third.x = baselines$X[baselines$Y == third.y][1]
# Now we should have the coordinates for home, first, second, and third
# as well as the location of the left and right field foul poles


# Read a couple spray charts from BrooksBaseball.net in the existing folder.
read_spraychart("CabreraMiguel09.png")
read_spraychart("CabreraMiguel16.png")
read_spraychart("OrtizDavid16.png")
read_spraychart("BautistaJose15.png")
# using the function above will read the spraychart create .csv dataset in the folder

# Now load the datasets
CabreraMiguel09 = read.csv("CabreraMiguel09.csv")
CabreraMiguel16 = read.csv("CabreraMiguel16.csv")
OrtizDavid16 = read.csv("OrtizDavid16.csv")
BautistaJose15 = read.csv("BautistaJose15.csv")
# now the hit location datasets are loaded in R

# Set the initial fielder locations and integer coordinates and take a look
fielders = data.frame("label" = c(1, 2, 3, 4, 6, 5, 7, 8, 9), 
                      "x" = c(300, 300, 360, 345, 255, 240, 160, 300, 440), 
                      "y" = c(132, 65, 155, 195, 195, 155, 300, 375, 300),
                      "int_x" = c(5, 5, 8, 6, 4, 2, 1, 5, 9),
                      "int_y" = c(3, 1, 5, 7, 7, 5, 9, 10, 9))

ggplot(fielders) + geom_point(aes(x = x, y = y)) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Original Fielder Locations") +  theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank(),
                                          axis.ticks.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) 

ggplot(fielders) + geom_point(aes(x = int_x, y = int_y)) + 
  ggtitle("Integer Coordinates") +  theme(axis.title.x = element_blank(),
                                                 axis.text.x = element_blank(),
                                                 axis.ticks.x = element_blank(),
                                                 axis.title.y = element_blank(),
                                                 axis.text.y = element_blank(),
                                                 axis.ticks.y = element_blank()) + 
  coord_fixed() + ylim(c(0, 11)) + xlim(c(0, 10)) + 
  geom_label(aes(x = int_x, y = int_y, label = paste(int_x, int_y, sep = ",")), nudge_y = 0.5)



########################################
##### Miguel Cabrera 2009 and 2016 #####
########################################

# Take a look at the Miguel Cabrera 2009 season

ggplot(CabreraMiguel09) + geom_point(aes(x = x, y = y, colour = factor(hit_type))) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Miguel Cabrera 2009") +  theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank(),
                                          axis.ticks.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) 

# Now let's try k-means k = 7 and k = 9

CabreraMiguel09_kmeans9 = as.data.frame(kmeans(CabreraMiguel09[,2:3], centers = 9)$centers)

CabreraMiguel09_kmeans9chart = ggplot(CabreraMiguel09) + 
  geom_point(aes(x = x, y = y, colour = factor(hit_type)), alpha = 0.5) + 
  geom_point(data = CabreraMiguel09_kmeans9, aes(x = x, y = y), size = 2) +  
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Miguel Cabrera 2009") + labs(subtitle = "with k-means clustering (k = 9)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600))

CabreraMiguel09_kmeans7 = as.data.frame(kmeans(CabreraMiguel09[,2:3], centers = 7)$centers)

CabreraMiguel09_kmeans7chart = ggplot(CabreraMiguel09) + 
  geom_point(aes(x = x, y = y, colour = factor(hit_type)), alpha = 0.5) + 
  geom_point(data = CabreraMiguel09_kmeans7, aes(x = x, y = y), size = 2) +
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Miguel Cabrera 2009") + labs(subtitle = "with k-means clustering (k = 7)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600))

grid.arrange(CabreraMiguel09_kmeans9chart, CabreraMiguel09_kmeans7chart, nrow = 1)
# pretty evident that k-means doesn't work, let's use our SOM algorithm



# som.field() will use the algorithm we've built
CabreraMiguel09_shift = som.field(CabreraMiguel09$x, CabreraMiguel09$y, fielders, nsim = 20000)
# takes a little bit, probably around a minute for nsim = 20000
# there are warnings at the end, this is a new development and I have to look into this

ggplot(CabreraMiguel09) + geom_point(aes(x = x, y = y, colour = factor(hit_type)), alpha = 0.3) + 
  geom_point(data = CabreraMiguel09_shift, aes(x = x, y = y), size = 2) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Miguel Cabrera 2009") +  theme(axis.title.x = element_blank(),
                                       axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank()) + 
  labs(subtitle = "with proposed SOM algorithm") + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) +
  geom_point(data = fielders, aes(x = x, y = y), colour = "blue", size = 2) 
  


# Try Miguel Cabrera's most current 2016 chart

ggplot(CabreraMiguel16) + geom_point(aes(x = x, y = y, colour = factor(hit_type))) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Miguel Cabrera 2016") +  theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank(),
                                          axis.ticks.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) 

CabreraMiguel16_shift = som.field(CabreraMiguel16$x, CabreraMiguel16$y, fielders, nsim = 20000)

ggplot(CabreraMiguel16) + geom_point(aes(x = x, y = y, colour = factor(hit_type)), alpha = 0.3) + 
  geom_point(data = CabreraMiguel16_shift, aes(x = x, y = y)) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Miguel Cabrera 2016") +  theme(axis.title.x = element_blank(),
                                       axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) +
  geom_point(data = fielders, aes(x = x, y = y), colour = "blue")  


# Check the distance metric to be sure that we're seeing improvement over the original locations
fielder_dist(CabreraMiguel16$x, CabreraMiguel16$y, fielders = fielders)
fielder_dist(CabreraMiguel16$x, CabreraMiguel16$y, fielders = CabreraMiguel16_shift)



############################
##### David Ortiz 2016 #####
############################

# Let's do all the same things with David Ortiz

ggplot(OrtizDavid16) + geom_point(aes(x = x, y = y, colour = factor(hit_type))) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("David Ortiz 2016") +  theme(axis.title.x = element_blank(),
                                       axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600))


OrtizDavid16_shift = som.field(OrtizDavid16$x, OrtizDavid16$y, fielders, nsim = 20000)



ggplot(OrtizDavid16) + geom_point(aes(x = x, y = y, colour = factor(hit_type)), alpha = 0.3) + 
  geom_point(data = OrtizDavid16_shift, aes(x = x, y = y)) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("David Ortiz 2016") +  theme(axis.title.x = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.ticks.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) +
  geom_point(data = fielders, aes(x = x, y = y), colour = "blue")  

fielder_dist(OrtizDavid16$x, OrtizDavid16$y, fielders = fielders)
fielder_dist(OrtizDavid16$x, OrtizDavid16$y, fielders = OrtizDavid16_shift)

#########################
##### Jose Bautista #####
#########################

ggplot(BautistaJose15) + geom_point(aes(x = x, y = y, colour = factor(hit_type))) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Jose Bautista 2015") +  theme(axis.title.x = element_blank(),
                                       axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600))

BautistaJose15_shift = som.field(BautistaJose15$x, BautistaJose15$y, fielders, nsim = 20000)

ggplot(BautistaJose15) + geom_point(aes(x = x, y = y, colour = factor(hit_type)), alpha = 0.3) + 
  geom_point(data = BautistaJose15_shift, aes(x = x, y = y)) + 
  scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
  geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
  geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
  geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
  geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
  ggtitle("Jose Bautista 2015") +  theme(axis.title.x = element_blank(),
                                       axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank()) + 
  coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600)) +
  geom_point(data = fielders, aes(x = x, y = y), colour = "blue")  


fielder_dist(BautistaJose15$x, BautistaJose15$y, fielders = fielders)
fielder_dist(BautistaJose15$x, BautistaJose15$y, fielders = BautistaJose15_shift)

