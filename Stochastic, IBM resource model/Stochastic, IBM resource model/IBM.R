###########################################
##
##             C++ Ex 10.6
##
##          Luis F.V.V. Boullosa
##        luis.boullosa@evobio.eu
##
##              24/Feb/2017
##
###########################################
rm(list = ls())
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)


input <- read.table("D:\\Documents\\Visual Studio 2017\\Projects\\Stochastic, IBM resource model\\Stochastic, IBM resource model\\simulation.txt", header = T, sep = "")

input$deR <- rep(0, nrow(input))
for(i in 2:nrow(input)) {
  input$deR[i] <- input$eR[i] - input$eR[i - 1] 
}
  

p1 <- input %>% ggplot(aes(x = t, y = N)) + 
  theme_classic() +
  coord_cartesian(ylim = c(0,max(input$N))) +
  labs(y = c("Population size")) +
  geom_point()

p2 <- input %>% ggplot(aes(x = t, y = iR)) + 
  theme_classic() +
  coord_cartesian(ylim = c(0,max(input$iR))) +
  labs(y = c("Total internal resources")) +
  geom_point()

p3 <- input %>% ggplot(aes(x = t, y = iR/N)) + 
  theme_classic() +
  coord_cartesian(ylim = c(0,max(input$iR/input$N))) +
  labs(y = c("Average Internal resources")) +
  geom_point()

p4 <- input %>% ggplot(aes(x = t, y = eR)) + 
  theme_classic() +
  coord_cartesian(ylim = c(0,max(input$eR))) +
  labs(y = c("External resources")) +
  geom_point()

consumption_function <- function(R){c * R / (R + c) - 1}
expected_consumption <- sapply(input$eR,consumption_function)
p5 <- input %>% ggplot(aes(x = t, y = deR/N)) + 
  theme_classic() +
  coord_cartesian(ylim = c(20,-20)) +
  labs(y = "Change in external resources per cell") +
  geom_point() +
  geom_abline(slope = 0, intercept = -c, linetype = "dashed", aes(colour = "gray35")) +
  geom_line(aes(x = t, y = -expected_consumption, linetype = "dashed", colour = "gray")) +
  annotate("text",y = -c + 1, x = median(input$t), label = "Max consumption rate" )

p6 <- input %>% ggplot(aes(x = t, y = c - abs(deR/N))) + 
  theme_classic() +
  coord_cartesian(ylim = c(20,0)) +
  labs(y = "Competition") +
  geom_point() 

p1            # Population dynamics

p2            # Internal resource dynamics
p3            # Cellular resource dynamics
p4
p5
p6

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g <- rbind(g1, g2, g3, g4, size="first") 


grid.newpage()
grid.draw(g)

######### Risks
b <- 0.2
d <- 0.05
c <- 20

# dying risk
x <- 0:1000
plot(x, yd, xlab = "Internal resources", ylab = "Dying probability")
d*(2+x)/(x+1)
yd <- 1-(1-d)*(x)/(x+1)

# dividing risk
plot(x, yb, xlab = "Internal resources", ylab = "Dividing probability", ylim = c(0,1))
points(x, yd)
yb <- b*(x)/(x+1)

plot(x, yb + yd, xlab = "Internal resources", ylab = "P of something", ylim = c(0,1))
yb + yd

# Consumption rates
yc <-  rpois(1, c) * x / (x + c) - 1
plot(x, yc, xlab = "External resources", ylab = "Consumption", ylim = c(0,max(2 * yc)), pch = 16, col = alpha("black", 0.2))
for(i in 1:100) {
  points(x, rpois(1, c) * x / (x + c) - 1, col = alpha("black", 0.04) )
}

yc <-  c * x / (x + c + rpois(1, c*10)) - 1
plot(x, yc, xlab = "External resources", ylab = "Consumption", ylim = c(0,max(2 * yc)), pch = 16, col = alpha("black", 0.2))
for(i in 1:100) {
  points(x,  + rpois(1, c) * x / (x + c) - 1, col = alpha("black", 0.01) )
}



rep(2, 10)
cbind(x, yc)
all(x > yc)

