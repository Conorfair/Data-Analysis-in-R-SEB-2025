P_1 <- rbinom(100, 1, 0.4)
P_2 <- rpois(100, 15)
P_10 <- rpois(100, 30)

P_1.1 <- rbinom(25, 1, 0.1)
P_1.2 <- rbinom(25, 1, 0.2)
P_1.8 <- rbinom(25, 1, 0.8)
P_1.9 <- rbinom(25, 1, 0.9)

P_1 <- c(P_1.1, P_1.2, P_1.8, P_1.9)

summary(P_1)

X_1 <- seq ( 0, 0.99, by=0.01)

logit <- as.data.frame(cbind(P_1, X_1))

plot(logit$X_1, logit$P_1, col="red", xlim=c(0, 1), ylim=c(0, 1), main="", xlab="", ylab="")

UGA_Icon <- system.file("/Users/conorfair/Library/CloudStorage/OneDrive-UniversityofGeorgia/Documents/GitHub/Conorfair.github.io/assets/img", "icon.png", package="png")

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("icon.png")
p <- get_png("ESA Pic.png")
r <- get_png("R_logo.svg.png")


t <- grid::roundrectGrob()

library(ggplot2)
library(ggimage)
myplot<- ggplot(logit, aes(x=X_1, y=P_1)) + 
  labs(x="",y="")+
  geom_point(col="red") + 
  geom_smooth(method="glm", method.args=list(family="binomial"), se=T, col="blue") +
  theme_classic()+
  theme(axis.text.x=element_blank(), 
                        axis.ticks.x=element_blank(), 
                        axis.text.y=element_blank(), 
                        axis.ticks.y=element_blank()) +
  annotation_custom(l, xmin = 0.0, xmax = 0.2, ymin = 0.3, ymax = 0.8)+
  annotation_custom(p, xmin = 0.0, xmax = 0.6, ymin = 0.7, ymax = 0.99)+
  annotation_custom(r, xmin = 0.6, xmax = 0.99, ymin = 0.1, ymax = 0.5)
myplot

library(jpeg)
library(grid)
path <- "/Users/conorfair/Library/CloudStorage/OneDrive-UniversityofGeorgia/Documents/GitHub/Conorfair.github.io/assets/img"

img <- readJPEG(path, native = TRUE)

myplot2 <- ggdraw()+
  draw_image(UGA_Icon, x=0.5, y=0.5, hjust=0.5, vjust=0.5, width=0.1, height=0.1)+
  draw_plot(myplot, x=0.5, y=0.5, width=0.8, height=0.8)
myplot2


P_1 <- rbinom(5, 1, 0.5)
P_2 <- rbinom(30, 30, 0.5)
P_10 <- rbinom(100, 100, 0.5)


plot(density(P_1), col="red", xlim=c(-2, 50), ylim=c(0, 0.4), main="", xlab="", ylab="")
lines(density(P_2), col="blue")
lines(density(P_10), col="green")




legend("topright", legend=c("lambda=1", "lambda=2", "lambda=10"), col=c("red", "blue", "green"), lty=1, cex=0.8)
