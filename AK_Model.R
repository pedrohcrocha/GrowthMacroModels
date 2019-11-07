# AK model

# Cobb-Douglas Hicks-Neutral Production Funcion: F(K,L) = A * K^alpha * L^(1-alpha)
productionFunction <- function(A, K, L,alpha = 1){
  Y = A*(K^alpha)*(L^(1-alpha))
  return(Y)}

# Equation of capital accumulation: dK/dt = s*F(K,L) - d*K
capitalAcumulation <- function(s,d,A,K,L){
  Kdot = s*productionFunction(A,K,L,alpha = 1) - d*K
  return(Kdot)}

# Result of the model: Income growth linearlly in a rate of g = s*A - d
incomeGrowthRate <- function(s,A,d){
  YdotY = s*A - d
  return(YdotY)}

# Data-frame with all of the results of the model
AKmodel <- function(nrows, s, d, A, L){
  ak <- data.frame()
  for(i in 1:nrows){
    Y = productionFunction(A, i, L)
    savingsLine = s*Y
    deprectionLINE = d*i
    Kdot = capitalAcumulation(s, d, A, i, L)
    ydot =  incomeGrowthRate(s, A, d)
    temp <- data.frame(Y, savingsLine, deprectionLINE, Kdot, ydot)
    ak <- rbind(ak, temp)}
  return(ak)
}

# Example
df <- AKmodel(1000, 0.4, 0.4, 10, 1)

# Graphic 1
library(ggplot2)

ggplot(data= df) +
  geom_line(mapping = aes(x = 1:nrow(df), y = Y), col='blue') +
  geom_line(mapping = aes(x = 1:nrow(df), y = savingsLine), col='green') +
  geom_line(mapping = aes(x = 1:nrow(df), y = deprectionLINE), col='red') +
  geom_segment(aes(x=500,xend=500, y=-1, yend=5000), linetype = "dashed", col='grey') +
  geom_segment(aes(x=0,xend=500, y=500*0.8, yend=500*0.8), linetype = "dashed", col='grey') +
  geom_segment(aes(x=0,xend=500, y=5000*0.4, yend=5000*0.4), linetype = "dashed", col='grey') +
  geom_segment(aes(x=0,xend=500, y=5000, yend=5000), linetype = "dashed", col='grey') +
  geom_point(aes(x=500, y= 500*0.8))+
  geom_point(aes(x=500, y= 5000*0.4))+
  geom_point(aes(x=500, y= 5000))+
  scale_x_continuous(limits = c(0,600))+
  xlab('K')+
  annotate("text", x = 600, y=6200, label="Y") + 
  annotate("text", x = 600, y=2600, label="sY") + 
  annotate("text", x = 600, y=800, label="dK") + 
  labs(title="Basic AK Model Equation")+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1, "lines"))

# Graphic 2
library(latex2exp)
ggplot(data = df) +
  geom_line(mapping = aes(x = 1:nrow(df), y = ydot), col='blue') +
  geom_line(mapping = aes(x = 1:nrow(df), y = rep(0.8,nrow(df))), col='red') +
  scale_x_continuous(limits = c(0,600))+
  ylim(c(0,3.5))+
  ylab(TeX("$\\dot{Y}/Y$"))+
  xlab(TeX("$\\K$")) +
  labs(title="Growth rate for AK model")+
  geom_segment(aes(x=300,xend=300, y=0.8, yend=3.2), linetype = "dashed") +
  annotate("text", x = 200, y=3.3, label="sA") + 
  annotate("text", x = 400, y=2, label="Growth rate") + 
  annotate("text", x = 200, y=0.9, label="d") + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1, "lines")
  )

# Effect of a permanent increase in savings
df1 <- AKmodel(1000, 0.6, 0.8, 10, 10)

# Graphic 1
ggplot(data= df) +
  geom_line(mapping = aes(x = 1:nrow(df), y = Y), col='blue') +
  geom_line(mapping = aes(x = 1:nrow(df), y = savingsLine), col='green') +
  geom_line(mapping = aes(x = 1:nrow(df), y = df1$savingsLine), col='orange') +
  geom_line(mapping = aes(x = 1:nrow(df), y = deprectionLINE), col='red') +
  geom_segment(aes(x=500,xend=500, y=-1, yend=5000), linetype = "dashed", , col='grey') +
  geom_segment(aes(x=0,xend=500, y=500*0.8, yend=500*0.8), linetype = "dashed", col='grey') +
  geom_segment(aes(x=0,xend=500, y=5000*0.4, yend=5000*0.4), linetype = "dashed", col='grey') +
  geom_segment(aes(x=0,xend=500, y=5000*0.6, yend=5000*0.6), linetype = "dashed", col='grey') +
  geom_segment(aes(x=0,xend=500, y=5000, yend=5000), linetype = "dashed", col='grey') +
  geom_point(aes(x=500, y= 500*0.8))+
  geom_point(aes(x=500, y= 5000*0.6))+
  geom_point(aes(x=500, y= 5000*0.4))+
  geom_point(aes(x=500, y= 5000))+
  scale_x_continuous(limits = c(0,600))+
  xlab('K')+
  annotate("text", x = 600, y=6300, label="Y") + 
  annotate("text", x = 600, y=4000, label="s'Y") + 
  annotate("text", x = 600, y=2700, label="sY") + 
  annotate("text", x = 600, y=800, label="dK") + 
  labs(title="Basic AK Model equations")+
  theme_classic()

# Graphic 2
library(latex2exp)
ggplot(data = df) +
  geom_line(mapping = aes(x = 1:nrow(df), y = df1$ydot), col='orange') +
  geom_line(mapping = aes(x = 1:nrow(df), y = ydot), col='blue') +
  geom_line(mapping = aes(x = 1:nrow(df), y = rep(0.8,nrow(df))), col='red') +
  scale_x_continuous(limits = c(0,600))+
  ylim(c(0,6))+
  ylab(TeX("$\\dot{Y}/Y$"))+
  xlab(TeX("$\\K$")) +
  labs(title="Growth rate for AK model")+
  geom_segment(aes(x=280,xend=280, y=0.8, yend=5.2), linetype = "dashed") +
  geom_segment(aes(x=300,xend=300, y=0.8, yend=3.2), linetype = "dashed") +
  annotate("text", x = 200, y=5.4, label="s'A") + 
  annotate("text", x = 200, y=3.4, label="sA") + 
  annotate("text", x = 400, y=2, label="Growth Rate") + 
  annotate("text", x = 200, y=1, label="d") + 
  theme_classic()
