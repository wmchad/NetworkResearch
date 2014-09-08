require(ggplot2);
setwd("c:/Code/NetworkResearch/R/Functions/Data/");
source("TowerFunctions.r");
setwd("c:/Code/NetworkResearch/R/Functions/Plotting/");
source("TowerVolumePlots.r");
setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/");
source("PlotHelper.r");

bombingCalls <- bombing1Calls[bombing1Calls$Day==6,];
inaugCalls <- inaug2Calls[inaug1Calls$Day==23,];
inaugCalls2 <- inaug1Calls[inaug1Calls$Day==15,];

bombingSummaryPlot <- ggplot(bombingCalls, aes(x=DayPeriod)) +
  geom_line(aes(y=InAvgCalls), color="blue", lty=2) +
  geom_line(aes(y=OutAvgCalls), color="red", lty=2) +
  geom_line(aes(y=InCalls), color="blue") +
  geom_line(aes(y=OutCalls), color="red") +
  labs(x="Time", y="Call Volume") +
  xTimeAxis(288, 1) + ylim(0, 300) +
  theme(text=element_text(size=28))

inaugSummaryPlot <- ggplot(inaugCalls, aes(x=DayPeriod)) +
  geom_line(aes(y=InAvgCalls), color="blue", lty=2) +
  geom_line(aes(y=OutAvgCalls), color="red", lty=2) +
  geom_line(aes(y=InCalls), color="blue") +
  geom_line(aes(y=OutCalls), color="red") +
  labs(x="Time", y="") +
  xTimeAxis(288, 1) + ylim(0, 300) +
  theme(text=element_text(size=28), axis.text.y=element_blank())

inaugSummaryPlot2 <- ggplot(inaugCalls2, aes(x=DayPeriod)) +
  geom_line(aes(y=InAvgCalls), color="blue", lty=2) +
  geom_line(aes(y=OutAvgCalls), color="red", lty=2) +
  geom_line(aes(y=InCalls), color="blue") +
  geom_line(aes(y=OutCalls), color="red") +
  labs(x="Time", y="") +
  xTimeAxis(288, 1) + ylim(0, 300) +
  theme(text=element_text(size=28), axis.text.y=element_blank())


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.3s.long.rdata");
load("results.events.day.5s.long.rdata");
load("results.events.day.10s.long.rdata");

bestSample.3s <- samplerResults.events.day.long.3s$bestSample;
bestSample.5s <- samplerResults.events.day.long.5s$bestSample;
bestSample.10s <- samplerResults.events.day.long.10s$bestSample;

z3 <- ReorderZs(bestSample.3s$z);
z5 <- ReorderZs(bestSample.5s$z);
z10 <- ReorderZs(bestSample.10s$z);

names3 <- rep("normal", length(z3));
names3[z3==3] <- "emergency";
names3[z3==1] <- "non-emergency";

names5 <- rep("normal", length(z5));
names5[z5==5] <- "emergency";
names5[z5==1] <- "non-emergency";

names10 <- rep("normal", length(z10));
names10[z10==8] <- "emergency";
names10[z10==7] <- "emergency";
names10[z10==6] <- "non-emergency";

df.events.day <- data.frame(time=c(1:288, (1:288) + 320, (1:288) + 640, (1:288) + 960),
                            event=c(rep("inaug1", 288), rep("bombing1", 288),
                              rep("inaug2", 288), rep("bombing2", 288)),
                            volume=.25 * events.y$day / max(events.y$day),
                            z3=z3,
                            names3=names3,
                            z5=z5,
                            names5=names5,
                            z10=z10,
                            names10=names10,
                            y.3s=-.13,
                            y.5s=-.215,
                            y.10s=-.3);

stateLwd <- 17;

eventStarts <- c(90, 320+11*12+9, 640+14*12, 960+11*12+1);
eventEnds <- c(144, 480, 640+19*12, 960+12*12+6);

multiEventSamplerCompPlot <- ggplot() +
  annotate("rect", xmin=eventStarts, xmax=eventEnds, ymin=-Inf, ymax=Inf,
           fill=rep(c("#FDDC00", "red"), 2), alpha=0.2) +
  annotate("rect", xmin=-150, xmax=-20, ymin=-Inf, ymax=Inf,
           fill="white") +
  geom_line(data=df.events.day, aes(x=time, y=volume, group=event), color="blue") +
  geom_line(data=df.events.day, aes(x=time, y=y.3s, color=names3, group=event), lwd=stateLwd) +
  geom_line(data=df.events.day, aes(x=time, y=y.5s, color=names5, group=event), lwd=stateLwd) +
  geom_line(data=df.events.day, aes(x=time, y=y.10s, color=names10, group=event), lwd=stateLwd) +
  labs(x="Time", y="Volume") +
  scale_color_manual(values=c("red", "#FDDC00", "#33FF33"),
                     guide=FALSE) +
  theme(panel.background=element_rect(fill="#CCCCCC")) +
  annotate("text", -90, c(-.13, -.215, -.3),
           label=c("3 states", "5 states", "10 states"), size=7) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) +
  scale_x_continuous(breaks=c(300, 620, 940), minor_breaks=NULL, limits=c(-125, 1250)) +
  scale_y_continuous(breaks=NULL)



setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

## blue is incoming, red is outgoing
ggsave("BombingSummaryPlot.png", height=8, width=10, plot=bombingSummaryPlot);
ggsave("InaugurationSummaryPlot.png", height=8, width=10, plot=inaugSummaryPlot);
ggsave("InaugurationSummaryPlot2.png", height=8, width=10, plot=inaugSummaryPlot2);
ggsave("MultiEventSamplerComparisonPlot.png", height=5, width=10, plot=multiEventSamplerCompPlot);



dat <- data.frame(my_x_series=1:192, my_y_series=5.0*runif(192))
rect_left <- c(4,52,100,148)
rectangles <- data.frame(
  xmin = rect_left,
  xmax = rect_left + 24,
  ymin = 0,
  ymax = 5
)

ggplot() +
  geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill='gray80', alpha=0.8) +
  geom_point(data=dat, aes(x=my_x_series, y=my_y_series))
