{library(ggplot2) #For plot
library(ggExtra)} #For plot

#Read archive
regression <- read.csv2("regression_sample.csv", h=T, dec=",")

#Shapiro-Wilk test (normality test). If p-value > 0.05 then the distribution is normal
shapiro.test(regression$Density)
shapiro.test(regression$Richness)

#Kolmogorov-Smirnov test (normality test)
ks.test(regression$Richness, regression$Density)

#Pearson's correlation (Normal/Linear distribution)
cor.test(regression$Richness, regression$Density)

#Spearman's correlation (Non-Normal distribution)
cor.test(regression$Richness, regression$Density, method="spearman")

#Kendall's rank correlation tau
cor.test(regression$Richness, regression$Density,method="kendall")

#Linear Regression
linear <- lm(Richness ~ Density, data=regression)
summary(linear)
plot(linear)

#The parameters a, b and c of the asymptotic exponential model are estimated according to the regression graph. a ~ the
#value of the y-axis at the top of the curve, intercept (i) is the value of the x-axis where the curve begins to stabilize,
#b = a - i, x and y is where the curve is rising most steeply and c = -(log((a - y)/b))/x.
#Non-Linear Regression Model (relation "asymptotic exponential" with three parameters)
{plot(Richness.Total ~ Density.Total, data=regression, xlab="Collection Density", ylab="Species Richness",las=1,cex=0.5)
  exponencial <- nls(Richness.Total ~ a-b*exp(-c*Density.Total), data=regression, start=list(a=90, b=40, c=0.02095362))
  sum_expo = summary(exponencial)
  xaxis <- seq(0, max(regression$Density.Total)+10, 0.1)
  yaxis <- predict(exponencial, list(Density.Total=xaxis), type="response")
  lines(xaxis, yaxis, col="black", lty=1, lwd=1)}

#Non-Linear Regression Model (relation "asymptotic exponential" with two parameters)
{plot(Richness.Total ~ Density.Total, data=regression, xlab="Collection Density", ylab="Species Richness",las=1,cex=0.5)
  exponencial2 <- nls(Richness.Total ~ a*(1-exp(-c*Density.Total)), data=regression, start=list(a=90, c=0.02095362))
  sum_expo2 = summary(exponencial2)
  xaxis <- seq(0, max(regression$Density.Total)+10, 0.1)
  yaxis <- predict(exponencial2, list(Density.Total=xaxis), type="response")
  lines(xaxis, yaxis, col="black", lty=1, lwd=1)}

#Analysis of Variance (ANOVA). If p > 0.05 then model 2 is the minimal adequate model.
anova(exponencial, exponencial2)

#Percentage of the response variable explained by the model (R Squared)
#100*(Sum Sq-degrees of freedom*residual standard error*residual standard error)/Sum Sq
{null.model <- lm(regression$Richness.Total ~ 1)
sum_null = summary.aov(null.model)
100*(sum_null[[1]][[2]]-sum_expo[[4]][[2]]*sum_expo[[3]]*sum_expo[[3]])/sum_null[[1]][[2]]}

#Plot regression
{expot <- nls(Richness.Total ~ SSasymp(Density.Total, Asym, R0, lrc), data=regression)
theme_set(theme_bw(base_size = 17))
gg <- ggplot(data=regression, aes(x=Density.Total, y=Richness.Total)) +
  geom_point(color = "red") +
  geom_smooth(method="nls", formula=y~SSasymp(x, Asym, R0, lrc), color="grey28", se=F, fullrange=T) +
  scale_x_continuous(limits=c(0, NA)) +
  xlab("Collection Density") + ylab("Species Richness")
gglab <- gg + theme(axis.title.x=element_text(color="black", size=14, face="bold"), 
                    axis.title.y=element_text(color="black", size=14, face="bold"))
ggbg <- gglab + theme(plot.background=element_rect(fill="transparent", colour=NA))
ggm <- ggMarginal(ggbg, type="histogram", fill="white")
plot(ggm)}

#Export regression .tif
tiff("Regression.tiff", units="px", width=3840, height=2160, res=300)
ggm
dev.off()
