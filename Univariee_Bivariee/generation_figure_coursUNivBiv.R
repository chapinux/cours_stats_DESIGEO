
library(palmerpenguins)
library(ggplot2)
library(dplyr)
data(package = 'palmerpenguins')
mydata <-  penguins
histo_mass <- ggplot(mydata)+
  geom_histogram(aes(x=body_mass_g), fill="darkorchid4", color="darkgray", bins=20)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (20 bins)")+
  ylab("Count")+theme_light()
histo_mass
#ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/histogramme1.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)


histo_mass <- ggplot(mydata)+
  geom_histogram(aes(x=body_mass_g), fill="darkorchid4", color="darkgray", bins=50)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (50 bins)")+
  ylab("Count")+theme_light()
histo_mass
#("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/histogramme2.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)




dens_mass <- ggplot(mydata)+
  geom_density(aes(x=body_mass_g), fill="darkorchid4", color="darkgray", alpha=0.8)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (50 bins)")+
  ylab("Count")+theme_light()
dens_mass
#ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/densité.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)




ggplot(penguins, aes(flipper_length_mm))+
  geom_histogram(aes(y=..density..), alpha=0.9, fill="darkcyan", color="darkgrey" ,
                 position="identity", bins=50)+
  geom_density(alpha=.5, fill="darkorchid")+
  labs(title= "Flipper length density of penguins population", x="Flipper length in mm")+
  theme_light()
#ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/histo_dens.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)


plot(penguins)




dens_mass <- ggplot(mydata)+
  geom_density(aes(x=flipper_length_mm), fill="darkorchid4", color="darkgray", alpha=0.8)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (50 bins)")+
  ylab("Count")+theme_light()
dens_mass
#ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/densité.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)

library(dplyr)
pokemons <-  read.csv("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/data/pokemons.csv")

 pokemons %>% select_if(is.numeric) %>% ggpairs
penguins         %>% select_if(is.numeric) %>% ggpairs
mtcars  %>% select_if(is.numeric) %>% ggpairs
mtcars %>% select(mpg) %>%  ggplot(aes(x=mpg))+geom_density()


x1 <- rnorm(500, mean = 20, 4)
x2 <- rnorm(500, mean = 50, 2) 
x3 <- rnorm(500, mean = 80, 10) 
x <-  c(x1,x2,x3) %>% data.frame()
names(x) <-  "valeur"
ggplot(x, aes(x=valeur))+
  geom_histogram(bins= 40, fill=NA, color="darkcyan")+
  labs(x= "valeur", y="effectif")
#ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/trimodale.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)


boxplt <- ggplot(penguins)+
  geom_boxplot(aes(x=flipper_length_mm, group=species, color=species))+
  labs(title = "Penguins Flipper Length", subtitle = "Boxplot by species")+
  theme_light()
boxplt
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/boxplot.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)


xx <- seq(-5,5, length.out = 100)
normale <-  dnorm(xx,mean = 0, sd=1)
xxx <- seq(0,10, length.out = 100)
droite <-  dgamma(xxx,2,1)

droiteValues <-  rgamma(1000,2,1)
droiteValues <-  rgamma(1000,2,1)

xxxx <-seq(10,0, length.out = 100) 
gauche <- dgamma(xxxx, 2,1)
mydata <- data.frame(value=xx, dens=normale, type="Normale", moy=0, med=0, mod=0)
droitedata <- data.frame(value=xxx, dens=droite, type="Asymétrique positive", moy=mean(droiteValues), med=median(droiteValues), mod = 1)
gauchedata <- data.frame(value=xxx, dens=gauche, type="Asymétrique négative", moy=10-mean(droiteValues), med=10-median(droiteValues), mod=9)

gauchedata <- rbind( gauchedata, mydata)
gauchedata <- rbind(gauchedata, droitedata)

ggplot(gauchedata, aes(x=value, y=dens))+geom_line(color="#44DD99", lwd=1.2)+
  geom_vline(aes(xintercept = moy), col="red")+
  geom_vline(aes(xintercept = med), col="blue")+
  geom_vline(aes(xintercept = mod), col="orange")+
  facet_grid(cols=vars(type), scales="free")+
  ylab(label = "density")+
  theme_light()+
  annotate("text", x=3.5, y=0.4, colour=c("red"),label="moyennne")+
  annotate("text", x=3.5, y=0.37, colour=c("blue"),label="médiane")+
  annotate("text", x=3.5, y=0.34, colour=c("orange"),label="mode")

ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/asymetrie.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)



normaleC <- 1 
picC <- 0.6
plateC <- 3


xx <- seq(-5,5, length.out = 100)
normale <-  dnorm(xx,mean = 0, sd=normaleC)
pic <-  dnorm(xx, mean = , sd=picC)
plate <- dnorm(xx, mean=, sd=plateC)

mydata <- data.frame(value=xx, dens=normale, type="Normale")
piquee <- data.frame(value=xx, dens=pic, type="Leptokurtique")
applatie <-  data.frame(value=xx, dens=plate, type="Platokurtique")


mydata <- rbind(mydata, piquee)
mydata <- rbind(mydata, applatie)

ggplot(mydata, aes(x=value, y=dens))+geom_line(color="#44DD99", lwd=1.2)+
  facet_grid(cols=vars(type), scales="free")+
  ylab(label = "density")+
  theme_light()
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/aplatissement.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)





cor(x,y5)

ggplot(data2,aes(x=x, y=y4))+
  geom_point(size =2, color = "#0FAF96", alpha=0.8)+
  xlab("x")+
  ylab("y")+
  labs(title="r=-0.002466139")+
  theme_light()

ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/sinus.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)




ggplot(data2,aes(x=x, y=y5))+
  geom_point(size =2, color = "#0FAF96", alpha=0.8)+
  xlab("x")+
  ylab("y")+
  labs(title="r=0.0380177")+
  theme_light()

ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/flou.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)



cor(x,rnorm(100)*2+(x-5)^2) 

ggplot(data2,aes(x=x, y=2*rnorm(100)+(x-5)^2))+
  geom_point(size =2, color = "#0FAF96", alpha=0.8)+
  xlab("x")+
  ylab("y")+
  labs(title="r=-0.0846620")+
  theme_light()

ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/quadratic.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)



X <-  c(3,2,3,4,1,2,3,4,5,2,3,4,3)
Y <-  c(1,2,2,2,3,3,3,3,3,4,4,4,5)
ggplot(data.frame(X, Y), aes(x=X, y=Y))+
  geom_point(size =2, color = "#0FAF96")+
  scale_x_continuous(limits=c(0,16))+
  scale_y_continuous(limits=c(0,16))+
  theme_light()

ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/outlier1.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)




X <-  c(3,2,3,4,1,2,3,4,5,2,3,4,3,15)
Y <-  c(1,2,2,2,3,3,3,3,3,4,4,4,5,15)
ggplot(data.frame(X, Y), aes(x=X, y=Y))+
  geom_point(size =2, color = "#0FAF96")+
  scale_x_continuous(limits=c(0,16))+
  scale_y_continuous(limits=c(0,16))+
  theme_light()

ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/outlier2.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)



regression <- lm(penguins$flipper_length_mm ~penguins$bill_length_mm * runif(nrow(penguins)))
pvals <- coef(summary(regression))[,4]

titre <- paste0("R2=", summary(regression)$adj.r.squared %>% round(3))
soustitre <-  paste0("p-values associées à a : ",pvals[2] %>% signif(3), ", à b: ", pvals[1] %>% signif(3) )
ggplot(penguins, aes(x=flipper_length_mm, y=bill_length_mm))+
  geom_point(aes(color=species))+
  geom_smooth(se=FALSE, method="lm", color="black")+
  theme_light()+
  labs(title=titre, subtitle = soustitre)
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/reglin_example1.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)

nbpts <- 1000
xx <-  runif(nbpts)
yy <- 2*xx + 4 + runif(nbpts,-5,5) 
df <-  data.frame(xx,yy)
regression <- lm(df$yy ~ df$xx)
pvals <- coef(summary(regression))[,4]
str(regression)

titre <- paste0("R2=", summary(regression)$adj.r.squared %>% round(3))
soustitre <-  paste0("p-values associées à a : ",pvals[2] %>% signif(3), ", à b: ", pvals[1] %>% signif(3) )
ggplot(df, aes(x=xx, y=yy))+
  geom_point()+
  geom_smooth(se=FALSE, method="lm")+
  theme_light()+
  labs(title=titre, subtitle = soustitre, x="Variable 1 ", y="Variable 2")


ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/reglin_example2.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)





x <- runif(100, min = 0, max=10)
y1 <- 8.5*x - 23 + rnorm(100,0, 12)
y2 <- -16*x +12 + rnorm(100,0,10)
y3 <- sqrt(exp(x)) + rnorm(100,0,8)
y4 <-  50*sin(x) + rnorm(100,0,10)
y5 <-  rnorm(100, 0, 60)
y6 <- 21.345
data2 <-  data.frame(x, y1, y2, y3, y4, y5, y6)
library(reshape2)
library(ggplot2)
data2 <- melt(data2, idvars=c("x"), measure.vars=c("y1","y2","y3","y4", "y5", "y6"))
plo2 <-  ggplot(data2, aes(x=x, y=value))+
  geom_point(size =1, color = "#0FAF96", alpha=0.8)+
  facet_wrap(~variable)+
  xlab("value of x variable")+
  ylab("value of yi variable")
plo2


ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/formes_dependances.png", device= "png",width = 900 , height = 400, units = "px", dpi=100)



library(palmerpenguins)
res <-  lm(penguins$flipper_length_mm ~ penguins$body_mass_g)
summary(res)




library(RColorBrewer)
mypalette <-  brewer.pal(3,"Dark2")
dev.off()
mosaicplot(~Class+Survived, data=Titanic, color=mypalette,main = "")


Titanic~Class+Survived


tt <- chisq.test(penguins$species, penguins$island)
tt$statistic
tt$p.value

library(corrplot)
corrplot(tt$residuals, is.cor = FALSE)


