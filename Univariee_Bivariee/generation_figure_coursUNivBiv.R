
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




