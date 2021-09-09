
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
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/histogramme1.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)


histo_mass <- ggplot(mydata)+
  geom_histogram(aes(x=body_mass_g), fill="darkorchid4", color="darkgray", bins=50)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (50 bins)")+
  ylab("Count")+theme_light()
histo_mass
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/histogramme2.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)




dens_mass <- ggplot(mydata)+
  geom_density(aes(x=body_mass_g), fill="darkorchid4", color="darkgray", alpha=0.8)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (50 bins)")+
  ylab("Count")+theme_light()
dens_mass
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/densité.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)




ggplot(penguins, aes(flipper_length_mm))+
  geom_histogram(aes(y=..density..), alpha=0.9, fill="darkcyan", color="darkgrey" ,
                 position="identity", bins=50)+
  geom_density(alpha=.5, fill="darkorchid")+
  labs(title= "Flipper length density of penguins population", x="Flipper length in mm")+
  theme_light()
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/histo_dens.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)


plot(penguins)




dens_mass <- ggplot(mydata)+
  geom_density(aes(x=flipper_length_mm), fill="darkorchid4", color="darkgray", alpha=0.8)+
  labs(title = "Penguins Body Mass", subtitle = "Histogram (50 bins)")+
  ylab("Count")+theme_light()
dens_mass
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/densité.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)

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
ggsave("~/coursDESIGEO/cours_stats_DESIGEO/Univariee_Bivariee/img/trimodale.png", device= "png",width = 800 , height = 400, units = "px", dpi=100)




