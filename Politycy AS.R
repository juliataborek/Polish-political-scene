library(readxl)
library(patchwork)

dane3 <- read_excel("C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Politycy_AS.xlsx")
dane3 <- as.data.frame(dane3)
str(dane3)
#View(dane3)
jednostki = dane3[,1]
jednostki

dane4 <- dane3[,-1]
dane4
row.names(dane4) <- jednostki
dane4
#View(dane4)

library(vegan)
library(jaccard)
library(proxy)
library(tidyverse)

df <- data.frame(dane4)
#liczba obecności
rowSums(df)
##Jaccrad
vegdist(df, method = "jaccard")  # odległość Jaccarda
Jaccard_podob <- 1- vegdist(df, method = "jaccard") 
Jaccard_podob # wsp. podobienstwa

Jaccard_odl <- vegdist(df, method = "jaccard") 
Jaccard_odl <- as.matrix(Jaccard_odl) [1:9,1:9]
Jaccard_odl <- round((Jaccard_odl), digits = 3)
Jaccard_odl
# najbardziej podobni Trzaskowski i Duda (odległość 0), dosyć podobni Kosiniak i Bosak (0,25)
# blisko idealnego tak samo Trzaskowski, Duda (0,1), 3 miejsce Tusk (0,333)


Jaccard_podob <- 1 - Jaccard_odl
Jaccard_podob <- round((1-Jaccard_odl), digits = 3)
Jaccard_podob

##### wizualizacje macierzy odległości i podobieństwa Jaccarda ######
library(factoextra)

Jaccard_odl <- get_dist(df, method = "binary", stand = FALSE) # w tej bibliotece binary to Jaccard
Jaccard_podob = 1- Jaccard_odl
Jaccard_podob
fviz_dist(Jaccard_odl)
fviz_dist(Jaccard_podob, order = TRUE)
fviz_dist(Jaccard_podob, order = TRUE, 
          gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Jaccarda")

library(corrplot)
par(mfrow = c(1,2))
Jaccard_odl <- as.matrix(Jaccard_odl)
Jaccard_podob <- as.matrix(Jaccard_podob)
corrplot(Jaccard_odl, order = 'AOE') #trzeba uciąć poniżej 0
corrplot(Jaccard_podob, order = 'AOE')

##### jaccard ####
library(cluster)
library(dendextend)

par(mfrow = c(1,1))
odl <- dist(dane4, method = "jaccard") # kolejna metoda na jaccarda
dend1 <- hclust(odl, method = "complete")
#View(dend1)
dend1$height
plot(dend1)
plot(dend1, hang=-1) 
wys <- c(0, dend1$height)
a = 0.7 # bardziej restrykcyjny
Mojena <- mean(wys) + a * sd(wys)
Mojena
abline(h=Mojena, col = "red")
grupy <- cutree(dend1, k=3)
grupy
rect.hclust(dend1, k=3, border = 'turquoise')

# 2 opcja za pomocą Mojeny - 2 skupienia
#plot(dend1, hang=-1)
#Mojena2 <- mean(wys) + 1.25 *sd(wys)
#Mojena2
#abline(h=Mojena2, col = "red")

df_jac <- df
df_jac$grupy = grupy #dopisujemy do naszych danych kolumnę z grupą
profile = df_jac %>%
  group_by(grupy) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
profile

profile_2 = t(profile)
#View(profile_2)
colnames(profile_2) = c("skupienie_1", "skupienie_2", "skupienie_3")

profile_2 = profile_2[-1,]
l = nrow(profile_2)  

profile_2 <- as.data.frame(profile_2)  
dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3),
                         grupy = c( rep("Liderzy niszowych ugrupowań", l), rep("Wpływowi", l),rep("Hołownia", l))))

dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3),
                         grupy = c(rep("Liderzy niszowych ugrupowań", l), rep("Wpływowi", l), rep("Hołownia", l)),
                         zmienne = c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", 
                                     "X10")))
df2 = dt2

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Blues") + 
  theme_minimal() + ggtitle("Profile skupień") + labs(fill = "Grupy")


##### wspołczynnik podobieństwa Sokala-Michenera ####
SM_podob <- simil(df, y=NULL, method =NULL, diag = FALSE, upper = FALSE, pairwise = FALSE, 
                  by_rows = TRUE, convert_distances = TRUE, auto_convert_data_frames = TRUE)
SM_podob
SM_podob <- as.matrix(SM_podob) [1:9,1:9]
diag(SM_podob) = 1 # dodajemy jedynkę na przekątnej
SM_podob 
SM_odl <- 1 - SM_podob
SM_odl
corrplot(SM_podob, order = 'AOE')


odl <- as.dist(SM_odl)
SM_podob <- 1 - odl
fviz_dist(SM_podob, order = TRUE, 
          gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Sokala-Michenera")

# wykresy obok siebie
labels_order <- rownames(as.matrix(Jaccard_podob))
Jaccard_odl <- get_dist(df, method = "binary", stand = FALSE) # w tej bibliotece binary to Jaccard
Jaccard_podob = 1- Jaccard_odl
plot1 <- fviz_dist(Jaccard_podob, order= FALSE,
          gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Jaccarda") +
  theme(axis.text.x = element_text(angle = 45))
plot2 <- fviz_dist(SM_podob, order = FALSE,
          gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Sokala-Michenera") +
  theme(axis.text.x = element_text(angle = 45))
combined_plot <- plot1 / plot2 + plot_layout(heights = c(1,1))
print(combined_plot)

dend2 <- hclust(odl, method = "complete")
dend2$height
plot(dend2)
plot(dend2, hang=-1)
grupy <- cutree(dend2, k=3)
grupy
rect.hclust(dend2, k=3, border = 'turquoise')

library(igraph)

fviz_dend(dend2, cex = 1.05, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "rectangle") + ggtitle("Podział na grupy",) +
  theme(plot.title = element_text(size = 40, face = "bold"))
fviz_dend(dend2, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "circular")
fviz_dend(dend2, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "phylogenic", 
          repel = TRUE, phylo_layout = "layout_as_tree")

############# odległość Gowera #####################################
library(cluster)
library(dendextend)

df_Gower <- read_excel("C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Politycy_Gower.xlsx")
df_Gower <- as.data.frame(df_Gower)
jednostki=df_Gower[,1]
df_Gower <- df_Gower[,-1]               
row.names(df_Gower) <- jednostki    #przypisanie nazw wierszy
df_Gower

# etykiety
df_Gower$`polityka_ młodość` <- factor(x = df_Gower$`polityka_ młodość`, levels = c("tak", "nie"))
df_Gower$wykształcenie <- factor(x = df_Gower$wykształcenie, levels = c("średnie", "wyższe"))
df_Gower$kierunek <- factor(x = df_Gower$kierunek, 
                            levels = c("brak", "prawo", "lekarski", "stosunki międzynarodowe", 
                                       "międzynarodowe stosunki gospodarcze i polityczne", "historyczne"))
df_Gower$Twitter <- factor(x = df_Gower$Twitter, levels = c("tak", "nie"))



# odleglosc Gowera  
daisy(df_Gower, metric = c("gower")) #pozwala na wyliczenie współczynnika Gower

Gower_odl <- daisy(df_Gower, metric = c("gower"))
Gower_odl
mean(Gower_odl) #0,38
Gower_odl <- as.matrix(Gower_odl) [1:9, 1:9]
Gower_odl <- round(Gower_odl, digits = 3)
Gower_odl
write_xlsx(as.data.frame(Gower_odl), "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Gower_odl.xlsx")

############ wskaźnik podobienstwa Gowera #########################

Gower_podob <- 1 - Gower_odl
Gower_podob <- round(Gower_podob, digits = 3)
Gower_podob

library(corrplot)
corrplot(Gower_podob, order ='AOE')

odl <- dist(df_Gower, method = c("Gower"))
Gower_podob <- 1 - odl
fviz_dist(Gower_podob, order = TRUE, 
          gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Gowera")

# wykresy obok siebie
plot1 <- fviz_dist(Jaccard_podob, order= FALSE,
                   gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Jaccarda") +
  theme(axis.text.x = element_text(angle = 45))
plot2 <- fviz_dist(Gower_podob, order = FALSE,
                   gradient = list(low = "lightblue1", mid = "paleturquoise3", high = "skyblue3")) +
  ggtitle("Wykres podobieństwa Gowera") +
  theme(axis.text.x = element_text(angle = 45))
combined_plot <- plot1 / plot2 + plot_layout(heights = c(1,1))
print(combined_plot)

############ analzia skupień ########################################

library(proxy)
odl <- dist(df_Gower, method = c("Gower"))
dend1 <- hclust(odl, method = "complete")
dend1$height
plot(dend1)
plot(dend1, hang = -1)
wys <- c(0, dend1$height)
a = 0.7 # bardziej restrykcyjny
Mojena <- mean(wys) + a * sd(wys)
Mojena
abline(h=Mojena, col = "red")
grupy <- cutree(dend1, k=3)
grupy
rect.hclust(dend1, k=3, border = 'turquoise')
