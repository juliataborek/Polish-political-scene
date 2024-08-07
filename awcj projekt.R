library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(reshape2) 
library(corrplot)
library(dplyr)
library(car)
library("VIM")
library(rcompanion)
library(ggthemes)

## WCZYTANIE PLIKU ####
dane <- read_excel("C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Politycy BAZA AWCJ 2024.xlsx")
#dane <- read_excel("Politycy BAZA AWCJ 2024.xlsx")
summary(dane)
# można zaobserwować, że skala niektórych odpowiedzi zaczyna się od 0 a według instrukcji powinna od 1
# zamiana 0 na 1
dane[dane == 0 ] <- 1
summary(dane)

# PODSTAWOWE STATYSTYKI ####
### BRAKI DANYCH ####
colSums(is.na(dane))
# nie ma braków danych, każdy udzielił wszystkich odpowiedzi 

### PŁEĆ ####
# zamiana zmiennej płeć z ilościowej na factor, skala dychotomiczna
dane$Płeć <- factor(dane$Płeć, levels = c(1,2), labels = c("Kobieta", "Mężczyzna"))
# podobna liczba kobiet i mężczyzn
round(summary(dane$Płeć) / nrow(dane) * 100, 2)
podsumowanie_płeć <- dane %>%
  group_by(Płeć) %>%
  summarise(liczba = n())
ggplot(podsumowanie_płeć, aes(x="", y=liczba, fill = Płeć)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Procentowy rozkład płci respondentów") +
  scale_fill_manual(values = c("Mężczyzna" = "lightblue2", "Kobieta" = "pink1")) +
  geom_text(aes(label = liczba/nrow(dane) * 100), position = position_stack(vjust = 0.5), color = "white")


### WYKSZTAŁCENIE ####
# zamiana wykształcenia na factor, skala porządkowa
dane$Wykształcenie <- factor(dane$Wykształcenie, labels = c("Podstawowe", "Zawodowe", "Średnie", "Wyższe"))

round(summary(dane$Wykształcenie) / nrow(dane) * 100, 2)
# mało osób z wykształceniem podstawowym i zawodowym, te 2 grupy stanowią niecałe 10% badanych
# ciężko będzie wnioskować coś dla wykształcenia 

ggplot(dane , aes(x=Wykształcenie, fill=Wykształcenie)) + 
  geom_bar() +
  labs(title = 'Rozkład wykształcenia') +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu")

### WIEK ####
# statystyki
summary(dane$Wiek)
ggplot(dane, aes(x=Wiek)) + 
  labs(title='Histogram wieku respondentów' ,x = 'Wiek') +
  geom_histogram(color="white", fill="skyblue2")

ggplot(dane , aes(x=factor(Wiek), fill=factor(Wiek))) +
  geom_bar() +
  theme(legend.position="none")
# najwięcej osób w wieku 23 lat - prawdopodobnie przez to, że każdy miał zbadać siebie 
# i część osób zbadało znajomych w tym samym wieku
sum(dane$Wiek == 23) #37 takich odpowiedzi
sum(dane$Wiek == 23) / nrow(dane) * 100 #ponad 10% respondetów w tym wieku

# piramidu wieku
# Stworzenie dwóch ramki danych dla każdej płci
mezczyzni <- dane[dane$Płeć == "Mężczyzna", ]
kobiety <- dane[dane$Płeć == "Kobieta", ]

# Stworzenie tabeli przestawnej dla mężczyzn
pivot_mezczyzni <- table(cut(mezczyzni$Wiek, breaks = seq(10, 90, by = 5)))

# Stworzenie tabeli przestawnej dla kobiet
pivot_kobiety <- table(cut(kobiety$Wiek, breaks = seq(10, 90, by = 5)))

# Utworzenie nowego ramki danych dla wykresu mężczyzn
df_mezczyzni <- data.frame(Przedzial = names(pivot_mezczyzni), Liczba_osob = as.numeric(pivot_mezczyzni))

# Utworzenie nowego ramki danych dla wykresu kobiet
df_kobiety <- data.frame(Przedzial = names(pivot_kobiety), Liczba_osob = as.numeric(pivot_kobiety))

# Dodanie kolumny z płcią do obu ramion danych
df_kobiety$Płeć <- "Kobieta"
df_mezczyzni$Płeć <- "Mężczyzna"

# Scalanie ramion danych w jedną ramkę danych
df_piramida <- rbind(df_kobiety, df_mezczyzni)

df_piramida %>% mutate(
  Liczba_osob = ifelse(Płeć=="Mężczyzna", Liczba_osob*(-1),
                      Liczba_osob*1))%>%
  ggplot(aes(x = Przedzial, y = Liczba_osob, fill=Płeć)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "Grupa wiekowa", y = "Liczebność grupy", 
       title = "Piramida wieku ankietowanych", fill = "Płeć") +
  scale_fill_brewer(palette="Pastel1") 
  #geom_text(data = df_piramida, aes(x = Przedzial, y = Liczba_osob, label = Liczba_osob), vjust = ifelse(df_piramida$Płeć == "Mężczyzna", 1.5, -0.5))


### GRUPY WIEKOWE ####
# grupy wiekowe: <=30, 31-49, 50-60, >60
dane$Wiek_1 <- recode(dane$Wiek, "14:23 ='<24'; 24:29 = '24-29'; 30:49= '30-49' ; 50:59='50-59' ;else = '60-88'")
table(dane$Wiek_1)
table(factor(dane$Wiek_1, levels = c("<24","24-29","30-49","50-59","60-88")))
# nie da się uzsykać lepiej zbilansowanych grup przy 5 grupach, ponieważ występuje 37 osób w wieku 23lat
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Wiek_1))) +
  geom_bar() +
  labs(title='Rozkład grup wiekowych', x = "Grupa wiekowa",
       y = "Liczebność") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu")
dane$Wiek_1 <- factor(dane$Wiek_1)

# rozkład płci w poszczególnych grupach wiekowych
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Płeć)))+
  geom_bar(position = "fill")  +
  labs(title='Rozkład płci w grupach wiekowych', fill = 'Płeć', 
       x = 'Grupy wiekowe', y ='Procentowy udział') +
  scale_fill_brewer(palette="Pastel1") 

# procentowo
table(dane$Płeć , dane$Wiek_1) / rowSums(table(dane$Płeć, dane$Wiek_1)) * 100
table(dane$Wiek_1 , dane$Płeć) / rowSums(table(dane$Wiek_1 , dane$Płeć) ) * 100 


# wykształcenie
# rozkład płci w poszczególnych grupach wiekowych
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Wykształcenie)))+
  geom_bar(position = "fill")  +
  labs(title='Rozkład wykształcenia w grupach wiekowych', fill = 'Wykształcenie', 
       x = 'Grupy wiekowe', y ='Procentowy udział') +
  scale_fill_brewer(palette="Pastel1") 

# procentowo
table(dane$Wykształcenie , dane$Wiek_1) / rowSums(table(dane$Wykształcenie, dane$Wiek_1)) * 100
  table(dane$Wiek_1 , dane$Wykształcenie) / rowSums(table(dane$Wiek_1 , dane$Wykształcenie) ) * 100 

# wykształcenie
# rozkład wykształcenia według płci
ggplot(dane , aes(x=factor(Płeć), fill=factor(Wykształcenie)))+
  geom_bar(position = "fill")  +
  labs(title='Rozkład wykształcenia według płci', fill = 'Wykształcenie', 
       x = 'Płeć', y ='Procentowy udział') +
  scale_fill_brewer(palette="Pastel1") 
  
# procentowo
table(dane$Wykształcenie , dane$Płeć) / rowSums(table(dane$Wykształcenie, dane$Płeć)) * 100
table(dane$Płeć , dane$Wykształcenie) / rowSums(table(dane$Płeć , dane$Wykształcenie) ) * 100 
  


## NAJLEPSZE I NAJGORSZE CECHY DANYCH POLITYKÓW ####
# BOSAK
colMeans(dane[,1:15])
max(colMeans(dane[,1:15])) #Patriotyczny
min(colMeans(dane[,1:15])) #Tolerancyjny
round(mean(colMeans(dane[,1:15])),3) #49,731

# CZARZASTY 
colMeans(dane[,16:30])
max(colMeans(dane[,16:30])) #Tolerancyjny
min(colMeans(dane[,16:30])) #Atrakcyjny
round(mean(colMeans(dane[,16:30])),3) #44,248

# DUDA
colMeans(dane[,31:45])
max(colMeans(dane[,31:45])) #Patriotyczny
min(colMeans(dane[,31:45])) #Bezinteresowny
round(mean(colMeans(dane[,31:45])),3) #41,294

#HOŁOWNIA - wysokie wyniki
colMeans(dane[,46:60])
max(colMeans(dane[,46:60])) #Medialny
min(colMeans(dane[,46:60])) #Bezinteresowny
round(mean(colMeans(dane[,46:60])),3) #61,136

#KACZYŃSKI
colMeans(dane[,61:75])
max(colMeans(dane[,61:75])) #Wpływowy
min(colMeans(dane[,61:75])) #Atrakcyjny
round(mean(colMeans(dane[,61:75])),3) #30,904

#KOSINIAK - KAMYSZ
colMeans(dane[,76:90])
max(colMeans(dane[,76:90])) #Kulturalny
min(colMeans(dane[,76:90])) #Bezinteresowny
round(mean(colMeans(dane[,76:90])),3) #50,937

#TRZASKOWSKI
colMeans(dane[,91:105])
max(colMeans(dane[,91:105])) #Medialny
min(colMeans(dane[,91:105])) #Bezinteresowny
round(mean(colMeans(dane[,91:105])),3) #60,734

#TUSK
colMeans(dane[,106:120])
max(colMeans(dane[,106:120])) #Wpływowy
min(colMeans(dane[,106:120])) #Bezinteresowny
round(mean(colMeans(dane[,106:120])),3) #57,258

#IDEALNY
colMeans(dane[,121:135])
max(colMeans(dane[,121:135])) #Wiarygodny
min(colMeans(dane[,121:135])) #Atrakcyjny
round(mean(colMeans(dane[,121:135])),3) #86,068

## NAJLEPSZE I NAJGORSZE CECHY DANYCH POLITYKÓW - WYKRES####
naj_cechy <- read_excel("C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/najlepsze_najgorsze_cechy.xlsx")
naj_cechy <- as.data.frame(naj_cechy)
naj_cechy <- naj_cechy[order(naj_cechy$`Średnia ocena`, decreasing = FALSE),]

# Ustawienie kolejności zgodnie z danymi
naj_cechy$Polityk <- factor(naj_cechy$Polityk, levels = unique(naj_cechy$Polityk))

naj_cechy <- naj_cechy %>% 
  mutate(`Średnia ocena` = ifelse(Cecha == "najsłabsza", `Średnia ocena` * (-1), `Średnia ocena`))

# Wykres
ggplot(naj_cechy, aes(x = Polityk, y = `Średnia ocena`, fill = Cecha)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = opis), position = position_stack(vjust = 0.5), size = 3, col = 'white') +  # Dodanie etykiet na słupkach
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-100, 100)) +  # Ustawienie symetrycznych limitów
  #geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dodanie poziomej linii w miejscu zera
  labs(x = "Polityk", y = "Średnia ocena", 
       title = "Najsłabsze i najlepsze cechy polityków", fill = "Cecha", font="Times New Roman") +
  scale_fill_brewer(palette = "Pastel2") 


# Wczytanie danych
naj_cechy <- read_excel("C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/najlepsze_najgorsze_cechy.xlsx")
naj_cechy <- as.data.frame(naj_cechy)

# Uporządkowanie danych malejąco według najsilniejszej cechy
naj_cechy <- naj_cechy %>% 
  mutate(`Średnia ocena` = ifelse(Cecha == "najsłabsza", `Średnia ocena` * (-1), `Średnia ocena`))

# Znalezienie najsilniejszych cech dla każdego polityka
naj_silniejsze_cechy <- naj_cechy %>% 
  group_by(Polityk) %>% 
  summarise(Naj_silniejsza_cecha = max(`Średnia ocena`)) %>% 
  arrange(Naj_silniejsza_cecha) # Sortowanie rosnąco (najmocniejsze cechy na dole wykresu)

# Ustawienie kolejności polityków zgodnie z najsilniejszymi cechami
naj_cechy$Polityk <- factor(naj_cechy$Polityk, levels = naj_silniejsze_cechy$Polityk)

# Wykres
ggplot(naj_cechy, aes(x = Polityk, y = `Średnia ocena`, fill = Cecha)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = opis), position = position_stack(vjust = 0.5), size = 3, col = 'white') +  # Dodanie etykiet na słupkach
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-100, 100)) +  # Ustawienie symetrycznych limitów
  labs(x = "Polityk", y = "Średnia ocena", 
       title = "Najsłabsze i najlepsze cechy polityków", fill = "Cecha", font="Times New Roman") +
  scale_fill_brewer(palette = "Pastel2")


## ŚREDNIE WYNIKI POLITYKÓW ####
sr_wyniki <- data.frame(round(colMeans(dane[,1:135]),3))
sr_wyniki <- cbind(newColName = rownames(sr_wyniki), sr_wyniki)
library("writexl")
#write_xlsx(sr_wyniki, "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/sr_wyniki.xlsx")

sr_wyniki <- matrix(sr_wyniki$round.colMeans.dane...1.135....3., nrow = 15, ncol = 9)
sr_wyniki <- as.data.frame(sr_wyniki)
colnames(sr_wyniki) <- c('Bosak','Czarzasty','Duda','Hołownia','Kaczyński',
                         'Kosiniak-Kamysz', 'Trzaskowski', 'Tusk', 'Idealny')
row.names(sr_wyniki) <- c('Uczciwy','Kulturalny','Tolerancyjny', 'Medialny',
                          'Bezinteresowny', 'Patriotyczny', 'Wiarygodny', 'Charyzmatyczny',
                          'Konsekwentny', 'Atrakcyjny', 'Rozsądny', 'Odpowiedzialny',
                          'Wpływowy', 'Komunikatywny','Elokwentny')
#write_xlsx(sr_wyniki, "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/sr_wyniki.xlsx")


##### JAK DALEKO DO IDEAŁU ####
# Odjęcie wektora sr idealnego od pozostalych politykow
wektor_idealny <- sr_wyniki$Idealny
ile_do_idealnego <- sweep(sr_wyniki, MARGIN = 1, STATS = wektor_idealny, FUN = "-")
ile_do_idealnego$Idealny <- sr_wyniki$Idealny
#write_xlsx(ile_do_idealnego, "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/ile_do_ideału.xlsx")

summary(ile_do_idealnego)
roznice_do_sr <- colMeans(abs(ile_do_idealnego[1:8]))
roznice_do_sr <- as.data.frame(roznice_do_sr)
library(tibble)
roznice_do_sr <- roznice_do_sr %>% rownames_to_column(var = "Polityk")
ggplot(roznice_do_sr, aes(x = Polityk, y = roznice_do_sr, group = 1)) +
  geom_point(color = "turquoise") +
  geom_line(color = "turquoise") +
  labs(x = "Polityk", y = "Średnia róznica", 
       title = "Średnia różnica między średnimi wynikami polityków a średnią dla idealnego") 

roznice_do_sr$Polityk <- factor(roznice_do_sr$Polityk, levels = roznice_do_sr$Polityk[order(roznice_do_sr$roznice_do_sr)])
ggplot(roznice_do_sr, aes(x = Polityk, y = roznice_do_sr, group = 1, fill = roznice_do_sr)) +
  geom_bar(stat = "identity") + 
  labs(x = "Polityk", y = "Średnia róznica", 
       title = "Średnia różnica między średnimi wynikami polityków a średnią dla idealnego",
      fill = "Średnia różnica") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue")

## NAJLEPSZY POLITYK W DANEJ CESZE ####
wykresy <- list()
for(x in 1:15){
  y <- seq(x,120+x,15)
  colMeans(dane[,y])
  srednie_df <- data.frame(Kolumny = names(colMeans(dane[,y])), Srednie = colMeans(dane[,y]))
  # Konwersja nazw kolumn na czynniki z odpowiednią kolejnością
  srednie_df$Kolumny <- factor(srednie_df$Kolumny, levels = srednie_df$Kolumny[order(srednie_df$Srednie)])
  # Tworzenie wykresu słupkowego za pomocą ggplot2 z odpowiednią kolejnością kolumn
  wykresy[[x]] <- ggplot(srednie_df, aes(x = Kolumny, y = Srednie)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(x = "Polityk", y = "Średnia", title = "Średnia wartość dla każdego polityka") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette="Pastel1")
}
for (i in 1:length(wykresy)) {
  print(wykresy[[i]])
}

# Trzaskowski jest bardziej atrakcyjny niż idelany polityk
# Hołownia bardziej medialny niż idealny polityk

## 3 NAJWAŻNIEJSZE CECHY POLITYKA ####
politycy <- c('Bosak','Czarzasty','Duda','Hołownia','Kaczyński',
              'Kosiniak-Kamysz', 'Trzaskowski', 'Tusk', 'Idealny')
# wiarygodność
y <- seq(7,120+7,15)
colMeans(dane[,y])
srednie_df <- data.frame(Kolumny = politycy, Srednie = colMeans(dane[,y]))

# Konwersja nazw kolumn na czynniki z odpowiednią kolejnością
srednie_df$Kolumny <- factor(srednie_df$Kolumny, levels = srednie_df$Kolumny[order(srednie_df$Srednie)])
# Tworzenie wykresu słupkowego za pomocą ggplot2 z odpowiednią kolejnością kolumn
ggplot(srednie_df, aes(x = Kolumny, y = Srednie, fill = Srednie)) +
  geom_bar(stat = "identity") +
  labs(x = "Polityk", y = "Średnia", title = "Średnia wartość wiarygodności dla każdego polityka") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue")

colMeans(dane[,y])
mean(c(dane$Bosak_Wiarygodny, dane$Czarzasty_Wiarygodny,dane$Duda_Wiarygodny,
       dane$Hołownia_Wiarygodny, dane$Kaczyński_Wiarygodny, dane$Kosiniak_Kamysz_Wiarygodny,
       dane$Trzaskowski_Wiarygodny, dane$Tusk_Wiarygodny))

sd(c(dane$Bosak_Wiarygodny, dane$Czarzasty_Wiarygodny,dane$Duda_Wiarygodny,
     dane$Hołownia_Wiarygodny, dane$Kaczyński_Wiarygodny, dane$Kosiniak_Kamysz_Wiarygodny,
     dane$Trzaskowski_Wiarygodny, dane$Tusk_Wiarygodny))

sd(c(dane$Bosak_Wiarygodny, dane$Czarzasty_Wiarygodny,dane$Duda_Wiarygodny,
     dane$Hołownia_Wiarygodny, dane$Kaczyński_Wiarygodny, dane$Kosiniak_Kamysz_Wiarygodny,
     dane$Trzaskowski_Wiarygodny, dane$Tusk_Wiarygodny)) /
  mean(c(dane$Bosak_Wiarygodny, dane$Czarzasty_Wiarygodny,dane$Duda_Wiarygodny,
         dane$Hołownia_Wiarygodny, dane$Kaczyński_Wiarygodny, dane$Kosiniak_Kamysz_Wiarygodny,
         dane$Trzaskowski_Wiarygodny, dane$Tusk_Wiarygodny))

# boxplot
melted_data_wiarygodny <- melt(dane[, y], id.vars = NULL)
ggplot(melted_data_wiarygodny, aes(x = variable, y = value)) +
  geom_boxplot(color="cornflowerblue") +
  labs(x = "Polityk", y = "Wartość", title = "Wiarygodność polityków") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(dane$Idealny_Wiarygodny)

#odpowiedzialność
y <- seq(12,120+12,15)
colMeans(dane[,y])
srednie_df <- data.frame(Kolumny = politycy, Srednie = colMeans(dane[,y]))
# Konwersja nazw kolumn na czynniki z odpowiednią kolejnością
srednie_df$Kolumny <- factor(srednie_df$Kolumny, levels = srednie_df$Kolumny[order(srednie_df$Srednie)])
# Tworzenie wykresu słupkowego za pomocą ggplot2 z odpowiednią kolejnością kolumn
ggplot(srednie_df, aes(x = Kolumny, y = Srednie, fill = Srednie)) +
  geom_bar(stat = "identity") +
  labs(x = "Polityk", y = "Średnia", title = "Średnia wartość odpowiedzialności dla każdego polityka") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue")

mean(c(dane$Bosak_Odpowiedzialny, dane$Czarzasty_Odpowiedzialny,dane$Duda_Odpowiedzialny,
       dane$Hołownia_Odpowiedzialny, dane$Kaczyński_Odpowiedzialny, dane$Kosiniak_Kamysz_Odpowiedzialny,
       dane$Trzaskowski_Odpowiedzialny, dane$Tusk_Odpowiedzialny))

sd(c(dane$Bosak_Odpowiedzialny, dane$Czarzasty_Odpowiedzialny,dane$Duda_Odpowiedzialny,
     dane$Hołownia_Odpowiedzialny, dane$Kaczyński_Odpowiedzialny, dane$Kosiniak_Kamysz_Odpowiedzialny,
     dane$Trzaskowski_Odpowiedzialny, dane$Tusk_Odpowiedzialny))

sd(c(dane$Bosak_Odpowiedzialny, dane$Czarzasty_Odpowiedzialny,dane$Duda_Odpowiedzialny,
     dane$Hołownia_Odpowiedzialny, dane$Kaczyński_Odpowiedzialny, dane$Kosiniak_Kamysz_Odpowiedzialny,
     dane$Trzaskowski_Odpowiedzialny, dane$Tusk_Odpowiedzialny)) /
  mean(c(dane$Bosak_Odpowiedzialny, dane$Czarzasty_Odpowiedzialny,dane$Duda_Odpowiedzialny,
         dane$Hołownia_Odpowiedzialny, dane$Kaczyński_Odpowiedzialny, dane$Kosiniak_Kamysz_Odpowiedzialny,
         dane$Trzaskowski_Odpowiedzialny, dane$Tusk_Odpowiedzialny))

# boxplot
melted_data_odpowiedzialny <- melt(dane[, y], id.vars = NULL)
ggplot(melted_data_odpowiedzialny, aes(x = variable, y = value)) +
  geom_boxplot(color="cornflowerblue") +
  labs(x = "Polityk", y = "Wartość", title = "Odpowiedzialność polityków") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# odpowiedzi osoby, ktora wymaga odpowiedzialnosci na poziomie 10
odpowiedzialny_10 <- dane[dane$Idealny_Odpowiedzialny == 10,]
barplot(as.matrix(odpowiedzialny_10[,c(121:135)]))

# uczciwość
y <- seq(1,120+1,15)
colMeans(dane[,y])
srednie_df <- data.frame(Kolumny = politycy, Srednie = colMeans(dane[,y]))
# Konwersja nazw kolumn na czynniki z odpowiednią kolejnością
srednie_df$Kolumny <- factor(srednie_df$Kolumny, levels = srednie_df$Kolumny[order(srednie_df$Srednie)])
# Tworzenie wykresu słupkowego za pomocą ggplot2 z odpowiednią kolejnością kolumn
ggplot(srednie_df, aes(x = Kolumny, y = Srednie, fill = Srednie)) +
  geom_bar(stat = "identity") +
  labs(x = "Polityk", y = "Średnia", title = "Średnia wartość uczciwości dla każdego polityka") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue")

mean(c(dane$Bosak_Uczciwy, dane$Czarzasty_Uczciwy,dane$Duda_Uczciwy,
       dane$Hołownia_Uczciwy, dane$Kaczyński_Uczciwy, dane$Kosiniak_Kamysz_Uczciwy,
       dane$Trzaskowski_Uczciwy, dane$Tusk_Uczciwy))

sd(c(dane$Bosak_Uczciwy, dane$Czarzasty_Uczciwy,dane$Duda_Uczciwy,
       dane$Hołownia_Uczciwy, dane$Kaczyński_Uczciwy, dane$Kosiniak_Kamysz_Uczciwy,
       dane$Trzaskowski_Uczciwy, dane$Tusk_Uczciwy))

sd(c(dane$Bosak_Uczciwy, dane$Czarzasty_Uczciwy,dane$Duda_Uczciwy,
     dane$Hołownia_Uczciwy, dane$Kaczyński_Uczciwy, dane$Kosiniak_Kamysz_Uczciwy,
     dane$Trzaskowski_Uczciwy, dane$Tusk_Uczciwy))/
  mean(c(dane$Bosak_Uczciwy, dane$Czarzasty_Uczciwy,dane$Duda_Uczciwy,
       dane$Hołownia_Uczciwy, dane$Kaczyński_Uczciwy, dane$Kosiniak_Kamysz_Uczciwy,
       dane$Trzaskowski_Uczciwy, dane$Tusk_Uczciwy))

# boxplot
melted_data_uczciwy <- melt(dane[, y], id.vars = NULL)
ggplot(melted_data_uczciwy, aes(x = variable, y = value)) +
  geom_boxplot(color="cornflowerblue") +
  labs(x = "Polityk", y = "Wartość", title = "Uczciwość polityków") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

uczciwy_1 <- dane[dane$Idealny_Uczciwy == 1,]
melted_uczciwy_1 <- melt(uczciwy_1[,c(121:135)])
ggplot(melted_uczciwy_1, aes(x = variable, y = value, fill = value)) +
  geom_bar( stat = 'identity') +
  labs(x = "Cecha", y = "Wartość", title = "Pozostałe oceny",
       fill = 'Wartość') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue")

## ŚREDNIA OCENA POLITYKA ####
Bosak_sr <- mean(as.matrix(dane[,c(1:15)]))
Czarzasty_sr <- mean(as.matrix(dane[,c(16:30)]))
Duda_sr <- mean(as.matrix(dane[,c(31:45)]))
Holownia_sr <- mean(as.matrix(dane[,c(46:60)]))
Kaczynski_sr <- mean(as.matrix(dane[,c(61:75)]))
Kosiniak_Kamysz_sr <- mean(as.matrix(dane[,c(76:91)]))
Trzaskowski_sr <- mean(as.matrix(dane[,c(91:105)]))
Tusk_sr <- mean(as.matrix(dane[,c(106:120)]))
Idealny_sr <- mean(as.matrix(dane[,c(121:135)]))
sr_ocena_polityka <- data.frame('Polityk' = politycy,
                                'Średnia_ocena' = c(Bosak_sr,Czarzasty_sr, Duda_sr,
                                                     Holownia_sr, Kaczynski_sr, Kosiniak_Kamysz_sr,
                                                     Trzaskowski_sr, Tusk_sr, Idealny_sr))

order(sr_ocena_polityka)

ggplot(sr_ocena_polityka, aes(x = reorder(Polityk, Średnia_ocena), y = Średnia_ocena, fill = Średnia_ocena)) +
  geom_bar(stat = 'identity') +
  labs(x = "Polityk", y = "Średnia ocena", title = "Średnie oceny polityków",
       fill = 'Średnia ocena') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue")

ggplot(sr_ocena_polityka, aes(x = reorder(Polityk, Średnia_ocena), y = Średnia_ocena, fill = Średnia_ocena)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(Średnia_ocena, 1)), vjust = -0.5) + # Dodanie znaczników z oceną
  labs(x = "Polityk", y = "Średnia ocena", title = "Średnie oceny polityków") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + # Usunięcie legendy
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") 


## POLITYK IDEALNY ####
kolumny_cechy <- c('Uczciwy','Kulturalny','Tolerancyjny', 'Medialny',
                'Bezinteresowny', 'Patriotyczny', 'Wiarygodny', 'Charyzmatyczny',
                'Konsekwentny', 'Atrakcyjny', 'Rozsądny', 'Odpowiedzialny',
                'Wpływowy', 'Komunikatywny','Elokwentny')
y <- seq(121,135,1)
colMeans(dane[,y])
srednie_df <- data.frame(Kolumny = kolumny_cechy, Srednie = colMeans(dane[,y]))
# Konwersja nazw kolumn na czynniki z odpowiednią kolejnością
srednie_df$Kolumny <- factor(srednie_df$Kolumny, levels = srednie_df$Kolumny[order(srednie_df$Srednie)])
# Tworzenie wykresu słupkowego za pomocą ggplot2 z odpowiednią kolejnością kolumn
ggplot(srednie_df, aes(x = Kolumny, y = Srednie, fill = Srednie)) +
  geom_bar(stat = "identity") +
  labs(x = "Cecha", y = "Średnia", title = "Średnie wyniki dla polityka idealnego") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "turquoise") +
  geom_hline(yintercept=90, linetype="dashed", 
             color = "darkblue", linewidth=1)

# podział na najmłodszych i nastarszych
młodzi <- dane[dane$Wiek_1=="<24" | dane$Wiek_1=="24-29",]
starsi <- dane[dane$Wiek_1=="50-59" | dane$Wiek_1=="60-88",]

sort(round(colMeans(młodzi[,y]),3))
sort(round(colMeans(starsi[,y]),3))

# różnica
sort(round(colMeans(młodzi[,y]),3) - round(colMeans(starsi[,y]),3))

# wykres
mlodzi_idealny <- data.frame(Kolumny = kolumny_cechy, Srednie = colMeans(młodzi[,y]))
starsi_idealny <- data.frame(Kolumny = kolumny_cechy, Srednie = colMeans(starsi[,y]))

ggplot() +
  geom_bar(data = mlodzi_idealny, aes(x = Kolumny, y = Srednie, fill = "Najmłodsi"), 
           stat = "identity", width = 0.4, position = position_nudge(x = 0.2)) +
  geom_bar(data = starsi_idealny, aes(x = Kolumny, y = Srednie, fill = "Najstarsi"),
           stat = "identity", width = 0.4, position = position_nudge(x = -0.2)) +
  scale_fill_manual(values = c("Najmłodsi" = "turquoise3", "Najstarsi" = "skyblue4"), 
                    name = "Wiek", 
                    labels = c("Najmłodsi", "Najstarsi")) +
  labs(
    x = "Cecha", y = "Nasilenie", title = "Idealny polityk według osób z najmłodszej grupy wiekowej a najstarszej") +
  expand_limits(y = 1) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



# porównanie wyników dla kobiet i mezczyzn
sort(round(colMeans(kobiety[,y]),3))
sort(round(colMeans(mezczyzni[,y]),3))
round(colMeans(kobiety[,y]),3) - round(colMeans(mezczyzni[,y]),3)

# wykres
kobiety_idealny <- data.frame(Kolumny = kolumny_cechy, Srednie = colMeans(kobiety[,y]))
mezczyzni_idealny <- data.frame(Kolumny = kolumny_cechy, Srednie = colMeans(mezczyzni[,y]))

ggplot() +
  geom_bar(data = mezczyzni_idealny, aes(x = Kolumny, y = Srednie, fill = "Mężczyźni"), 
           stat = "identity", width = 0.4, position = position_nudge(x = 0.2)) +
  geom_bar(data = kobiety_idealny, aes(x = Kolumny, y = Srednie, fill = "Kobiety"),
           stat = "identity", width = 0.4, position = position_nudge(x = -0.2)) +
  scale_fill_manual(values = c("Mężczyźni" = "turquoise3", "Kobiety" = "palevioletred2"), 
                    name = "Płeć", 
                    labels = c("Mężczyźni", "Kobiety")) +
  labs(
    x = "Cecha", y = "Nasilenie", title = "Idealny polityk według kobiet i mężczyzn") +
  expand_limits(y = 1) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

# wykształcenie
podstawowe <- dane[dane$Wykształcenie == "Podstawowe",]
średnie <- dane[dane$Wykształcenie == "Średnie",]
zawodowe <- dane[dane$Wykształcenie == "Zawodowe",]
wyższe <- dane[dane$Wykształcenie == "Wyższe",]
sort(round(colMeans(podstawowe[,y]),3))
sort(round(colMeans(średnie[,y]),3))
sort(round(colMeans(zawodowe[,y]),3))
sort(round(colMeans(wyższe[,y]),3))
wykształcenie_idealny <- data.frame(Podstawowe  = c(round(colMeans(podstawowe[,y]),3)),
           Średnie = c(round(colMeans(średnie[,y]),3)),
           Zawodowe = c(round(colMeans(zawodowe[,y]),3)),
           Wyższe = c(round(colMeans(wyższe[,y]),3)))
write_xlsx(wykształcenie_idealny, "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/wyksztalcenie.xlsx")


## PYTANIA ####
#### ŚREDNIE ODPOWIEDZI ####
dane[, 136:157] <- lapply(dane[, 136:157], as.integer)
summary(dane[,136:157])

pytania_srednie <- data.frame(Kolumny = 1:22, Srednie = colMeans(dane[,136:157]))
ggplot(pytania_srednie, aes(x = Kolumny, y = Srednie)) +
  geom_line(color = "darkturquoise") + geom_point(color="turquoise") +
  labs(x = "Numer pytania", y = "Średnia wartość", title = "Średnie wartości dla pytań 1-22")



# Przekształcenie danych do długiego formatu
kolumny_pytania <- names(dane[,136:157])
melted_data <- melt(dane[, c(kolumny_pytania)], id.vars = NULL)

### BOXPLOTY
ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot(color="cornflowerblue") +
  labs(x = "Numer pytania", y = "Wartość", title = "Boxploty dla pytań 1-22") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### TWARZE ####
library(aplpack)
faces(t(sr_wyniki))
# mikołaje
faces(t(sr_wyniki), face.type = 2)
faces(t(sr_wyniki), ncolors = 20)
# dodatkowe parametry 
faces(t(sr_wyniki), 
      face.type = 1,  # Możesz zmienić na inny typ twarzy: 1, 2, 3 itd.
      main = "Twarze Chernoffa",
      scale = TRUE)  # Skalowanie twarzy


### PROFIL DAMSKI I MĘSKI ####
sr_pyt_kobiety <- data.frame(Kolumny = 1:22, Srednie = colMeans(dane[dane$Płeć == "Kobieta", 136:157]), 
                             Płeć = "Kobieta")

sr_pyt_mezczyzni <- data.frame(Kolumny = 1:22, Srednie = colMeans(dane[dane$Płeć == "Mężczyzna", 136:157]), 
                               Płeć = "Mężczyzna")

# Połączenie ramek danych
sr_pytania <- rbind(sr_pyt_kobiety, sr_pyt_mezczyzni)

# Wykres liniowo-punktowy z oddzielnymi liniami dla kobiet i mężczyzn
ggplot(sr_pytania, aes(x = Kolumny, y = Srednie, color = Płeć, group = Płeć)) +
  geom_point() +
  geom_line() +
  labs(x = "Numer pytania", y = "Średnia wartość", title = "Średnie wartości dla pytań 1-22") +
  scale_color_manual(values = c("Kobieta" = "violet", "Mężczyzna" = "darkturquoise")) +
  scale_x_continuous(breaks = 1:22)

ggplot(sr_pytania, aes(x = Kolumny, y = Srednie, color = Płeć, group = Płeć)) +
  geom_point() +
  geom_line() +
  labs(x = "Numer pytania", y = "Średnia wartość", title = "Średnie wartości dla pytań 1-22") +
  scale_color_manual(values = c("Kobieta" = "violet", "Mężczyzna" = "darkturquoise")) +
  scale_x_continuous(breaks = 1:22) +
  theme(legend.position = "bottom")

#różnica średnich
round(sr_pyt_kobiety$Srednie - sr_pyt_mezczyzni$Srednie,3)

### PROFIL NAJSTARSI I NAJMŁODSI ####
sr_pyt_mlodzi <- data.frame(Kolumny = 1:22, Srednie = colMeans(dane[dane$Wiek_1 == "<24", 136:157]), 
                             Wiek_1 = "<24")

sr_pyt_starsi <- data.frame(Kolumny = 1:22, Srednie = colMeans(dane[dane$Wiek_1 == "60-88", 136:157]), 
                               Wiek_1 = "60-88")

# Połączenie ramek danych
sr_pytania_wg_wiek <- rbind(sr_pyt_mlodzi, sr_pyt_starsi)

# Wykres liniowo-punktowy z oddzielnymi liniami dla kobiet i mężczyzn
ggplot(sr_pytania_wg_wiek, aes(x = Kolumny, y = Srednie, color = Wiek_1, group = Wiek_1)) +
  geom_point() +
  geom_line() +
  labs(x = "Numer pytania", y = "Średnia wartość", 
       title = "Średnie wartości dla pytań 1-22", color = "Grupa wiekowa") +
  scale_color_manual(values = c("<24" = "violet", "60-88" = "darkturquoise")) +
  scale_x_continuous(breaks = 1:22)

#różnica średnich
round(sr_pyt_mlodzi$Srednie - sr_pyt_starsi$Srednie,3)

# POSZCZEGÓLNE PYTANIA ####
# zamiana odpowiedzi na pytania na factor, skala nominalna
#dane[, 136:157] <- lapply(dane[, 136:157], as.integer)
summary(dane)
colMeans(dane[,136:157])
indeksy_kolumn <- c(seq(136,157,1))

par(mfrow = c(2, 4))
# Iterujemy przez wybrane kolumny i tworzymy dla każdej z nich wykres słupkowy
for (kolumna in indeksy_kolumn) {
  # Tworzymy tytuł wykresu na podstawie nazwy kolumny
  tytul_wykresu <- paste("kolumna", colnames(dane)[kolumna])
  
  # Tworzymy wykres słupkowy
  barplot(table(dane[, kolumna]), main = tytul_wykresu, xlab = "Wartość", ylab = "Liczba wystąpień")
}

### PYTANIA Z NAJWIĘKSZA LICZBA ODPOWIEDZI 7 ####
# pytanie 6 (nepotyzm) - 29,43% odpowiedzi zdecydowanie się zgadzam
summary(factor(dane$Pyt_2_6))
round(table(dane$Pyt_2_6) / nrow(dane) *100, 2)
round(table(dane$Wiek_1, dane$Pyt_2_6) / rowSums(table(dane$Wiek_1, dane$Pyt_2_6))* 100,2)
# w przypadku starszych osób dużo mniej osób udzieliło odp - 7
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_6))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o nepotyzm ze wględu na grupy wiekowe',
       fill = 'Pyt_2_6') 

# pytanie 9 (kościół) - 40% odpowiedzi zdecydowanie się zgadzam
round(table(dane$Pyt_2_9) / nrow(dane) *100, 2)
round(table(dane$Wiek_1, dane$Pyt_2_9) / rowSums(table(dane$Wiek_1, dane$Pyt_2_9))* 100,2)
# w przypadku starszych osób dużo mniej osób udzieliło odp - 7
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_9))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o kościół ze wględu na grupy wiekowe',
       fill = 'Pyt_2_9') 


# dla pytania 16 (elektrownia) duża przewaga odpowiedzi 7 - ponad 40%
round(table(dane$Pyt_2_16) / nrow(dane) *100, 2)
round(table(dane$Płeć, dane$Pyt_2_16) / rowSums(table(dane$Płeć, dane$Pyt_2_16))* 100,2)
ggplot(dane , aes(x=factor(Płeć), fill=factor(Pyt_2_16))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o elekrownie jądrową ze względu na płeć',
       fill = 'Odpowiedź',
       x = "Płeć", y = "Odsetek odpowiedzi") +
  scale_fill_brewer(palette="PuBu")
# ponad 50% mężczyzn się zdecydowanie zgadza
ggplot(dane, aes(x=as.integer(Pyt_2_16)))+
  geom_histogram(color="black", fill="cyan")+
  facet_grid(Płeć ~ .)

# pytanie 20 (aborcja) - 38,5% odpowiedzi zdecydowanie się zgadzam
round(table(dane$Pyt_2_20) / nrow(dane) *100, 2)
round(table(dane$Płeć, dane$Pyt_2_20) / rowSums(table(dane$Płeć, dane$Pyt_2_20))* 100,2)
ggplot(dane , aes(x=factor(Płeć), fill=factor(Pyt_2_20))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o aborcję ze względu na płeć',
       fill = 'Odpowiedź',
       x = "Płeć", y = "Odsetek odpowiedzi") +
  scale_fill_brewer(palette="PuBu")

### INNE PYTANIA ZE WGLĘDU NA PŁEĆ ####
# pytanie 15 (służba wojskowa) - największa grupa, prawie 30% zdecydowanie się nie zgadza
round(table(dane$Pyt_2_15) / nrow(dane) *100, 2)
round(table(dane$Płeć, dane$Pyt_2_15) / rowSums(table(dane$Płeć, dane$Pyt_2_15))* 100,2)
ggplot(dane , aes(x=factor(Płeć), fill=factor(Pyt_2_15))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o obowiązkową służbę wojskową mężczyzn ze względu na płeć',
       fill = 'Pyt_2_15') 
# mężczyźni częściej udzielali skrajnych odpowiedzi 1 lub 7

# pytanie 22 (wiek emerytalny) - największa grupa, prawie 30% zdecydowanie się nie zgadza
round(table(dane$Pyt_2_22) / nrow(dane) *100, 2)
round(table(dane$Płeć, dane$Pyt_2_22) / rowSums(table(dane$Płeć, dane$Pyt_2_22))* 100,2)
ggplot(dane , aes(x=factor(Płeć), fill=factor(Pyt_2_22))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o zrównanie wieku emerytalnego ze względu na płeć',
       fill = 'Odpowiedź', x='Płeć',
       y = 'Odsetek odpowiedzi') +
  scale_fill_brewer(palette = 'PuBu')
# ponad 1/4 kobiet zdecydowanie się nie zgadza z zrównaniem wieku emerytalnegp
# natomiast ponad 1/4 mężczyzn się zgadza

# pytanie 12 (małzeństwa) - największa grupa, ponad 32% zdecydowanie się nie zgadza
round(table(dane$Pyt_2_12) / nrow(dane) *100, 2)
round(table(dane$Płeć, dane$Pyt_2_12) / rowSums(table(dane$Płeć, dane$Pyt_2_12))* 100,2)
ggplot(dane , aes(x=factor(Płeć), fill=factor(Pyt_2_12))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o małżeństwa homoseksulane ze względu na płeć',
       fill = 'Pyt_2_12') 
# więcej kobiet się zgadza

### INNE PYTANIA ZE WGLĘDU NA WIEK ####
# pytanie 12 (małzeństwa) - największa grupa, ponad 32% zdecydowanie się nie zgadza
round(table(dane$Pyt_2_12) / nrow(dane) *100, 2)
round(table(dane$Wiek_1, dane$Pyt_2_12) / rowSums(table(dane$Wiek_1, dane$Pyt_2_12))* 100,2)
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_12))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o małżeństwa homoseksulane ze względu na wiek',
         fill = 'Odpowiedź', x='Grupa wiekowa',
       y = 'Odsetek odpowiedzi') +
  scale_fill_brewer(palette = 'PuBu')
# widać, że wraz z wiekiem maleje procent odpowiedzi na zdecydowanie sie zgadzam
# za wyjątkiem kategorii wiekowej 50-59lat

# pytanie 10 (tv) - największa grupa, ponad 32% zdecydowanie się nie zgadza
round(table(dane$Pyt_2_10) / nrow(dane) *100, 2)
round(table(dane$Wiek_1, dane$Pyt_2_10) / rowSums(table(dane$Wiek_1, dane$Pyt_2_10))* 100,2)
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_10))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o usnięcie telewizji publicznej ze względu na wiek',
       fill = 'Pyt_2_10') 
# widać, że wraz z wiekiem rośnie procent odpowiedzi na zdecydowanie sie zgadzam
# blisko 48% w najstarszej kategorii zdecydowanie sie nie zgadza

# pytanie 17 (ochrona zdrowia) - zrównoważone odp
round(table(dane$Pyt_2_17) / nrow(dane) *100, 2)
round(table(dane$Wiek_1, dane$Pyt_2_17) / rowSums(table(dane$Wiek_1, dane$Pyt_2_17))* 100,2)
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_17))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o składke zdrowotną ze względu na wiek',
       fill = 'Pyt_2_17') 

# pytanie 22 (wiek emerytalny) 
round(table(dane$Wiek_1, dane$Pyt_2_22) / rowSums(table(dane$Wiek_1, dane$Pyt_2_22))* 100,2)
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_22))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o zrównanie wieku emerytalnego ze względu na płeć',
       fill = 'Odpowiedź', x='Grupa wiekowa',
       y = 'Odsetek odpowiedzi') +
  scale_fill_brewer(palette = 'PuBu')
# wraz z wiekiem rośnie procent odpowiedzi 'zdecydowanie sie nie zgadzam'

# pytanie 20 (aborcja)
round(table(dane$Wiek_1, dane$Pyt_2_20) / rowSums(table(dane$Wiek_1, dane$Pyt_2_20))* 100,2)
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_20))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o aborcję ze względu na płeć',
       fill = 'Odpowiedź', x='Grupa wiekowa',
       y = 'Odsetek odpowiedzi') +
  scale_fill_brewer(palette = 'Pastel1')

# pytanie 9 - kościół
round(table(dane$Wiek_1, dane$Pyt_2_9) / rowSums(table(dane$Wiek_1, dane$Pyt_2_9))* 100,2)
ggplot(dane , aes(x=factor(Wiek_1), fill=factor(Pyt_2_9))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o kościół ze wględu na płeć',
       fill = 'Odpowiedź', x='Grupa wiekowa',
       y = 'Odsetek odpowiedzi') +
  scale_fill_brewer(palette = 'Pastel1')

### PYTANIA ZE WZGLĘDU NA WYKSZTAŁCENIE ####  
# pytanie 21 (praca domowa) - 
round(table(dane$Pyt_2_21) / nrow(dane) *100, 2)
round(table(dane$Wykształcenie, dane$Pyt_2_21) / rowSums(table(dane$Wykształcenie, dane$Pyt_2_21))* 100,2)
ggplot(dane , aes(x=factor(Wykształcenie), fill=factor(Pyt_2_21))) +
  geom_bar(position = "fill") +
  labs(title='Rozkład odpowiedzi na pytanie o składke zdrowotną ze względu na wiek',
       fill = 'Pyt_2_21') 
# nie ma duzych różnic, jedynie najmłodsi najczęściej zaznaczali odpowiedź 3

### INNE PYTANIE ####
# pytanie 11
ggplot(dane, aes(x=Pyt_2_11)) + geom_histogram(color = "darkturquoise",fill="white", bins = 7) +
  labs(title = "Histogram odpowiedzi na pytanie 11")
ggplot(dane, aes(x=Pyt_2_11, color=Płeć)) + geom_histogram(fill="white")

ggplot(dane, aes(x=Pyt_2_11))+
  geom_histogram(color="black", fill="white")+
  facet_grid(Płeć ~ .)


# pytanie 14
ggplot(dane, aes(x=as.integer(Pyt_2_1))) + geom_histogram(color = "darkturquoise",fill="white", bins = 7) +
  labs(title = "Histogram odpowiedzi na pytanie 14")

# CHI - kwadrat ####
par(mfrow=c(1,1))
# aborcja i płeć
table(dane$Płeć, dane$Pyt_2_20)
# podział na 3 grupy
płeć_aborcja <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_20), breaks = c(0, 3, 4, 7), 
                     labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
płeć_aborcja
# wizualizacja tablicy
mosaicplot(płeć_aborcja)
mosaicplot(płeć_aborcja, 
           color = c("skyblue", "lightgreen", "turquoise"))
mosaic(płeć_aborcja, gp = shading_max, 
       split_vertical = TRUE,
       main = "Płeć a poparcie aborcji")

płeć_aborcja_df <- as.data.frame.table(płeć_aborcja)

# Tworzenie wykresu mosaic plot w ggplot2
ggplot(płeć_aborcja_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  labs(x = "Odpowiedź w pytaniu o aborcje", y = "Płeć", fill = "Liczebność",
       title = "Płeć a poparcie dla aborcji") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") +
  guides(fill = guide_legend(title = "Liczebność"))

##
# Plot
plot(
  0,
  0,
  # Type of plotting symbol
  pch = "",
  # Range of x-axis
  xlim = c(0, 5.5),
  # Range of y-axis
  ylim = c(0, 6.5),
  # Suppresses both x and y axes
  axes = FALSE,
  # Label of x-axis
  xlab = "Poparcie aborcji",
  # Label of y-axis
  ylab = ""
) 

# Write a for-loop that adds the bubbles to the plot
for (i in 1:dim(płeć_aborcja)[1]) {
  symbols(
    c(1:dim(płeć_aborcja)[2]),
    rep(i, dim(płeć_aborcja)[2]),
    circle = sqrt(płeć_aborcja[i, ] / 200 / pi),
    add = TRUE,
    inches = FALSE,
    fg = brewer.pal(5, "PRGn"),
    bg = brewer.pal(5, "PRGn")
  )
}
axis(1,
     col = "white",
     col.axis = "black",
     at = c(1:3),
     label = colnames(płeć_aborcja))
axis(
  2,
  at = c(1:2),
  label = rownames(płeć_aborcja),
  las = 1,
  col.axis = "black",
  col = "white"
)

# Add numbers to plot
for (i in 1:2) {
  text(c(1:3), rep(i, 2), płeć_aborcja[i, ])
}

# Plot with better visibility and title
plot(
  0,
  0,
  pch = "",
  xlim = c(0.5, 3.5),
  ylim = c(0.5, 2.5),
  axes = FALSE,
  xlab = "Poparcie aborcji",
  ylab = "",
  main = "Poparcie aborcji w zależności od płci"
) 


require(vcd)
spine(płeć_aborcja)
heatmap(płeć_aborcja, Rowv = NA, Colv = NA, col = cm.colors(256), 
        scale = "column", margins = c(5, 10))
mosaic(płeć_aborcja, shade=T, legend=T)
assoc(płeć_aborcja, shade=T, legend=T)

# chi2
chisq.test(płeć_aborcja)
chisq.test(płeć_aborcja)$expected
cramerV(płeć_aborcja)
assocstats(płeć_aborcja)
library(DescTools)
TschuprowT(płeć_aborcja)
ContCoef(płeć_aborcja)
C <- ContCoef(płeć_aborcja)

# jezeli liczba wierszy jest rozna od liczby kolumn (w != k)
w = 2
k = 3

# Cmax - tylko do wyliczenia skorygowanego 
Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
# Ckor - skorygowany Pearson
Ckor = C/Cmax
Ckor

# podział na 2 grupy
płeć_aborcja <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_20), breaks = c(0, 4, 7), 
                                     labels = c("1-4", "5-7"), include.lowest = TRUE))
płeć_aborcja
chisq.test(płeć_aborcja)
chisq.test(płeć_aborcja)$expected
assocstats(płeć_aborcja)
TschuprowT(płeć_aborcja)

# zrównanie wieku emerytalnego i płeć

table(dane$Płeć, dane$Pyt_2_22)
# podział na 3 grupy
płeć_emerytura <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_22), breaks = c(0, 3, 4, 7), 
                                     labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
płeć_emerytura
chisq.test(płeć_emerytura)
chisq.test(płeć_emerytura)$expected
assocstats(płeć_emerytura)
TschuprowT(płeć_emerytura)
C <- ContCoef(płeć_emerytura)
Ckor = C/Cmax
Ckor

płeć_emerytura_df <- as.data.frame.table(płeć_emerytura)

# Tworzenie wykresu mosaic plot w ggplot2
ggplot(płeć_emerytura_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  labs(x = "Odpowiedź w pytaniu o zrównanie wieku emerytalnego", y = "Płeć", fill = "Liczebność",
       title = "Płeć a poparcie dla zrównania wieku emerytalnego") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") +
  guides(fill = guide_legend(title = "Liczebność"))

# podział na 2 grupy
płeć_emerytura <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_22), breaks = c(0, 4, 7), 
                                     labels = c("1-4", "5-7"), include.lowest = TRUE))
płeć_emerytura
chisq.test(płeć_emerytura)
chisq.test(płeć_emerytura)$expected
assocstats(płeć_emerytura)
TschuprowT(płeć_emerytura)


# płeć a elekrownia
# podział na 3 grupy
płeć_elektrownia <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_16), breaks = c(0, 3, 4, 7), 
                                       labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
płeć_elektrownia
chisq.test(płeć_elektrownia)
chisq.test(płeć_elektrownia)$expected
assocstats(płeć_elektrownia)
TschuprowT(płeć_elektrownia)
C <- ContCoef(płeć_elektrownia)
Ckor = C/Cmax
Ckor

płeć_elektrownia_df <- as.data.frame.table(płeć_elektrownia)

# Tworzenie wykresu mosaic plot w ggplot2
ggplot(płeć_elektrownia_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  labs(x = "Odpowiedź w pytaniu o elektrownię jądrową w Polsce", y = "Płeć", fill = "Liczebność",
       title = "Płeć a poparcie dla elektrowni jądrowej") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") +
  guides(fill = guide_legend(title = "Liczebność"))

# podział na 2 grupy
płeć_elektrownia<- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_16), breaks = c(0, 4, 7), 
                                       labels = c("1-4", "5-7"), include.lowest = TRUE))
płeć_elektrownia
chisq.test(płeć_elektrownia)
chisq.test(płeć_elektrownia)$expected
assocstats(płeć_elektrownia)

# płeć a służba wojskowa
# podział na 3 grupy
płeć_wojsko <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_15), breaks = c(0, 3, 4, 7), 
                                         labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
płeć_wojsko
chisq.test(płeć_wojsko)
chisq.test(płeć_wojsko)$expected
assocstats(płeć_wojsko)
TschuprowT(płeć_wojsko)

# płeć a małżeństwa
# podział na 3 grupy
płeć_malzenstwa <- table(dane$Płeć, cut(as.numeric(dane$Pyt_2_12), breaks = c(0, 3, 4, 7), 
                                    labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
płeć_malzenstwa
chisq.test(płeć_malzenstwa)
chisq.test(płeć_malzenstwa)$expected
assocstats(płeć_malzenstwa)
TschuprowT(płeć_malzenstwa)


## WIEK 
# wiek a aborcja
# podział na 3 grupy
wiek_aborcja <- table(dane$Wiek_1, cut(as.numeric(dane$Pyt_2_20), breaks = c(0, 3, 4, 7), 
                                         labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
wiek_aborcja
chisq.test(wiek_aborcja)
chisq.test(wiek_aborcja)$expected

assocstats(wiek_aborcja)
TschuprowT(wiek_aborcja)
C <- ContCoef(wiek_aborcja)

# jezeli liczba wierszy jest rozna od liczby kolumn (w != k)
w = 5
k = 3

# Cmax - tylko do wyliczenia skorygowanego 
Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
# Ckor - skorygowany Pearson
Ckor = C/Cmax
Ckor
# 0,324
wiek_aborcja_df <- as.data.frame.table(wiek_aborcja)

# Tworzenie wykresu mosaic plot w ggplot2
ggplot(wiek_aborcja_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  labs(x = "Odpowiedź w pytaniu o legalną aborcję do 12. tyg ciąży",
       y = "Płeć", fill = "Liczebność",
       title = "Płeć a poparcie dla legalnej aborcji") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") +
  guides(fill = guide_legend(title = "Liczebność"))

# podzial na 2 kat wiekowe
dane$Wiek_3 <- recode(dane$Wiek, "14:49 ='poniżej 50'; else = '50 i powyżej'")
wiek2_aborcja <- table(dane$Wiek_3, cut(as.numeric(dane$Pyt_2_20), breaks = c(0, 3, 4, 7), 
                                          labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
wiek2_aborcja
chisq.test(wiek2_aborcja)
chisq.test(wiek2_aborcja)$expected

assocstats(wiek2_aborcja)
TschuprowT(wiek2_aborcja)
C <- ContCoef(wiek2_aborcja)

# jezeli liczba wierszy jest rozna od liczby kolumn (w != k)
w = 2
k = 3

# Cmax - tylko do wyliczenia skorygowanego 
Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
# Ckor - skorygowany Pearson
Ckor = C/Cmax
Ckor
# 0,344
wiek2_aborcja_df <- as.data.frame.table(wiek2_aborcja)

# Tworzenie wykresu mosaic plot w ggplot2
ggplot(wiek2_aborcja_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  labs(x = "Odpowiedź w pytaniu o legalną aborcję do 12. tyg ciąży",
       y = "Wiek", fill = "Liczebność",
       title = "Wiek a poparcie dla legalnej aborcji") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") +
  guides(fill = guide_legend(title = "Liczebność"))


# wiek a emerytura
wiek_emerytura <- table(dane$Wiek_1, cut(as.numeric(dane$Pyt_2_22), breaks = c(0, 3, 4, 7), 
                                         labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
wiek_emerytura
chisq.test(wiek_emerytura)
round(chisq.test(wiek_emerytura)$expected,3)
assocstats(wiek_emerytura)
TschuprowT(wiek_emerytura)

# podział n1 2 grupy wiekowe
wiek2_emerytura <- table(dane$Wiek_3, cut(as.numeric(dane$Pyt_2_22), breaks = c(0, 3, 4, 7), 
                                          labels = c("1-3", "4", "5-7"), include.lowest = TRUE))
wiek2_emerytura
chisq.test(wiek2_emerytura)
round(chisq.test(wiek2_emerytura)$expected,3)
assocstats(wiek2_emerytura)
TschuprowT(wiek2_emerytura)

C <- ContCoef(wiek2_emerytura)

# jezeli liczba wierszy jest rozna od liczby kolumn (w != k)
w = 2
k = 3

# Cmax - tylko do wyliczenia skorygowanego 
Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
# Ckor - skorygowany Pearson
Ckor = C/Cmax
Ckor
# 0,344
wiek2_emerytura_df <- as.data.frame.table(wiek2_emerytura)

# Tworzenie wykresu mosaic plot w ggplot2
ggplot(wiek2_emerytura_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  labs(x = "Odpowiedź w pytaniu o zrównanie wieku emerytalnego",
       y = "Wiek", fill = "Liczebność",
       title = "Wiek a poparcie zrównania wieku emerytalnego") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue") +
  guides(fill = guide_legend(title = "Liczebność"))

# SKORELOWANIE PYTAN 1-22 ####
par(mfrow=c(1,1))
heatmap(cor(dane[,136:157], method = 'spearman'))
corrplot(cor(dane[,136:157], method = 'spearman'))
round(cor(dane[,136:157], method = 'spearman'),2)
round(table(dane$Pyt_2_9, dane$Pyt_2_20) / rowSums(table(dane$Pyt_2_9, dane$Pyt_2_20))* 100,2)
round(table(dane$Pyt_2_12, dane$Pyt_2_20) / rowSums(table(dane$Pyt_2_12, dane$Pyt_2_20))* 100,2)
cor(dane$Pyt_2_12,dane$Pyt_2_20)
cor(dane$Pyt_2_9,dane$Pyt_2_20)
cor(dane$Pyt_2_12,dane$Pyt_2_8)

table(dane$Pyt_2_12, dane$Pyt_2_20)

