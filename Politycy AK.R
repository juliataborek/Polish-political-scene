library(readxl)
library(dplyr)
library(reshape2)
library(tidyr)

## Wgranie danych ####
tablica_danych <- read_excel("sr_wyniki.xlsx")
tablica_danych <- as.data.frame(tablica_danych)
row.names(tablica_danych) <- c('Uczciwy','Kulturalny','Tolerancyjny', 'Medialny',
                          'Bezinteresowny', 'Patriotyczny', 'Wiarygodny', 'Charyzmatyczny',
                          'Konsekwentny', 'Atrakcyjny', 'Rozsądny', 'Odpowiedzialny',
                          'Wpływowy', 'Komunikatywny','Elokwentny')
tablica_danych <- t(tablica_danych)
tablica_danych <- as.matrix(tablica_danych)
tablica_danych

## Wykresy danych####
library("gplots")
library(ggplot2)
library(ggpubr)
balloonplot(as.table(tablica_danych), main ="Cechy polityków", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE, label.size = 1)

ggballoonplot(tablica_danych, fill = "value")
ggballoonplot(round(tablica_danych,0), fill = "value", color = "lightgray",
              size = 10, show.label = TRUE)+
  gradient_fill(c("blue", "white", "red"))

ggballoonplot(round(tablica_danych,0), fill = "value", color = "lightgray",
              size = 10, show.label = TRUE)+
  gradient_fill(c("blue", "white", "darkturquoise")) +
  labs(fill="Średnia", title = "Średnie oceny cech polityków",
       y = "Polityk", x = "Cecha")

tablica_danych_df <- as.data.frame(tablica_danych)
tablica_danych_df_t <- t(tablica_danych_df)
tablica_danych_df_t <- as.data.frame(tablica_danych_df_t)
tablica_danych_df_t <- tablica_danych_df_t[order(tablica_danych_df_t$Idealny, decreasing = TRUE),]
tablica_danych_df <- t(tablica_danych_df_t)
tablica_danych_df <- as.data.frame(tablica_danych_df)
tablica_danych_df_t <- tablica_danych_df_t[,order(tablica_danych_df$Wiarygodny, decreasing = TRUE)]

# Przekształcenie ramki danych do formatu długiego
tablica_danych_long <- melt(round(t(tablica_danych_df_t), 0))

# Generowanie wykresu ggballoonplot
ggballoonplot(tablica_danych_long, x = "Var2", y = "Var1", fill = "value", color = "lightgray",
              size = 10, show.label = TRUE) +
  gradient_fill(c("blue", "white", "darkturquoise")) +
  labs(fill = "Średnia", title = "Średnie oceny cech polityków",
       y = "Cecha", x = "Polityk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

## Wyliczanie rozkladow brzegowych ####
# moze sie przydac do raportu
suma <- addmargins(tablica_danych)
suma

#
rozkład_brzegowy <- prop.table(tablica_danych) #proporcje
rozkład_brzegowy #macierz P z wykladu, macierz korespondencji, procent całosci
sumawzg <- addmargins(rozkład_brzegowy)
sumawzg
sumawzg <- as.data.frame(sumawzg)
library("writexl")
write_xlsx(sumawzg, "rozkłady_brzegowe.xlsx")
# sum - masy, profile średnie, czestosci brzegowe, bedaca podstawa do rysowania srodka wykresu
# przecietne profile wierszowe / kolumnowe
# te sum warto wykorzystac do rysowania profila sredniego
# macierz czestosci zaobserwowanych
# masa - najliczniejsza zona i byc moze bedzie to brane pod  uwage
# najliczniejszy wariant reprezentowany - pranie, 

## profile ####
profil_wierszowy_wzg <- prop.table(tablica_danych, margin = 1)
profil_wierszowy_wzg
# macierz profili wierszowych
profil_wierszowy_wzg_suma <- addmargins(profil_wierszowy_wzg, margin = 2,
                                        FUN = sum)
profil_wierszowy_wzg_suma 
profil_wierszowy_wzg_suma <- as.data.frame(profil_wierszowy_wzg_suma)

#ggplot(as.data.frame(profil_wierszowy_wzg),aes(x = colnames(profil_wierszowy_wzg)))
#matplot(profil_wierszowy_wzg, type="l")


## wykres profili ####
profile_wykres <- profil_wierszowy_wzg_suma
profile_wykres[10,] <- sumawzg[10,]
profile_wykres <- profile_wykres[,-16]
profile_wykres$Polityk <- rownames(profile_wykres)
df_long <- profile_wykres %>% pivot_longer(-Polityk, names_to = "Series", values_to = "Value")
ggplot(df_long, aes(x = Series, y = Value, color = Polityk, group = Polityk)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Profile polityków",
       x = "Cecha",
       y = "Wkład średniej wartości cechy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set3")

# macierz profili kolumnowych
profil_kolumnowy_wzg <- prop.table(tablica_danych, margin = 2)
profil_kolumnowy_wzg_suma <- addmargins(profil_kolumnowy_wzg, margin = 1,
                                        FUN = sum)
profil_kolumnowy_wzg_suma

### chi2 i wsp kontyngencji ####
chi <- chisq.test(tablica_danych)
chi
# X-squared = 130.6, df = 112, p-value = 0.1105
chi$expected
# wysokie liczebnosci oczekiwane

library(DescTools)
TschuprowT(tablica_danych)
ContCoef(tablica_danych)
CramerV(tablica_danych)

C <- ContCoef(tablica_danych)

# jezeli liczba wierszy jest rozna od liczby kolumn (w != k)
w = 9
k = 15

# Cmax - tylko do wyliczenia skorygowanego 
Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
# Ckor - skorygowany Pearson
Ckor = C/Cmax
Ckor
# niski związek między badanymi cechami


## ANALIZA KORESPONDENCJI #####
library(ca)
library(ggplot2)
library(factoextra)

danek <- ca(tablica_danych, graph = FALSE)
danek
danek$sv #wartosci osobliwe - pierwiastek z wartości własnych
ev <- get_eigenvalue(danek)
ev
inercja <- sum(ev[,1])
inercja

# wyklad
plot(danek)
# wklad wymiarow w inercje
fviz_screeplot(danek, addlabels = TRUE, ylim = c(0,50), barfill = "cornflowerblue") + 
  labs(title = "Inercja wyjaśniana przez poszczególne wymiary", x = "Wymiar", 
       y = "Procent wyjaśnianej inercji")

# dane dotyczace wierszy 
# te dane powinny byc w raporcie
row <- get_ca_row(danek)
row
#View(row)
#View(danek)
round(danek$rowmass,3)          # masy wierszowe, czestosci brzegowe wierszy, sredni profil kolumnowy
round(row$coord,3)              # wspolrzedne wariantow cechy 1 w przestrzeni (wiersze)
round(row$cos2,3)                # cos2 dla wszystkich wymiarow - traktowac jako kor wariantu z wymiarem
round(rowSums(row$cos2[,1:2]),3)# jakosc dla 2 wymiarow
round(rowSums(row$cos2[,1:3]),3)
round(row$contrib,3)             # bezwladnosc po wymiarach, suma = 1
row$inertia             # intercja dla wariantow cechy 1 (wiersze), wariancja
sum(row$inertia)
round((row$inertia/sum(row$inertia)) * 100,3) #wzgledna bezwladnosc, inercja, suma = 100%

# KOLUMNY
col <- get_ca_col(danek)
col
#View(col)
#View(danek)
round(danek$colmass,3)           # masy wierszowe, czestosci brzegowe wierszy, sredni profil kolumnowy
round(col$coord,3)               # wspolrzedne wariantow cechy 1 w przestrzeni (wiersze)
round(col$cos2,2)                # cos2 dla wszystkich wymiarow - traktowac jako kor wariantu z wymiarem
round(rowSums(col$cos2[,1:2]),3) # jakosc dla 2 wymiarow
rowSums(col$cos2[,1:3]) # jakosc dla 3 wymiarow
round(col$contrib,3)             # bezwladnosc po wymiarach, suma = 1
col$inertia             # intercja dla wariantow cechy 1 (wiersze), wariancja
sum(col$inertia)
round((col$inertia/sum(col$inertia)) * 100,3) #wzgledna bezwladnosc, inercja, suma = 100%

# mapa koncowa
fviz_ca_biplot(danek, repel = TRUE, col.col="cos2", alpha.row = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),)  +
  labs( x = "Postępowość vs tradycjonalizm (46.4%)",
       y ="Moralność vs wizerunkowość (24.6%)" )

# Przykład odwrócenia osi
danek$rowcoord[, ] <- -danek$rowcoord[, ]  # Odwrócenie osi X
danek$colcoord[, ] <- -danek$colcoord[, ]  # Odwrócenie osi Y

# Wywołanie fviz_ca_biplot z odwróconymi osiami
fviz_ca_biplot(danek, repel = TRUE, col.col = "cos2", alpha.row = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs( x = "Konserwatyzm vs postępowość (46.4%)",
        y ="Wizerunkowość vs moralność(24.6%)" )
