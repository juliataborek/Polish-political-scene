library(readxl)

pdane1 <- read_excel("C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Politycy BAZA AWCJ 2024.xlsx")
pdane1 <- as.data.frame(pdane1)
str(pdane1)

pdane1 <- pdane1[,136:157]

pdane <- pdane1

## ZBADANIE KORELACJI ####
library(corrplot)
library(RColorBrewer)
library(ggplot2)

Spearman = cor(pdane, method = c("spearman")) 
Spearman
pkor <- round(Spearman,2)
pkor
#write_xlsx(as.data.frame(pkor), "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Korelacja_pytan.xlsx")

corrplot(pkor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos = 'd')
corrplot(pkor, method = 'ellipse', order = 'AOE', type = 'upper')

### WYBÓR ZMIENNYCH DO ANALIZY ####
library(psych)

# test Bartletta
cortest.bartlett(pdane)
KMO(pdane)
pdane

# dane bez dotrzymywania obietnic wyborczych - x3
pdaneb3 <- pdane[,-3]
cortest.bartlett(pdaneb3)
KMO(pdaneb3)

# dane bez składki zdrowotnej - x17
pdaneb17 <- pdaneb3[,-16]
cortest.bartlett(pdaneb17)
KMO(pdaneb17)

# dane bez wojska - x14
pdaneb14 <- pdaneb17[,-13]
cortest.bartlett(pdaneb14)
KMO(pdaneb14)

# dane bez sluzby wojskowej - x15
pdaneb15 <- pdaneb14[,-13]
cortest.bartlett(pdaneb15)
KMO(pdaneb15)

# korzystamy z danych bez x3,17,x14,x15
pdf <- pdaneb15
Spearman = cor(pdf, method = c("spearman")) 
Spearman
pkor <- round(Spearman,2)
pkor
corrplot(pkor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos = 'd')
corrplot(pkor, method = 'ellipse', order = 'AOE', type = 'upper')

## PYTANIA POWYŻEJ 0,6  ####
# ustalenie liczby czybbików stosujac Very Simple Structure (VSS) Analysis
vss(pdf)

# ustalanie liczby czynników metodą Parallel Analysis
fa.parallel(pdf)

#fa - factor analysis
fa.parallel(pdf, fa='fa', fm='pa', main='Scree Plot') # metoda principal axis
abline(h=1, col = 'green', lwd = 2, lty =  2) # macierz danych, Pearson

fa.parallel(pdf, fa='fa', fm='ml', main='Scree Plot') # metoda najwiekszej wiarygodnosci
abline(h=1, col = 'green', lwd = 2, lty =  2) # macierz danych, Pearson

fa.parallel(pkor, fa='fa', fm='pa', main='Scree Plot') # metoda principal axis
abline(h=1, col = 'green', lwd = 2, lty =  2) # Spearman

fa.parallel(pkor, fa='fa', fm='minrest', main='Scree Plot') # metoda minrest
abline(h=1, col = 'green', lwd = 2, lty =  2) # Spearman

#### PCA #####
# analiza głownych skladowych
library(psych)

# metoda bazująca na macierzy korelacji Pearsona
# wszystko co nierotowane to 0
pc0 <- principal(r=pdf, 18, rotate = 'none', cor = TRUE)
pc0
# h2 zawsze 1 w Pearsonie
# u2 - swoistość


# 2 to liczba czynników jaką uznamy, że w tym badaniu wystepują
pc1 <- principal(r=pdf,3, rotate = 'none', cor = TRUE)
pc1

pc2 <- principal(r=pdf,3, rotate = 'varimax', cor = TRUE)
pc2

# wykresy 
fa.diagram(pc2)

# PCA z macierzą Spearmana 
pc3 <- principal(pkor,3,rotate = 'none')
pc3
fa.diagram(pc3)


# PCA z macierzą Spearmana i rotacją varimax
pc4 <- principal(pkor,3,rotate = 'varimax')
pc4
fa.diagram(pc4)

library(GPArotation)
# tylko do
pc5 <- principal(pdf,3,rotate = 'oblimin', cor = TRUE)
pc5
fa.diagram(pc5)

#### METODA NAJWIEKSZEJ WIARYGODNOSCI ####
ml0 <- fa(pkor, nfactors = 3, rotate = 'none', fm='ml',
          residuals = TRUE)
ml0

ml1 <- fa(pkor, nfactors = 3, rotate = 'varimax', fm='ml',
          residuals = TRUE)
ml1
fa.diagram(ml1)


#### METODA MINRES ####
library(GPArotation)
mm0 <-  fa(pkor, nfactors = 3, rotate = 'none', fm='minres')
mm0


mm1 <-  fa(pkor, nfactors = 3, rotate = 'varimax', fm='minres')
mm1

mm2 <-  fa(pkor, nfactors = 3, rotate = 'quartimax', fm='minres')
mm2

mm3 <-  fa(pkor, nfactors = 3, rotate = 'equamax', fm='minres')
mm3

mm4 <-  fa(pkor, nfactors = 3, rotate = 'varimin', fm='minres')
mm4

# przyciecie ladunkow do danego poziomu
# ulatwia analize
print(fa(pkor, nfactors = 3, rotate = 'varimin', fm='minres')$loadings, cut =0.5)

fa.diagram(mm4)

#### METODA OSI GŁOWNYCH ####
# mozemy stosowac z roznymi rotacjami
pa2 <- fa(r=pkor, 3, fm='pa', rotate = 'varimax')$loadings
pa2
print(fa(r=pkor, 3, fm='pa', rotate = 'varimax')$loadings, cut =0.5)
fa.diagram(pa2)


## WYŻSZY PRÓG - 0,7####
pdaneb15
KMO(pdaneb15)

# dane bez tv - x10
pdaneb10 <- pdaneb15[,-9]
cortest.bartlett(pdaneb10)
KMO(pdaneb10)

# dane bez pensji - x1
pdaneb1 <- pdaneb10[,-1]
cortest.bartlett(pdaneb1)
KMO(pdaneb1)


# politycy data frame uzywana do factor analysis
pdf <- pdaneb1
Spearman = cor(pdf, method = c("spearman")) 
Spearman
pkor <- round(Spearman,2)
pkor
#write_xlsx(as.data.frame(pkor), "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/Korelacja_pytan2.xlsx")

corrplot(pkor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos = 'd')
corrplot(pkor, method = 'ellipse', order = 'AOE', type = 'upper')



# ustalenie liczby czybbików stosujac Very Simple Structure (VSS) Analysis
vss(pdf)

# ustalanie liczby czynników metodą Parallel Analysis
fa.parallel(pdf)

#fa - factor analysis
fa.parallel(pdf, fa='fa', fm='pa', main='Scree Plot') # metoda principal axis
abline(h=1, col = 'green', lwd = 2, lty =  2) # macierz danych, Pearson

fa.parallel(pdf, fa='fa', fm='ml', main='Scree Plot') # metoda najwiekszej wiarygodnosci
abline(h=1, col = 'green', lwd = 2, lty =  2) # macierz danych, Pearson

fa.parallel(pkor, fa='fa', fm='pa', main='Scree Plot') # metoda principal axis
abline(h=1, col = 'green', lwd = 2, lty =  2) # Spearman

fa.parallel(pkor, fa='fa', fm='minrest', main='Scree Plot') # metoda minrest
abline(h=1, col = 'green', lwd = 2, lty =  2) # Spearman

#### PCA #####
# analiza głownych skladowych
library(psych)

# metoda bazująca na macierzy korelacji Pearsona
# wszystko co nierotowane to 0
pc0 <- principal(r=pdf, 16, rotate = 'none', cor = TRUE)
pc0


# uznaje, że są 2 czynniki
pc1 <- principal(r=pdf,2, rotate = 'none', cor = TRUE)
pc1
fa.diagram(pc1)

pc2 <- principal(r=pdf,2, rotate = 'varimax', cor = TRUE)
pc2

# 3 czynniki
pc1 <- principal(r=pdf,3, rotate = 'none', cor = TRUE)
pc1
fa.diagram(pc1)

pc2 <- principal(r=pdf,3, rotate = 'varimax', cor = TRUE)
pc2

# wykresy 
fa.diagram(pc2)
# najgorsza sytuacja z 22 a potem z 21

# PCA z macierzą Spearmana 
pc3 <- principal(pkor,3,rotate = 'none')
pc3
fa.diagram(pc3)

pc4 <- principal(pkor,3,rotate = 'varimax')
pc4
fa.diagram(pc4)

pc3 <- principal(pkor,3,rotate = 'none')
pc3
fa.diagram(pc3)
# 3 ujemne korelacje


# PCA z macierzą Spearmana i rotacją varimax
pc4 <- principal(pkor,3,rotate = 'varimax')
pc4
fa.diagram(pc4)
# 9 cecha zla sytuacja com 2,9

library(GPArotation)
# tylko do
pc5 <- principal(pdf,3,rotate = 'oblimin', cor = TRUE)
pc5
fa.diagram(pc5)

#### METODA NAJWIEKSZEJ WIARYGODNOSCI ####
ml0 <- fa(pkor, nfactors = 3, rotate = 'none', fm='ml',
          residuals = TRUE)
ml0
fa.diagram(ml0)

ml1 <- fa(pkor, nfactors = 3, rotate = 'varimax', fm='ml',
          residuals = TRUE)
ml1
fa.diagram(ml1)


#### METODA MINRES ####
#library(GPArotation)
mm0 <-  fa(pkor, nfactors = 3, rotate = 'none', fm='minres')
mm0
fa.diagram(mm0)

mm1 <-  fa(pkor, nfactors = 3, rotate = 'varimax', fm='minres')
mm1

mm2 <-  fa(pkor, nfactors = 3, rotate = 'quartimax', fm='minres')
mm2
fa.diagram(mm2)

mm3 <-  fa(pkor, nfactors = 3, rotate = 'equamax', fm='minres')
mm3
fa.diagram(mm3)

mm4 <-  fa(pkor, nfactors = 3, rotate = 'varimin', fm='minres')
mm4
fa.diagram(mm4)

# przyciecie ladunkow do danego poziomu
# ulatwia analize
print(fa(pkor, nfactors = 3, rotate = 'varimin', fm='minres')$loadings, cut =0.5)

fa.diagram(mm4)

#### METODA OSI GŁOWNYCH ####
# mozemy stosowac z roznymi rotacjami
pa2 <- fa(r=pkor, 3, fm='pa', rotate = 'varimax')$loadings
pa2
print(fa(r=pkor, 3, fm='pa', rotate = 'varimax')$loadings, cut =0.5)
fa.diagram(pa2)

## BEZ PYT22 ####
pdaneb22 <- pdaneb1[,-16]
cortest.bartlett(pdaneb22)
KMO(pdaneb22)


# politycy data frame uzywana do factor analysis
pdf <- pdaneb22
Spearman = cor(pdf, method = c("spearman")) 
Spearman
pkor <- round(Spearman,2)
pkor

corrplot(pkor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos = 'd')
corrplot(pkor, method = 'ellipse', order = 'AOE', type = 'upper')

# ustalenie liczby czybbików stosujac Very Simple Structure (VSS) Analysis
vss(pdf)

# ustalanie liczby czynników metodą Parallel Analysis
fa.parallel(pdf)

#fa - factor analysis
fa.parallel(pdf, fa='fa', fm='pa', main='Scree Plot') # metoda principal axis
abline(h=1, col = 'green', lwd = 2, lty =  2) # macierz danych, Pearson

fa.parallel(pdf, fa='fa', fm='ml', main='Scree Plot') # metoda najwiekszej wiarygodnosci
abline(h=1, col = 'green', lwd = 2, lty =  2) # macierz danych, Pearson

fa.parallel(pkor, fa='fa', fm='pa', main='Scree Plot') # metoda principal axis
abline(h=1, col = 'green', lwd = 2, lty =  2) # Spearman

fa.parallel(pkor, fa='fa', fm='minrest', main='Scree Plot') # metoda minrest
abline(h=1, col = 'green', lwd = 2, lty =  2) # Spearman

#### PCA #####
# analiza głownych skladowych

# metoda bazująca na macierzy korelacji Pearsona
# wszystko co nierotowane to 0
pc0 <- principal(r=pdf, 15, rotate = 'none', cor = TRUE)
pc0
pc0$values
# h2 zawsze 1 w Pearsonie
# u2 - swoistość


pc1 <- principal(r=pdf,3, rotate = 'none', cor = TRUE)
pc1
fa.diagram(pc1)

pc2 <- principal(r=pdf,3, rotate = 'varimax', cor = TRUE)
pc2

# wykresy 
fa.diagram(pc2)

# PCA z macierzą Spearmana 
pc3 <- principal(pkor,3,rotate = 'none')
pc3
fa.diagram(pc3)
# 3 ujemne korelacje


# PCA z macierzą Spearmana i rotacją varimax
pc4 <- principal(pkor,3,rotate = 'varimax')
pc4
fa.diagram(pc4)
# 9 cecha zla sytuacja com 2,9

library(GPArotation)
# tylko do
pc5 <- principal(pdf,3,rotate = 'oblimin', cor = TRUE)
pc5
fa.diagram(pc5)

#### METODA NAJWIEKSZEJ WIARYGODNOSCI ####
ml0 <- fa(pkor, nfactors = 3, rotate = 'none', fm='ml',
          residuals = TRUE)
ml0
fa.diagram(ml0)

ml1 <- fa(pkor, nfactors = 3, rotate = 'varimax', fm='ml',
          residuals = TRUE)
ml1
fa.diagram(ml1)


#### METODA MINRES ####
#library(GPArotation)
mm0 <-  fa(pkor, nfactors = 3, rotate = 'none', fm='minres')
mm0
fa.diagram(mm0)

mm1 <-  fa(pkor, nfactors = 3, rotate = 'varimax', fm='minres')
mm1
fa.diagram(mm1)

mm2 <-  fa(pkor, nfactors = 3, rotate = 'quartimax', fm='minres')
mm2
fa.diagram(mm2)

mm3 <-  fa(pkor, nfactors = 3, rotate = 'equamax', fm='minres')
mm3
fa.diagram(mm3)

mm4 <-  fa(pkor, nfactors = 3, rotate = 'varimin', fm='minres')
mm4
fa.diagram(mm4)

# przyciecie ladunkow do danego poziomu
# ulatwia analize
print(fa(pkor, nfactors = 3, rotate = 'varimin', fm='minres')$loadings, cut =0.5)

fa.diagram(mm4)

#### METODA OSI GŁOWNYCH ####
# mozemy stosowac z roznymi rotacjami
pa0 <- fa(r=pkor, 3, fm='pa', rotate = 'none')
pa0
fa.diagram(pa0)

pa2 <- fa(r=pkor, 3, fm='pa', rotate = 'varimax')
pa2
print(fa(r=pkor, 3, fm='pa', rotate = 'varimax')$loadings, cut =0.5)
fa.diagram(pa2)

#### TEST ALFA CRONBACHA #####


# w tym momencie zaczniemy dzielic w zaleznosci od czynnikow

czynnik1 <- pdf[,c(5:7,9,14)]
czynnik1
czynnik2 <- pdf[,c(1,3:4,10,13)]
czynnik2
czynnik3 <- pdf[,c(2,8,11,12,15)]
czynnik3

pdf$Pyt_2_8 <- recode(pdf$Pyt_2_8, "1 = 7; 2 =6 ; 3=5; 4=4; 5=3; 6=2; 7=1")
czynnik1 <- pdf[,c(5:7,9,14)]
czynnik1


# wysokie wsp Cronbacha swiadcza o wewnetrznej spojnosci

czyn1 <- cor(czynnik1)
czyn2 <- cor(czynnik2)
czyn3 <- cor(czynnik3)


par(mfrow=c(1,3)) 

corrplot(czyn1, method='square', diag =FALSE, order = 'hclust', 
         addrect = 3, rect.col = 'darkblue', rect.lwd = 2, tl.pos = 'd')
mtext("czynnik 1", side=3, line=2, cex=1.5)  

corrplot(czyn2, method='square', diag =FALSE, order = 'hclust', 
         addrect = 3, rect.col = 'darkblue', rect.lwd = 2, tl.pos = 'd')
mtext("czynnik 2", side=3, line=2, cex=1.5)  

corrplot(czyn3, method='square', diag =FALSE, order = 'hclust', 
         addrect = 3, rect.col = 'darkblue', rect.lwd = 2, tl.pos = 'd')
mtext("czynnik 3", side=3, line=2, cex=1.5)  

#write_xlsx(pdf, "C:/Users/julia/Documents/informatyka i ekonometria/semestr 2/analiza wielowymiarowa cech jakościowych/pdf.xlsx")


# CronbachAlpha
library(DescTools)
CronbachAlpha(czynnik1)
CronbachAlpha(czynnik2)
CronbachAlpha(czynnik3)



