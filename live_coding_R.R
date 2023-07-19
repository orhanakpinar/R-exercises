#first time connecting R to Git. Also first time using Git
#got experience with visualisation, used in a thesis.
#I will live code, therefore I would need to think twice before commiting.
#normally I re-edit mistaken codes a lot, this would be a good exercise as well.
#First commit on github.com, checking R studio. On live soon
#plus this is useful for code documentation in general
#also as a coding diary; for dana analysis with R

#can't set yet, I will make an xy plane by random triangles 
#limits = 100 for x & y, first create samples,
#then combinde 3 by 3 for grouping,
#then create another 2 randon sample with 3 additional features. Then visualize

#I still couldn't figured it out, having problems about my administrator.
#I also might need to arrange some windows system files.
#is this the first commit? 
#is this the first commit? 

library(tidyverse)

a <-(1:100)
b <- sample(1000, 100, replace=T)

ab <- tibble(a,b)

#it's working

#let's create a deck of cards

cardsuit <-  rep(c("spades", "clubs", "hearts", "diamonds"), each=13)
cardlabel <- rep(c("ace", (2:10), "jack", "queen", "king"), times=4)
cardcolor <- rep(c("black", "red"), each=26)

carddeck <- tibble(cardsuit, cardlabel, cardcolor) 

view(carddeck)
#ready to play...
