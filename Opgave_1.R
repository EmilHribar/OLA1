# Opgave 1 - Data Science Modellen

# Opgave 1.1 - Find data
{ # Data på ejendomme til salg i opgave 1 er fremkommet via webscrapping af 
  # boligsiden.dk. Der ligger en csv-fil i mappen OLA-opgaven. Her er to billeder
  # fra boligsiden.dk. Find de to rækker i csv-filen, som matcher de to huse.
  
# Vi starter med at indlæse filen
  library(readr)
  boligsiden <- read_csv("OLA1 - lavet igen/data/boligsiden.csv")
  View(boligsiden)

# Derefter laver vi subset, hvor vi kan finde vores rækker.
hus_1 <- subset(boligsiden,vej=="tousvej")
hus_2 <- subset(boligsiden,vej=="egevej" & vejnr==20)

}

# Opgave 1.2 - Vælg
{ # Udvælg 2 ejendomme fra csv-filen og find dem på boligsiden.dk.
opgave_1.2 <- boligsiden[sample(nrow(boligsiden),2),]
print(opgave_1.2)
# pris       vej   vejnr postnr by    kvmpris
# <chr>      <chr> <dbl>  <dbl> <chr>   <dbl>
#  1 2.650.000… roed…    60   4780 stege   10.1 
# 2 2.695.000… prov…    11   7900 nyko…    3.24

# De er ikke tjekket på boligsiden.dk
}

# Opgave 1.3 - Beskriv variabler
{ #Forklar NA-værdierne i csv-filen ud fra, hvad I har observeret i 1.1 og 1.2. 
  # Derudover gør rede for variable i mener mangler i CSV-filen sammenligned med
  # boligsiden.dk (Hint: Hvordan vil I unikt identificere en bolig til salg via 
  # boligsiden.dk)

# Se tidligere opgave.
  
}

# Opgave 1.4 - Research goal
{ # Med udgangspunkt i Data Science Modellen sakl I gøre rede for de skridt, der
  # er blevet taget for at nå frem til csv-filen. I skal komme med bud på et
  # "Research Goal" som kunne have optimeret processen med at fremskaffe data fra
  # boligsiden.dk
  
# Se tidligere opgave. 
  

}