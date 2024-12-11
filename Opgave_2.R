# Opgave 2 
# Opgave 2.1 - Beskrivende Statistik
{
# Fjerne outliers på for høj pris
{
# Fjern ".kr" og punktum fra "pris"
boligsiden_clean$pris <- gsub("\\.", "", boligsiden_clean$pris)  # Fjern punktum
boligsiden_clean$pris <- gsub(" kr", "", boligsiden_clean$pris)  # Fjern ' kr'

# Konverter til numerisk
boligsiden_clean$pris <- as.numeric(boligsiden_clean$pris)

# Vi fjerne alle boliger med pris højere end 25mio kroner
boligsiden_clean <- boligsiden_clean[boligsiden_clean$pris <= 25000000, ]

# Vi fjerner alle boliger med kvmpris over 150.000kr 
# Fjern punktum og konverter til numerisk
boligsiden_clean$kvmpris <- as.numeric(gsub("\\.", "", boligsiden_clean$kvmpris))
boligsiden_clean <- boligsiden_clean[boligsiden_clean$kvmpris >= 2000 & boligsiden_clean$kvmpris <= 150000, ]

# Vi fjerner alle boliger opført før 1800.
boligsiden_clean <- boligsiden_clean[boligsiden_clean$opført >= 1800, ]

# Vi fjerner alle boliger med md udgift over 20k
boligsiden_clean <- boligsiden_clean[boligsiden_clean$mdudg <= 20.000, ]
}

# Beskrivende statistik

# Boligpris
mean(boligsiden_clean$pris)
# 3.275.155
median(boligsiden_clean$pris)
# 2295000
sd(boligsiden_clean$pris)
# 3.183.502

# kvmpris
mean(boligsiden_clean$kvmpris)
# 17.833,65
median(boligsiden_clean$kvmpris)
# 13.466
sd(boligsiden_clean$kvmpris)
# 14895

# Månedlig ugift
mean(boligsiden_clean$mdudg)
# 2.914
median(boligsiden_clean$mdudg)
# 2.4905
sd(boligsiden_clean$mdudg)
# 1.6171
}

# Opgave 2.2 - Korrelation
# Hvad er korrelationen mellem m2 og prisen for boliger lagt på boligsiden.dk? 
# Giv en forklaring på begrebet korrelation.
{
# Vi opretter vores to variable
m2 <- boligsiden_clean$størrelse
pris <- boligsiden_clean$pris

# Beregn korrelation
cor(m2,pris)

# plot
library(ggplot2)
library(scales)
# Opret et ggplot scatterplot
ggplot(data = boligsiden_clean, aes(x = m2, y = pris)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) + # Plot punkter
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regressionslinje
  labs(
    title = "Sammenhæng mellem Boligstørrelse og Pris",
    x = "Boligstørrelse (m²)",
    y = "Pris (kr)",
    caption = "Kilde: Boligsiden"
  ) +
  scale_y_continuous(labels = comma) + # Formatter y-aksen med almindelige kommaer (f.eks. 20,000)
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )
}

# Opgave 2.3 - Simple regressioner
# Regressioner:
{
pris.m2 <- boligsiden_clean$kvmpris
# Korrelation mellem pris og månedlig udgift

# Vi opretter vores variable
mdudgift <- boligsiden_clean$mdudg

# Beregn korrelation
cor(pris.m2,mdudgift)

# Vi opretter vores variable
grund <- boligsiden_clean$grund

# Beregn korrelation
cor(pris.m2,grund)

# Vi opretter vores variable
værelser <- boligsiden_clean$værelser

# Beregn korrelation
cor(pris.m2,værelser)

# Vi opretter vores variable
opført <- boligsiden_clean$opført

# Beregn korrelation
cor(pris.m2,opført)
}

# Korrelationsmatrix

# Opret data frame med relevante variabler
korrelationsmatrix <- data.frame(
  pris.m2 = boligsiden_clean$kvmpris,
  mdudgift = boligsiden_clean$mdudg,
  grund = boligsiden_clean$grund,
  værelser = boligsiden_clean$værelser,
  opført = boligsiden_clean$opført
)

# Beregn korrelationsmatrix
korrelation_matrix <- cor(korrelationsmatrix)

# Vis korrelationsmatrix
print(korrelation_matrix)

# Plot
library(corrplot)

# Plot korrelationsmatrixen uden farver, kun tal, og vis kun den øverste halvdel
corrplot(korrelation_matrix, 
         method = "number",    # Viser korrelationerne som tal
         type = "upper",       # Viser kun den øverste del af matrixen
         col = "black",        # Ingen farve (sort tekst)
         tl.col = "black",     # Farve på teksten
         tl.srt = 0,           # Rotér teksten i 0 grader
         number.cex = 0.8)     # Juster størrelse på talene (kan ændres efter behov)


