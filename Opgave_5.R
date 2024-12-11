# Opgave 5 - Dataframes
# Opgave 5.1 – Månedlige observationer
{
# Lav en 36 x 3 dataframe med kolonnenavne ”Klasse”,”Uge”,”Score”. Første 
# kolonne skal fyldes med A,B,C,D så der startes med 9 A’er, derpå 9 B’er osv. 
# Anden kolonne skal fyldes med tallene 1 til 9, der gentages for hvert bogstav.
# Sidste kolonne skal fyldes med observationer. Det er op til jer, hvilke værdier I 
# vil putte i framen. (hint: benyt R-funktionen seq())

# Vi laver vores df
opgave_5.1 <- as.data.frame(matrix(NA, nrow = 36, ncol = 3))

# Vi angiver kolonnenavne
colnames(opgave_5.1) <- c("Klasse","Uge","Score")

# Vi sætter A,B,C og D ind
opgave_5.1$Klasse <- rep(c("A","B","C","D"), each = 9)

# Vi sætter ugenumre ind
opgave_5.1$Uge <- rep(1:9,times=4)

# Vi sætter scores ind
opgave_5.1$Score <- sample(c(-3,00,02,4,7,10,12),36,replace = TRUE)

# Barplot af scoren
library(ggplot2)
ggplot(opgave_5.1, aes(x = Klasse, y = Score, fill = Klasse)) +
  stat_summary(fun = mean, geom = "bar", width = 0.7) +  # Summerer score pr. klasse
  labs(title = "Samlet Score pr. Klasse", 
       x = "Klasse", 
       y = "Samlet Score") +
  theme_minimal() +
  theme(legend.position = "none")  # Fjerner legend (valgfrit)

# Plot over udvikling i ugerne
ggplot(opgave_5.1, aes(x = Uge, y = Score, color = Klasse, group = Klasse)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +  # Linje for gennemsnit pr. klasse
  stat_summary(fun = mean, geom = "point", size = 3) +   # Punkter for gennemsnit pr. klasse
  labs(title = "Udvikling i Gennemsnitlig Score pr. Uge fordelt på Klasser", 
       x = "Uge", 
       y = "Gennemsnitlig Score", 
       color = "Klasse") +
  theme_minimal()

}

# Opgave 5.2 - Kvartalsvise observationer
{
# Vi laver vores df
opgave_5.2 <- data.frame(Klasse = character(), Uge = character(),
                         Gennemsnit = numeric())

# Loop igennem dataframe aggreger hver tredje uge
for (i in seq(3,nrow(opgave_5.1), by = 3)) {
  
  # Udvælg de tre uger, som skal inkluderes i gennemsnitsscoren
  uge_data <- opgave_5.1$Score[(i-2):i]
  
  # Beregn gennemsnit af de 3 uger
  gns_score <- mean(uge_data)

  # Vi ønsker at definere ugerne som 1-3, 4-6, 7-9 osv.
  periode <- paste(opgave_5.1$Uge[i-2], "-",opgave_5.1$Uge[i], sep = "")
  
  # Opret en ny række til Opgave_5.2
  ny_kolonne <- data.frame(
    Klasse = opgave_5.1$Klasse[i],
    Uge = periode,
    Gennemsnit = gns_score
  )
  
  # Tilføj den nye række til Opgave_5.2
  opgave_5.2 <- rbind(opgave_5.2,ny_kolonne)
}
# Til sidst afrunder vi tallene
opgave_5.2$Gennemsnit <- round(opgave_5.2$Gennemsnit,1)
}

# Opgave 5.3 - Pivot
# I skal nu konvertere denne nye dataframe til en ny dataframe som har følgende navne på kolonnerne:
# ”Uge”,”A”,”B”,”C”,D” og rækkerne indeholder de gennemsnit som I har beregnet.

library(tidyr)

# Pivot opgave_5.2
Opgave_5.3 <- pivot_wider(
  opgave_5.2, 
  names_from = Klasse,  # Kolonner dannes ud fra 'Klasse'
  values_from = Gennemsnit  # Værdier tages fra 'Gennemsnit'
)

# Print resultatet
print(Opgave_5.3)

