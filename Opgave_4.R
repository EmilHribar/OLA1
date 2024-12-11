#Opgave 4 - Danskernes forhold til alkohol
#Opgave 4.1 - Hent Data
#Hent data fra tabel FU02, alle forbrugsgrupper under 02.1 (alkoholiske drikkevarer) i faste priser for
#perioden 2000 til 2022 og indlæs i R. Illustrer udviklingen i de enkelte grupper.

#Først indlæser vi (readxl) for at R kan indlæse excel filer.
library(readxl)

#Dernæst indlæser vi vores excel fil fra dst.dk 
alkohol_dataset_excel <- read_excel("data/alkohol_dataset excel.xlsx")

#Nu bruger vi views funktionen for at vise datasættet vi får frem
View(alkohol_dataset_excel)

#Nu ønsker jeg at gøre mit datasæt pænt.
#Jeg fjerner første kolonne. Dernæst fjerner jeg de to første rækker. 
#Til sidst fjerner jeg 12:13 række.
alkohol_dataset_excel <- alkohol_dataset_excel[-1,]
alkohol_dataset_excel <- alkohol_dataset_excel[-12,]
alkohol_dataset_excel <- alkohol_dataset_excel[-12,]

#Nu vender jeg min tabel, så jeg kan lave et plot ud fra dataframen.
alkohol_dataset_excel1 <- t(alkohol_dataset_excel)


#Nu skifter jeg alle tal i datasættet fra at være "characters" til "numbers".
alkohol_dataset_excel2 <-  matrix(as.numeric(alkohol_dataset_excel1),nrow = nrow(alkohol_dataset_excel1)
                                  ,ncol = ncol(alkohol_dataset_excel1))
df_alkohol_dataset_excel <- as.data.frame(alkohol_dataset_excel2)

#Nu navngiver jeg alle rækker, så de har de rigtige navne.
colnames(df_alkohol_dataset_excel)[1] <- "År"
colnames(df_alkohol_dataset_excel)[2] <- "Spiritus og likør"
colnames(df_alkohol_dataset_excel)[3] <- "Alkoholiske læskedrikke"
colnames(df_alkohol_dataset_excel)[4] <- "Vin af druer"
colnames(df_alkohol_dataset_excel)[5] <- "Vin af andre frugter"
colnames(df_alkohol_dataset_excel)[6] <- "Hedvin"
colnames(df_alkohol_dataset_excel)[7] <- "Vinbaserede drikkevarer og alkoholfri vin"
colnames(df_alkohol_dataset_excel)[8] <- "Pilsnerøl, guldøl"
colnames(df_alkohol_dataset_excel)[9] <- "Andre alkoholholdige øl"
colnames(df_alkohol_dataset_excel)[10] <- "Øl med lavt alkoholindhold og alkoholfri øl"
colnames(df_alkohol_dataset_excel)[11] <- "Øl-baserede drikkevarer"

# Vi sletter to overskydene kolonner øverst
df_alkohol_dataset_excel <- df_alkohol_dataset_excel[c(-1:-2),]

# Vi indlæser nu de to pakker, som vi skal bruge for at lave ggplots.
library(ggplot2)
library(tidyr)

#For at vi skal kunne lave et plot i ggplot, skal vi have lavet vores dataframe i "long format".
#Det gør vi ved hjælp af "pivot_longer()"
df_long <- df_alkohol_dataset_excel %>%
  pivot_longer(cols = -År,  # Alle kolonner pånær "År" skal samles
               names_to = "Forbrugstype",  # Nyt kolonne navn for kategori
               values_to = "Forbrugsværdi")  # Ny kolonne for værdi

# Vi viser lige vores nye dataframe
head(df_long)

# Nu plotter vi dataen i ggplot
ggplot(df_long, aes(x = År, y = Forbrugsværdi, color = Forbrugstype, group = Forbrugstype)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 2) +  # Optionally, add points on the lines
  labs(title = "Alkoholforbrug for husstande 2000-2022",
       x = "År",
       y = "Forbrug per hustand, kr",
       color = "Forbrugstype") +  # Add a legend title
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Nu klipper vi plottet i to, så vi bedre kan se udviklingen.
# Filter data for values below 565
df_below_565 <- df_long %>%
  filter(df_long$Forbrugsværdi < 565)

# Filter data for values equal to or above 565
df_above_565 <- df_long %>%
  filter(df_long$Forbrugsværdi >= 565)


#Nu plotter vi de to nye plots.
# Plot for values below 565
plot_below_565 <- ggplot(df_below_565, aes(x = År, y = Forbrugsværdi, color = Forbrugstype, group = Forbrugstype)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 2) +  # Optionally, add points on the lines
  labs(title = "Alkoholforbrug per husstand (Værdi under 565)",
       x = "Year",
       y = "Forbrug per husstand, kr",
       color = "Forbrugstype") +  # Add a legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the first plot
plot_below_565

#Opgave 4.2 Lav en korrelationsmatrix over forbrugsgrupperne under 02.1 og konkludér på resultaterne

#Vi ønsker ikke at have "År" med i vores korrelationsmatrix. Derfor ekskluderer vi denne.
df_alkohol_dataset_excel_uden_År <- df_alkohol_dataset_excel[,-1]

# Compute correlation matrix
correlation_matrix <- cor(df_alkohol_dataset_excel_uden_År, use = "complete.obs")


#Nu vil vi gerne plotte korrelationen i et plot.

# Vi indlæser pakken i R.
library(ggcorrplot)

# Her blotter vi korrelationsmatrixen ved hjælp af ggcorrplot
ggcorrplot(correlation_matrix, 
           method = "square",       # Firkantet layout
           type = "upper",          # Kun øverste trekant af matrix
           lab = TRUE,              # Vis tal
           lab_size = 3,            # Juster størrelse på talene
           tl.col = "black",        # Sort tekst for labels
           tl.srt = 45,             # Drej labels 45 grader
           outline.color = "black", # Sort kant på felterne
           colors = c("white", "white", "white")) # Ensfarvede bokse (ingen farver)


# Print the correlation matrix
print(correlation_matrix)
