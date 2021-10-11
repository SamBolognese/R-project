# Inlämningsuppgift 1: "Miljö och förtroende i Stockstadens kommun"
# Kurs och termin: A5 Tillämpad statistik, HT20
# Samuel Bucht Stjernman

options(encoding = "UTF-8")
setwd("C:/Users/Samuel/Desktop/Statistik A/R-projekt/Inlämningsuppgift_1")

# Läser in datan
data <- readRDS("stockstaden.rds")

# Inspekterar datan
View(data)
summary(data)
summary(data$V246)

#---------1.TVÄTT--------------------------------
# V240 ska inte innehålla värden under 0
data$V240[data$V240 < 0] <- NA

# V246 innehåller stavfel
data$V246[data$V246 == "Univertet, högskola: med examen"] <- "Universitet, högskola: med examen"

# V119 ska inte innehålla värden under 1
data$V119[data$V119 < 1] <- NA

# V124 ska inte innehålla värden under 1
data$V124[data$V124 < 1] <- NA

# V81 ska inte innehålla värden under -3 eller över 2
data$V81[(data$V81 < -3) | (data$V81 > 2)] <- NA

# V193 ska inte innehålla värden under 1
data$V193[data$V193 < 1] <- NA

#--------2.utbildning_kat------------------------

data$utbildning_kat <- NA
data$utbildning_kat[(data$V246 == "Universitet, högskola: utan examen") | (data$V246 == "Universitet, högskola: med examen")] <- "eftergymnasial"
data$utbildning_kat[(data$V246 == "Folkhögskola, realskola") | (data$V246 == "") | (data$V246 == "Gymnasium: Praktisk yrkesförberedande") | (data$V246 == "Gymnasium: Studieförberedande")] <- "gymnasial"
data$utbildning_kat[is.na(data$utbildning_kat)] <- "grundskola"

summary(data$utbildning_kat)

#-------3.Binära förtroendevariabler-------------

data$V110_bin <- NA
data$V110_bin[data$V110 <= 2] = 1
data$V110_bin[data$V110 >= 3] = 0

data$V113_bin <- NA
data$V113_bin[data$V113 <= 2] = 1
data$V113_bin[data$V113 >= 3] = 0

data$V114_bin <- NA
data$V114_bin[data$V114 <= 2] = 1
data$V114_bin[data$V114 >= 3] = 0

data$V116_bin <- NA
data$V116_bin[data$V116 <= 2] = 1
data$V116_bin[data$V116 >= 3] = 0

data$V119_bin <- NA
data$V119_bin[data$V119 <= 2] = 1
data$V119_bin[data$V119 >= 3] = 0
  
data$V120_bin <- NA
data$V120_bin[data$V120 <= 2] = 1
data$V120_bin[data$V120 >= 3] = 0

data$V122_bin <- NA
data$V122_bin[data$V122 <= 2] = 1
data$V122_bin[data$V122 >= 3] = 0

data$V124_bin <- NA
data$V124_bin[data$V124 <= 2] = 1
data$V124_bin[data$V124 >= 3] = 0

data$V125_bin <- NA
data$V125_bin[data$V125 <= 2] = 1
data$V125_bin[data$V125 >= 3] = 0

data$V126_bin <- NA
data$V126_bin[data$V126 <= 2] = 1
data$V126_bin[data$V126 >= 3] = 0

#-------4.förtroende_index-----------------------
data$förtroende_index <- NA
data$förtroende_index <- rowSums(data[21:30], na.rm = TRUE)

summary(data$förtroende_index)

#-------Andel invånare givit pengar till M.Ö.----

data$V82_bin <- 0
data$V82_bin[data$V82 == 1] <- 1

# Populationsstorlek 
N <- 650000
# Stickprovsstorlek  
n <- 1206
# Medelvärde  
xbar <- mean(data$V82_bin)
# Stickprovsvarians  
s2 <- var(data$V82_bin)
# Skattningen av stickprovsmedelvärdets varians  
vhatxbar <- (1 - n/N)*(s2/n)
# Konfidensgraden 100(1-alpha)%.  
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
# Konfidensintervallets gränser  
ll <- xbar - z_alpha*sqrt(vhatxbar)
ul <- xbar + z_alpha*sqrt(vhatxbar)
# Resultat
resultat <- c(xbar, ll, ul)
resultat

# Efterstratifiering med kön
# Populationsstorlek 
N_f <- 305500
N_m <- 344500
N <- N_f + N_m
# Stickprovsstorlek  
n_f <- 637
n_m <- 569
n <- n_f + n_m
# Medelvärde  
xbar_f <- mean(data$V82_bin[data$V238 == "Kvinna"])
xbar_m <- mean(data$V82_bin[data$V238 == "Man"])
xbar_st <- (N_f/N)*xbar_f + (N_m/N)*xbar_m
# Stickprovsvarians  
s2_f <- var(data$V82_bin[data$V238 == "Kvinna"])
s2_m <- var(data$V82_bin[data$V238 == "Man"])
# Skattningen av stickprovsmedelvärdets varians  
vhatxbar_f <- (1 - n_f/N_f)*(s2_f/n_f)    
vhatxbar_m <- (1 - n_m/N_m)*(s2_m/n_m)  
vhatxbar_st <- (N_f/N)^2*vhatxbar_f +(N_m/N)^2*vhatxbar_m
# Konfidensgraden 100(1-alpha)%.  
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
# Konfidensintervallets gränser  
ll <- xbar_st - z_alpha*sqrt(vhatxbar_st)
ul <- xbar_st + z_alpha*sqrt(vhatxbar_st)
# Resultat
resultat <- c(xbar_st, ll, ul)
resultat

#-------Samband utbildning donationer------------
# Dessa kommer läggas in i en matris
donation_efter <- data$V82_bin[data$utbildning_kat == "eftergymnasial"]
donation_gymn <- data$V82_bin[data$utbildning_kat == "gymnasial"]
donation_grund <- data$V82_bin[data$utbildning_kat == "grundskola"]

# Skapar en matris som tabell 2 i undersökningen kommer baseras på
don_matrix <- cbind(mean(donation_grund), mean(donation_gymn), mean(donation_efter))

# Chi-squared test för att se om skillnaderna statistiskt signifikanta
freq_table <- table(data$V82_bin, data$utbildning_kat)
chisq.test(freq_table)

#-------Invånarnas förtroende--------------------
# Skapar en dataframe med namn på institutionerna och -
# antalet svar som är Mycket stort förtroende eller ganska stort förtroende
förtroende_df <- data.frame(namn = c("Tidningar", "Polis", "Domstolar", "Partier", "Universiteten", "Företagen", "Miljöorganisationer", "Biståndsorganisationer", "EU", "FN"),
                          förtroende = c(sum(data$V110_bin, na.rm = TRUE), sum(data$V113_bin, na.rm = TRUE), sum(data$V114_bin, na.rm = TRUE), sum(data$V116_bin, na.rm = TRUE), sum(data$V119_bin, na.rm = TRUE), sum(data$V120_bin, na.rm = TRUE), sum(data$V122_bin, na.rm = TRUE), sum(data$V124_bin, na.rm = TRUE), sum(data$V125_bin, na.rm = TRUE), sum(data$V126_bin, na.rm = TRUE)))

#Kollar att datan ser korrekt ut
View(förtroende_df)

# Barplot som visar sum av de binära förtroendevariablerna för alla institutioner
par(mar=c(10, 5, 4, 2))
barplot(förtroende_df$förtroende, 
        names.arg = förtroende_df$namn, 
        ylim=range(pretty(c(0, förtroende_df$förtroende))),
        las = 2,
        col = terrain.colors(11),
        border = "white",
        ylab = "Antal individer med mycket stort- eller ganska stort förtroende")

#-------Diagram förtroendeindex------------------
# Skapar data framen som kommer användas för att skapa en bar plot
förtroendeindex_df <- data.frame(förtroende = c(
  length(data$förtroende_index[data$förtroende_index == 0]),
  length(data$förtroende_index[data$förtroende_index == 1]),
  length(data$förtroende_index[data$förtroende_index == 2]),
  length(data$förtroende_index[data$förtroende_index == 3]),
  length(data$förtroende_index[data$förtroende_index == 4]),
  length(data$förtroende_index[data$förtroende_index == 5]),
  length(data$förtroende_index[data$förtroende_index == 6]),
  length(data$förtroende_index[data$förtroende_index == 7]),
  length(data$förtroende_index[data$förtroende_index == 8]),
  length(data$förtroende_index[data$förtroende_index == 9]),
  length(data$förtroende_index[data$förtroende_index == 10])
),
namn = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ))

# Kollar att datan ser korrekt ut
View(förtroendeindex_df)

# Skapar bar plot av förtroendeindex_df och fixar utseendet
barplot(förtroendeindex_df$förtroende, 
        names.arg = förtroendeindex_df$namn,
        ylim=range(pretty(c(0, förtroendeindex_df$förtroende))),
        col = rainbow(30),
        border = "white",
        ylab = "frekvens",
        xlab = "en individs summerade förtroende"
        )

#-------Genomsnittligt förtroendeindex-----------
# Populationsstorlek 
N <- 650000
# Stickprovsstorlek  
n <- 1206
# Medelvärde  
xbar <- mean(data$förtroende_index)
# Stickprovsvarians  
s2 <- var(data$förtroende_index)
# Skattningen av stickprovsmedelvärdets varians  
vhatxbar <- (1 - n/N)*(s2/n)
# Konfidensgraden 100(1-alpha)%.  
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
# Konfidensintervallets gränser  
ll <- xbar - z_alpha*sqrt(vhatxbar)
ul <- xbar + z_alpha*sqrt(vhatxbar)
# Resultat
resultat <- c(xbar, ll, ul)
resultat

# Efterstratifiering med kön
# Populationsstorlek 
N_f <- 305500
N_m <- 344500
N <- N_f + N_m
# Stickprovsstorlek  
n_f <- 637
n_m <- 569
n <- n_f + n_m
# Medelvärde  
xbar_f <- mean(data$förtroende_index[data$V238 == "Kvinna"])
xbar_m <- mean(data$förtroende_index[data$V238 == "Man"])
xbar_st <- (N_f/N)*xbar_f + (N_m/N)*xbar_m
# Stickprovsvarians  
s2_f <- var(data$förtroende_index[data$V238 == "Kvinna"])
s2_m <- var(data$förtroende_index[data$V238 == "Man"])
# Skattningen av stickprovsmedelvärdets varians  
vhatxbar_f <- (1 - n_f/N_f)*(s2_f/n_f)    
vhatxbar_m <- (1 - n_m/N_m)*(s2_m/n_m)  
vhatxbar_st <- (N_f/N)^2*vhatxbar_f +(N_m/N)^2*vhatxbar_m
# Konfidensgraden 100(1-alpha)%.  
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
# Konfidensintervallets gränser  
ll <- xbar_st - z_alpha*sqrt(vhatxbar_st)
ul <- xbar_st + z_alpha*sqrt(vhatxbar_st)
# Resultat
resultat <- c(xbar_st, ll, ul)
resultat
#-------Samband utbildning förtroendeindex-------
# Fixar så att kategorierna visas i specifik ordning i boxplot
data$utbildning_kat <- factor(data$utbildning_kat, levels = c("grundskola", "gymnasial", "eftergymnasial"))

# skapar boxplotten
par(mar=c(5, 5, 3, 2))
boxplot(data$förtroende_index ~ data$utbildning_kat,
        col = terrain.colors(5),
        ylab = "förtroendeindex",
        xlab = "utbildningsnivå")



