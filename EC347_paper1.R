library(dplyr)
library(readxl)
library(readr)
library(tidyverse)
library(scales)
library(patchwork)
library(showtext)
library(lubridate)
library(tsibbledata)
library(feasts)
library(fable)
library(fabletools)
library(fpp3)
library(sugrrants)
library(janitor)
library(seasonal)

corruption_index <- read_excel("corruption_index.xlsx")
colnames(corruption_index) <- c("Country", "CPI", "Time")
fdi2 <- FDI_GDP_data


#fdi2 <- merge(fdi2, corruption_index, by = c("Country", "Time"))
View(fdi2)

fdi2[fdi2 == ".."] <- NA
fdi2$FDI <- as.numeric(fdi2$FDI)
fdi2$GDP <- as.numeric(fdi2$GDP)
fdi2$Inflation <- as.numeric(fdi2$Inflation)
fdi2$TO <- as.numeric(fdi2$TO)
fdi2$SLE <- as.numeric(fdi2$SLE)
fdi2$Corruption <- as.numeric(fdi2$Corruption)
#fdi2$CPI <- as.numeric(fdi2$CPI)


##### Creating Colony Variable #####

colonies <- fdi2 %>%
  filter(Country %in% c("Afghanistan", "Algeria", "Angola",
                      "Bahrain", "Bangladesh", "Bhutan", "Botswana",
                      "Myanmar", "Brunei", "Cambodia", "Cameroon", "Central African Republic",
                      "Burkina Faso", "Chad", "East Timor", "Egypt, Arab Rep.", "Equatorial Guinea", "Eritrea",
                      "Fiji", "Ghana", "Guinea", "Hong Kong", "India", 
                      "Indonesia", "Iraq", "Cote d'Ivoire", "Jordan", "Kenya", "Kuwait",
                      "Lao PDR", "Lebanon", "Liberia", "Libya", "Madagascar", "Malawi",
                      "Malaysia", "Maldives", "Mali", "Mauritania", "Mauritius", 
                      "Mongolia", "Morocco", "Mozambique", "Namibia", 
                      "Niger", "Nigeria", "Pakistan", "Rwanda", "Senegal",
                      "Sierra Leone", "Singapore", "Solomon Islands", "Somalia",
                      "South Africa", "South Sudan", "Eswatini",
                      "Sudan", "Swaziland", "Syria", "Taiwan", "Tanzania", "Thailand",
                      "Togo", "Tunisia", "Uganda", "United Arab Emirates", "Vietnam", "Yemen", "Zambia", "Zimbabwe", 
                      "Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", "Burundi"
                      , "Eritrea", "Guinea-Bissau", "Lesotho")) %>%
  mutate(colony = 1)

View(colonies)

`%!in%` <- Negate(`%in%`)

non_colonies <- fdi2 %>%
  filter(Country %!in% c("Afghanistan", "Algeria", "Angola",
                        "Bahrain", "Bangladesh", "Bhutan", "Botswana",
                        "Burma", "Cambodia", "Cameroon", "Central African Republic",
                        "Chad", "East Timor", "Egypt", "Equatorial Guniea", "Eritrea",
                        "Fiji", "Gabon", "Ghana", "Guinea", "Hong Kong", "India", 
                        "Indonesia", "Iraq", "Ivory Coast", "Jordan", "Kenya", "Kuwait",
                        "Laos", "Lebanon", "Liberia", "Libya", "Madagascar", "Malawi",
                        "Malaysia", "Maldives", "Mali", "Mauritania", "Mauritius", 
                        "Mongolia", "Morocco", "Mozambique", "Namibia", "Nepal", 
                        "Niger", "Nigeria", "Oman", "Pakistan", "Papua New Guinea", 
                        "Philippines", "Qatar", "Rwanda", "Samoa", "Senegal",
                        "Sierra Leone", "Singapore", "Solomon Islands", "Somalia",
                        "South Africa", "South Korea", "South Sudan", "Sri Lanka",
                        "Sudan", "Swaziland", "Syria", "Taiwan", "Tanzania", "Thailand",
                        "Togo", "Tonga", "Tunisia", "Uganda", "United Arab Emirates", 
                        "Uzbekistan", "Vanuatu", "Vietnam", "Yemen, Rep.", "Zambia", "Zimbabwe",
                        "Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.")) %>%
  mutate(colony = 0)


fdi2 <- rbind(colonies, non_colonies)
View(fdi2)

fdi2 <- fdi2[
  with(fdi2, order(fdi2$Country, fdi2$Time)),
]
View(fdi2)


fdi2_clean <- fdi2[!is.na(fdi2$Country),]

View(fdi2_clean)

for (i in 2:nrow(fdi2_clean)) {
  if(fdi2_clean[i,2] == fdi2_clean[i-1,2]){
    fdi2_clean[i,11] = (fdi2_clean[i,5] - fdi2_clean[i-1,5])/fdi2_clean[i-1,5]
  } else{
    fdi2_clean[i,11] = NA
  }
}


View(fdi2_clean)

colnames(fdi2_clean)[11] <- "delta_gdp"
View(fdi2_clean)

fdi_clean <- na.omit(fdi2_clean)

View(fdi_clean %>%
       filter(colony == 1))

summary(lm(delta_gdp ~ FDI + Inflation + TO + colony + SLE + colony:FDI, data = fdi_clean))


colonies <- fdi_clean %>%
  filter(colony == 1)
non_colonies <- fdi_clean %>%
  filter(Country %in% 
           c("Albania", "Argentina", "Austria", "Azerbaijan", "Bahamas, The",
         "Barbados", "Belarus", "Belguim", "Belize", "Bolivia", 
         "Bosnia and Herzogovina", "Bulgaria", "China", "Croatia",
         "Czech Republic", "Finland", "France", "Georgia", "Germany",
         "Colombia", "Brazil", "Hungary", "Iceland", "Iran, Islamic Rep.",
         "Ireland", "Italy", "Japan", "Kazakhstan", "Kosovo", "Kyrgyz Republic",
         "Latvia", "Lithuania", "Luxemburg", "Macedonia, FYR", "Mexico", "Canada",
         "Montenegro", "Australia", "New Zealand", "Nicaragua", "Norway", "Panama",
         "Paraguay", "Peru", "Portugal", "Poland", "Romania", "Russian Federation",
         "Saudi Arabia", "Serbia", "Slovak Republic", "Slovenia", "Spain", "Sweden",
         "Switzerland", "Tajikistan", "Turkey", "Ukraine", "United Kingdom", 
         "United States", "Uruguay", "Venezuela, RB"))

View(colonies)

nrow(colonies)
nrow(non_colonies)

summary(lm(delta_gdp ~ FDI, data = colonies))
summary(lm(delta_gdp ~ FDI, data = non_colonies))

summary(lm(delta_gdp ~ FDI + Inflation + TO + SLE + Corruption + TO:FDI + Corruption:FDI + SLE:FDI + Inflation:FDI, data = colonies))
summary(lm(delta_gdp ~ FDI + Inflation + TO + SLE + Corruption + TO:FDI + Corruption:FDI + SLE:FDI + Inflation:FDI, data = non_colonies))

fdi_clean <- rbind(colonies, non_colonies)

favstats(fdi_clean$FDI)
favstats(fdi_clean$Inflation)
favstats(fdi_clean$TO)
favstats(fdi_clean$SLE)
favstats(fdi_clean$delta_gdp)
favstats(fdi_clean$Corruption)


library(mosaic)
nrow(colonies) + nrow(non_colonies)


