
# Suicídio ------------------------------------------------------------------------
# Autora do script: Jeanne Franco -------------------------------------------------
# Data: 01/09/22 ------------------------------------------------------------------
# Referência: https://ourworldindata.org/suicide ----------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Mortes por suicídio é uma questão extremamente complexa que causa dor a
### centenas de milhares de pessoas todos os anos em volta do mundo. O objetivo
### desses dados é contribuir para um debate informativo e aberto sobre formas
### para prevenir o suicídio. Se você tem pensamento suicida, você pode receber
### ajuda imediata visitando o site Suicide.org.

### Cada suicídio é uma tregédia. A Organização Mundial de Saúde e o Global
### Burden of Disease estimam que quase 800.000 pessoas morrem de suicídio 
### a cada ano. Isso é uma pessoa a cada 40 segundos.

### Com intervenções atempadas e baseadas em provas, os suicídios podem ser
### evitados.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(pals)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

sui <- read.csv("suicide-death-rates.csv")
view(sui)
names(sui)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

sui1 <- sui %>%
  select(-Code) %>%
  rename(taxa_suicidio = Deaths...Self.harm...Sex..Both...Age..Age.standardized..Rate.) %>%
  filter(Entity %in% c("Angola", "Brazil", "China", "Russsia",
                       "United States", "France", "Germany",
                       "Ghana", "India", "Haiti", "Hungary", 
                       "Iceland", "Israel", "Portugal", "Italy")) %>%
  view()

sui2 <- sui1 %>%
  group_by(Entity) %>%
  summarise(media = mean(taxa_suicidio),
            dp = sd(taxa_suicidio),
            n = n(),
            ep = dp/sqrt(n))
view(sui2)

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

ggplot(sui1, aes(x = Year, y = taxa_suicidio, group = Entity, color = Entity)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = as.vector(alphabet(n = 14))) +
  labs(x = "Tempo (anos", y = "Taxa de suicídio (%)",
       color = "Países") +
  theme_classic(base_size = 14)





