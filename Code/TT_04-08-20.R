## Tidy Tuesday - European Energy

# Link: https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-04

# Packages

library(treemapify)
library(RColorBrewer)
library(geofacet)
library(ggtext)
library(tidyverse)

# Read in data

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

## Data cleaning

# get rid of NA and replace with UK 

country_totals <- country_totals %>% 
  replace_na(list(country="UK",country_name="United Kingdom"))

energy_types <- energy_types %>% 
  replace_na(list(country="UK",country_name="United Kingdom"))

# Wide to long format
 energy_types <- energy_types %>% 
   gather(key="Year",value="Energy",c(5:7)) %>% 
   mutate(type = as_factor(type),country=as_factor(country),country_name=as_factor(country_name))

unique(energy_types$type) # spelling etc all right

# Group renewable categories together, pick one year and choose country names 

energy_types2 <- energy_types %>% 
  filter(type != "Pumped hydro power") %>% 
  mutate(new_type = case_when(type =="Conventional thermal" ~ "Fossil Fuels",
                              type =="Nuclear" ~ "Nuclear",
                              type=="Hydro"~ "Renewables",
                              type=="Wind"~"Renewables",
                              type=="Solar" ~"Renewables",
                              type=="Geothermal"~"Renewables",
                              TRUE ~ "Other"),
         new_type=fct_relevel(new_type, "Renewables", "Nuclear", "Fossil Fuels", "Other")) %>% 
  filter(Year==2018) %>% 
  mutate(country_name= recode(country_name,"Bosnia & Herzegovina"="B&H", "United Kingdom" = "UK", 
                              "North Macedonia" = "Macedonia")) %>% 
  group_by(country_name,new_type) %>% 
  summarise(Energy=sum(Energy))

# energy_types.test <- energy_types2 %>% 
#   filter(country_name =="Netherlands") %>% 
#   mutate(prop=Energy/sum(Energy))


# Modify geofacet grid

grid_design(data = europe_countries_grid1, img = "http://bit.ly/eu-grid")

# New grid

europe_countries_grid2  <-data.frame(
  row = c(1, 1, 1, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8),
  col = c(5, 6, 7, 8, 1, 2, 5, 4, 8, 8, 5, 7, 4, 6, 9, 6, 3, 5, 4, 7, 8, 6, 5, 2, 1, 8, 7, 7, 8, 6, 4, 9, 7, 8, 3, 10),
  code = c("NO", "SE", "FI", "EE", "IE", "GB", "DK", "NL", "LV", "LT", "DE", "PL", "BE", "CZ", "UA", "SK", "FR", "AT", "LU", "RS", "RO", "HU", "SI", "ES", "PT", "BG", "BA", "ME", "MK", "HR", "IT", "TR", "AL", "GR", "MT", "CY"),
  name = c("Norway", "Sweden", "Finland", "Estonia", "Ireland", "UK", "Denmark", "Netherlands", "Latvia", "Lithuania", "Germany", "Poland", "Belgium", "Czechia", "Ukraine", "Slovakia", "France", "Austria", "Luxembourg", "Serbia", "Romania", "Hungary", "Slovenia", "Spain", "Portugal", "Bulgaria", "B&H", "Montenegro", "Macedonia", "Croatia", "Italy", "Turkey", "Albania", "Greece", "Malta", "Cyprus"),
  stringsAsFactors = FALSE
)

# Check colour palette

display.brewer.pal(n=8, name = 'Accent')
brewer.pal(n = 8, name = "Accent")

# Plot it

European_Energy <- ggplot(energy_types2, aes(area=Energy,fill=new_type,label=country_name)) +
  geom_treemap(colour="white",size=3,start="topleft") +
  scale_fill_brewer(palette="Accent")+
 facet_geo(~country_name, grid="europe_countries_grid2", labeller = label_wrap_gen(width=20)) +
 labs(title= "<span style='font-size:22pt'>**European Energy Mix - 2018**  
    <span style='font-size:18pt'>Treemaps indicating the proportion of
    <span style='color:#fdc086;'>**Fossil Fuels**</span>, 
    <span style='color:#7fc97f;'>**Renewables**</span>, <br>
    <span style='color:#beaed4;'>**Nuclear**</span>, and
    <span style='color:#ffff99;'>**Other**</span> 
    <span style='font-size:18pt'>methods of energy generation per country.",
      caption = "<span style='font-size:13pt'>Data source: Eurostat via #TidyTuesday") +
  theme_void() +
  theme(strip.text= element_text(size=14,vjust=1,colour='black'),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        plot.title = element_markdown(),
        legend.position="none",
        plot.caption = element_markdown(), 
        plot.background = element_rect(fill = 'linen'))

ggsave("European_Energy.png", plot=European_Energy, width=12, height=8, dpi=320)      


