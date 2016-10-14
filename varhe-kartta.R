source('C:/Temp/DataKuntakartalle/dataKuntakartalle.r')
library(tidyverse)
library(kaariLaskuri)

if (dir.exists("data") & 
    file.exists( file.path("data","kartta.rda") ) ) {
  load( file.path("data","kartta.rda") )
} else {
  kartta <- loadGeoData
  
  if(!dir.exists("data") ) dir.create("data")
  save(kartta, file=file.path("data", "kartta.rda") )
}

meta <- org_meta  %>% 
  filter(tila=="Voimassaoleva", tyyppiluokka=="Kunnat ja kaupungit")  %>% 
  select(nimi, kuntakoodi, ytunnus) %>% 
  distinct()

data <- kuel_meta  %>% 
  left_join( varhe_meta %>% select(vuosi, pieni, suuri) ) %>% 
  mutate(varhemaksu = varhemaksu / palkkasumma,
         luokka = (palkkasumma > pieni) + (palkkasumma > suuri) , 
         luokka = factor(luokka, labels = c("pieni","Keskisuuret (osavastuu)","Suuret työnantajat (omavastuu)") ) 
         ) %>% 
  group_by(luokka, vuosi) %>% 
  mutate(varhe=ntile(varhemaksu, 10)) %>% 
  ungroup

aikasarja <- data %>% 
  group_by(vuosi, luokka) %>% 
  summarise(maara=n(),
            alakvartiili=quantile(varhemaksu)[2],
            mediaani=quantile(varhemaksu)[3],
            ylakvartiili=quantile(varhemaksu)[4]) %>% 
  gather(muuttuja, luku, -vuosi, -luokka, -maara)

aikasarja %>% 
  filter(luokka!="pieni") %>% 
  mutate(luokka = droplevels(luokka),
         luku = luku *100,
         muuttuja = ifelse(muuttuja=="ylakvartiili", "yläkvartiili", muuttuja)) %>% 
  ggplot(aes(x=vuosi, y=luku, group=muuttuja, color=muuttuja)) + 
  geom_line() +  
  facet_grid(.~luokka) + 
  ylim(0, 1.75) + 
  ylab("osuus palkkasummasta") + ggtitle("Varhe-maksun kehitys")

data <- data %>% 
  filter(luokka!="pieni", vuosi==2015) %>% 
  select(ytunnus,varhe, luokka) %>% 
  left_join(meta) %>% 
  na.omit() 




merged <- mergeDataIntoMap(data, "kuntakoodi", kartta)

leafletMap <- vizualizeDataOnMap("varhe", 
                                 "Varhe-maksudesiili vuonna 2015", 
                                 merged, 
                                 FALSE, 
                                 colorNumeric(c("#84a214", "#b5bd00", "#eada24", "#ffb549", "#ef6079",  "#c83061"),NULL, na.color = "#FFFFFF"), 
                                 "#FFFFFF")
leafletMap

leafletMap <- vizualizeDataOnMap("varhe", 
                                 "Varhe-maksudesiili vuonna 2015", 
                                 merged, 
                                 FALSE, 
                                 colorNumeric(c("#0085b0", "#00c1d5", "#ff00dd", "#ffb549", "#ef6079",  "#c83061"),NULL, na.color = "#FFFFFF"), 
                                 "#FFFFFF")

leafletMap

