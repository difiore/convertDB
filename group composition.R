# the script below will take csv files of os, av, fs, and fd data, merge them, and scan the group composition field to tally which named individuals were ever seen in any group composition in a given month

rm(list=ls())

library(here) # for working directory
library(tidyverse) # for dplyr, readr, stringr, tidyr, tibble, and purrr
library(lubridate) # supposedly in tidyverse but maybe not
library(readxl) # to read in xlsx files
library(lettercase)

options(tibble.width = NULL)

os <- read_xlsx("observer samples pre 2015.xlsx", guess_max = 10000) # guess max set close to length of file
os <- rowid_to_column(os, "rowID")
vars <- c("rowID", "Obs Sample ID", "Observer", "Date", "GPS Used", "Comments")
os <- select(os,vars)

av <- read_xlsx("avistajes pre 2015.xlsx", guess_max = 30000) # guess max set close to length of file
av <- rowid_to_column(av, "rowID")
vars <- c("rowID", "Obs Sample ID", "Avistaje ID", "Taxon", "Group", "Time Enc", "Time Left/Lost", "Avistaje Notes")
av <- select(av,vars)

fs <- read_xlsx("focal samples pre 2015.xlsx", guess_max = 30000) # guess max set close to length of file
fs <- rowid_to_column(fs, "rowID")
vars <- c("rowID", "Avistaje ID", "Focal Sample ID", "Focal Animal", "Time Start", "Time End")
fs <- select(fs,vars)

fd <- read_xlsx("focal data atelines pre 2015.xlsx", guess_max = 30000) # guess max set close to length of file
fd <- rowid_to_column(fd, "rowID")
vars <- c("rowID", "Focal Sample ID", "Focal Data ID", "Time","Behavior","Partner", "NN ID", "NN Distance", "Group Composition")
fd <- select(fd,vars)

d <- right_join(fs, fd, by = "Focal Sample ID")
d <- right_join(av, d, by = "Avistaje ID")
d <- right_join(os, d, by = "Obs Sample ID")

vars <- c("Obs Sample ID", "Observer", "Date", "Comments", "GPS Used", "Avistaje ID", "Taxon", "Group", "Time Enc", "Time Left/Lost","Avistaje Notes", "Focal Sample ID","Focal Animal","Time Start", "Time End", "Focal Data ID", "Time","Behavior","Partner", "NN ID", "NN Distance", "Group Composition")

d <- select(d,vars)
rm(vars)

d <- filter(d, Taxon == "Ateles" & Group == "Ateles MQ-1" & `Group Composition` > 0)
d <- select(d,-c(Comments, `GPS Used`, `Avistaje Notes`,`Focal Data ID`))
d$`Focal Animal` <- str_lower(d$`Focal Animal`)
d$`Partner` <- str_lower(d$`Partner`)
d$`NN ID` <- str_lower(d$`NN ID`)
d$`Composition Cleaned` <- str_lower(d$`Group Composition`)
d <- d %>% mutate_all(funs(iconv(.,"latin1","utf-8")))
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"unk/","unknown/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"oikoma/","oikamo/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"violeya/","violeta/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"vioeta/","violeta/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"vioketa/","violeta/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"nenk/","nenki/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"ne.nki/","nenki/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"makis/","maquis/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"maqu.is/","maquis/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"ma.quis/","maquis/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"lliana/","liana/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"l.iana/","liana/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"kauo./","kauo/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"k.auoka/","kauoka/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"evit/","evita/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"elen.a/","elena/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"coatinga/","cotinga/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"aya x/","ayax/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"ayac/","ayax/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"ajax/","ayax/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"au.ra/","aura/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"anaaura/","ana/aura/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"ana./","ana/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"violetaoikamo/","violeta/oikamo/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"taigaandreo/","taiga/andreo/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"sam.my/","sammy/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"oikamoi/","oikamo/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"o.ikamo/","oikamo/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"nika./","nika/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"lucassammy/","lucas/sammy/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`," /","/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"/ ","/")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"\\\r","")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"\\\n","")
d$`Composition Cleaned` <- str_replace_all(d$`Composition Cleaned`,"\\.","")
d$`Composition Array` <- str_split(d$`Composition Cleaned`,"/")

animals <- sort(unique(unlist(d$`Composition Array`)))

indivs <- read_csv("list of ateles.csv")
indivs <- str_lower(indivs$Name)

#removes the empty elements from composition array - not necessary!
for (i in 1:length(d$`Composition Array`)) {
 d$`Composition Array`[[i]] <- unique(d$`Composition Array`[[i]][d$`Composition Array`[[i]] != ""])
}

saveRDS(d, "d.rds")

r <- tibble()
for (i in 1:nrow(d)) {
	n <- indivs %in% d$`Composition Array`[[i]]
	r <- rbind(r, n)
}

names(r) <- indivs
head(r)

saveRDS(r, "r.rds")

f <- cbind(d,r)
head(f)

f$Y <- year(f$Date)
f$m <- month(f$Date)
f$d <- day(f$Date)

f$H <- hour(f$`Time Enc`)
f$M <- minute(f$`Time Enc`)
f$S <- second(f$`Time Enc`)

f$`Time Enc` <- make_datetime(year=f$Y,month=f$m,day=f$d, hour=f$H, min=f$M, sec=f$S)

f$H <- hour(f$`Time Left/Lost`)
f$M <- minute(f$`Time Left/Lost`)
f$S <- second(f$`Time Left/Lost`)

f$`Time Left/Lost` <- make_datetime(year=f$Y,month=f$m,day=f$d, hour=f$H, min=f$M, sec=f$S)

f$H <- hour(f$`Time`)
f$M <- minute(f$`Time`)
f$S <- second(f$`Time`)

f$Time <- make_datetime(year=f$Y,month=f$m,day=f$d, hour=f$H, min=f$M, sec=f$S)
f <- f[with(f, order(Date,Time)), ]

f$H <- hour(f$`Time Start`)
f$M <- minute(f$`Time Start`)
f$S <- second(f$`Time Start`)

f$`Time Start` <- make_datetime(year=f$Y,month=f$m,day=f$d, hour=f$H, min=f$M, sec=f$S)

f$H <- hour(f$`Time End`)
f$M <- minute(f$`Time End`)
f$S <- second(f$`Time End`)

f$`Time End` <- make_datetime(year=f$Y,month=f$m,day=f$d, hour=f$H, min=f$M, sec=f$S)

f <- f[with(f, order(Date,Time)), ]

f <- select(f, -c(`Y`,`m`,`d`,`H`,`M`,`S`))

head(f)
saveRDS(f, "f.rds")

g <- select(f, -c(`Time Enc`, `Time Left/Lost`))

g$Month <- month(g$Date)
g$Year <- year(g$Date)

head(g)

h <- g %>% group_by(Year, Month) %>% summarise_at (vars(indivs), sum)

j <- h %>% ungroup() %>% select(-c(Year,Month))
j[j > 0] <- 1

j <- cbind(h$Year,h$Month,j)
names(j) <- c("Year","Month", indivs)
j <- j %>% mutate(Date = make_date(year=Year, month=Month, day=15)) %>% select(Date,indivs)

j <- j %>%
	gather(var, val, 2:ncol(j)) %>%
	spread(names(j)[1], val)

write_csv(j,"~/Desktop/demography.csv")
