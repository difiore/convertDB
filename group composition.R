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
d$`composition` <- str_lower(d$`Group Composition`)
d <- d %>% mutate_all(funs(iconv(.,"latin1","utf-8")))
d$`composition` <- str_replace_all(d$`composition`,"unk/","unknown/")
d$`composition` <- str_replace_all(d$`composition`,"oikoma/","oikamo/")
d$`composition` <- str_replace_all(d$`composition`,"violeya/","violeta/")
d$`composition` <- str_replace_all(d$`composition`,"vioeta/","violeta/")
d$`composition` <- str_replace_all(d$`composition`,"vioketa/","violeta/")
d$`composition` <- str_replace_all(d$`composition`,"nenk/","nenki/")
d$`composition` <- str_replace_all(d$`composition`,"ne.nki/","nenki/")
d$`composition` <- str_replace_all(d$`composition`,"makis/","maquis/")
d$`composition` <- str_replace_all(d$`composition`,"maqu.is/","maquis/")
d$`composition` <- str_replace_all(d$`composition`,"ma.quis/","maquis/")
d$`composition` <- str_replace_all(d$`composition`,"lliana/","liana/")
d$`composition` <- str_replace_all(d$`composition`,"l.iana/","liana/")
d$`composition` <- str_replace_all(d$`composition`,"kauo./","kauo/")
d$`composition` <- str_replace_all(d$`composition`,"k.auoka/","kauoka/")
d$`composition` <- str_replace_all(d$`composition`,"evit/","evita/")
d$`composition` <- str_replace_all(d$`composition`,"elen.a/","elena/")
d$`composition` <- str_replace_all(d$`composition`,"coatinga/","cotinga/")
d$`composition` <- str_replace_all(d$`composition`,"aya x/","ayax/")
d$`composition` <- str_replace_all(d$`composition`,"ayac/","ayax/")
d$`composition` <- str_replace_all(d$`composition`,"ajax/","ayax/")
d$`composition` <- str_replace_all(d$`composition`,"au.ra/","aura/")
d$`composition` <- str_replace_all(d$`composition`,"anaaura/","ana/aura/")
d$`composition` <- str_replace_all(d$`composition`,"ana./","ana/")
d$`composition` <- str_replace_all(d$`composition`,"violetaoikamo/","violeta/oikamo/")
d$`composition` <- str_replace_all(d$`composition`,"taigaandreo/","taiga/andreo/")
d$`composition` <- str_replace_all(d$`composition`,"sam.my/","sammy/")
d$`composition` <- str_replace_all(d$`composition`,"oikamoi/","oikamo/")
d$`composition` <- str_replace_all(d$`composition`,"o.ikamo/","oikamo/")
d$`composition` <- str_replace_all(d$`composition`,"nika./","nika/")
d$`composition` <- str_replace_all(d$`composition`,"lucassammy/","lucas/sammy/")
d$`composition` <- str_replace_all(d$`composition`," /","/")
d$`composition` <- str_replace_all(d$`composition`,"/ ","/")
d$`composition` <- str_replace_all(d$`composition`,"\\\r","")
d$`composition` <- str_replace_all(d$`composition`,"\\\n","")
d$`composition` <- str_replace_all(d$`composition`,"\\.","")
d$`Composition Array` <- str_split(d$`composition`,"/")

animals <- sort(unique(unlist(d$`Composition Array`)))

indivs <- read_csv("list of ateles.csv")
indivs <- str_lower(indivs$Name)

#removes the empty elements from composition array - not necessary!
for (i in 1:length(d$`Composition Array`)) {
 d$`Composition Array`[[i]] <- unique(d$`Composition Array`[[i]][d$`Composition Array`[[i]] != ""])
}

r <- tibble()
for (i in 1:nrow(d)) {
	n <- indivs %in% d$`Composition Array`[[i]]
	r <- rbind(r, n)
}

names(r) <- indivs
r




f <- cbind(d,r)

f$y <- year(parse_date_time(f$Date,"mdyHMS", tz="America/Bogota"))
f$m <- month(parse_date_time(f$Date,"mdyHMS", tz="America/Bogota"))
f$d <- day(parse_date_time(f$Date,"mdyHMS", tz="America/Bogota"))
f$tz <- tz(parse_date_time(f$Date,"mdyHMS", tz="America/Bogota"))

f$newdate <- make_date(year=f$y,month=f$m,day=f$d)

f$h <- hour(parse_date_time(f$`Time Enc`,"mdyHMS"))
f$m <- minute(parse_date_time(f$`Time Enc`,"mdyHMS"))
f$s <- second(parse_date_time(f$`Time Enc`,"mdyHMS"))

f$timeenc <- make_datetime(year=year(f$newdate),month=month(f$newdate),day=day(f$newdate), hour=f$h, min=f$m, sec=f$s)

f$h <- hour(parse_date_time(f$`Time Left/Lost`,"mdyHMS"))
f$m <- minute(parse_date_time(f$`Time Left/Lost`,"mdyHMS"))
f$s <- second(parse_date_time(f$`Time Left/Lost`,"mdyHMS"))

f$timeend <- make_datetime(year=year(f$newdate),month=month(f$newdate),day=day(f$newdate), hour=f$h, min=f$m, sec=f$s)

f$h <- hour(f$Time)
f$m <- minute(f$`Time`)
f$s <- second(f$`Time`)

f$Time <- make_datetime(year=year(f$newdate),month=month(f$newdate),day=day(f$newdate), hour=f$h, min=f$m)

f$Date <- f$newdate

g <- select(f, -c(`GPS Used`, `Time Enc`, `Time Left/Lost`,`y`,`m`,`d`,`h`,`m`,`s`,`tz`, `timeenc`,`timeend`,`newdate`))

g$Month <- month(g$Date)
g$Year <- year(g$Date)

h <- g %>% group_by(Year, Month) %>% summarise_at (vars(indivs), sum)
h <- h %>% mutate_all (var(indivs), funs(sqrt))

j <- h %>% ungroup() %>% select(-c(Year,Month))
j[j > 0] <- 1

j <- cbind(h$Year,h$Month,j)
names(j) <- c("Year","Month", indivs)
j <- j %>% mutate(Date = make_date(year=Year, month=Month, day=15)) %>% select(Date,indivs)

library(dplyr)
library(tidyr)
j %>%
	gather(var, val, 2:ncol(j)) %>%
	spread(names(j)[1], val)
