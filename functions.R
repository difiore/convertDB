fixdate <- function(x, field="Date"){
	Y <- year(parse_date_time(x[[field]],"mdYHMS"))
	m <- month(parse_date_time(x[[field]],"mdYHMS"))
	d <- day(parse_date_time(x[[field]],"mdYHMS"))
	x[[field]] <- make_date(year=Y,month=m,day=d)
	return(x)
}

fixtime <- function(x, timezone="UTC", offset=-5, field="Time Enc"){
	# field gets recoded into "time of day" based on difftime class from hms
	# H <- hour(parse_date_time(x[[field]],"mdYHMS")) - offset
	H <- hour(parse_date_time(x[[field]],"mdYHMS"))
	M <- minute(parse_date_time(x[[field]],"mdYHMS"))
	S <- second(parse_date_time(x[[field]],"mdYHMS"))
	#Y <- year(x$Date)
	#m <- month(x$Date)
	#d <- day(x$Date)
	#x[[field]] <- make_datetime(year=Y,month=m,day=d,hour=H,min=M,sec=S,tz=timezone)
	x[[field]] <- hms(seconds = S, minutes = M, hours = H)
	return(x)
}

av <- av[order(av$`Time Enc`, decreasing = FALSE),]
tail(av$`Time Enc`,1000)
