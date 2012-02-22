library(XML)
library(googleVis)


gvisCountryTeaConsumption <- function(){
	url2 <- "http://en.wikipedia.org/wiki/List_of_countries_by_tea_consumption_per_capita"
	page2 <- readLines(url2)
	
	z <- readHTMLTable(page2, which=1)

	colnames(z)[3]<-"Tea_Consumption"
	z$Tea_Consumption<-as.character(z$Tea_Consumption)
	z$Country<-as.character(z$Country)
	a<-strsplit(z$Tea_Consumption,' ')
	for (j in 1:length(a)){
		z$Tea_Consumption[j]<-a[[j]][1]
	}	
	
	p2 <- gvisGeoMap(z, "Country", "Tea_Consumption",
					options=list(colors="[0x91BFDB, 0XFC8D59]"))
	return(p2)
	
}	


gvisCountryCoffeeConsumption <- function(){
	url <- "http://en.wikipedia.org/wiki/List_of_countries_by_coffee_consumption_per_capita"
	page <- readLines(url)
	
	x <- readHTMLTable(page, which=1)
	
	colnames(x)[3]<-"Coffee_Consumption"
	x$Coffee_Consumption<-as.character(x$Coffee_Consumption)
	x$Country<-as.character(x$Country)
	y<-strsplit(x$Coffee_Consumption,' ')
	
	for (i in 1:length(y)){
	x$Coffee_Consumption[i]<-y[[i]][1]
	if (x$Country[i]=="Belgium  Luxembourg") 
		x$Country[i]<-"Belgium"	
	}
	
	x<-rbind(x,data.frame(Rank=8,Country="Luxembourg",Coffee_Consumption="6.8"))
	
	
	
	p <- gvisGeoMap(x, "Country", "Coffee_Consumption",
					options=list(colors="[0x91BFDB, 0XFC8D59]"))
	return(p)
}

M<-gvisMerge(gvisCountryCoffeeConsumption(),gvisCountryTeaConsumption())

plot(M)
