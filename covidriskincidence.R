needed <- function(){
  
  install.packages("rvest")
  install.packages("dplyr")
  library("rvest")
  library("dplyr")
}




covid <- function() {
 
  link = "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
  page = read_html(link)
  name = page %>% html_nodes("td:nth-child(1) , td:nth-child(1) strong") %>% html_text()
  inzidenz = page %>% html_nodes(".right+ .right strong , .right:nth-child(5) , td:nth-child(1) strong") %>% html_text()
  name = name[1:16]
  name[8] = "Mecklenburg-Vorpommern"
  inzidenz = inzidenz[2:17]
  gesamt = data.frame(cbind(name,inzidenz))
  gesamt[,2] = as.numeric(gsub(",", ".", gsub("\\.", "", gesamt[,2])))
  Bundesland = cbind(1:16,name)
  g= c()
  var1 = readline(prompt = "Enter size of your group : ")
  var1 = as.numeric(var1)
  print(Bundesland)
  var3 = readline(prompt = "Choose the state of you current location and enter the corresponding number of the list e.g. if you are in Berlin enter 3: ")
  var3 = as.numeric(var3)
  var2 = gesamt[var3,2]
  var2 <- as.integer(var2)
  for (i in 1:1300){
    g[i] <- (1-(1-(i/100000))^var1)*100
  }
  prob1 <- (1-(1-(var2/100000))^var1)*100
  erg1 <- paste("Mean risk to have a person infected with COVID in your group is:", round(prob1,2), "%")
  erg2 <- paste("The current seven-day-incidence in your state is:",var2)
  plot(1:1300,g,type="l",ylab="Prob. of contact with COVID19",xlab="Incidence Rate (per 100000)",main = c("Prob. of COVID19 positive Person in your group", "According to your group size for Incidence Rates 1-1300 per 100.000"),lwd=2)
  points(var2,prob1,type="p",col="red",lwd=10)
  text(x = 600, y = prob1+5, erg1,cex = 1.2) 
  text(x = 600, y =2, erg2,cex = 1.2) 
  return()
  
  }

needed()
covid()







  
  






