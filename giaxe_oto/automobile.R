
library(readr)

giaxeoto <- read.csv("~/Dropbox/giaxe_oto_022017.csv",
                     header = FALSE, stringsAsFactors = FALSE,
                     colClasses = "character")

names(giaxeoto) <- c("mauxe", "hang", "loaixe", "nguongoc", "gia_niemyet",
                     "gia_thamkhao", "dongco", "congsuat", "maluc")

table(giaxeoto$gia_niemyet)

giaxeoto$loaixe[giaxeoto$loaixe == "Station Wagon"] <- "SUV"
giaxeoto$loaixe[giaxeoto$loaixe == "pick-up"] <- "Pick-up"
giaxeoto$loaixe[giaxeoto$loaixe == "coupe"] <- "Coupé"
giaxeoto$loaixe[giaxeoto$loaixe == "Coupe 4 cửa"] <- "Coupé"
giaxeoto$loaixe[giaxeoto$loaixe == "coupe"] <- "Coupé"
giaxeoto$loaixe[giaxeoto$loaixe == "Coupe"] <- "Coupé"
giaxeoto$loaixe[giaxeoto$loaixe == "sedan"] <- "Sedan"
giaxeoto$loaixe[giaxeoto$loaixe == "Crossover"] <- "SUV"
giaxeoto$loaixe[giaxeoto$loaixe == "SUV-Coupe"] <- "SUV"
giaxeoto$loaixe[giaxeoto$loaixe == "SUV-Wagon"] <- "SUV"
giaxeoto$loaixe[giaxeoto$loaixe == "Siêu xe"] <- "Sports"
giaxeoto$nguongoc[giaxeoto$nguongoc == "Nhâp khẩu"] <- "Nhập khẩu"

giaxeoto$gia_niemyet <- gsub("\\.", "", giaxeoto$gia_niemyet)
giaxeoto$gia_thamkhao <- gsub("\\.", "", giaxeoto$gia_thamkhao)

giaxeoto$gia_niemyet <- as.numeric(giaxeoto$gia_niemyet)
giaxeoto$gia_thamkhao <- as.numeric(giaxeoto$gia_thamkhao)
giaxeoto$gia_thamkhao[giaxeoto$gia_thamkhao > giaxeoto$gia_niemyet] <-
  giaxeoto$gia_thamkhao[giaxeoto$gia_thamkhao > giaxeoto$gia_niemyet]/10
giaxeoto$chenh_lech <- giaxeoto$gia_niemyet - giaxeoto$gia_thamkhao

subset(giaxeoto, chenh_lech < 0, select = c(mauxe, gia_niemyet, gia_thamkhao))



subset(giaxeoto, mauxe == "Phantom")

library(lattice)

xyplot(speed ~ dist, data = cars, pch = 19, cex = 1.1,
       main = "Scatter plot", col = "cadetblue",
       par.settings = list(background = list(col = "gray98")))


library(grid)

while (TRUE) {
  grid.newpage()
  grid.text(format(Sys.time(), format="%H:%M:%S"),
            gp=gpar(cex=10))
  Sys.sleep(1)
}
