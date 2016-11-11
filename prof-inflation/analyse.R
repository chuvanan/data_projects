


profs <- read.csv("~/ownCloud/data_projects/prof-inflation/cleaned-profs.csv",
                  stringsAsFactors = FALSE)

as.data.frame(
  table(profs$nam, dnn = "nam"),
  responseName = "n",
  stringsAsFactors = FALSE
)
