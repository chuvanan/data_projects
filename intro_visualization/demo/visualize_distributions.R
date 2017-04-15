


profs <- read.csv("~/Dropbox/intro_visualization/demo/professors.csv",
                  stringsAsFactors = F)

## dim(profs)
## head(profs)

profs <- subset(profs, !ngaysinh %in% c("", "119-05-28", "243-03-26", "339-02-24"))
profs$age <- profs$nam - as.numeric(substr(profs$ngaysinh, 1, 4))

## -----------------------------------------------------------------------------
## HISTOGRAM
## -----------------------------------------------------------------------------

## default
hist(profs$age)                         

## add axis labels, title
hist(profs$age,
     xlab = "Age", main = "Age Distribution of Vietnamese Professors Since 1980")

## change breaks
hist(profs$age,
     xlab = "Age", main = "Age Distribution of Vietnamese Professors Since 1980",
     breaks = 40)

hist(profs$age,
     xlab = "Age", main = "Age Distribution of Vietnamese Professors Since 1980",
     breaks = seq(20, 90, 2))

## add color
hist(profs$age,
     xlab = "Age", main = "Age Distribution of Vietnamese Professors Since 1980",
     breaks = seq(20, 90, 2),
     col = "ivory4",
     border = "white")

## density
hist(profs$age,
     freq = F,
     xlab = "Age", main = "Age Distribution of Vietnamese Professors Since 1980",
     breaks = seq(20, 90, 2),
     col = "ivory4",
     border = "white")


## -----------------------------------------------------------------------------
## BOXPLOT
## -----------------------------------------------------------------------------

## default
boxplot(x = profs$age)

## add box color, border color and border width
boxplot(profs$age, col = "cadetblue", border = "gray40", lwd = 1.5)

## make it horizontal
boxplot(profs$age, col = "cadetblue", border = "gray40", lwd = 1.5,
        horizontal = T)

## multiple boxplot
boxplot(age ~ nam, data = profs,
        col = "cadetblue", border = "gray40", lwd = 1.5)

## add labels
boxplot(age ~ nam, data = profs,
        col = "cadetblue", border = "gray40", lwd = 1.5,
        xlab = "Year", ylab = "Age",
        main = "Age Distribution of Vietnamese Professors Since 1980",
        cex.main = 1)
