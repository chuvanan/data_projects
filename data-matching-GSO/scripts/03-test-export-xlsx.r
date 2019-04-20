

l <- list("IRIS" = iris, "MTCARS" = mtcars)
write.xlsx(l, file = "../outputs/writeXLSX2.xlsx")
write.xlsx(l, file = "../outputs/writeXLSXTable2.xlsx", asTable = TRUE)

hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",halign = "center", valign = "center", textDecoration = "Bold",border = "TopBottomLeftRight", textRotation = 45)
write.xlsx(iris, file = "../outputs/writeXLSX4.xlsx", borders = "rows", headerStyle = hs)
