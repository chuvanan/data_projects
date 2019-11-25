
require(tictoc)
require(microbenchmark)
require(rray)


img <- imager::load.image("tiger_low_constrast.jpg")
img_array <- as.array(img)
img_rray <- rray::as_rray(img_array)

dim(img_array)
dim(img_rray)

adjust_contrast <- function(img, contrast) {
    img <- img * contrast
    img[img > 255] <- 255
    img
}

rray_adjust_contrast <- function(img, contrast) {
    img <- img %b*% contrast
    img[[img > 255]] <- 255
    img
}

test1 <- adjust_contrast(img, 2)
test2 <- rray_adjust_contrast(img_rray, 2)
test3 <- rray::as_array(test2)
all.equal(test1, test3, check.attributes = FALSE)

microbenchmark(
    bench_img = {adjust_contrast(img, 2)},
    times = 5
)

microbenchmark(
    bench_img = {rray_adjust_contrast(img_rray, 2)},
    times = 5,
    unit = "s"
)
