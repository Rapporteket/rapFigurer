## code to prepare `DataTraktplott` dataset goes here
AntShus <- 15
Sykehus <- as.character(paste("Sykehus", LETTERS[1:AntShus]))
set.seed(1234)
DataTraktplott <- data.frame(
  N = sample(50:500, AntShus),
  Sykehus = Sykehus,
  Andel = runif(AntShus, 0.6, 1)
)

usethis::use_data("DataTraktplott")
