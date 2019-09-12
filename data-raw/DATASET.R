## code to prepare `DATASET` dataset goes here

covsraw <- raster::stack(list.files(pattern='.asc',
                         full.names = TRUE))
covsraw <- brick(covsraw)
covsraw <- stack(covsraw)

usethis::use_data(covsraw)
