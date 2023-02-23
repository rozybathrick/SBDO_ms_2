#install.packages("usethis")
usethis::edit_r_profile()

# add the following text or replace existing repos option
options(repos = c(ianjonsen = 'https://ianjonsen.r-universe.dev',
                  CRAN = 'https://cloud.r-project.org'))

# install from my R-universe repository
install.packages("aniMotum", 
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = TRUE)

# for Mac
install.packages("path_to_file/aniMotum_1.1-03.tgz", 
                 repos=NULL, type="mac.binary", dependencies = TRUE)


library(aniMotum)

