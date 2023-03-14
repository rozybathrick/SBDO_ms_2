R.Version()
# install from my R-universe repository
install.packages("aniMotum", 
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = TRUE)
Yes
library("aniMotum")

install.packages('TMB', type = 'source')
install.packages("terra")
remove.packages("TMB")
