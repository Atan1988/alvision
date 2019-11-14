library(RoogleVision)
library(jsonlite) # to import credentials

# For image processing
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")
library(EBImage)

# For Latitude Longitude Map
library(leaflet)

# Credentials file I downloaded from the cloud console
creds = fromJSON('inst/credential.json')

# Google Authentication - Use Your Credentials
options("googleAuthR.client_id" = creds$installed$client_id)
options("googleAuthR.client_secret" = creds$installed$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

dog_mountain_label = getGoogleVisionResponse('inst/dog_mountain.png', feature = 'LABEL_DETECTION')
head(dog_mountain_label)
