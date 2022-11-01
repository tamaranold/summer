#  Progressive Web App (PWA)

#install packages
remotes::install_github("cran/erratum")
remotes::install_github("RinteRface/charpente")

# load packages
library(charpente)

# create files
create_charpente(getwd(), license = "glp-3")

create_dependency(
  "framework7", 
  tag = "5.7.14", 
  options = charpente_options(bundle = TRUE)
)


set_pwa(path= "/summer/",
        register_service_worker = F,
        create_dependencies = F,
        startUrl = "https://tamnol.shinyapps.io/summer/",)


