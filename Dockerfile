FROM gcr.io/forsyse/shinybase:latest

## app folder
RUN mkdir /root/forsys
RUN mkdir /root/forsys/data
RUN mkdir /root/forsys/R

RUN R -e "install.packages('class')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('directlabels')"
RUN R -e "install.packages('doParallel')"
RUN R -e "install.packages('foreign')"
RUN R -e "install.packages('gtools')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('maptools')"
RUN R -e "install.packages('pacman')"
RUN R -e "install.packages('raster')"
RUN R -e "install.packages('rgdal')"
RUN R -e "install.packages('rgeos')"
RUN R -e "install.packages('roxygen2')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyBS')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('tidyverse')"

COPY ./R/ForSys.R /root/forsys/R/ForSys.R
COPY ./R/forsys_functions.R /root/forsys/R/forsys_functions.R
COPY ./R/forsys_libraries.R /root/forsys/R/forsys_libraries.R
COPY ./R/forsys_results_functions.R /root/forsys/R/forsys_results_functions.R
COPY ./R/forsys_scenario_functions.R /root/forsys/R/forsys_scenario_functions.R

COPY ./app.R /root/forsys/app.R
COPY ./server.R /root/forsys/server.R
COPY ./ui.R /root/forsys/ui.R

EXPOSE 3838

# run app
CMD ["R", "-e", "shiny::runApp('/root/forsys', host = '0.0.0.0', port = 3838)"]
