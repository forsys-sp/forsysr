FROM gcr.io/forsyse/shinybase:1.12

## app folder
RUN mkdir /root/forsys
RUN mkdir /root/forsys/data
RUN mkdir /root/forsys/R

RUN R -e "pacman::p_install('class', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('data.table', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('directlabels', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('doParallel', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('foreign', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('gtools', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('leaflet', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('maptools', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('raster', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('rgdal', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('rgeos', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('roxygen2', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('sf', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('shinyBS', repos='http://cran.rstudio.com/', force = FALSE)"
RUN R -e "pacman::p_install('shinyjs', repos='http://cran.rstudio.com/', force = FALSE)"

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
