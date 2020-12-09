FROM gcr.io/forsyse/shinybase:latest

## app folder
RUN mkdir /root/forsys
RUN mkdir /root/forsys/data
RUN mkdir /root/forsys/R

RUN R -e "install.packages('maptools')"
RUN R -e "install.packages('pacman')"
RUN R -e "install.packages('rgdal')"
RUN R -e "install.packages('rgeos')"
RUN R -e "install.packages('roxygen2')"
RUN R -e "install.packages('shinyBS')"
RUN R -e "install.packages('sp')"

COPY ./data/IDHexnet_North20190523_Final.dbf /root/forsys/data/IDHexnet_North20190523_Final.dbf
COPY ./R/ForSys.R /root/forsys/R/ForSys.R
COPY ./R/forsys_functions.R /root/forsys/R/forsys_functions.R
COPY ./R/forsys_input_summarization.R /root/forsys/R/forsys_input_summarization.R
COPY ./R/forsys_libraries.R /root/forsys/R/forsys_libraries.R
COPY ./R/forsys_scenario_functions.R /root/forsys/R/forsys_scenario_functions.R
COPY ./R/forsys_xml.R /root/forsys/R/forsys_xml.R
COPY ./config_Idaho.R /root/forsys/R/config_Idaho.R
COPY ./server.R /root/forsys/server.R
COPY ./ui.R /root/forsys/ui.R
COPY ./app.R /root/forsys/app.R


EXPOSE 3838

# run app
CMD ["R", "-e", "shiny::runApp('/root/forsys', host = '0.0.0.0', port = 3838)"]
