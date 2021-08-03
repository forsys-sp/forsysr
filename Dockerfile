FROM r-base:4.1.0

RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('pacman', repos='http://cran.rstudio.com/')"

RUN mkdir /root/forsys
RUN mkdir /root/forsys/configs
RUN mkdir /root/forsys/data
RUN mkdir /root/forsys/man
RUN mkdir /root/forsys/R

COPY DESCRIPTION /root/forsys/DESCRIPTION
COPY NAMESPACE /root/forsys/NAMESPACE

COPY ./configs/* /root/forsys/configs/
COPY ./data/* /root/forsys/data/
COPY ./man/* /root/forsys/man/
COPY ./R/* /root/forsys/R/


CMD R