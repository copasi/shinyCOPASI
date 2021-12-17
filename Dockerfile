FROM rocker/shiny:4.0.3

RUN apt-get update
RUN apt-get install --yes libxml2-dev
RUN apt-get install --yes libssl-dev
RUN apt-get install --yes libudunits2-dev
RUN apt-get install --yes libharfbuzz-dev libfribidi-dev
RUN apt-get install --yes libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
RUN Rscript -e 'install.packages("shinyjs", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'install.packages("remotes", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'remotes::install_github("jpahle/CoRC")'
RUN Rscript -e 'install.packages("reshape2", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'install.packages("ggplot2", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinyTree", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'install.packages("formattable", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'install.packages("XML", repos="http://cran.r-project.org", dependencies = TRUE)'
RUN Rscript -e 'install.packages("DT", repos="http://cran.r-project.org", dependencies = TRUE)'
COPY shiny-server.conf /etc/shiny-server/
COPY --chown=shiny:shiny about.md title.md ui.R server.R /srv/shiny-server/
COPY --chown=shiny:shiny www/ /srv/shiny-server/www/
# could use the folling to inject and display the latest build time at the webif
#RUN sed -i "s/BUILD_TIMESTAMP/$(TZ=":America/New_York" date -Iseconds)/" ui.R
