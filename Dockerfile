################################################################################
# Dockerfile for building and running analysis  
# 
################################################################################

# get the base image, this one has R, RStudio and pandoc
FROM rocker/verse:3.4.1

# required
MAINTAINER Matt Mulvahill <matthew.mulvahill@ucdenver.edu>

# Copy contents of this directory to the container
COPY . /bp-strauss-study
WORKDIR /bp-strauss-study

# Docker Hub (and Docker in general) chokes on memory issues when compiling
# with gcc, so copy custom CXX settings to /root/.R/Makevars and use ccache and
# clang++ instead

# Make ~/.R
RUN mkdir -p $HOME/.R

# $HOME doesn't exist in the COPY shell, so be explicit
COPY ./R/Makevars /root/.R/Makevars

# Install ggplot extensions like ggstance and ggrepel
# Install ed, since nloptr needs it to compile.
# Install all the dependencies needed by rstanarm and friends
# Install multidplyr for parallel tidyverse magic
RUN apt-get -qq update \
		&& apt-get -qq -y install \
				clang  \
				ccache \
				git \
    && install2.r --error \
        pbapply pryr assertr ggthemes\
    && R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-14/')" \
		&& git submodule init \
		&& git submodule update \
    && R -e "library(devtools); install('lib/pulsatile');" 


 # go into the repo directory
#RUN   apt-get update \
#  #&& sudo apt-get install r-cran-rjags -y \
#	# source environment file
#  && . /etc/environment \ 
#  
#  # build this compendium package, get deps from MRAN
#  # set date here manually
#  #&& R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-14/'); devtools::install('/thesis-analysis', dep=TRUE)" \
#  #&& R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-14/'); devtools::install_github('BayesPulse/pulsatile')" \
#  && R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-14/')" \
#
#
#	# Execute test script
#	#&& R -e "rmarkdown::render('/thesis-analysis/test.Rmd')"
#
#  # render the manuscript into a docx
#  #&& R -e "rmarkdown::render('/mjb1989excavationpaper/vignettes/analysis-of-dates-lithics-shell-from-1989-excavations.Rmd')"
#



# Get my package source files from github and download onto Docker. The built 
# package that we already got above is no good because it doesn't have the 
# vignette directory in the same structure as the package source
# RUN git clone https://github.com/benmarwick/1989-excavation-report-Madjebebe.git  


# to build this image:
# docker build -t benmarwick/mjb1989excavationpaper https://raw.githubusercontent.com/benmarwick/1989-excavation-report-Madjebebe/master/Dockerfile

# to run this container:
# docker -dp 8787:8787 benmarwick/mjb1989excavationpaper
# then open broswer at localhost:8787 or http://192.168.59.103:8787/
