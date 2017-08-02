################################################################################
# Dockerfile for building and running analysis  
# 
################################################################################

# get the base image, this one has R, RStudio and pandoc
FROM rocker/verse:3.4.1

# required
MAINTAINER Matt Mulvahill <matthew.mulvahill@ucdenver.edu>

# Copy contents of this directory to the container
#COPY . /bp-strauss-study
#WORKDIR /bp-strauss-study

# Docker Hub (and Docker in general) chokes on memory issues when compiling
# with gcc, so copy custom CXX settings to /root/.R/Makevars and use ccache and
# clang++ instead

# Make ~/.R
RUN mkdir -p $HOME/.R

# Install ggplot extensions like ggstance and ggrepel
RUN apt-get -qq update \
		&& apt-get -qq -y install \
				clang  \
				ccache \
				git 
        

# Below only transitory -- would be req'd in each session
# R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-14/')" \ 

# Install R packages not in tidyverse
RUN install2.r --error \
        pbapply pryr assertr ggthemes

RUN git clone https://github.com/mmulvahill/bp-strauss-study.git 
WORKDIR /bp-strauss-study
RUN git checkout test_sd_sticky_points

# $HOME doesn't exist in the COPY shell, so be explicit
COPY ./R/Makevars /root/.R/Makevars
#RUN R -e "getwd(); dir()"

RUN git submodule init \
		&& git submodule update \
    && R -e "library(devtools); install('lib/pulsatile');" 

####

