FROM r-base:latest
RUN apt-get -y update
RUN apt-get install -t testing -y --no-install-recommends \
    default-jdk \
    default-jre \
    libssl-dev \
    libcurl4-openssl-dev/sid \
    libxml2-dev
RUN apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN R -e 'install.packages(c("xlsx", "httr", "stringr", "yaml", "xml2", "plyr", "reshape2"))'
RUN mkdir -p /src
WORKDIR /src
