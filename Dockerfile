FROM --platform=linux/amd64 rocker/geospatial
RUN install2.r rmapshaper
RUN installGithub.r dblodgett-cycling/gpxr
WORKDIR /workdir
