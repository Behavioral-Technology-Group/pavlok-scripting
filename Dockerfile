FROM ubuntu:20.04
MAINTAINER Maneesh Sethi maneesh@pavlok.com
WORKDIR /app
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y python3 python3-pip
COPY requirements.txt requirements.txt
RUN python3 -m pip install -r requirements.txt
COPY . .
CMD [ "python3", "app.py"]
