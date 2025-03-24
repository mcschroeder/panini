FROM ubuntu:22.04
ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y python3 python3-pip libgraphviz-dev cmake
RUN pip3 install fuzzingbook graphviz
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 1

WORKDIR /eval
COPY . /eval
ENTRYPOINT ["python3", "eval.py"]
