FROM maven:3.9.9-ibm-semeru-17-focal

RUN apt-get update && \
    apt-get install -y \
    python3 \
    && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY pom.xml .
COPY src ./src
COPY run_eval.sh .
RUN mvn clean package -DskipTests
CMD ["./run_eval.sh"]
