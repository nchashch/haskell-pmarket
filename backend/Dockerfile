FROM haskell:8
WORKDIR /code
COPY . /code
RUN stack build
EXPOSE 8080
CMD ["stack", "run"]
