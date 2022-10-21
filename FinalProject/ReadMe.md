---
author: "Noah"
date: "2022-10-21"
output: html_document
---
## Calling Docker
Begin by building the docker container with the command
``` 
docker build .
```
To run this report, download Docker and call it with the following command
```
docker run -v %cd%/datasci611/FinalProject:/home/rstudio/storage -e PASSWORD=PW123 -p 8787:8787 2b7c92cab74d
```

From here, connect to the localhost by typing the following into a web browser
```
https://localhost:8787
```

Lastly, type the following in the rstudio terminal to create the report
```
make FinalReport.pdf
```
