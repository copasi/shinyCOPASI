
<div style="text-align: center">
<img src="www/copasi_new.png" style="width: 500px;"/>
</div>

**ShinyCOPASI** allows COPASI models to be explored through a web browser. It runs on a web server and is built using the R package **Shiny**. It also uses the R package _CoRC_, which provides a high-level API for COPASI in the R language. The application is available at: [http://shiny.copasi.org/](http://shiny.copasi.org/).

**COPASI** is a simulator for biochemical networks. It is a joint project by the Hoops group ([Biocomplexity Institute of Virginia Tech](http://www.bi.vt.edu/)), the Mendes group ([UCONN School of Medicine](https://health.uconn.edu/quantitative-medicine/)), the Kummer, and Sahle groups ([University of Heidelberg](http://www.bioquant.uni-heidelberg.de/research/groups/modeling_of_biological_processes.html)).
 
One way to use/try this web application is to run the Docker image (based on [rocker/shiny](https://hub.docker.com/r/rocker/shiny)) from the [Docker package](https://github.com/copasi/shinyCOPASI/pkgs/container/shiny-copasi), which is (currently) automatically rebuilt on every push to the master branch. Assuming you have [Docker](https://www.docker.com/get-started) installed, and the docker daemon running . . .
```bash
sudo docker run --rm -p 3838:3838 ghcr.io/copasi/shiny-copasi
```
Once all the image layers have downloaded, and the server starts, you should then be able to use the web app from your browser, e.g. at 'localhost:3838'.
The '--rm' above will remove the container, once the server is stopped (e.g. with 'Ctrl-C'), thus removing any state saved in the writable overlay layer on top of the base image.

 For details about the web application, please read:
 
 [Paper](paper/paper.md)
 
 [User Guide](User_Guide.md)
 
 
