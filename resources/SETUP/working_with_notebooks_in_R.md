#### The following instructions will help you get started with using jupyter notebooks in R. These instructions have been tested on Windows 7 64 bit OS.
#### These instructions are derived from [here](https://www.datacamp.com/community/blog/jupyter-notebook-r#gs.Uue2ltE)

First, you'll need to install some packages. Make sure that you don't do this in your RStudio console, but in a regular R terminal, otherwise you'll get an error like this:

`Error in IRkernel::installspec() :
Jupyter or IPython 3.0 has to be installed but could neither run “jupyter” nor “ipython”, “ipython2” or “ipython3”.
(Note that “ipython2” is just IPython for Python 2, but still may be IPython 3.0)
$ R`

Open up the R console and type the following command,

> `install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))`

Select a CRAN mirror closest to your location to install the necessary packages. 

Now execute the following command, > `devtools::install_github('IRkernel/IRkernel')`

Your computer must have a working instantiation of `IPython`. If it is not installed, please install it from [here](https://www.anaconda.com/download/). Makes sure you choose, `Python 2.7` for installation. Install Anaconda IPython. Once the installation is completed, type the command, > `IRkernel::installspec(user = FALSE)` 

Now, launch the `Anaconda Prompt` terminal window and type the command `jupyter notebook`. Do not type it in the R console for it will not work. This will launch the IPython notebook in your browser window. You'll see R appearing in the list of kernels when you create a new notebook. 

Enjoy and keep calm.
