# CAP

Resources for analyzing data for CAP (cognition and affect during the COVID-19 pandemic) project.

## Setup

### Prerequisites

Please make sure that you have recent versions of R and RStudio installed. If you plan to use GitHub Desktop, please follow [these installation instructions](https://www.startyourlab.com/docs/github-desktop) if you have not already done so.

### Clone to your local machine

To contribute to this project, please clone the repository to your local machine. You can do this using the green **Code** button with a dropdown menu. There are two preferred methods, and we recommend using whichever you prefer:

#### 1. Open with GitHub Desktop

Click **Open with GitHub Desktop**, and pick a location in your personal file system to store it using the GitHub Desktop cloning interface. If using Linux, please see option 2.

#### 2. Command line using HTTPS method

If using Linux or prefer the command line, run the following line within your projects directory:

```sh
git clone https://github.com/sokolhessnerlab/cap.git
```

### Open the project in RStudio

With the repository cloned to your computer, open the `cap.Rproj` file, which should start a session for you in RStudio.

### Configure your system user details in `config.yml`

With the project open in RStudio, copy-and-paste the following line of R code into your RStudio's Console and press enter.

```r
Sys.info()["user"]
```

The string value returned will be your system's name, which is what the configuration file will use to determine your paths to data and the like. You should then replace the `user` tag in `config.yml` for your system user, and remove the `#` comment strings. You can see Ari's system user `metis` as an example.

To use the configured information, you must install the [`config` package](https://cran.r-project.org/web/packages/config/vignettes/introduction.html) using `install.packages('config')` in the R console. Then, when needed, load the package in your scripts or RMarkdown files:

```r
library(config)
```

And then you can access the `config` object by getting and assigning it to a variable `config`.

```r
config <- config::get()
```

For example, you access the path to data using `config$path$data`. Please use this method for paths instead of hard-coding them!

## [Preprocessing](./preprocessing)

Code for cleaning, combining, and exclusion

## [Documentation](./documentation)

1. Code summaries
2. Column information for datasets
3. Collection summary for the study
4. Storage structure of data on R drive

## [Analysis](./analysis)

Analysis scripts for RDM, AX, and Qualtrics data

1. [RDM](./analysis/RDM)
2. [AX](./analysis/AX)
3. [Qualtrics](./analysis/Qualtrics)
