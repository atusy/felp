# Tokyo as default CRAN mirror
options(repos = c(CRAN = "https://cran.ism.ac.jp/"))

# Pkg

## Installed packages
installed <- rownames(utils::installed.packages())

## Stop if callr not installed
if(!('callr' %in% installed)) stop('Install callr to load .Rprofile') 

## Required packages on CRAN and GitHub

cran <- c(
  'data.table',
  'dplyr',
  'ggplot2',
  'here',
  'pacman',
  'pipeR',
  'purrr',
  'stringr',
  'tidyr'
)

gh <- c('atusy/mytools')
gh2 <- gsub('.*/', '', gh)

## Which required packages are not installed?

cran_missing <- setdiff(c('devtools', cran), installed)
gh_missing <- gh[!(gh2 %in% installed)]

## Install missing packages

callr::r(
  function(cran, gh) {
    if(length(cran) > 0) utils::install.packages(cran)
    if(length(gh) > 0) devtools::install_github(gh)
    invisible(NULL)
  },
  args = list(cran = cran_missing, gh = gh_missing),
  repos = getOption('repos'),
  user_profile = FALSE
)

# Done if interactive mode on RStudio

if(interactive() && "RSTUDIO" %in% names(Sys.getenv())) {
  
  ## Copy .Rprofile to project root
  if(!file.exists('.Rprofile') && dir(pattern = '\\.Rproj$') > 0)
    file.copy('~/R/.Rprofile', '.Rprofile')
  
  ## Set default packages
  options(defaultPackages =
    c(getOption('defaultPackages'), cran, gh2)
  )
}

# Remove values

rm(installed, cran, gh, gh2)
