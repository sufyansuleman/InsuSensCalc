@echo off
REM Run from package root: install missing R deps and run R CMD check
cd /d %~dp0
echo Installing missing R packages (if any)...
Rscript --vanilla -e "options(repos = c(CRAN='https://cloud.r-project.org')); pkgs <- c('dplyr','tibble','magrittr','tidyr','knitr','rmarkdown','remotes','testthat'); missing <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]; if(length(missing)) install.packages(missing)"
echo Running R CMD check...
if exist "C:\Program Files\R\R-4.4.0\bin\R.exe" (
  "C:\Program Files\R\R-4.4.0\bin\R.exe" CMD check --no-manual --as-cran .
) else (
  R CMD check --no-manual --as-cran .
)
exit /b %ERRORLEVEL%
