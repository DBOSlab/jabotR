% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jabot_download.R
\name{jabot_download}
\alias{jabot_download}
\title{Download plant specimen records from JABOT}
\usage{
jabot_download(herbarium = NULL,
               verbose = TRUE,
               dir = "jabot_download")
}
\arguments{
\item{herbarium}{A vector of specific herbarium acronyms (collection code) in
uppercase letters or leave it as NULL to download records for all JABOT herbaria.}

\item{verbose}{Logical, if \code{FALSE}, a message showing steps when downloading
herbarium specimen records will not be printed in the console in full.}

\item{dir}{Pathway to the computer's directory, where the file will be saved.
The default is to create a directory named \code{jabot_download}
and the results will be saved within a subfolder named by each searched
JABOT-associated herbarium collection.}
}
\value{
Folder with DwC-A files for an specific or all JABOT-associated herbaria.
}
\description{
Download plant specimen records in Darwin Core Format from any
herbarium collection at \href{https://ipt.jbrj.gov.br/jabot}{JABOT}
hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
}
\examples{
\dontrun{

jabot_download(herbarium = c("AFR", "RB"),
               verbose = TRUE,
               dir = "jabot_download")
}

}
\seealso{
\code{\link{jabot_summary}}
}
\author{
Domingos Cardoso
}
