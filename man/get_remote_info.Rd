% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_remote_info.R
\name{get_remote_info}
\alias{get_remote_info}
\alias{remote_package_names}
\title{Get Remote Information}
\usage{
get_remote_info(path = "DESCRIPTION")

remote_package_names(path = "DESCRIPTION")
}
\arguments{
\item{path}{Path to DESCRIPTION file}
}
\value{
List of parsed files
}
\description{
Parses DESCRIPTION file
}
\examples{
path = example_description_file()
desc = get_remote_info(path)
}
