% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_pdf.R
\name{ocr_pdf}
\alias{ocr_pdf}
\title{ocr a pdf file}
\usage{
ocr_pdf(
  pdf_file,
  hmax = 200,
  cropped_tm_dir,
  azure_creds,
  box_push_to_az = F,
  box_highlight = F,
  remove_fl = F,
  dpi = 400
)
}
\arguments{
\item{pdf_file}{pdf file location}

\item{hmax}{maximum box height to include}

\item{cropped_tm_dir}{temporary dir for cropped img}

\item{azure_creds}{azure credential}

\item{box_push_to_az}{whether to push the individual boxes to azure}

\item{box_highlight}{whether to highlight boxes, this allows the check to use results from the main push if it seems to work well}

\item{remove_fl}{whether to remove files written by the box level push}

\item{dpi}{dpi of the converted images}
}
\description{
wrap of the ocr wrapper for images
}
