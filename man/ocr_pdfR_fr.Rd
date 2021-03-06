% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_pdf.R
\name{ocr_pdfR_fr}
\alias{ocr_pdfR_fr}
\title{ocr a pdf file via R with multicore support}
\usage{
ocr_pdfR_fr(
  pdf_file,
  hmax = 300,
  cropped_tm_dir,
  azure_creds,
  box_push_to_az = F,
  box_highlight = F,
  remove_fl = F,
  dpi = 400,
  main_cl = 1,
  sub_cl = 3
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

\item{main_cl}{number of cores for main processes}

\item{sub_cl}{number of cores for sub processes}
}
\description{
wrap of the ocr pdf with multicore support
}
