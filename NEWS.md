# mathpix 0.2.0

* Ability to not insert the LaTeX block `(insert = FALSE)`.
* Errors encountered in processing now show to the user rather than returning an empty block (#4).
* Ability to retry equation detection with a pre-processed image `(retry=TRUE)` (#3, #5).
  - explicit `magick` support now required.
  - tested with `tools/eq_no_05_screencapfixes.jpg` and retrying works.
* Providing a non-existent file produces an error rather than a warning.
* README is now self-contained (images in `tools/`).
* README now directs users to https://dashboard.mathpix.com for key requests (#2).
* Added Appveyor support.
* Added `covr` support.

# mathpix 0.1.0

* Preparing for initial CRAN release. 
* Feature-complete access to the Mathpix API.
* Includes a latex renderer via texPreview.




