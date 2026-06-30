# Forensic-BodyFluid-DimRed 

This repository provides R scripts for the dimensionality reduction and visualization of capillary electrophoresis (CE) fluorescence data. It is specifically designed for forensic body fluid identification, supporting the analysis of peripheral blood (PB), saliva (SA), semen (SE), vaginal secretion (VS), and menstrual blood (MB).
## 📊 Usage

1. Clone this repository to your local machine.
2. Place your raw fluorescence data in the project directory. The expected input format is a matrix where the first two columns are metadata (`Body fluid type`, `SampleID`), and the remaining columns are numerical features (genes/fluorescence signals).
3. Update the file paths in the scripts (e.g., `"data/your_file.csv"`) and run the code directly in RStudio.
