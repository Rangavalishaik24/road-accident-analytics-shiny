# 🚗 Interactive Road Traffic Accident Analytics Dashboard

An interactive, responsive **R Shiny Dashboard** built to aggregate, filter, visualize, and analyze real-world traffic collision datasets. This application serves as an analytics engine enabling users to slice data by environmental parameters and evaluate driver metrics dynamically.

Developed as a B.Tech Computer Science & Engineering mini-project at **Lingayas Institute of Management and Technology** (Affiliated to JNTUK).

---

## 💡 Core Analytical Capabilities
* **Dynamic Global Filtering:** Seamless reactive filtering across the entire application ecosystem using multi-select inputs for `Accident Severity` and `Weather Conditions`.
* **Demographic Cross-Analysis:** Examines correlations between the driver's age band, sex, and driving experience against crash outcomes.
* **Statistical Validation Engine:** Runs live **Chi-Square Independence Tests** on the dataset subset to mathematically validate the relationship between `Driving_experience` and `Accident_severity`.

---

## 🛠️ Tech Stack & Packages

* **Core Framework:** `shiny`, `shinydashboard`, `shinydashboardPlus`
* **Data Engineering & Manipulation:** `dplyr`
* **Data Visualization:** `ggplot2`, `viridis`
* **Tabular Rendering:** `DT` (DataTables engine)

---

## 📂 Project Directory Structure

The repository is organized within a self-contained RStudio Project environment for seamless, configuration-free deployment:

```text
├── cleaned.csv                             # Preprocessed road traffic accident dataset
├── app.R                                   # Full Interactive R Shiny UI & Server core logic
├── plots.R                                 # Core data visualization test scripts
├── stats.R                                 # Statistical analysis and testing algorithms
├── R mini Project.Rproj                    # RStudio project environment configuration file
└── R mini project Documentation final...   # Comprehensive project report PDF
