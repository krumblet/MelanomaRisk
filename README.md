# Sun Safety Policies and School Metrics Correlation Analysis

This repository contains the data and code used for analyzing the correlation between sun safety policies in Texas high schools and various school metrics. The analysis focused on evaluating whether certain policies are associated with school and community characteristics.

## Data Description

The data include the following components:
- **Sun Safety Policies**: Presence and strength of sun protection policies across multiple high schools. Policies are coded based on various elements like UV-protective clothing, hats, and sunscreen allowances.
- **School Metrics**: Characteristics of the schools, such as:
  - **Total number of students**
  - **Number of full-time staff**
  - **Number of nurses**
  - **Number of female students**
  - **Teacher turnover rate**
  - Additional school/community attributes like property wealth and community type

## Analysis

The analysis calculates **Spearman rank correlations** between sun safety policy strength and various school metrics. This non-parametric method was chosen due to the ordinal nature of the policy strength data and the potential non-linear relationships with school metrics.

### Methodology

1. **Data Processing**:
   - Missing or unavailable values are handled by replacing them with `NA` and converting appropriate columns to numeric.
   - Policy strengths are summed across components when the intent directly addresses sun protection.

2. **Spearman Rank Correlation**:
   - Correlations between the summed policy strengths and individual school metrics are computed.
   - P-values are reported to assess the statistical significance of each correlation.

## Repository Structure

- `data/`: Contains the processed datasets used in the analysis.
- `scripts/`: Code for performing the correlation analysis.
- `results/`: Spearman rank correlation results, including ranks and p-values for each metric.

## Usage

To replicate the analysis:
1. Clone this repository.
2. Run the scripts in the `scripts/` folder.
3. Refer to the `results/` folder for output.

## License

This project is licensed under the MIT License.
