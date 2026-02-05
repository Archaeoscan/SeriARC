---
title: 'SeriARC: Archaeological Seriation and Correspondence Analysis with Radiocarbon Integration'
tags:
  - R
  - archaeology
  - seriation
  - correspondence analysis
  - radiocarbon dating
  - OxCal
authors:
  - name: Daniel Meixner
    orcid: 0009-0009-1633-9600
    affiliation: 1
affiliations:
 - name: University of Regensburg, Germany
   index: 1
date: 5 February 2025
bibliography: paper.bib
---

# Summary

Archaeological seriation is a fundamental method for establishing relative chronologies by ordering assemblages according to similarity in artifact composition. Correspondence Analysis (CA) has become the standard statistical approach for archaeological seriation, revealing chronological patterns through dimensional reduction of artifact frequency tables. However, integrating these relative chronologies with absolute radiocarbon (¹⁴C) dating remains methodologically challenging, as traditional approaches treat seriation and chronometric dating as separate analytical stages.

`SeriARC` is an interactive R/Shiny application that addresses this challenge by providing an integrated workflow combining correspondence analysis, seriation, radiocarbon calibration, and OxCal integration. The software enables archaeologists to visualize CA results alongside calibrated radiocarbon dates and generate OxCal Chronological Query Language (CQL) code for Bayesian sequence modeling, all within a user-friendly bilingual interface.

# Statement of Need

Archaeological chronology construction requires synthesizing multiple lines of evidence: artifact typology, stratigraphic relationships, and absolute dates. While tools like OxCal [@bronk2009bayesian] excel at Bayesian radiocarbon modeling and TOSCA [@plutniak2022tosca] provides excellent seriation visualization, no existing software seamlessly integrates CA-based seriation with radiocarbon dating in an accessible interface.

`SeriARC` fills this gap by providing:

1. **Integrated CA and ¹⁴C Visualization**: Direct linkage between correspondence analysis results and radiocarbon dates within a single interface
2. **OxCal CQL Generation**: Automated creation of OxCal sequence code based on CA ordering, exportable for further Bayesian analysis
3. **Interactive Exploration**: Dynamic visualization of seriation results, clustering, and chronological relationships
4. **Accessibility**: User-friendly interface designed for archaeologists without programming experience, with bilingual support (German/English)
5. **Reproducibility**: Complete workflow from data import through publication-quality visualizations, with full export capabilities

The software is particularly valuable for prehistoric archaeology, where chronological resolution is limited and multiple dating methods must be synthesized. Typical use cases include Bronze Age and Neolithic assemblages with sparse radiocarbon coverage and high uncertainty overlap.

# Implementation

`SeriARC` is built as a modular Shiny application using established R packages for statistical analysis (`FactoMineR` for CA, `rcarbon` for calibration, `oxcAAR` for OxCal integration). The architecture separates data processing, statistical modeling, and visualization into distinct modules, enabling extensibility and maintenance.

The software implements CA-based seriation with multiple transformation options (relative frequencies, binary, logarithmic) and provides comprehensive clustering algorithms (K-means, hierarchical, fuzzy c-means, Gaussian mixture models) with diagnostic plots. Radiocarbon dates are calibrated using the IntCal20 curve [@reimer2020intcal20] and can be overlaid on CA plots to visually assess chronological coherence.

For users with local OxCal installations, SeriARC provides direct integration for running Bayesian sequence models. For cloud deployments, the software generates CQL code that users can copy into their local OxCal installation, ensuring functionality across different execution environments.

To ensure broad accessibility, `SeriARC` supports two execution modes: a full local installation with optional OxCal integration for advanced users, and a cloud-deployable version (e.g., on shinyapps.io) that provides core functionality without external dependencies.

# Development and Testing

The development of `SeriARC` was assisted by AI-powered coding tools (Claude by Anthropic), which accelerated module development and helped implement robust error handling. All AI-generated code was reviewed, tested, and integrated by the author.

Due to the highly interactive nature of the Shiny interface, automated unit testing of the complete user workflow is impractical. Instead, `SeriARC` was tested manually using standardized archaeological datasets (including the Michelsberg Culture assemblage from the `archdata` package) to verify all analytical workflows, visualizations, and export functions in both supported languages.

The application provides extensive in-app documentation through tooltips, help modals, and contextual explanations integrated directly into the user interface, making external user manuals unnecessary.

# Availability

`SeriARC` is available under GPL-3.0 license. The source code is hosted on GitHub, with example datasets included. A web-accessible demonstration version is available at: https://archaeoscan.shinyapps.io/SeriARC-v-1-0-0/

# Acknowledgments

Development was supported by AI-assisted programming using Claude (Anthropic). IntCal20 calibration data provided by Reimer et al. [-@reimer2020intcal20]. Michelsberg dataset from the `archdata` R package.

# References
