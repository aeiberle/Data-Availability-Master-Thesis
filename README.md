# To mob or not to mob: Social facilitation of recruitment events in meerkat groups

This repository contains the R scripts used for the analysis in the MSc thesis of Annika Eiberle, conducted at the University of Konstanz. The thesis investigates the dynamics of recruitment behavior in meerkat groups, focusing on signal propagation and social facilitation during mobbing events.

## Table of Contents

- [About](#about)
- [Data Collection](#data-collection)
- [Scripts](#scripts)
- [Usage](#usage)
- [License](#license)
- [Acknowledgements](#acknowledgements)

## About

Group living in meerkats offers advantages such as improved foraging and predator defense, including cooperative mobbing behavior. This study explores how individual participation in risky actions influences recruitment and group dynamics. Using GPS loggers and audio recorders on a habituated wild meerkat population at the Kalahari Research Center in South Africa, individual movement and vocalizations were recorded. The analysis, performed using these R scripts, revealed a cascading effect in signal propagation, with vocal responses spreading within the group. A higher change in 'lead' call rate was present, increasing the likelihood of meerkats approaching a stimulus source.

## Data Collection

Data was collected at the Kalahari Research Center in South Africa using:

*   GPS loggers for individual movement tracking.
*   Audio recorders for vocalization recording.

## Scripts

The repository contains the following R scripts:

*   `1_new_sync_script.R`: New synchronization script.
*   `2_audio_cleaning.R`: Script for audio data cleaning.
*   `2.1_audio_fusion_withGD.R`: Script for audio fusion with GD.
*   `3_getting_points_script_new.R`: Script for obtaining points.
*   `4_audio_gps_timeline.R`: Script for creating an audio-GPS timeline.
*   `5_call_type_density_plots.R`: Script for generating call type density plots.
*   `5.1_call_type_density_plots_120secs.R`: Script for call type density plots (120 seconds).
*   `6_change_point_analysis.R`: Script for change point analysis.
*   `6.1_change_point_analysis_sex.R`: Script for change point analysis between sexes.
*   `6.2_change_point_analysis_socialcat.R`: Script for change point analysis across social categories.
*   `7_signal_propagation.R`: Script for analyzing signal propagation.
*   `8_mean_distances_speaker.R`: Script for mean individual distances to speaker plots.
*   `9_mobbing_recruitment_duration.R`: Script for analyzing mobbing and recruitment duration.
*   `10_testing_pull_influence.R`: Script for testing pull influence.
*   `getting_points_for_veg_assessment.R`: Script for obtaining points for vegetation assessment.
*   `Sample_sizes.R`: Script calculating sample sizes.

*(It would be very helpful to have a slightly more detailed description of each script if possible. Even a short sentence would improve understanding. If not available, this is still a good starting point)*

## Usage

These scripts are written in R and require an R environment to run. Specific R packages used within the scripts are not explicitly listed here, but the scripts themselves contain `library()` calls that specify dependencies. If you encounter errors, please ensure you have the necessary packages installed using `install.packages("package_name")`.

## License

## Acknowledgements

This work was conducted as part of an MSc thesis by Annika Eiberle at the University of Konstanz. Data collection was performed at the Kalahari Research Center in South Africa.
