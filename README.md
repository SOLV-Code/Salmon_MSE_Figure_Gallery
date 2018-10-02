## Instructions for Contributors

### How to Contribute
* [Clone](https://help.github.com/articles/cloning-a-repository/) or [fork](https://help.github.com/articles/fork-a-repo/) the repository
* In your local copy, add a named folder
* In your named folder, create 1 subfolder for each plot you want to add
* In the plot folder, include a *.png file with the plot and a *.R with the code to generate the plot, as well as any required data files as *.csv.
* or other. Add a brief blurb and a link to the *.png file in this README.md file, using the same template as the existing examples below.
* Commit your changes and push them to the repository using [Git Bash](https://dont-be-afraid-to-commit.readthedocs.io/en/latest/git/commandlinegit.html) or
[a Git GUI](https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Graphical-Interfaces)


## Gallery

#### Percent Rank Transformation

* **Challenge**: Display patterns in a group of populations that differ by several orders of magnitude (hundreds to millions of fish), both between populations and over time. Plotting the time series on a log scale is the typical approach, but it creates problems for visual interpretation, particularly with the mixed audiences in collaborative planning processes.
* **Approach**: Transform the data into percent ranks, which rescale the smallest observation as 0, the largest observation as 1, and the median as 0.5. This puts all time series on the same vertical axis and gives the values a more direct interpretation. The examples below illustrate these benefits for visual communication, but the question remains whether percent ranks are mathematically appropriate transformations for this type of data. 


<p float="left">

<img src="https://github.com/SOLV-Code/Graph-Gallery/blob/master/SOLV/PercentRanks/PercRank_FinalPlots.png" width="250" height="250"> 

<img src="https://github.com/SOLV-Code/Graph-Gallery/blob/master/SOLV/PercentRanks/PercRank_ProgressionInSparklines.png" width="250" height="250">

</p>

### Trajectories


#### Four-panel trajectory plots

* **Challenge**: Display simulated trajectories across scenarios. In scenarios where productivity or carrying capacity are altered, reference points change between scenarios.
* **Approach**: Display results for each stock separately to avoid too busy of plots. 

<p float="left">

<img src="https://github.com/brookemdavis/Salmon_MSE_Figure_Gallery/blob/master/Salmon%20MSE/Four-panel%20Trajectories/4PanelEsc.png" width="250" height="250"> 


</p>



