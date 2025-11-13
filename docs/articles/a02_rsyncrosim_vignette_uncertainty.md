# rsyncrosim: introduction to uncertainty

This vignette will cover Monte Carlo realizations for modeling
uncertainty using the `rsyncrosim` package within the
[SyncroSim](https://syncrosim.com/) software framework. For an overview
of [SyncroSim](https://syncrosim.com/) and
[`rsyncrosim`](https://cran.r-project.org/web/packages/rsyncrosim/index.html),
as well as a basic usage tutorial for `rsyncrosim`, see the
[Introduction to
`rsyncrosim`](https://syncrosim.github.io/rsyncrosim/articles/a01_rsyncrosim_vignette_basic.html)
vignette.

## SyncroSim Package: `helloworldUncertainty`

To demonstrate how to quantify model uncertainty using the `rsyncrosim`
interface, we will need the
[helloworldUncertainty](https://github.com/ApexRMS/helloworldUncertainty)
SyncroSim package. `helloworldUncertainty` was designed to be a simple
package for introducing iterations to SyncroSim modeling workflows. The
use of iterations allows for repeated simulations, known as “Monte Carlo
realizations”, in which each simulation independently samples from a
distribution of values.

The package takes from the user 3 inputs, *mMean*, *mSD*, and *b*. For
each iteration, a value *m*, representing the slope, is sampled from a
normal distribution with mean of *mMean* and standard deviation of
*mSD*. The *b* value represents the intercept. These input values are
run through a linear model, *y=mt+b*, where *t* is *time*, and the *y*
value is returned as output.

![Infographic of helloworldUncertainty
package](infographic-uncertainty.png)

Infographic of helloworldUncertainty package

For more details on the different features of the
`helloworldUncertainty` SyncroSim package, consult the SyncroSim
[Enhancing a Package: Representing
Uncertainty](https://docs.syncrosim.com/how_to_guides/package_create_iterations.html)
tutorial.

## Setup

### Install SyncroSim

Before using `rsyncrosim` you will first need to [download and
install](https://syncrosim.com/download/) the SyncroSim software.
Versions of SyncroSim exist for both Windows and Linux.

*Note*: this tutorial was developed using `rsyncrosim` version 2.0. To
use `rsyncrosim` version 2.0 or greater, SyncroSim version 3.0 or
greater is required.

### Installing and loading R packages

You will need to install the `rsyncrosim` R package, either using
[CRAN](https://cran.r-project.org/) or from the `rsyncrosim` [GitHub
repository](https://github.com/syncrosim/rsyncrosim/releases/). Versions
of `rsyncrosim` are available for both Windows and Linux.

In a new R script, load the `rsyncrosim` package.

``` r

# Load R package for working with SyncroSim
library(rsyncrosim)
```

### Connecting R to SyncroSim using `session()`

Finish setting up the R environment for the `rsyncrosim` workflow by
creating a SyncroSim Session object. Use the
[`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
function to connect R to your installed copy of the SyncroSim software.

``` r

mySession <- session("path/to/install_folder")      # Create a Session based SyncroSim install folder
mySession <- session()                              # Using default install folder (Windows only)
mySession                                           # Displays the Session object
```

    ## class               : Session
    ## filepath [character]: C:\PROGRA~1\SYNCRO~1
    ## silent [logical]    : TRUE
    ## printCmd [logical]  : FALSE
    ## condaFilepath [NULL]:

Use the
[`version()`](https://syncrosim.github.io/rsyncrosim/reference/version.md)
function to ensure you are using the latest version of SyncroSim.

``` r

version(mySession)
```

    ## [1] "3.1.20"

### Installing SyncroSim packages using `installPackage()`

Install `helloworldUncertainty` using the `rynscrosim` function
[`installPackage()`](https://syncrosim.github.io/rsyncrosim/reference/installPackage.md).
This function takes a package name as input and then queries the
SyncroSim package server for the specified package.

``` r

# Install helloworldUncertainty
installPackage("helloworldUncertainty")
```

    ## Package <helloworldUncertainty v2.1.0> installed

`helloworldUncertainty` should now be included in the package list when
we call the
[`packages()`](https://syncrosim.github.io/rsyncrosim/reference/packages.md)
function:

``` r

# Get list of installed packages
packages()
```

    ##                    name version
    ## 1 helloworldUncertainty   2.1.0
    ##                                                   description
    ## 1 Example demonstrating how to use iterations with an R model
    ##                                                                                    location
    ## 1 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldUncertainty\\2.1.0
    ##   status
    ## 1     OK

## Create a modeling workflow

When creating a new modeling workflow from scratch, we need to create
objects of the following scopes:

- [Library](https://docs.syncrosim.com/how_to_guides/library_overview.html)
- [Projects](https://docs.syncrosim.com/how_to_guides/library_overview.html)
- [Scenarios](https://docs.syncrosim.com/how_to_guides/library_overview.html)

For more information on these scopes, see the [Introduction to
`rsyncrosim`](https://syncrosim.github.io/rsyncrosim/rsyncrosim_vignette_basic.html)
vignette.

### Set up library, project, and scenario

``` r

# Create a new library
myLibrary <- ssimLibrary(name = "helloworldLibrary.ssim",
                         session = mySession,
                         packages = "helloworldUncertainty",
                         overwrite = TRUE)
```

    ## Package <helloworldUncertainty v2.1.0> added

``` r

# Open the default project
myProject = project(ssimObject = myLibrary, project = "Definitions")

# Create a new scenario (associated with the default project)
myScenario = scenario(ssimObject = myProject, scenario = "My first scenario")
```

### View model inputs using `datasheet()`

View the datasheets associated with your new scenario using the
[`datasheet()`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md)
function from `rsyncrosim`.

``` r

# View all datasheets associated with a library, project, or scenario
datasheet(myScenario)
```

    ##       scope                                  name             displayName
    ## 25 scenario                core_DistributionValue           Distributions
    ## 26 scenario            core_ExternalVariableValue      External Variables
    ## 27 scenario                         core_Pipeline                Pipeline
    ## 28 scenario           core_SpatialMultiprocessing Spatial Multiprocessing
    ## 29 scenario  helloworldUncertainty_InputDatasheet                  Inputs
    ## 30 scenario helloworldUncertainty_OutputDatasheet                 Outputs
    ## 31 scenario      helloworldUncertainty_RunControl             Run Control

From the list of datasheets above, we can see that there are three
datasheets specific to the `helloworldUncertainty` package. Let’s view
the contents of the `Inputs` datasheet as an R data frame.

``` r

# View the contents of the Inputs datasheet for the scenario
datasheet(myScenario, name = "helloworldUncertainty_InputDatasheet")
```

    ## [1] mMean mSD   b    
    ## <0 rows> (or 0-length row.names)

### Configure model inputs using `datasheet()` and `addRow()`

**Inputs Datasheet**

Currently our input scenario datasheet is empty! We need to add some
values to our `Inputs` datasheet (`InputDatasheet`) so we can run our
model. First, assign the contents of the `Inputs` datasheet to a new
data frame variable using
[`datasheet()`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md),
then check the columns that need input values.

``` r

# Load the Inputs datasheet to an R data frame
myInputDataframe <- datasheet(myScenario,
                              name = "helloworldUncertainty_InputDatasheet")

# Check the columns of the input data frame
str(myInputDataframe)
```

    ## 'data.frame':    0 obs. of  3 variables:
    ##  $ mMean: num 
    ##  $ mSD  : num 
    ##  $ b    : num

The `Inputs` datasheet requires three values:

- `mMean` : the mean of the slope normal distribution.
- `mSD` : the standard deviation of the slope normal distribution.
- `b` : the intercept of the linear equation.

Add these values to a new data frame, then use the
[`addRow()`](https://syncrosim.github.io/rsyncrosim/reference/addRow.md)
function from `rsyncrosim` to update the input data frame.

``` r

# Create input data and add it to the input data frame
myInputRow <- data.frame(mMean = 2, mSD = 4, b = 3)
myInputDataframe <- addRow(myInputDataframe, myInputRow)

# Check values
myInputDataframe
```

    ##   mMean mSD b
    ## 1     2   4 3

Finally, save the updated R data frame to a SyncroSim datasheet using
[`saveDatasheet()`](https://syncrosim.github.io/rsyncrosim/reference/saveDatasheet.md).

``` r

# Save input R data frame to a SyncroSim datasheet
saveDatasheet(ssimObject = myScenario, data = myInputDataframe,
              name = "helloworldUncertainty_InputDatasheet")
```

    ## Datasheet <helloworldUncertainty_InputDatasheet> saved

**Pipeline Datasheet**

Next, we need to add data to the Pipeline datasheet. The Pipeline
datasheet determines which transformers the scenarios will run and in
which order. Use the code below to assign the Pipeline datasheet to a
new data frame variable and check the values required by the datasheet.

``` r

# Assign contents of the Pipeline datasheet to an R data frame
myPipeline <- datasheet(myScenario,
                        name = "core_Pipeline")

# Check the columns of the Pipeline data frame
str(myPipeline)
```

    ## 'data.frame':    0 obs. of  2 variables:
    ##  $ StageNameId: Factor w/ 1 level "Hello World Uncertainty (R)": 
    ##  $ RunOrder   : num

The Pipeline datasheet requires 2 values:

- `StageNameId` : the pipeline stage (transformer). This column is a
  factor that has only a single level: “Hello World Uncertainty (R)”.
- `RunOrder` : the numerical order in which the stages will be run.

Below, we use the
[`addRow()`](https://syncrosim.github.io/rsyncrosim/reference/addRow.md)
and
[`saveDatasheet()`](https://syncrosim.github.io/rsyncrosim/reference/saveDatasheet.md)
functions to update the Pipeline datasheet with the transformer(s) we
want to run and the order in which we want to run them. In this case,
there is only a single transformer available from the
`helloworldUncertainty` package, called “Hello World Uncertainty (R)”,
so we will add this transformer to the data frame and set the `RunOrder`
to `1`.

``` r

# Create pipeline data and add it to the pipeline data frame
myPipelineRow <- data.frame(StageNameId = "Hello World Uncertainty (R)", RunOrder = 1)
myPipeline <- addRow(myPipeline, myPipelineRow)

# Check values
myPipeline
```

    ##                   StageNameId RunOrder
    ## 1 Hello World Uncertainty (R)        1

``` r

# Save Pipeline R data frame to a SyncroSim Datasheet
saveDatasheet(ssimObject = myScenario, data = myPipeline,
              name = "core_Pipeline")
```

    ## Datasheet <core_Pipeline> saved

**Run Control Datasheet**

The `Run Control` datasheet provides information about how many time
steps and iterations to use in the model. Here, we set the *number of
iterations*, as well as the minimum and maximum time steps for our
model. The number of iterations we set is equivalent to the number of
Monte Carlo realizations, so the greater the number of iterations, the
more accurate the range of output values we will obtain. Let’s take a
look at the columns that need input values.

``` r

# Load Run Control datasheet to a new R data frame
runSettings <- datasheet(myScenario, name = "helloworldUncertainty_RunControl")

# Check the columns of the Run Control data frame
str(runSettings)
```

    ## 'data.frame':    0 obs. of  3 variables:
    ##  $ MinimumTimestep : num 
    ##  $ MaximumTimestep : num 
    ##  $ MaximumIteration: num

The `Run Control` datasheet requires the following 3 columns:

- `MaximumIteration` : total number of iterations to run the model for.
- `MinimumTimestep` : the starting time point of the simulation.
- `MaximumTimestep` : the end time point of the simulation.

*Note:* A fourth hidden column, `MinimumIteration`, also exists in the
`Run Control` datasheet (default=1).

We’ll add this information to an R data frame and then add it to the
`Run Control` data frame using
[`addRow()`](https://syncrosim.github.io/rsyncrosim/reference/addRow.md).
For this example, we will use only five iterations.

``` r

# Create Run Control data and add it to the Run Control data frame
runSettingsRow <- data.frame(MaximumIteration = 5,
                             MinimumTimestep = 1,
                             MaximumTimestep = 10)
runSettings <- addRow(runSettings, runSettingsRow)

# Check values
runSettings
```

    ##   MinimumTimestep MaximumTimestep MaximumIteration
    ## 1               1              10                5

Finally, save the R data frame to a SyncroSim datasheet using
[`saveDatasheet()`](https://syncrosim.github.io/rsyncrosim/reference/saveDatasheet.md).

``` r

# Save Run Control R data frame to a SyncroSim datasheet
saveDatasheet(ssimObject = myScenario, 
              data = runSettings,
              name = "helloworldUncertainty_RunControl")
```

    ## Datasheet <helloworldUncertainty_RunControl> saved

## Run scenarios

### Setting the number of multiprocessing jobs

If we have a large model and we want to parallelize the run using
multiprocessing, we can modify the library-scoped “core_Multiprocessing”
datasheet. Since we are using five iterations in our model, we will set
the number of jobs to five so each multiprocessing core will run a
single iteration.

``` r

# Load list of available library-scoped datasheets
datasheet(myLibrary)
```

    ##      scope                              name                    displayName
    ## 1  library                       core_Backup                         Backup
    ## 2  library                     core_JlConfig                          Julia
    ## 3  library              core_Multiprocessing                Multiprocessing
    ## 4  library                       core_Option                        Options
    ## 5  library         core_ProcessorGroupOption        Processor Group Options
    ## 6  library          core_ProcessorGroupValue         Processor Group Values
    ## 7  library                     core_PyConfig                         Python
    ## 8  library                      core_RConfig                              R
    ## 9  library                      core_Setting                       Settings
    ## 10 library core_SpatialMultiprocessingOption Spatial Multiprocessing Option
    ## 11 library                core_SpatialOption                Spatial Options
    ## 12 library                    core_SysFolder                        Folders
    ## 13 library                  core_Terminology                    Terminology

``` r

# Load the library-scoped multiprocessing datasheet
multiprocess <- datasheet(myLibrary, name = "core_Multiprocessing")
```

    ## [1] "Note: MaximumJobs should be between 1 and 9999"

``` r

# Check required inputs
str(multiprocess)
```

    ## 'data.frame':    1 obs. of  4 variables:
    ##  $ EnableMultiprocessing  : logi FALSE
    ##  $ MaximumJobs            : num 15
    ##  $ EnableMultiScenario    : logi FALSE
    ##  $ EnableCopyExternalFiles: logi NA

``` r

# Enable multiprocessing
multiprocess$EnableMultiprocessing <- TRUE

# Set maximum number of jobs to 5
multiprocess$MaximumJobs <- 4

# Save multiprocessing configuration
saveDatasheet(ssimObject = myLibrary, 
              data = multiprocess, 
              name = "core_Multiprocessing")
```

    ## Datasheet <core_Multiprocessing> saved

### Setting run parameters with `run()`

Now, when we run our scenario, it will use the desired multiprocessing
configuration.

``` r

# Run the first scenario we created
myResultScenario <- run(myScenario)
```

    ## [1] "Running scenario [1] My first scenario"

    ## This model uses Conda environments, but no Conda installation was found. Using local environment.

Running the original scenario creates a new scenario object, known as a
result scenario, that contains a read-only snapshot of the `Inputs`
datasheets, as well as the `Outputs` datasheets filled with result data.
We can view which scenarios are result scenarios using the
[`scenario()`](https://syncrosim.github.io/rsyncrosim/reference/scenario.md)
function from `rsyncrosim`.

``` r

# Check that we have two scenarios, and one is a result scenario
scenario(myLibrary)
```

    ##   ScenarioId ProjectId ParentId                                           Name
    ## 1          1         1       NA                              My first scenario
    ## 2          2         1        1 My first scenario ([1] @ 12-Nov-2025 10:27 AM)
    ##   Owner MergeDependencies IgnoreDependencies IsResult IsReadOnly
    ## 1   N/A                No                 NA       No         No
    ## 2   N/A                No                 NA      Yes         No
    ##         DateLastModified
    ## 1 2025-11-12 at 10:27 AM
    ## 2 2025-11-12 at 10:27 AM

## View results

### Viewing results with `datasheet()`

The next step is to view the `Outputs` datasheets added to the result
scenario when it was run. We can load the result tables using the
[`datasheet()`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md)
function. In this package, the datasheet containing the results is
called “OutputDatasheet”.

``` r

# Results of first scenario
resultsSummary <- datasheet(myResultScenario,
                            name = "helloworldUncertainty_OutputDatasheet")

# View results table
head(resultsSummary)
```

    ## [1] y
    ## <0 rows> (or 0-length row.names)

### Plotting uncertainty in SyncroSim Studio

Now that we have run multiple iterations, we can visualize the
uncertainty in our results. For this plot, we will plot the average *y*
values over time, while showing the 20th and 80th percentiles.

To create a plot using the result scenario we just generated, open the
current library in SyncroSim Studio and sync the updates from
`rsyncrosim` using the “refresh” button in the upper toolbar (circled in
red below). All the updates made in `rsyncrosim` should appear in
SyncroSim Studio. We can now add the result scenario to the Results
Viewer and create our plot. For more information on generating plots in
SyncroSim Studio, see the SyncroSim tutorials on
[creating](http://docs.syncrosim.com/how_to_guides/results_chart_create.md)
and
[customizing](http://docs.syncrosim.com/how_to_guides/results_chart_customize.md)
charts.

![Using rsyncrosim with SyncroSim Studio to plot
uncertainty](rsyncrosim-with-UI-uncertainty.png)

Using `rsyncrosim` with SyncroSim Studio to plot uncertainty
