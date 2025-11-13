# Sets the progress bar in the SyncroSim User Interface

This function is designed to facilitate the development of R-based
Syncrosim Packages, such as beginning, stepping, ending, and reporting
the progress for a SyncroSim simulation.

## Usage

``` r
progressBar(
  type = "step",
  iteration = NULL,
  timestep = NULL,
  totalSteps = NULL,
  message
)
```

## Arguments

- type:

  character. Update to apply to progress bar. Options include "begin",
  "end", "step", "report", and "message" (Default is "step")

- iteration:

  integer. The current iteration. Only used if `type = "report"`

- timestep:

  integer. The current timestep. Only used if `type = "report"`

- totalSteps:

  integer. The total number of steps in the simulation. Only used if
  `type = "begin"`

- message:

  character. An arbitrary messsage to be printed to the status bar. Only
  used if `type = "message"`.

## Value

No returned value, used for side effects

## Examples

``` r
if (FALSE) { # \dontrun{
# Begin the progress bar for a simulation
progressBar(type = "begin", totalSteps = numIterations * numTimesteps)

# Increase the progress bar by one step for a simulation
progressBar(type = "step")

# Report progress for a simulation
progressBar(type = "report", iteration = iter, timestep = ts)

# Report arbitrary progress message 
progressBar(type = "message", message = msg)

# End the progress bar for a simulation
progressBar(type = "end")
} # }
```
