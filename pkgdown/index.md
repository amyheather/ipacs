
<!-- index.md is generated from index.Rmd. Please edit that file -->
<!-- Remember to knit index.Rmd to recreate index.md -->

# IPACS

This package contains models from the project “Improving Patient flow
between Acute, Community and Social care (IPACS)”, converted into a
package format.

**How do I run the model?** Click here: “[How to run the IPACS
model](https://amyheather.github.io/ipacs/articles/how_to_run_the_ipacs_model.html)”.

**How do I find out more about the functions?** Click here: “[Function
reference](https://amyheather.github.io/ipacs/reference/index.html)”.

<img src="reference/figures/flowmap.png">

The IPACS v1 model is a generic, open source, stochastic simulation
model which enables scenario modelling of ‘Discharge to Assess’ (D2A)
resource requirements. It models simple flows of ’complex care‘ patients
who have been identified as suitable for discharge from an acute
hospital bed into a non-acute care setting for a further, time-limited,
period of rehabilitation and assessment for their longer-term care
requirements. It comprises three distinct care pathways where additional
inputs of community health and social care are provided as seen in the
flowchart. The complex care pathways are:

- P1 (visits-based care where the patient is in their own home and
  receives care inputs in that location. Funded by the community health
  provider)  
- P2 (non-acute, bed-based care funded by the community health
  provider)  
- P3 (non-acute, bed-based care funded by local authority social care
  provider).

This package version just includes an example of the P1 pathway
(visit-based simulation). It does not currently include the bed-based
simulation or report generation.
