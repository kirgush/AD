

# Plan for QRM
Do we want to build an independent LSMC engine `Algorithmic Adjoint Differentiation`-friendly ?

  - Prototype (learning, Wolfram Language):

    - value a Bermudean option
    - implement LSMC
        - can it be turned on/off ?
    - implement AAD from scratch and check against light-weight automatic AAD
    - consider scenarios where the propagation through LSMC is relevant
    - improve performance
    - re-factor code, use nesting ?
    - implement forward loop
    - implement price factors ?

  - Prototype (Python)

    - value a Bermudean option
    - implement bespoke LSMC
    - use `autograd`

  - Generic LSMC

    - Methodology
        - `numpy`, `MKL` ?
        - can it be turned on/off ?
    - Inputs
    - Requirements on curve simulators and payoffs


# Issues

  - Look into poor performance on larger lists.
  - Introduce helper propagation for complex functions ?
  - Manual calculation of sensitivities requires the full `rvs`.
      - Move the calculation of spot to inside the function for the valuation of the  regression coefficients ?
      - Does this make the calculation of the regression coefficients model dependent ?
      - Would that imply doing backward loop and valuation of the regression coefficients at the same time ?
  - Abstract object: things that are needed to make a decision at a given time step.
  - Binary relation of `ad` objects.
  - Equality of `ad` objects.
  - Derivatives of `ad` objects. They cannot be derived from the `ad` object itself.


# Bugs

  - ~~Simple value update, no inverse => sensitivities not matching.~~ FIXED
  - ~~Wrong intrinsic value~~ ? FIXED
  - Bad match between `ad` and `manual` when using modified payoff.
  - Need to define derivative of modified payoff in order to catch singularities: ~~introduce a third argument/flag~~ ? Used alternative approach.

    - ~~Can one define the derivatives~~ ? Used alternative approach.

  - Double check `stheta` at 0 in $\Theta(\mu - 1)$ and $\Theta(\mu - m)$.
  - Need the values on the forward loop as well ? Yes.
  - Wrong `dndE`.
