# Modify only documents matching a logical condition

Applies the modification only to documents matching a condition.

## Arguments

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.
