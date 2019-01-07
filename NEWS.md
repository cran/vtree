# vtree 1.0.0 

This is a major release that incorporates several new features.


## New default features

* Automatic variable coloring using gradients.

* More compact display (reduced node separation and tighter margins).

* The color of the root node can be specified using the `rootfillcolor` parameter.

* All missing-value nodes are colored white by default.
This can be changes using the `NAfillcolor` parameter.

* By default, the root node now has no title.

* **NOTE** Display from version 0.1.4 can be obtained by specifying `plain=TRUE`.
This sets:

    * variable-specific shades of blue

    * greater node separation
  
    * wider margins
  
    * missing-value nodes have the same color (shade of blue) as all other nodes at that level
  

## New optional features

* `showlevels` is now deprecated. Use `showvarnames` instead.

* Legends, with marginal frequencies and percentages.

* To put node labels on the same line as frequencies and percentages, specify `sameline=TRUE`.

* A new parameter, `squeeze` takes a value between 0 and 1,
and controls how the tightly-packed the display is.

* "Sequences" of variables can be displayed by specifying `seq=TRUE`.

* Additional `summary` codes: 
`%node=n%` to show summary information only in specified node.
`%trunc=n%` to truncate the summary to the first *n* characters.

* Settings for multi-way intersections can be specified with `Venn=TRUE`.

* `graphattr`, `nodeattr`, and `edgeattr`
parameters allow additional Graphviz attributes to be set.

* Other new parameters: `palette`, `gradient`, `revgradient`, `singlecolor`,
`showpct`, `showlpct`, `showcount`, `varnamepointsize`, `lsplitwidth`,
`margin`, `showempty`, `prunelone`



# vtree 0.1.4 (initial release version)
