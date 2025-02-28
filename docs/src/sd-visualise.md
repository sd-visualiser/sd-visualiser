# Visualising string diagrams

SD Visualiser is an interactive tool to visualise string diagrams, based on idea that string diagrams as hypergraphs provide a foundation for building programming languages, building on the work of [1].

A toy language, `sd-lang`, is defined in a similar vein to the `spartan` language [2], and is used to define programs which can be represented as string diagrams.
SD Visualiser provides a playground to experiment with `sd-lang`, and to visualise and interact with the corresponding string diagram.

The main binary is called `sd-gui`.

## Graphical interface

TODO(@NickHu): UI tutorial

## Command-line interface

`sd-gui` exposes a command-line interface to simplify the process of launching the tool with a given input program, by running `sd-gui --spartan <FILE>`.
This is equivalent to opening the graphical interface and loading the file `<FILE>`.

# References


[1] D. R. Ghica, K. Muroya, and T. W. Ambridge, ‘A robust graph-based approach to observational equivalence’. arXiv, Sep. 23, 2021. doi: 10.48550/arXiv.1907.01257.

[2] T. W. Ambridge, ‘Sᴘᴀʀᴛᴀɴ Visualiser’, Spartan Visualiser. https://tnttodda.github.io/Spartan-Visualiser/.

[1]: https://arxiv.org/abs/1907.01257
[2]: https://tnttodda.github.io/Spartan-Visualiser/
