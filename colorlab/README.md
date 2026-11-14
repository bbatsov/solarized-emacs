# colorlab

This utility generates color palette lists for use in Solarized themes.

It is implemented in Emacs Lisp (`colorlab.el`) and provides color space
conversions and manipulations (sRGB, CIE XYZ, CIE L\*a\*b\*, CIELCh, HSL,
Oklab, and Oklch) to compute accent pairs and generate palette alists for
`solarized-palettes.el`.

## Usage

### Command Line (Batch Mode)

To rewrite all palettes in `solarized-palettes.el`:

```sh
make colorlab
```

or directly using `emacs`:

```sh
emacs -Q --batch -l colorlab/colorlab.el -f colorlab-main
```

#### Command Line Options

- `--no-update`: Print generated palettes to stdout without modifying `solarized-palettes.el`.
- `--palette <name>`: Generate or update only the specified palette (e.g. `--palette solarized-dark`).
- `--file <path>`: Specify an alternative target palette file (defaults to `../solarized-palettes.el` or `./solarized-palettes.el`).
- `-h`, `--help`: Display usage and help message.

Examples:

```sh
# Preview all generated palettes without writing to file
make colorlab ARGS="--no-update"

# Preview a single palette
make colorlab ARGS="--no-update --palette zenburn"

# Update only a single palette in solarized-palettes.el
make colorlab ARGS="--palette kanagawa-dragon"
```

### Interactive Emacs

Load `colorlab.el` into Emacs:

- `M-x colorlab-update-palettes`: Updates palette sections in `solarized-palettes.el`.
- `M-x colorlab-generate-palette`: Prompts for a palette name with completion and displays the formatted alist in a preview buffer.
