// Package emacs contains features specific for generating elisp themes
package emacs

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/bbatsov/solarized-emacs/colorlab/pkg/clab"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/sol"
)

// PrintAlist prints the named colors list in a way that is compatible with solarized-palettes.el.
func PrintAlist(w io.Writer, n clab.NamedColors, indent int) (int, error) {
	keys := sol.OrderedKeys(n)
	var longestKey int
	for _, n := range keys {
		l := len(n)
		if l > longestKey {
			longestKey = l
		}
	}
	longestKey = longestKey
	prefix := ""
	for i := 0; i < indent; i++ {
		prefix += " "

	}
	var nt int

	for _, name := range keys {
		n, err := fmt.Fprintf(w, "%s(%-"+strconv.Itoa(longestKey)+"s . \"%s\")\n", prefix, name, n[name])
		n = n + nt
		if err != nil {
			return nt, err
		}

	}
	return nt, nil
}

func RewritePalette(nc clab.NamedColors, paletteName string) {

	var dst bytes.Buffer

	file, err := os.Open("../solarized-palettes.el")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var insideReplacement bool
	for scanner.Scan() {
		txt := scanner.Text()
		if insideReplacement && strings.HasSuffix(txt, ";; palette end") {
			insideReplacement = false
		}
		if !insideReplacement {
			dst.WriteString(txt)
			dst.WriteString("\n")

		}
		if strings.HasSuffix(txt, fmt.Sprintf(";; %s palette", paletteName)) {
			insideReplacement = true
			PrintAlist(&dst, nc, 4)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	file.Close()
	ioutil.WriteFile("../solarized-palettes.el", dst.Bytes(), 0x776)

}
