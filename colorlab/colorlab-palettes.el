;;; colorlab-palettes.el --- Colorlab palette definitions -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Bozhidar Batsov
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Author: Thomas Frössman <thomasf@jossystem.se>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Base and accent palette definitions for Solarized and other themes.

;;; Code:

(require 'cl-lib)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name default-directory)))
(require 'colorlab-engine)

;;; Palette Configurations

;; The original solarized color palette
(defconst colorlab-solarized
  (list
   :base
   `(("base03" . "#002b36")
     ("base02" . "#073642")
     ("base01" . "#586e75")
     ("base00" . "#657b83")
     ("base0" . "#839496")
     ("base1" . "#93a1a1")
     ("base2" . "#eee8d5")
     ("base3" . "#fdf6e3"))
   :accents
   `(("yellow" . "#b58900")
     ("orange" . "#cb4b16")
     ("red" . "#dc322f")
     ("magenta" . "#d33682")
     ("violet" . "#6c71c4")
     ("blue" . "#268bd2")
     ("cyan" . "#2aa198")
     ("green" . "#859900"))))

(defconst colorlab-solarized-dark-high-contrast
  (list :base (colorlab-change-base-lightness (plist-get colorlab-solarized :base) 0.04 -0.02)
        :accents (colorlab-change-accents-lightness (plist-get colorlab-solarized :accents) 0.05)))

(defconst colorlab-solarized-light-high-contrast
  (list :base (colorlab-change-base-lightness (plist-get colorlab-solarized :base) 0.02 -0.05)
        :accents (colorlab-change-accents-lightness (plist-get colorlab-solarized :accents) -0.05)))

(defconst colorlab-gruvbox-dark
  (list
   :base
   `(("base03" . "#282828")
     ("base02" . "#32302f")
     ("base01" . "#7c6f64")
     ("base00" . "#282828")
     ("base0" . "#a89984")
     ("base1" . "#bdae93")
     ("base2" . "#a89984")
     ("base3" . "#fbf1c7"))
   :accents
   `(("yellow" . "#d79921")
     ("orange" . "#d65d0e")
     ("red" . "#fb4933")
     ("magenta" . "#d3869b")
     ("violet" . "#b16286")
     ("blue" . "#458588")
     ("cyan" . "#689d6a")
     ("green" . "#98971a"))))

;; note: not inversed here
(defconst colorlab-gruvbox-light
  (list
   :base
   `(("base03" . "#282828")
     ("base02" . "#32302f")
     ("base01" . "#665c54")
     ("base00" . "#7c6f64")
     ("base0" . "#3c3836")
     ("base1" . "#a89984")
     ("base2" . "#ebdbb2")
     ("base3" . "#fbf1c7"))
   :accents
   `(("yellow" . "#b57614")
     ("orange" . "#af3a03")
     ("red" . "#9d0006")
     ("magenta" . "#d3869b")
     ("violet" . "#8f3f71")
     ("blue" . "#076678")
     ("cyan" . "#689d6a")
     ("green" . "#98971a"))))

(defconst colorlab-zenburn
  (list
   :base
   `(("base03" . "#3F3F3F")
     ("base02" . "#4F4F4F")
     ("base01" . ,(colorlab-blend-oklab "#656555" "#DCDCCC" 0.3))
     ("base00" . "#6F6F6F")
     ("base0"  . "#DCDCCC")
     ("base1"  . "#FFFFEF")
     ("base2"  . ,(colorlab-blend-oklab "#FFFFEF" "#FFFFFD" 0.5))
     ("base3"  . "#FFFFFD"))
   :accents
   `(("yellow"  . "#F0DFAF")
     ("orange"  . "#DFAF8F")
     ("red"     . "#CC9393")
     ("magenta" . "#DC8CC3")
     ("violet"  . ,(colorlab-blend-oklab "#8CD0D3" "#DC8CC3" 0.5))  ;; blend of blue and magenta
     ("blue"    . "#8CD0D3")
     ("cyan"    . "#93E0E3")
     ("green"   . "#7F9F7F"))))

(defconst colorlab-monokai
  (list
   :base
   `(("base03" . "#272822")
     ("base02" . "#3E3D31")
     ("base01" . "#75715E")
     ("base00" . "#49483E")
     ("base0" . "#F8F8F2")
     ("base1" . "#F8F8F2")
     ("base2" . "#F8F8F2")
     ("base3" . "#F8F8F2"))
   :accents
   `(("yellow" . "#E6DB74")
     ("orange" . "#FD971F")
     ("red" . "#F92672")
     ("magenta" . "#FD5FF0")
     ("violet" . "#AE81FF")
     ("blue" . "#66D9EF")
     ("cyan" . "#A1EFE4")
     ("green" . "#A6E22E"))))

(defconst colorlab-selenized-dark
  (list
   :base
   `(("base03" . "#103c48")
     ("base02" . "#184956")
     ("base01" . "#72898f")
     ("base00" . "#103c48")
     ("base0" . "#adbcbc")
     ("base1" . "#cad8d9")
     ("base2" . "#ece3cc")
     ("base3" . "#fbf3db"))
   :accents
   `(("yellow" . "#dbb32d")
     ("orange" . "#ed8649")
     ("red" . "#fa5750")
     ("magenta" . "#f275be")
     ("violet" . "#af88eb")
     ("blue" . "#4695f7")
     ("cyan" . "#41c7b9")
     ("green" . "#75b938"))))

(defconst colorlab-selenized-black
  (list
   :base
   `(("base03" . "#181818")
     ("base02" . "#252525")
     ("base01" . "#777777")
     ("base00" . "#181818")
     ("base0" . "#b9b9b9")
     ("base1" . "#dedede")
     ("base2" . "#53676d")
     ("base3" . "#3a4d53"))
   :accents
   `(("yellow" . "#dbb32d")
     ("orange" . "#e67f43")
     ("red" . "#ed4a46")
     ("magenta" . "#eb6eb7")
     ("violet" . "#a580e2")
     ("blue" . "#368aeb")
     ("cyan" . "#3fc5b7")
     ("green" . "#70b433"))))

(defconst colorlab-selenized-light
  (list
   :base
   `(("base03" . "#cad8d9")
     ("base02" . "#adbcbc")
     ("base01" . "#3a4d53")
     ("base00" . "#53676d")
     ("base0" . "#fbf3db")
     ("base1" . "#909995")
     ("base2" . "#ece3cc")
     ("base3" . "#fbf3db"))
   :accents
   `(("yellow" . "#ad8900")
     ("orange" . "#c25d1e")
     ("red" . "#d2212d")
     ("magenta" . "#ca4898")
     ("violet" . "#8762c6")
     ("blue" . "#0072d4")
     ("cyan" . "#009c8f")
     ("green" . "#489100"))))

(defconst colorlab-selenized-white
  (list
   :base
   `(("base03" . "#dedede")
     ("base02" . "#b9b9b9")
     ("base01" . "#282828")
     ("base00" . "#474747")
     ("base0" . "#ffffff")
     ("base1" . "#878787")
     ("base2" . "#ebebeb")
     ("base3" . "#ffffff"))
   :accents
   `(("yellow" . "#c49700")
     ("orange" . "#d04a00")
     ("red" . "#d6000c")
     ("magenta" . "#dd0f9d")
     ("violet" . "#7f51d6")
     ("blue" . "#0064e4")
     ("cyan" . "#00ad9c")
     ("green" . "#1d9700"))))

;; Everforest dark, medium contrast (sainnhe/everforest). Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-everforest-dark
  (list
   :base
   `(("base03" . "#2d353b")
     ("base02" . "#343f44")
     ("base01" . "#859289")
     ("base00" . "#859289")
     ("base0"  . "#d3c6aa")
     ("base1"  . "#d3c6aa")
     ("base2"  . "#f4f0d9")
     ("base3"  . "#fdf6e3"))
   :accents
   `(("yellow"  . "#dbbc7f")
     ("orange"  . "#e69875")
     ("red"     . "#e67e80")
     ("magenta" . "#d699b6")
     ("violet"  . ,(colorlab-set-oklch-hue "#d699b6" 300.0))  ;; magenta turned toward blue
     ("blue"    . "#7fbbb3")
     ("cyan"    . "#83c092")
     ("green"   . "#a7c080"))))

;; Everforest light, medium contrast (sainnhe/everforest). Base0, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-everforest-light
  (list
   :base
   `(("base03" . "#d3c6aa")
     ("base02" . "#d3c6aa")
     ("base01" . "#5c6a72")
     ("base00" . "#5c6a72")
     ("base0"  . "#939f91")
     ("base1"  . "#939f91")
     ("base2"  . "#f4f0d9")
     ("base3"  . "#fdf6e3"))
   :accents
   `(("yellow"  . "#dfa000")
     ("orange"  . "#f57d26")
     ("red"     . "#f85552")
     ("magenta" . "#df69ba")
     ("violet"  . ,(colorlab-modify-oklch "#df69ba" 0.65 0.14 300.0))  ;; magenta turned toward blue
     ("blue"    . "#3a94c5")
     ("cyan"    . "#35a77c")
     ("green"   . "#8da101"))))

;; Flexoki dark (stephango.com/flexoki): bg, bg-2, tx-3, tx and the 400 accents. Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-flexoki-dark
  (list
   :base
   `(("base03" . "#100f0f")
     ("base02" . "#1c1b1a")
     ("base01" . "#575653")
     ("base00" . "#575653")
     ("base0" . "#cecdc3")
     ("base1" . "#cecdc3")
     ("base2" . "#f2f0e5")
     ("base3" . "#fffcf0"))
   :accents
   `(("yellow" . "#d0a215")
     ("orange" . "#da702c")
     ("red" . "#d14d41")
     ("magenta" . "#ce5d97")
     ("violet" . "#8b7ec8")
     ("blue" . "#4385be")
     ("cyan" . "#3aa99f")
     ("green" . "#879a39"))))

;; Flexoki light (stephango.com/flexoki): paper, bg-2, tx-3, tx and the 600 accents. Base0, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-flexoki-light
  (list
   :base
   `(("base03" . "#cecdc3")
     ("base02" . "#cecdc3")
     ("base01" . "#100f0f")
     ("base00" . "#100f0f")
     ("base0" . "#b7b5ac")
     ("base1" . "#b7b5ac")
     ("base2" . "#f2f0e5")
     ("base3" . "#fffcf0"))
   :accents
   `(("yellow" . "#ad8301")
     ("orange" . "#bc5215")
     ("red" . "#af3029")
     ("magenta" . "#a02f6f")
     ("violet" . "#5e409d")
     ("blue" . "#205ea6")
     ("cyan" . "#24837b")
     ("green" . "#66800b"))))

;; Kanagawa wave (rebelot/kanagawa.nvim). Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-kanagawa-wave
  (list
   :base
   `(("base03" . "#1f1f28")
     ("base02" . "#2a2a37")
     ("base01" . "#727169")
     ("base00" . "#727169")
     ("base0" . "#dcd7ba")
     ("base1" . "#dcd7ba")
     ("base2" . "#e7dba0")
     ("base3" . "#f2ecbc"))
   :accents
   `(("yellow" . "#c0a36e")
     ("orange" . "#ffa066")
     ("red" . "#c34043")
     ("magenta" . "#d27e99")
     ("violet" . "#957fb8")
     ("blue" . "#7e9cd8")
     ("cyan" . "#6a9589")
     ("green" . "#76946a"))))

;; Kanagawa dragon (rebelot/kanagawa.nvim). Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-kanagawa-dragon
  (list
   :base
   `(("base03" . "#181616")
     ("base02" . "#282727")
     ("base01" . "#737c73")
     ("base00" . "#737c73")
     ("base0"  . "#c5c9c5")
     ("base1"  . "#c5c9c5")
     ("base2"  . "#e7dba0")
     ("base3"  . "#f2ecbc"))
   :accents
   `(("yellow"  . "#c4b28a")
     ("orange"  . ,(colorlab-set-oklch-chroma "#b6927b" 0.09))  ;; dragonOrange with OKLCH chroma raised
     ("red"     . "#c4746e")
     ("magenta" . "#a292a3")
     ("violet"  . "#8992a7")
     ("blue"    . "#658594")  ;; dragonBlue (terminal blue and cyan are nearly identical)
     ("cyan"    . "#7aa89f")  ;; waveAqua2
     ("green"   . "#8a9a7b"))))

;; Kanagawa lotus (rebelot/kanagawa.nvim). Base0, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-kanagawa-lotus
  (list
   :base
   `(("base03" . "#dcd7ba")
     ("base02" . "#dcd7ba")
     ("base01" . "#43436c")
     ("base00" . "#545464")
     ("base0"  . "#8a8980")
     ("base1"  . "#8a8980")
     ("base2"  . "#e7dba0")
     ("base3"  . "#f2ecbc"))
   :accents
   `(("yellow"  . "#77713f")
     ("orange"  . "#cc6d00")
     ("red"     . "#c84053")
     ("magenta" . "#b35b79")
     ("violet"  . "#5d57a3")  ;; lotusBlue5 (lotusViolet4 is bright magenta)
     ("blue"    . "#4d699b")
     ("cyan"    . "#597b75")
     ("green"   . "#6f894e"))))

;; Nightfox. Base2 and Base3 are dayfox's backgrounds.
(defconst colorlab-nightfox
  (list
   :base
   `(("base03" . "#192330")
     ("base02" . "#212e3f")
     ("base01" . "#738091")
     ("base00" . "#738091")
     ("base0" . "#cdcecf")
     ("base1" . "#d6d6d7")
     ("base2" . "#e4dcd4")
     ("base3" . "#f6f2ee"))
   :accents
   `(("yellow" . "#dbc074")
     ("orange" . "#f4a261")
     ("red" . "#c94f6d")
     ("magenta" . "#d67ad2")
     ("violet" . "#9d79d6")
     ("blue" . "#719cd6")
     ("cyan" . "#63cdcf")
     ("green" . "#81b29a"))))

;; Dayfox.
(defconst colorlab-dayfox
  (list
   :base
   `(("base03" . "#cdcecf")
     ("base02" . "#cdcecf")
     ("base01" . "#302b5d")
     ("base00" . "#3d2b5a")
     ("base0" . "#837a72")
     ("base1" . "#837a72")
     ("base2" . "#e4dcd4")
     ("base3" . "#f6f2ee"))
   :accents
   `(("yellow" . "#ac5402")
     ("orange" . "#955f61")
     ("red" . "#a5222f")
     ("magenta" . "#a440b5")
     ("violet" . "#6e33ce")
     ("blue" . "#2848a9")
     ("cyan" . "#287980")
     ("green" . "#396847"))))

;; Dawnfox.
(defconst colorlab-dawnfox
  (list
   :base
   `(("base03" . "#e0def4")
     ("base02" . "#e0def4")
     ("base01" . "#4c4769")
     ("base00" . "#575279")
     ("base0"  . "#9893a5")
     ("base1"  . "#9893a5")
     ("base2"  . "#ebe5df")
     ("base3"  . "#faf4ed"))
   :accents
   `(("yellow"  . ,(colorlab-modify-oklch "#ea9d34" 0.64 0.11))  ;; toned down to align with other accents
     ("orange"  . "#d7827e")
     ("red"     . "#b4637a")
     ("magenta" . "#d685af")
     ("violet"  . "#907aa9")
     ("blue"    . "#286983")
     ("cyan"    . "#56949f")
     ("green"   . "#618774"))))

;; Duskfox. Base2 and Base3 are dawnfox's backgrounds.
(defconst colorlab-duskfox
  (list
   :base
   `(("base03" . "#232136")
     ("base02" . "#2d2a45")
     ("base01" . "#817c9c")
     ("base00" . "#817c9c")
     ("base0"  . "#e0def4")
     ("base1"  . "#eae8ff")
     ("base2"  . "#ebe5df")
     ("base3"  . "#faf4ed"))
   :accents
   `(("yellow"  . "#f6c177")
     ("orange"  . "#ea9a97")
     ("red"     . "#eb6f92")
     ("magenta" . "#eb98c3")
     ("violet"  . "#c4a7e7")
     ("blue"    . "#569fba")
     ("cyan"    . "#9ccfd8")
     ("green"   . "#a3be8c"))))

;; Nordfox. Base2 and Base3 are its white and bright white.
(defconst colorlab-nordfox
  (list
   :base
   `(("base03" . "#2e3440")
     ("base02" . "#39404f")
     ("base01" . "#60728a")
     ("base00" . "#60728a")
     ("base0"  . "#cdcecf")
     ("base1"  . "#e5e9f0")  ;; its white (fg0 has less contrast than fg1)
     ("base2"  . "#e5e9f0")
     ("base3"  . "#e7ecf4"))
   :accents
   `(("yellow"  . "#ebcb8b")
     ("orange"  . "#c9826b")
     ("red"     . "#bf616a")
     ("magenta" . "#bf88bc")
     ("violet"  . ,(colorlab-set-oklch-hue "#b48ead" 300.0))  ;; purple turned toward blue
     ("blue"    . "#81a1c1")
     ("cyan"    . "#88c0d0")
     ("green"   . "#a3be8c"))))

;; Terafox. Base2 and Base3 are its white and bright white.
(defconst colorlab-terafox
  (list
   :base
   `(("base03" . "#152528")
     ("base02" . "#1d3337")
     ("base01" . "#6d7f8b")
     ("base00" . "#6d7f8b")
     ("base0"  . "#e6eaea")
     ("base1"  . "#eaeeee")
     ("base2"  . "#ebebeb")
     ("base3"  . "#eeeeee"))
   :accents
   `(("yellow"  . "#fda47f")
     ("orange"  . "#ff8349")
     ("red"     . "#e85c51")
     ("magenta" . "#ad5c7c")
     ("violet"  . ,(colorlab-set-oklch-hue "#ad5c7c" 300.0))  ;; magenta turned toward blue (no native purple)
     ("blue"    . "#5a93aa")
     ("cyan"    . "#a1cdd8")
     ("green"   . "#7aa4a1"))))

;; Carbonfox. Base2 and Base3 are its white and bright white; bg and fg are
;; the values upstream computes.
(defconst colorlab-carbonfox
  (list
   :base
   `(("base03" . "#161616")
     ("base02" . "#252525")
     ("base01" . "#6e6f70")
     ("base00" . "#6e6f70")
     ("base0"  . "#f2f4f8")
     ("base1"  . "#f9fbff")
     ("base2"  . "#dfdfe0")
     ("base3"  . "#e4e4e5"))
   :accents
   `(("yellow"  . "#f1c21b")  ;; IBM Carbon yellow 30 (upstream has teal)
     ("orange"  . "#ff832b")  ;; IBM Carbon orange 40 (upstream has teal)
     ("red"     . "#fa4d56")  ;; IBM Carbon red 50 (upstream has magenta)
     ("magenta" . "#ff7eb6")
     ("violet"  . "#be95ff")
     ("blue"    . "#78a9ff")
     ("cyan"    . "#08bdba")  ;; its teal (upstream yellow, cyan is close to blue)
     ("green"   . "#25be6a"))))

;; Carbon dark (g100): grade 40 accents, gray 60 comments. Base2 and Base3
;; are the white theme's backgrounds.
(defconst colorlab-carbon-dark
  (list
   :base
   `(("base03" . "#161616")
     ("base02" . "#262626")
     ("base01" . "#6f6f6f")
     ("base00" . "#6f6f6f")
     ("base0"  . "#c6c6c6")
     ("base1"  . "#f4f4f4")
     ("base2"  . "#f4f4f4")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#d2a106")
     ("orange"  . "#ff832b")
     ("red"     . "#fa4d56")  ;; red 50 (Carbon's support-error) instead of red 40, to distinguish from magenta
     ("magenta" . "#ff7eb6")
     ("violet"  . "#be95ff")
     ("blue"    . "#78a9ff")
     ("cyan"    . "#08bdba")
     ("green"   . "#42be65"))))

;; Carbon light (white): grade 60 accents, gray 50 comments.
(defconst colorlab-carbon-light
  (list
   :base
   `(("base03" . "#c6c6c6")
     ("base02" . "#c6c6c6")
     ("base01" . "#161616")
     ("base00" . "#525252")
     ("base0"  . "#8d8d8d")
     ("base1"  . "#8d8d8d")
     ("base2"  . "#f4f4f4")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#8e6a00")
     ("orange"  . "#ba4e00")
     ("red"     . "#da1e28")
     ("magenta" . "#d02670")
     ("violet"  . "#8a3ffc")
     ("blue"    . "#0072c3")  ;; cyan 60 (blue 60 is far more saturated)
     ("cyan"    . "#007d79")
     ("green"   . "#198038"))))

;; Apple dark: system colors. Base2 and Base3 are the light backgrounds.
(defconst colorlab-apple-dark
  (list
   :base
   `(("base03" . "#1c1c1e")
     ("base02" . "#2c2c2e")
     ("base01" . "#8e8e93")
     ("base00" . "#8e8e93")
     ("base0"  . "#ebebf5")
     ("base1"  . "#ffffff")
     ("base2"  . "#f2f2f7")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#ffd600")
     ("orange"  . "#ff9230")
     ("red"     . "#ff4245")
     ("magenta" . "#db34f2")  ;; systemPurple (pink is close to red)
     ("violet"  . "#6d7cff")  ;; systemIndigo
     ("blue"    . "#0091ff")
     ("cyan"    . "#00dac3")  ;; systemMint (teal/cyan is close to blue)
     ("green"   . "#30d158"))))

;; Apple light: the increased contrast colors, as the system yellow, orange,
;; green and mint are under 2.4:1 on white.
(defconst colorlab-apple-light
  (list
   :base
   `(("base03" . "#ebebf5")
     ("base02" . "#ebebf5")
     ("base01" . "#000000")
     ("base00" . "#3c3c43")
     ("base0"  . "#8e8e93")
     ("base1"  . "#8e8e93")
     ("base2"  . "#f2f2f7")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#a16a00")
     ("orange"  . "#c55300")
     ("red"     . "#e9152d")
     ("magenta" . "#b02fc2")  ;; systemPurple (pink is close to red)
     ("violet"  . "#564ade")  ;; systemIndigo
     ("blue"    . "#1e6ef4")
     ("cyan"    . "#008575")  ;; systemMint (teal/cyan is close to blue)
     ("green"   . "#008932"))))


(provide 'colorlab-palettes)
;;; colorlab-palettes.el ends here
