;;; kanagawa-wave-theme.el --- port of 'rebelot/kanagawa.nvim' by Tommaso Laurenzi. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'modus-themes)
(defconst kanagawa-themes-wave-palette-partial
  '((roninYellow "FF9E3B")
    (springViolet2 "9CABCA")
    (waveRed "E46876")
    (surimiOrange "FFA066")
    (cursor "#A3D4D5") ; lightBlue
    (bg-main "#181820") ; sumiInk1
    (bg-dim "#1A1A22") ; sumiInk2
    (bg-alt "#1F1F28") ; sumiInk3
    (fg-main "#DCD7BA") ; fujiWhite
    (fg-dim "#717C7C") ; katanaGray
    (fg-alt "#C8C093") ; oldWhite
    (bg-active "#54546D") ; sumiInk6
    (bg-inactive "#363636") ; sumiInk5
    (border "#727169") ; fujiGray

    (red "#C34043") ; autumnRed
    (red-warmer "#9B3034")
    (red-cooler "#CE6367")
    (red-faint "#DA8A8D")
    (green "#76946A") ; autumnGreen
    (green-warmer "#5D7353")
    (green-cooler "#8FA785")
    (green-faint "#AABCA3")
    (yellow "#C0A36E") ; boartYellow2
    (yellow-warmer "#AD894C")
    (yellow-cooler "#CEB891")
    (yellow-faint "#DECFB4")
    (blue "#7E9CD8") ; crystalBlue
    (blue-warmer "#577DCB")
    (blue-cooler "#A5B9E3")
    (blue-faint "#CCD7EF")
    (magenta "#957FB8") ; oniViolet
    (magenta-warmer "#775BA3")
    (magenta-cooler "#AD9DC7")
    (magenta-faint "#C8BDDA")
    (cyan "#6A9589") ; waveAqua1
    (cyan-warmer "#55766D")
    (cyan-cooler "#88A9A0")
    (cyan-faint "#A6BEB7")

    (bg-red-intense "#E82424") ; samuraiRed
    (bg-green-intense "#98BB6C") ; springGreen
    (bg-yellow-intense "#E6C384") ; carpYellow
    (bg-blue-intense "#7FB4CA") ; springBlue
    (bg-magenta-intense "#938AA9") ; springViolet1
    (bg-cyan-intense "#7AA89F") ; waveAqua2

    (bg-red-subtle "#43242B") ; winterRed
    (bg-green-subtle "#2B3328") ; winterGreen
    (bg-yellow-subtle "#49443C") ; winterYellow
    (bg-blue-subtle "#252535") ; winterBlue
    (bg-magenta-subtle "#C0A36E") ; boatYellow2
    (bg-cyan-subtle "#658594") ; dragonBlue

    (bg-added "#304a3f")
    (bg-added-faint "#1a3630")
    (bg-added-refine "#2f6757")
    (fg-added "#76946A") ; autumnGreen

    (bg-changed "#51512f")
    (bg-changed-faint "#40332f")
    (bg-changed-refine "#64651f")
    (fg-changed "#DCA561") ; autumnYellow

    (bg-removed "#5a3142")
    (bg-removed-faint "#4a2034")
    (bg-removed-refine "#782a4a")
    (fg-removed "#C34043") ; autumnRed

    (bg-mode-line-active "#16161D") ; sumiInk0
    (fg-mode-line-active "#B8B4D0") ; oniViolet2
    (bg-completion "#354864")
    (bg-hover "#8f7a7f")
    (bg-hover-secondary "#415960")
    (bg-hl-line "#344255")
    (bg-paren-match "#706069")
    (bg-err "#4f231f") ; check with err
    (bg-warning "#3f3c2f") ; check with warning
    (bg-info "#104032") ; check with info
    (bg-region "#404f66")))
(defconst kanagawa-themes-wave-palette-mappings-partial
  '((err bg-red-intense)
    (warning roninYellow)
    (info bg-cyan-subtle)

    (fg-link cyan-warmer)
    (fg-link-visited yellow-cooler)
    (name blue)
    (keybind green-cooler)
    (identifier bg-yellow-intense)
    (fg-prompt blue-cooler)

    (builtin blue)
    (comment border)
    (constant surimiOrange)
    (fnname blue)
    (fnname-call blue-cooler)
    (keyword magenta)
    (preprocessor waveRed)
    (docstring bg-magenta-subtle)
    (string bg-green-intense)
    (type bg-cyan-intense)
    (variable blue-warmer)
    (variable-use blue-faint)
    (rx-backslash springViolet2) ; compare with `string'
    (rx-construct red)

    (accent-0 cyan-cooler)
    (accent-1 yellow-cooler)
    (accent-2 red)
    (accent-3 green)

    (date-common cyan-cooler)
    (date-deadline red)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday red)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday cyan-cooler)
    (date-weekend red-faint)

    (fg-prose-code green-warmer)
    (prose-done green)
    (fg-prose-macro green-cooler)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula err)
    (prose-tag cyan-faint)
    (prose-todo red-warmer)
    (fg-prose-verbatim blue)

    (mail-cite-0 cyan)
    (mail-cite-1 green-cooler)
    (mail-cite-2 blue-warmer)
    (mail-cite-3 yellow-cooler)
    (mail-part magenta)
    (mail-recipient cyan-warmer)
    (mail-subject blue-cooler)
    (mail-other cyan-cooler)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-cyan-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (bg-space-err bg-yellow-intense)

    (rainbow-0 green-cooler)
    (rainbow-1 blue)
    (rainbow-2 cyan-cooler)
    (rainbow-3 magenta-cooler)
    (rainbow-4 yellow-cooler)
    (rainbow-5 green-warmer)
    (rainbow-6 magenta-warmer)
    (rainbow-7 cyan-warmer)
    (rainbow-8 yellow)))
(defconst kanagawa-themes-palette-common
  '((fringe unspecified)
    (fg-region unspecified)

    (bg-diff-context bg-dim)

    (bg-tab-bar bg-alt)
    (bg-tab-current bg-main)
    (bg-tab-other bg-active)

    (fg-link-symbolic fg-alt)
    (underline-link border)
    (underline-link-symbolic border)

    (border-mode-line-active border)
    (bg-mode-line-inactive bg-alt)
    (fg-mode-line-inactive fg-dim)
    (border-mode-line-inactive border)

    (bg-line-number-active unspecified)
    (fg-line-number-active accent-0)
    (bg-line-number-inactive unspecified)

    (bg-prominent-err bg-err)
    (bg-prominent-warning bg-warning)
    (bg-prominent-note bg-info)
    (fg-prominent-err err)
    (fg-prominent-warning warning)
    (fg-prominent-note info)

    (bg-space unspecified)
    (fg-space border)

    (bg-active-argument bg-warning)
    (fg-active-argument warning)
    (bg-active-value bg-info)
    (fg-active-value info)

    (bg-mark-delete bg-err)
    (fg-mark-delete err)
    (bg-mark-select bg-info)
    (fg-mark-select info)
    (bg-mark-other bg-warning)
    (fg-mark-other warning)

    (fg-search-current fg-main)
    (fg-search-lazy fg-main)
    (fg-search-static fg-main)
    (fg-search-replace fg-main)

    (fg-search-rx-group-0 fg-main)
    (fg-search-rx-group-1 fg-main)
    (fg-search-rx-group-2 fg-main)
    (fg-search-rx-group-3 fg-main)

    (fg-completion-match-0 accent-0)
    (fg-completion-match-1 accent-1)
    (fg-completion-match-2 accent-2)
    (fg-completion-match-3 accent-3)

    (fg-heading-0 rainbow-0)
    (fg-heading-1 rainbow-1)
    (fg-heading-2 rainbow-2)
    (fg-heading-3 rainbow-3)
    (fg-heading-4 rainbow-4)
    (fg-heading-5 rainbow-5)
    (fg-heading-6 rainbow-6)
    (fg-heading-7 rainbow-7)
    (fg-heading-8 rainbow-8)))
(defconst kanagawa-themes-wave-palette
  (modus-themes-generate-palette
   kanagawa-themes-wave-palette-partial
   nil
   nil
   (append kanagawa-themes-wave-palette-mappings-partial kanagawa-themes-palette-common)))
(modus-themes-theme
 'kanagawa-wave
 'kanagawa-themes
 "port of rebelot/kanagawa.nvim by Tommaso Laurenzi."
 'dark
 'modus-themes-vivendi-tinted-palette
 'kanagawa-themes-wave-palette
 nil)
(provide 'kanagawa-wave-theme)
;;; kanagawa-wave-theme.el ends here
