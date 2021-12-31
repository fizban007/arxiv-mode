;;; arxiv-vars.el --- Defining the common variables for arxiv-mode  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defgroup arxiv nil
  "A mode for reading arXiv abstracts"
  :prefix "arxiv-"
  :group 'applications)

(defgroup arxiv-fontification nil
  "Faces for the arxiv mode"
  :group 'arxiv)

(defgroup arxiv-preferences nil
  "General preferences for the arxiv mode"
  :group 'arxiv)

(defvar arxiv-mode-hook nil
  "A list of functions to call when entering `arxiv-mode'.")

(defvar arxiv-abstract-mode-hook nil
  "A list of functions to call when entering `arxiv-abstract-mode'.")

(defvar arxiv-buffer nil
  "Current buffer for viewing arXiv updates.")

(defvar arxiv-abstract-window nil
  "Current window for viewing the arXiv abstract.")

(defvar arxiv-abstract-buffer nil
  "Current buffer for viewing the arXiv abstract.")

(defvar arxiv-highlight-overlay nil
  "Overlay for displaying the selected article in arXiv article list.")

(defvar arxiv-entry-list nil
  "Entries for arXiv articles.")

(defvar arxiv-current-entry nil
  "Current entry in the arXiv article list.")

(defvar arxiv-query-results-min nil
  "Current minimun entry of query result.")

(defvar arxiv-query-results-max nil
  "Current maxmum entry of query result.")

(defvar arxiv-query-data-list nil
  "List of current query data.
Elements of this list must have the form (field condition context)
Available fields are 'all, 'id, 'time, 'title, 'author, 'abstract,
'comment, 'journal and 'category.
If condition is nil then the the search excludes the context and vice versa.
context is a string seperated by quotes and spaces.")

(defvar arxiv-query-info ""
  "A string containing the information of query data displayed in the header line.")

(defvar arxiv-mode-entry-function nil
  "Variables showing the entry function used to enter `arxiv-mode'.")

(defvar arxiv-categories
  '(CoRR cs.AI cs.AR cs.CC cs.CE cs.CG
         cs.CL cs.CR cs.CV cs.CY cs.DB cs.DC cs.DL cs.DM cs.DS cs.ET
         cs.FL cs.GL cs.GR cs.GT cs.HC cs.IR cs.IT cs.LG cs.LO cs.MA
         cs.MM cs.MS cs.NA cs.NE cs.NI cs.OH cs.OS cs.PF cs.PL cs.RO
         cs.SC cs.SD cs.SE cs.SI cs.SY econ econ.EM econ.GN econ.TH eess
         eess.AS eess.IV eess.SP eess.SY math math.AC math.AG math.AP
         math.AT math.CA math.CO math.CT math.CV math.DG math.DS math.FA
         math.GM math.GN math.GR math.GT math.HO math.IT math.KT math.LO
         math.MG math.MP math.NA math.NT math.OA math.OC math.PR math.QA
         math.RA math.RT math.SG math.SP math.ST astro-ph astro-ph.CO
         astro-ph.EP astro-ph.GA astro-ph.HE astro-ph.IM astro-ph.SR
         cond-mat cond-mat.dis-nn cond-mat.mes-hall cond-mat.mtrl-sci
         cond-mat.other cond-mat.quant-gas cond-mat.soft
         cond-mat.stat-mech cond-mat.str-el cond-mat.supr-con gr-qc
         hep-ex hep-lat hep-ph hep-th math-ph nlin nlin.AO nlin.CD
         nlin.CG nlin.PS nlin.SI nucl-ex nucl-th physics physics.acc-ph
         physics.ao-ph physics.app-ph physics.atm-clus physics.atom-ph
         physics.bio-ph physics.chem-ph physics.class-ph physics.comp-ph
         physics.data-an physics.ed-ph physics.flu-dyn physics.gen-ph
         physics.geo-ph physics.hist-ph physics.ins-det physics.med-ph
         physics.optics physics.plasm-ph physics.pop-ph physics.soc-ph
         physics.space-ph quant-ph q-bio q-bio.BM q-bio.CB q-bio.GN
         q-bio.MN q-bio.NC q-bio.OT q-bio.PE q-bio.QM q-bio.SC q-bio.TO
         q-fin q-fin.CP q-fin.EC q-fin.GN q-fin.MF q-fin.PM q-fin.PR
         q-fin.RM q-fin.ST q-fin.TR stat stat.AP stat.CO stat.ME stat.ML
         stat.OT stat.TH)
  "Availble categories in arXiv searching.")

(defvar arxiv-subject-classifications
  '((cs.AI . "Artificial Intelligence")
    (cs.AR . "Hardware Architecture")
    (cs.CC . "Computational Complexity")
    (cs.CE . "Computational Engineering, Finance, and Science")
    (cs.CG . "Computational Geometry")
    (cs.CL . "Computation and Language")
    (cs.CR . "Cryptography and Security")
    (cs.CV . "Computer Vision and Pattern Recognition")
    (cs.CY . "Computers and Society")
    (cs.DB . "Databases")
    (cs.DC . "Distributed, Parallel, and Cluster Computing")
    (cs.DL . "Digital Libraries")
    (cs.DM . "Discrete Mathematics")
    (cs.DS . "Data Structures and Algorithms")
    (cs.ET . "Emerging Technologies")
    (cs.FL . "Formal Languages and Automata Theory")
    (cs.GL . "General Literature")
    (cs.GR . "Graphics")
    (cs.GT . "Computer Science and Game Theory")
    (cs.HC . "Human-Computer Interaction")
    (cs.IR . "Information Retrieval")
    (cs.IT . "Information Theory")
    (cs.LG . "Machine Learning")
    (cs.LO . "Logic in Computer Science")
    (cs.MA . "Multiagent Systems")
    (cs.MM . "Multimedia")
    (cs.MS . "Mathematical Software")
    (cs.NA . "Numerical Analysis")
    (cs.NE . "Neural and Evolutionary Computing")
    (cs.NI . "Networking and Internet Architecture")
    (cs.OH . "Other Computer Science")
    (cs.OS . "Operating Systems")
    (cs.PF . "Performance")
    (cs.PL . "Programming Languages")
    (cs.RO . "Robotics")
    (cs.SC . "Symbolic Computation")
    (cs.SD . "Sound")
    (cs.SE . "Software Engineering")
    (cs.SI . "Social and Information Networks")
    (cs.SY . "Systems and Control")
    (econ.EM . "Econometrics")
    (econ.GN . "General Economics")
    (econ.TH . "Theoretical Economics")
    (eess.AS . "Audio and Speech Processing")
    (eess.IV . "Image and Video Processing")
    (eess.SP . "Signal Processing")
    (eess.SY . "Systems and Control")
    (math.AC . "Commutative Algebra")
    (math.AG . "Algebraic Geometry")
    (math.AP . "Analysis of PDEs")
    (math.AT . "Algebraic Topology")
    (math.CA . "Classical Analysis and ODEs")
    (math.CO . "Combinatorics")
    (math.CT . "Category Theory")
    (math.CV . "Complex Variables")
    (math.DG . "Differential Geometry")
    (math.DS . "Dynamical Systems")
    (math.FA . "Functional Analysis")
    (math.GM . "General Mathematics")
    (math.GN . "General Topology")
    (math.GR . "Group Theory")
    (math.GT . "Geometric Topology")
    (math.HO . "History and Overview")
    (math.IT . "Information Theory")
    (math.KT . "K-Theory and Homology")
    (math.LO . "Logic")
    (math.MG . "Metric Geometry")
    (math.MP . "Mathematical Physics")
    (math.NA . "Numerical Analysis")
    (math.NT . "Number Theory")
    (math.OA . "Operator Algebras")
    (math.OC . "Optimization and Control")
    (math.PR . "Probability")
    (math.QA . "Quantum Algebra")
    (math.RA . "Rings and Algebras")
    (math.RT . "Representation Theory")
    (math.SG . "Symplectic Geometry")
    (math.SP . "Spectral Theory")
    (math.ST . "Statistics Theory")
    (astro-ph.CO . "Cosmology and Nongalactic Astrophysics")
    (astro-ph.EP . "Earth and Planetary Astrophysics")
    (astro-ph.GA . "Astrophysics of Galaxies")
    (astro-ph.HE . "High Energy Astrophysical Phenomena")
    (astro-ph.IM . "Instrumentation and Methods for Astrophysics")
    (astro-ph.SR . "Solar and Stellar Astrophysics")
    (cond-mat.dis-nn . "Disordered Systems and Neural Networks")
    (cond-mat.mes-hall . "Mesoscale and Nanoscale Physics")
    (cond-mat.mtrl-sci . "Materials Science")
    (cond-mat.other . "Other Condensed Matter")
    (cond-mat.quant-gas . "Quantum Gases")
    (cond-mat.soft . "Soft Condensed Matter")
    (cond-mat.stat-mech . "Statistical Mechanics")
    (cond-mat.str-el . "Strongly Correlated Electrons")
    (cond-mat.supr-con . "Superconductivity")
    (gr-qc . "General Relativity and Quantum Cosmology")
    (hep-ex . "High Energy Physics - Experiment")
    (hep-lat . "High Energy Physics - Lattice")
    (hep-ph . "High Energy Physics - Phenomenology")
    (hep-th . "High Energy Physics - Theory")
    (math-ph . "Mathematical Physics")
    (nlin.AO . "Adaptation and Self-Organizing Systems")
    (nlin.CD . "Chaotic Dynamics")
    (nlin.CG . "Cellular Automata and Lattice Gases")
    (nlin.PS . "Pattern Formation and Solitons")
    (nlin.SI . "Exactly Solvable and Integrable Systems")
    (nucl-ex . "Nuclear Experiment")
    (nucl-th . "Nuclear Theory")
    (physics.acc-ph . "Accelerator Physics")
    (physics.ao-ph . "Atmospheric and Oceanic Physics")
    (physics.app-ph . "Applied Physics")
    (physics.atm-clus . "Atomic and Molecular Clusters")
    (physics.atom-ph . "Atomic Physics")
    (physics.bio-ph . "Biological Physics")
    (physics.chem-ph . "Chemical Physics")
    (physics.class-ph . "Classical Physics")
    (physics.comp-ph . "Computational Physics")
    (physics.data-an . "Data Analysis, Statistics and Probability")
    (physics.ed-ph . "Physics Education")
    (physics.flu-dyn . "Fluid Dynamics")
    (physics.gen-ph . "General Physics")
    (physics.geo-ph . "Geophysics")
    (physics.hist-ph . "History and Philosophy of Physics")
    (physics.ins-det . "Instrumentation and Detectors")
    (physics.med-ph . "Medical Physics")
    (physics.optics . "Optics")
    (physics.plasm-ph . "Plasma Physics")
    (physics.pop-ph . "Popular Physics")
    (physics.soc-ph . "Physics and Society")
    (physics.space-ph . "Space Physics")
    (quant-ph . "Quantum Physics")
    (q-bio.BM . "Biomolecules")
    (q-bio.CB . "Cell Behavior")
    (q-bio.GN . "Genomics")
    (q-bio.MN . "Molecular Networks")
    (q-bio.NC . "Neurons and Cognition")
    (q-bio.OT . "Other Quantitative Biology")
    (q-bio.PE . "Populations and Evolution")
    (q-bio.QM . "Quantitative Methods")
    (q-bio.SC . "Subcellular Processes")
    (q-bio.TO . "Tissues and Organs")
    (q-fin.CP . "Computational Finance")
    (q-fin.EC . "Economics")
    (q-fin.GN . "General Finance")
    (q-fin.MF . "Mathematical Finance")
    (q-fin.PM . "Portfolio Management")
    (q-fin.PR . "Pricing of Securities")
    (q-fin.RM . "Risk Management")
    (q-fin.ST . "Statistical Finance")
    (q-fin.TR . "Trading and Market Microstructure")
    (stat.AP . "Applications")
    (stat.CO . "Computation")
    (stat.ME . "Methodology")
    (stat.ML . "Machine Learning")
    (stat.OT . "Other Statistics")
    (stat.TH . "Statistics Theory"))
  "ArXiv subjects alist for displaying.")

(defcustom arxiv-startup-with-abstract-window nil
  "Whether to start `arxiv-mode' with an abstract window."
  :group 'arxiv-preferences
  :type 'boolean)

(defcustom arxiv-use-variable-pitch nil
  "Whether to use variable pitch fonts in `arxiv-mode' buffers."
  :group 'arxiv-preferences
  :type 'boolean)

(defcustom arxiv-entries-per-fetch 100
  "Number of entries per page in the article list."
  :group 'arxiv-preferences
  :type 'integer)

(defcustom arxiv-author-list-maximum 10
  "Maximum number of authors shown per entry on the article list.
0 means no maximum limit."
  :group 'arxiv-preferences
  :type 'integer)

(defcustom arxiv-default-category "hep-th"
  "Default search category when using `arxiv-read'."
  :group 'arxiv-preferences
  :type 'string
  :options arxiv-categories)

(defcustom arxiv-default-download-folder "~/Downloads/"
  "Default download folder to save PDF file."
  :group 'arxiv-preferences
  :type 'string)

(defcustom arxiv-default-bibliography ""
  "Default master bibliography file to append for `arxiv-mode'."
  :group 'arxiv-preferences
  :type 'string)

(defcustom arxiv-pdf-open-function 'find-file-other-window
  "Default function to open PDF file."
  :group 'arxiv-preferences
  :type 'function)

;; Defining custom faces
(defvar arxiv-title-face 'arxiv-title-face)
(defface arxiv-title-face
  '((t (:inherit font-lock-keyword-face :height 1.2)))
  "Face name for article titles in the arXiv article list."
  :group 'arxiv-fontification)

(defvar arxiv-keyword-face 'arxiv-keyword-face)
(defface arxiv-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face name for keywords in the arXiv article list."
  :group 'arxiv-fontification)

(defvar arxiv-author-face 'arxiv-author-face)
(defface arxiv-author-face
  '((t (:inherit font-lock-type-face)))
  "Face name for authors in the arXiv article list."
  :group 'arxiv-fontification)

(defvar arxiv-date-face 'arxiv-date-face)
(defface arxiv-date-face
  '((t (:inherit shadow)))
  "Face name for date in the arXiv article list."
  :group 'arxiv-fontification)

(defvar arxiv-abstract-face 'arxiv-abstract-face)
(defface arxiv-abstract-face
  '((t (:inherit font-lock-doc-face)))
  "Face name for abstract in the arXiv abstract viewing window."
  :group 'arxiv-fontification)

(defvar arxiv-abstract-title-face 'arxiv-abstract-title-face)
(defface arxiv-abstract-title-face
  '((t (:inherit font-lock-keyword-face :height 1.5 :weight semi-bold :underline t)))
  "Face name for title in the arXiv abstract viewing window."
  :group 'arxiv-fontification)

(defvar arxiv-abstract-author-face 'arxiv-abstract-author-face)
(defface arxiv-abstract-author-face
  '((t (:inherit font-lock-type-face :height 1.2 :underline t)))
  "Face name for authors in the arXiv abstract viewing window."
  :group 'arxiv-fontification)

(defvar arxiv-subfield-face 'arxiv-subfield-face)
(defface arxiv-subfield-face
  '((t (:inherit default)))
  "Face name for subfields (comments, subjects, etc.) in the arXiv abstract viewing window."
  :group 'arxiv-fontification)

(defvar arxiv-abstract-math-face 'arxiv-abstract-math-face)
(defface arxiv-abstract-math-face
  '((t (:inherit font-lock-reference-face :family "Monospace")))
  "Face name for the latex content in abstract in the arXiv
abstract viewing window."
  :group 'arxiv-fontification)

(defvar arxiv-abstract-syntax-table
  (let ((synTable (make-syntax-table text-mode-syntax-table)))
        (modify-syntax-entry ?$ "($" synTable)
        (modify-syntax-entry ?$ ")$" synTable)
        synTable))

(defvar arxiv-abstract-prettify-symbols-alist
  '( ;; Lowercase Greek letters.
    ("\\alpha" . ?α)
    ("\\beta" . ?β)
    ("\\gamma" . ?γ)
    ("\\delta" . ?δ)
    ("\\epsilon" . ?ϵ)
    ("\\zeta" . ?ζ)
    ("\\eta" . ?η)
    ("\\theta" . ?θ)
    ("\\iota" . ?ι)
    ("\\kappa" . ?κ)
    ("\\lambda" . ?λ)
    ("\\mu" . ?μ)
    ("\\nu" . ?ν)
    ("\\xi" . ?ξ)
    ;; There is no \omicron because it looks like a latin o.
    ("\\pi" . ?π)
    ("\\rho" . ?ρ)
    ("\\sigma" . ?σ)
    ("\\tau" . ?τ)
    ("\\upsilon" . ?υ)
    ("\\phi" . ?φ)
    ("\\chi" . ?χ)
    ("\\psi" . ?ψ)
    ("\\omega" . ?ω)
    ;; Uppercase Greek letters.
    ("\\Gamma" . ?Γ)
    ("\\Delta" . ?Δ)
    ("\\Lambda" . ?Λ)
    ("\\Phi" . ?Φ)
    ("\\Pi" . ?Π)
    ("\\Psi" . ?Ψ)
    ("\\Sigma" . ?Σ)
    ("\\Theta" . ?Θ)
    ("\\Upsilon" . ?Υ)
    ("\\Xi" . ?Ξ)
    ("\\Omega" . ?Ω)

    ;; Other math symbols (taken from leim/quail/latin-ltx.el).
    ("\\Box" . ?□)
    ("\\Bumpeq" . ?≎)
    ("\\Cap" . ?⋒)
    ("\\Cup" . ?⋓)
    ("\\Diamond" . ?◇)
    ("\\Downarrow" . ?⇓)
    ("\\H{o}" . ?ő)
    ("\\Im" . ?ℑ)
    ("\\Join" . ?⋈)
    ("\\Leftarrow" . ?⇐)
    ("\\Leftrightarrow" . ?⇔)
    ("\\Ll" . ?⋘)
    ("\\Lleftarrow" . ?⇚)
    ("\\Longleftarrow" . ?⇐)
    ("\\Longleftrightarrow" . ?⇔)
    ("\\Longrightarrow" . ?⇒)
    ("\\Lsh" . ?↰)
    ("\\Re" . ?ℜ)
    ("\\Rightarrow" . ?⇒)
    ("\\Rrightarrow" . ?⇛)
    ("\\Rsh" . ?↱)
    ("\\Subset" . ?⋐)
    ("\\Supset" . ?⋑)
    ("\\Uparrow" . ?⇑)
    ("\\Updownarrow" . ?⇕)
    ("\\Vdash" . ?⊩)
    ("\\Vert" . ?‖)
    ("\\Vvdash" . ?⊪)
    ("\\aleph" . ?ℵ)
    ("\\amalg" . ?∐)
    ("\\angle" . ?∠)
    ("\\approx" . ?≈)
    ("\\approxeq" . ?≊)
    ("\\ast" . ?∗)
    ("\\asymp" . ?≍)
    ("\\backcong" . ?≌)
    ("\\backepsilon" . ?∍)
    ("\\backprime" . ?‵)
    ("\\backsim" . ?∽)
    ("\\backsimeq" . ?⋍)
    ("\\backslash" . ?\\)
    ("\\barwedge" . ?⊼)
    ("\\because" . ?∵)
    ("\\beth" . ?ℶ)
    ("\\between" . ?≬)
    ("\\bigcap" . ?⋂)
    ("\\bigcirc" . ?◯)
    ("\\bigcup" . ?⋃)
    ("\\bigstar" . ?★)
    ("\\bigtriangledown" . ?▽)
    ("\\bigtriangleup" . ?△)
    ("\\bigvee" . ?⋁)
    ("\\bigwedge" . ?⋀)
    ("\\blacklozenge" . ?✦)
    ("\\blacksquare" . ?▪)
    ("\\blacktriangle" . ?▴)
    ("\\blacktriangledown" . ?▾)
    ("\\blacktriangleleft" . ?◂)
    ("\\blacktriangleright" . ?▸)
    ("\\bot" . ?⊥)
    ("\\bowtie" . ?⋈)
    ("\\boxminus" . ?⊟)
    ("\\boxplus" . ?⊞)
    ("\\boxtimes" . ?⊠)
    ("\\bullet" . ?•)
    ("\\bumpeq" . ?≏)
    ("\\cap" . ?∩)
    ("\\cdots" . ?⋯)
    ("\\centerdot" . ?·)
    ("\\checkmark" . ?✓)
    ("\\chi" . ?χ)
    ("\\cdot" . ?⋅)
    ("\\cdots" . ?⋯)
    ("\\circ" . ?∘)
    ("\\circeq" . ?≗)
    ("\\circlearrowleft" . ?↺)
    ("\\circlearrowright" . ?↻)
    ("\\circledR" . ?®)
    ("\\circledS" . ?Ⓢ)
    ("\\circledast" . ?⊛)
    ("\\circledcirc" . ?⊚)
    ("\\circleddash" . ?⊝)
    ("\\clubsuit" . ?♣)
    ("\\coloneq" . ?≔)
    ("\\complement" . ?∁)
    ("\\cong" . ?≅)
    ("\\coprod" . ?∐)
    ("\\cup" . ?∪)
    ("\\curlyeqprec" . ?⋞)
    ("\\curlyeqsucc" . ?⋟)
    ("\\curlypreceq" . ?≼)
    ("\\curlyvee" . ?⋎)
    ("\\curlywedge" . ?⋏)
    ("\\curvearrowleft" . ?↶)
    ("\\curvearrowright" . ?↷)
    ("\\dag" . ?†)
    ("\\dagger" . ?†)
    ("\\daleth" . ?ℸ)
    ("\\dashv" . ?⊣)
    ("\\ddag" . ?‡)
    ("\\ddagger" . ?‡)
    ("\\ddots" . ?⋱)
    ("\\diamond" . ?⋄)
    ("\\diamondsuit" . ?♢)
    ("\\divideontimes" . ?⋇)
    ("\\doteq" . ?≐)
    ("\\doteqdot" . ?≑)
    ("\\dotplus" . ?∔)
    ("\\dotsquare" . ?⊡)
    ("\\downarrow" . ?↓)
    ("\\downdownarrows" . ?⇊)
    ("\\downleftharpoon" . ?⇃)
    ("\\downrightharpoon" . ?⇂)
    ("\\ell" . ?ℓ)
    ("\\emptyset" . ?∅)
    ("\\eqcirc" . ?≖)
    ("\\eqcolon" . ?≕)
    ("\\eqslantgtr" . ?⋝)
    ("\\eqslantless" . ?⋜)
    ("\\equiv" . ?≡)
    ("\\exists" . ?∃)
    ("\\fallingdotseq" . ?≒)
    ("\\flat" . ?♭)
    ("\\forall" . ?∀)
    ("\\frown" . ?⌢)
    ("\\ge" . ?≥)
    ("\\geq" . ?≥)
    ("\\geqq" . ?≧)
    ("\\geqslant" . ?≥)
    ("\\gets" . ?←)
    ("\\gg" . ?≫)
    ("\\ggg" . ?⋙)
    ("\\gimel" . ?ℷ)
    ("\\gnapprox" . ?⋧)
    ("\\gneq" . ?≩)
    ("\\gneqq" . ?≩)
    ("\\gnsim" . ?⋧)
    ("\\gtrapprox" . ?≳)
    ("\\gtrdot" . ?⋗)
    ("\\gtreqless" . ?⋛)
    ("\\gtreqqless" . ?⋛)
    ("\\gtrless" . ?≷)
    ("\\gtrsim" . ?≳)
    ("\\gvertneqq" . ?≩)
    ("\\hbar" . ?ℏ)
    ("\\heartsuit" . ?♥)
    ("\\hookleftarrow" . ?↩)
    ("\\hookrightarrow" . ?↪)
    ("\\iff" . ?⇔)
    ("\\imath" . ?ı)
    ("\\in" . ?∈)
    ("\\infty" . ?∞)
    ("\\int" . ?∫)
    ("\\intercal" . ?⊺)
    ("\\langle" . 10216)          ; Literal ?⟨ breaks indentation.
    ("\\lbrace" . ?{)
    ("\\lbrack" . ?\[)
    ("\\lceil" . ?⌈)
    ("\\ldots" . ?…)
    ("\\le" . ?≤)
    ("\\leadsto" . ?↝)
    ("\\leftarrow" . ?←)
    ("\\leftarrowtail" . ?↢)
    ("\\leftharpoondown" . ?↽)
    ("\\leftharpoonup" . ?↼)
    ("\\leftleftarrows" . ?⇇)
    ;; ("\\leftparengtr" ?〈), see bug#12948.
    ("\\leftrightarrow" . ?↔)
    ("\\leftrightarrows" . ?⇆)
    ("\\leftrightharpoons" . ?⇋)
    ("\\leftrightsquigarrow" . ?↭)
    ("\\leftthreetimes" . ?⋋)
    ("\\leq" . ?≤)
    ("\\leqq" . ?≦)
    ("\\leqslant" . ?≤)
    ("\\lessapprox" . ?≲)
    ("\\lessdot" . ?⋖)
    ("\\lesseqgtr" . ?⋚)
    ("\\lesseqqgtr" . ?⋚)
    ("\\lessgtr" . ?≶)
    ("\\lesssim" . ?≲)
    ("\\lfloor" . ?⌊)
    ("\\lhd" . ?◁)
    ("\\rhd" . ?▷)
    ("\\ll" . ?≪)
    ("\\llcorner" . ?⌞)
    ("\\lnapprox" . ?⋦)
    ("\\lneq" . ?≨)
    ("\\lneqq" . ?≨)
    ("\\lnsim" . ?⋦)
    ("\\longleftarrow" . ?←)
    ("\\longleftrightarrow" . ?↔)
    ("\\longmapsto" . ?↦)
    ("\\longrightarrow" . ?→)
    ("\\looparrowleft" . ?↫)
    ("\\looparrowright" . ?↬)
    ("\\lozenge" . ?✧)
    ("\\lq" . ?‘)
    ("\\lrcorner" . ?⌟)
    ("\\ltimes" . ?⋉)
    ("\\lvertneqq" . ?≨)
    ("\\maltese" . ?✠)
    ("\\mapsto" . ?↦)
    ("\\measuredangle" . ?∡)
    ("\\mho" . ?℧)
    ("\\mid" . ?∣)
    ("\\models" . ?⊧)
    ("\\mp" . ?∓)
    ("\\multimap" . ?⊸)
    ("\\nLeftarrow" . ?⇍)
    ("\\nLeftrightarrow" . ?⇎)
    ("\\nRightarrow" . ?⇏)
    ("\\nVDash" . ?⊯)
    ("\\nVdash" . ?⊮)
    ("\\nabla" . ?∇)
    ("\\napprox" . ?≉)
    ("\\natural" . ?♮)
    ("\\ncong" . ?≇)
    ("\\ne" . ?≠)
    ("\\nearrow" . ?↗)
    ("\\neg" . ?¬)
    ("\\neq" . ?≠)
    ("\\nequiv" . ?≢)
    ("\\newline" . ? )
    ("\\nexists" . ?∄)
    ("\\ngeq" . ?≱)
    ("\\ngeqq" . ?≱)
    ("\\ngeqslant" . ?≱)
    ("\\ngtr" . ?≯)
    ("\\ni" . ?∋)
    ("\\nleftarrow" . ?↚)
    ("\\nleftrightarrow" . ?↮)
    ("\\nleq" . ?≰)
    ("\\nleqq" . ?≰)
    ("\\nleqslant" . ?≰)
    ("\\nless" . ?≮)
    ("\\nmid" . ?∤)
    ;; ("\\not" ?̸)              ;FIXME: conflict with "NOT SIGN" ¬.
    ("\\notin" . ?∉)
    ("\\nparallel" . ?∦)
    ("\\nprec" . ?⊀)
    ("\\npreceq" . ?⋠)
    ("\\nrightarrow" . ?↛)
    ("\\nshortmid" . ?∤)
    ("\\nshortparallel" . ?∦)
    ("\\nsim" . ?≁)
    ("\\nsimeq" . ?≄)
    ("\\nsubset" . ?⊄)
    ("\\nsubseteq" . ?⊈)
    ("\\nsubseteqq" . ?⊈)
    ("\\nsucc" . ?⊁)
    ("\\nsucceq" . ?⋡)
    ("\\nsupset" . ?⊅)
    ("\\nsupseteq" . ?⊉)
    ("\\nsupseteqq" . ?⊉)
    ("\\ntriangleleft" . ?⋪)
    ("\\ntrianglelefteq" . ?⋬)
    ("\\ntriangleright" . ?⋫)
    ("\\ntrianglerighteq" . ?⋭)
    ("\\nvDash" . ?⊭)
    ("\\nvdash" . ?⊬)
    ("\\nwarrow" . ?↖)
    ("\\odot" . ?⊙)
    ("\\oint" . ?∮)
    ("\\ominus" . ?⊖)
    ("\\oplus" . ?⊕)
    ("\\oslash" . ?⊘)
    ("\\otimes" . ?⊗)
    ("\\par" . ? )
    ("\\parallel" . ?∥)
    ("\\partial" . ?∂)
    ("\\perp" . ?⊥)
    ("\\pitchfork" . ?⋔)
    ("\\prec" . ?≺)
    ("\\precapprox" . ?≾)
    ("\\preceq" . ?≼)
    ("\\precnapprox" . ?⋨)
    ("\\precnsim" . ?⋨)
    ("\\precsim" . ?≾)
    ("\\prime" . ?′)
    ("\\prod" . ?∏)
    ("\\propto" . ?∝)
    ("\\qed" . ?∎)
    ("\\qquad" . ?⧢)
    ("\\quad" . ?␣)
    ("\\rangle" . 10217)            ; Literal ?⟩ breaks indentation.
    ("\\rbrace" . ?})
    ("\\rbrack" . ?\])
    ("\\rceil" . ?⌉)
    ("\\rfloor" . ?⌋)
    ("\\rightarrow" . ?→)
    ("\\rightarrowtail" . ?↣)
    ("\\rightharpoondown" . ?⇁)
    ("\\rightharpoonup" . ?⇀)
    ("\\rightleftarrows" . ?⇄)
    ("\\rightleftharpoons" . ?⇌)
    ;; ("\\rightparengtr" ?⦔) ;; Was ?〉, see bug#12948.
    ("\\rightrightarrows" . ?⇉)
    ("\\rightthreetimes" . ?⋌)
    ("\\risingdotseq" . ?≓)
    ("\\rtimes" . ?⋊)
    ("\\times" . ?×)
    ("\\sbs" . ?﹨)
    ("\\searrow" . ?↘)
    ("\\setminus" . ?∖)
    ("\\sharp" . ?♯)
    ("\\shortmid" . ?∣)
    ("\\shortparallel" . ?∥)
    ("\\sim" . ?∼)
    ("\\simeq" . ?≃)
    ("\\smallamalg" . ?∐)
    ("\\smallsetminus" . ?∖)
    ("\\smallsmile" . ?⌣)
    ("\\smile" . ?⌣)
    ("\\spadesuit" . ?♠)
    ("\\sphericalangle" . ?∢)
    ("\\sqcap" . ?⊓)
    ("\\sqcup" . ?⊔)
    ("\\sqsubset" . ?⊏)
    ("\\sqsubseteq" . ?⊑)
    ("\\sqsupset" . ?⊐)
    ("\\sqsupseteq" . ?⊒)
    ("\\square" . ?□)
    ("\\squigarrowright" . ?⇝)
    ("\\star" . ?⋆)
    ("\\straightphi" . ?φ)
    ("\\subset" . ?⊂)
    ("\\subseteq" . ?⊆)
    ("\\subseteqq" . ?⊆)
    ("\\subsetneq" . ?⊊)
    ("\\subsetneqq" . ?⊊)
    ("\\succ" . ?≻)
    ("\\succapprox" . ?≿)
    ("\\succcurlyeq" . ?≽)
    ("\\succeq" . ?≽)
    ("\\succnapprox" . ?⋩)
    ("\\succnsim" . ?⋩)
    ("\\succsim" . ?≿)
    ("\\sum" . ?∑)
    ("\\supset" . ?⊃)
    ("\\supseteq" . ?⊇)
    ("\\supseteqq" . ?⊇)
    ("\\supsetneq" . ?⊋)
    ("\\supsetneqq" . ?⊋)
    ("\\surd" . ?√)
    ("\\swarrow" . ?↙)
    ("\\therefore" . ?∴)
    ("\\thickapprox" . ?≈)
    ("\\thicksim" . ?∼)
    ("\\to" . ?→)
    ("\\top" . ?⊤)
    ("\\triangle" . ?▵)
    ("\\triangledown" . ?▿)
    ("\\triangleleft" . ?◃)
    ("\\trianglelefteq" . ?⊴)
    ("\\triangleq" . ?≜)
    ("\\triangleright" . ?▹)
    ("\\trianglerighteq" . ?⊵)
    ("\\twoheadleftarrow" . ?↞)
    ("\\twoheadrightarrow" . ?↠)
    ("\\ulcorner" . ?⌜)
    ("\\uparrow" . ?↑)
    ("\\updownarrow" . ?↕)
    ("\\upleftharpoon" . ?↿)
    ("\\uplus" . ?⊎)
    ("\\uprightharpoon" . ?↾)
    ("\\upuparrows" . ?⇈)
    ("\\urcorner" . ?⌝)
    ("\\u{i}" . ?ĭ)
    ("\\vDash" . ?⊨)
    ("\\varepsilon" . ?ε)
    ("\\varprime" . ?′)
    ("\\varpropto" . ?∝)
    ("\\varrho" . ?ϱ)
    ;; ("\\varsigma" ?ς)		;FIXME: Looks reversed with the non\var.
    ("\\vartriangleleft" . ?⊲)
    ("\\vartriangleright" . ?⊳)
    ("\\vdash" . ?⊢)
    ("\\vdots" . ?⋮)
    ("\\vee" . ?∨)
    ("\\veebar" . ?⊻)
    ("\\vert" . ?|)
    ("\\wedge" . ?∧)
    ("\\wp" . ?℘)
    ("\\wr" . ?≀)
    ("\\Bbb{N}" . ?ℕ)			; AMS commands for blackboard bold
    ("\\Bbb{P}" . ?ℙ)			; Also sometimes \mathbb.
    ("\\Bbb{Q}" . ?ℚ)
    ("\\Bbb{R}" . ?ℝ)
    ("\\Bbb{Z}" . ?ℤ)
    ("--" . ?–)
    ("---" . ?—)
    ("\\ordfeminine" . ?ª)
    ("\\ordmasculine" . ?º)
    ("\\lambdabar" . ?ƛ)
    ("\\celsius" . ?℃)
    ("\\textmu" . ?µ)
    ("\\textfractionsolidus" . ?⁄)
    ("\\textbigcircle" . ?⃝)
    ("\\textmusicalnote" . ?♪)
    ("\\textdied" . ?✝)
    ("\\textcolonmonetary" . ?₡)
    ("\\textwon" . ?₩)
    ("\\textnaira" . ?₦)
    ("\\textpeso" . ?₱)
    ("\\textlira" . ?₤)
    ("\\textrecipe" . ?℞)
    ("\\textinterrobang" . ?‽)
    ("\\textpertenthousand" . ?‱)
    ("\\textbaht" . ?฿)
    ("\\textnumero" . ?№)
    ("\\textdiscount" . ?⁒)
    ("\\textestimated" . ?℮)
    ("\\textopenbullet" . ?◦)
    ("\\textlquill" . 8261)		; Literal ?⁅ breaks indentation.
    ("\\textrquill" . 8262)             ; Literal ?⁆ breaks indentation.
    ("\\textcircledP" . ?℗)
    ("\\textreferencemark" . ?※))
  "A `prettify-symbols-alist' used when viewing math source code. Taken from for tex-mode.el.")


(provide 'arxiv-vars)

;;; arxiv-vars.el ends here
