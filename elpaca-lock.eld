;; Installed package revisions. Update with elpaca-write-lock-file.
((ace-window :source "elpaca-menu-lock-file" :recipe
	     (:package "ace-window" :repo "abo-abo/ace-window"
		       :fetcher github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :id ace-window :type git
		       :protocol https :inherit t :depth treeless :ref
		       "77115afc1b0b9f633084cf7479c767988106c196"))
 (acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		 "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el"
			   "*-test.el" "*-tests.el" "LICENSE"
			   "README*" "*-pkg.el"))
		:source "MELPA" :id acp :type git :protocol https
		:inherit t :depth treeless :ref
		"2959da5fa8657da17a78ac6ceef303a50e3e69de"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
	      (:package "agent-shell" :fetcher github :repo
			"xenodium/agent-shell" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :id agent-shell :type git
			:protocol https :inherit t :depth treeless
			:ref
			"78a2c94daa255b97ea001bd8a6507c60af20e89d"))
 (agent-shell-math-renderer :source "elpaca-menu-lock-file" :recipe
			    (:source "Init file" :package
				     "agent-shell-math-renderer" :id
				     agent-shell-math-renderer :host
				     github :repo
				     "alberti42/agent-shell-math-renderer"
				     :type git :protocol https
				     :inherit t :depth treeless :ref
				     "b16e836ebca53644162516f4ff2048f56bbf6584"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
	   (:package "apheleia" :fetcher github :repo
		     "radian-software/apheleia" :files
		     (:defaults ("scripts" "scripts/formatters"))
		     :source "MELPA" :id apheleia :type git :protocol
		     https :inherit t :depth treeless :ref
		     "047119b7c6f18ffdbf5f88116a427f7e38653cc6"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		 "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el"
			   "*-test.el" "*-tests.el" "LICENSE"
			   "README*" "*-pkg.el"))
		:source "MELPA" :id avy :type git :protocol https
		:inherit t :depth treeless :ref
		"933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (batppuccin-themes :source "elpaca-menu-lock-file" :recipe
		    (:source "Init file" :package "batppuccin-themes"
			     :id batppuccin-themes :host github :repo
			     "bbatsov/batppuccin-emacs" :main
			     "batppuccin.el" :type git :protocol https
			     :inherit t :depth treeless :ref
			     "233399d074499eea3268779b4bcc99cb2176a079"))
 (bazel :source "elpaca-menu-lock-file" :recipe
	(:package "bazel" :repo "bazelbuild/emacs-bazel-mode" :fetcher
		  github :old-names (bazel-mode) :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :id bazel :type git :protocol https
		  :inherit t :depth treeless :ref
		  "77afb8c8cf7620a198f123cc4f1d87937e189797"))
 (breadcrumb :source "elpaca-menu-lock-file" :recipe
	     (:package "breadcrumb" :repo
		       ("https://github.com/joaotavora/breadcrumb"
			. "breadcrumb")
		       :tar "1.0.1" :host gnu :files
		       ("*" (:exclude ".git")) :source "GNU ELPA" :id
		       breadcrumb :type git :protocol https :inherit t
		       :depth treeless :ref
		       "bcf7f1d1c102f8430c017d66aeee5d12e26585a4"))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
		   (:package "browse-at-remote" :repo
			     "rmuslimov/browse-at-remote" :fetcher
			     github :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info"
			      "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      "docs/dir" "docs/*.info" "docs/*.texi"
			      "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el"
					"tests.el" "*-test.el"
					"*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :id browse-at-remote
			     :type git :protocol https :inherit t
			     :depth treeless :ref
			     "38e5ffd77493c17c821fd88f938dbf42705a5158"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el"
			    "*-test.el" "*-tests.el" "LICENSE"
			    "README*" "*-pkg.el"))
		 :source "MELPA" :id cape :type git :protocol https
		 :inherit t :depth treeless :ref
		 "1c543fce9151821e8400258aefcfdee2fb24920d"))
 (citar :source "elpaca-menu-lock-file" :recipe
	(:package "citar" :repo "emacs-citar/citar" :fetcher github
		  :files (:defaults (:exclude "citar-embark.el"))
		  :old-names (bibtex-actions) :source "MELPA" :id
		  citar :type git :protocol https :inherit t :depth
		  treeless :ref
		  "5efc5ac78f235cdaafe361758d03c7a51a015dd1"))
 (citar-embark :source "elpaca-menu-lock-file" :recipe
	       (:package "citar-embark" :repo "emacs-citar/citar"
			 :fetcher github :files ("citar-embark.el")
			 :source "MELPA" :id citar-embark :type git
			 :protocol https :inherit t :depth treeless
			 :ref
			 "5efc5ac78f235cdaafe361758d03c7a51a015dd1"))
 (citeproc :source "elpaca-menu-lock-file" :recipe
	   (:package "citeproc" :fetcher github :repo
		     "andras-simonyi/citeproc-el" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id citeproc :type git :protocol
		     https :inherit t :depth treeless :ref
		     "4bde999a41803fe519ea80eab8b813d53503eebd"))
 (compat :source "elpaca-menu-lock-file" :recipe
	 (:package "compat" :repo
		   ("https://github.com/emacs-compat/compat"
		    . "compat")
		   :tar "31.0.0.2" :host gnu :files
		   ("*" (:exclude ".git")) :source "GNU ELPA" :id
		   compat :type git :protocol https :inherit t :depth
		   treeless :ref
		   "90880f81419577e1d3f68424d2a3adf31e6d663e"))
 (cond-let :source "elpaca-menu-lock-file" :recipe
	   (:package "cond-let" :fetcher github :repo
		     "tarsius/cond-let" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id cond-let :type git :protocol
		     https :inherit t :depth treeless :ref
		     "3b88187fe067d4ca3dec3ef8a329b0ce18bdb356"))
 (consult :source "elpaca-menu-lock-file" :recipe
	  (:package "consult" :repo "minad/consult" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :id consult :type git :protocol
		    https :inherit t :depth treeless :ref
		    "9979fbb02e633267d0f6bc6cafa266fe87a0007f"))
 (corfu :source "elpaca-menu-lock-file" :recipe
	(:package "corfu" :repo "minad/corfu" :files
		  (:defaults "extensions/corfu-*.el") :fetcher github
		  :source "MELPA" :id corfu :type git :protocol https
		  :inherit t :depth treeless :ref
		  "b468efac023dda39332acc943edc9895c80b5a6f"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
		 ("dash.el" "dash.texi") :source "MELPA" :id dash
		 :type git :protocol https :inherit t :depth treeless
		 :ref "d746dd9edcb67a108818beb0cdc78dc1cb466832"))
 (diredfl :source "elpaca-menu-lock-file" :recipe
	  (:package "diredfl" :fetcher github :repo "purcell/diredfl"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :id diredfl :type git :protocol
		    https :inherit t :depth treeless :ref
		    "fe72d2e42ee18bf6228bba9d7086de4098f18a70"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
		(:package "doom-modeline" :repo
			  "seagle0128/doom-modeline" :fetcher github
			  :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :id doom-modeline :type git
			  :protocol https :inherit t :depth treeless
			  :ref
			  "8af4db0f7ee63b9382c28f6db1f5b60e6203e0b3"))
 (eglot-tempel :source "elpaca-menu-lock-file" :recipe
	       (:package "eglot-tempel" :fetcher github :repo
			 "fejfighter/eglot-tempel" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			  "*.texinfo" "doc/dir" "doc/*.info"
			  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			  "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el"
				    "tests.el" "*-test.el"
				    "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :id eglot-tempel :type git
			 :protocol https :inherit t :depth treeless
			 :ref
			 "c6c9a18eba61f6bae7167fa62bab9b637592d20d"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
	     (:package "elisp-refs" :repo "Wilfred/elisp-refs"
		       :fetcher github :files
		       (:defaults (:exclude "elisp-refs-bench.el"))
		       :source "MELPA" :id elisp-refs :type git
		       :protocol https :inherit t :depth treeless :ref
		       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source "elpaca-menu-lock-file" :recipe
	 (:source "Init file" :package "elpaca" :id elpaca :repo
		  "https://github.com/progfolio/elpaca.git" :ref
		  "78b8e7cc98c198c8dbeb18140649e2d668126712" :depth 1
		  :inherit ignore :files
		  (:defaults "elpaca-test.el" (:exclude "extensions"))
		  :build (:not elpaca-activate) :type git :protocol
		  https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
		     (:package "elpaca-use-package" :wait t :repo
			       "https://github.com/progfolio/elpaca.git"
			       :files
			       ("extensions/elpaca-use-package.el")
			       :main
			       "extensions/elpaca-use-package.el"
			       :build
			       (:not elpaca-source elpaca-build-docs)
			       :source "Elpaca extensions" :id
			       elpaca-use-package :type git :protocol
			       https :inherit t :depth treeless :ref
			       "78b8e7cc98c198c8dbeb18140649e2d668126712"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
	  (:package "emacsql" :fetcher github :repo "magit/emacsql"
		    :files (:defaults "README.md" "sqlite") :source
		    "MELPA" :id emacsql :type git :protocol https
		    :inherit t :depth treeless :ref
		    "5430450af72ebe4917ff87a6bf3ac8f613f6dced"))
 (embark :source "elpaca-menu-lock-file" :recipe
	 (:package "embark" :repo "oantolin/embark" :fetcher github
		   :files ("embark.el" "embark-org.el" "embark.texi")
		   :source "MELPA" :id embark :type git :protocol
		   https :inherit t :depth treeless :ref
		   "87e53827cf6659dcc4ac4e54be9af34aeca44f6e"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
		 (:package "embark-consult" :repo "oantolin/embark"
			   :fetcher github :files
			   ("embark-consult.el") :source "MELPA" :id
			   embark-consult :type git :protocol https
			   :inherit t :depth treeless :ref
			   "87e53827cf6659dcc4ac4e54be9af34aeca44f6e"))
 (envrc :source "elpaca-menu-lock-file" :recipe
	(:package "envrc" :fetcher github :repo "purcell/envrc" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :id envrc :type git :protocol https
		  :inherit t :depth treeless :ref
		  "1ecb82e01745d700578754eb35d6c1758290b869"))
 (erlang :source "elpaca-menu-lock-file" :recipe
	 (:package "erlang" :fetcher github :repo "erlang/otp"
		   :version-regexp "OTP-%v" :files
		   ("lib/tools/emacs/*.el"
		    (:exclude "lib/tools/emacs/erlang_appwiz.el"))
		   :source "MELPA" :id erlang :type git :protocol
		   https :inherit t :depth treeless :ref
		   "09162a3a2869a693d580bbd117c6d0ec61d6ef3d"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
		 (:defaults "doc/build/texinfo/evil.texi"
			    (:exclude "evil-test-helpers.el"))
		 :source "MELPA" :id evil :type git :protocol https
		 :inherit t :depth treeless :ref
		 "6a3e1ddd04ac504a016590940d0af2a3361b9efd"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
		  (:package "evil-collection" :fetcher github :repo
			    "emacs-evil/evil-collection" :files
			    (:defaults "modes") :source "MELPA" :id
			    evil-collection :type git :protocol https
			    :inherit t :depth treeless :ref
			    "2cca4b8e479b1e7923bcef40d3006f6b7e33d996"))
 (evil-escape :source "elpaca-menu-lock-file" :recipe
	      (:package "evil-escape" :fetcher github :repo
			"emacsorphanage/evil-escape" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :id evil-escape :type git
			:protocol https :inherit t :depth treeless
			:ref
			"aebd1a78a6bd33e5164e7552096b3fe1172d3012"))
 (evil-iedit-state :source "elpaca-menu-lock-file" :recipe
		   (:package "evil-iedit-state" :repo
			     "syl20bnr/evil-iedit-state" :fetcher
			     github :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info"
			      "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      "docs/dir" "docs/*.info" "docs/*.texi"
			      "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el"
					"tests.el" "*-test.el"
					"*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :id evil-iedit-state
			     :type git :protocol https :inherit t
			     :depth treeless :ref
			     "6f7b502447ba35676375169d7707372ebad2791f"))
 (evil-org :source "elpaca-menu-lock-file" :recipe
	   (:package "evil-org" :fetcher github :repo
		     "Somelauw/evil-org-mode" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id evil-org :type git :protocol
		     https :inherit t :depth treeless :ref
		     "b1f309726b1326e1a103742524ec331789f2bf94"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
		(:package "evil-surround" :repo
			  "emacs-evil/evil-surround" :fetcher github
			  :old-names (surround) :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :id evil-surround :type git
			  :protocol https :inherit t :depth treeless
			  :ref
			  "e6548372e8359ee55e67d73ca418314086011f1a"))
 (excali :source "elpaca-menu-lock-file" :recipe
	 (:source "Init file" :package "excali" :id excali :host
		  github :repo "yibie/excali-mode" :files
		  (:defaults "excali-module.*" "fonts") :build
		  (:before elpaca-build-link +excali-build) :type git
		  :protocol https :inherit t :depth treeless :ref
		  "bf92e954f82b5799ae46ceea1bbe1457824abf12"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
		       (:package "exec-path-from-shell" :fetcher
				 github :repo
				 "purcell/exec-path-from-shell" :files
				 ("*.el" "*.el.in" "dir" "*.info"
				  "*.texi" "*.texinfo" "doc/dir"
				  "doc/*.info" "doc/*.texi"
				  "doc/*.texinfo" "lisp/*.el"
				  "docs/dir" "docs/*.info"
				  "docs/*.texi" "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el"
					    "tests.el" "*-test.el"
					    "*-tests.el" "LICENSE"
					    "README*" "*-pkg.el"))
				 :source "MELPA" :id
				 exec-path-from-shell :type git
				 :protocol https :inherit t :depth
				 treeless :ref
				 "6146fdc16e9882df270be7e58ae8d628032d6bc4"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
		(:package "expand-region" :repo
			  "magnars/expand-region.el" :fetcher github
			  :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :id expand-region :type git
			  :protocol https :inherit t :depth treeless
			  :ref
			  "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
	       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
	       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
	       "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el"
			 "*-test.el" "*-tests.el" "LICENSE" "README*"
			 "*-pkg.el"))
	      :source "MELPA" :id f :type git :protocol https :inherit
	      t :depth treeless :ref
	      "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flyspell-correct :source "elpaca-menu-lock-file" :recipe
		   (:package "flyspell-correct" :repo
			     "d12frosted/flyspell-correct" :fetcher
			     github :files
			     ("flyspell-correct.el"
			      "flyspell-correct-ido.el")
			     :source "MELPA" :id flyspell-correct
			     :type git :protocol https :inherit t
			     :depth treeless :ref
			     "cabf0c9bdba3a08ae3c517302ea6ad55cd638098"))
 (general :source "elpaca-menu-lock-file" :recipe
	  (:package "general" :fetcher github :repo
		    "noctuid/general.el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :id general :wait t :type git
		    :protocol https :inherit t :depth treeless :ref
		    "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
		  (:package "git-timemachine" :fetcher codeberg :repo
			    "pidu/git-timemachine" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info"
			     "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			     "docs/dir" "docs/*.info" "docs/*.texi"
			     "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el"
				       "tests.el" "*-test.el"
				       "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :id git-timemachine :type
			    git :protocol https :inherit t :depth
			    treeless :ref
			    "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
	   (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher
		     github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id goto-chg :type git :protocol
		     https :inherit t :depth treeless :ref
		     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :source "elpaca-menu-lock-file" :recipe
	(:package "gptel" :repo "karthink/gptel" :fetcher github
		  :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :id gptel :type git :protocol https
		  :inherit t :depth treeless :ref
		  "44deae37928175764bbe8314dbbc79bdec28ab4c"))
 (helpful :source "elpaca-menu-lock-file" :recipe
	  (:package "helpful" :repo "Wilfred/helpful" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :id helpful :type git :protocol
		    https :inherit t :depth treeless :ref
		    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hide-mode-line :source "elpaca-menu-lock-file" :recipe
		 (:package "hide-mode-line" :repo
			   "hlissner/emacs-hide-mode-line" :fetcher
			   github :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			    "*.texinfo" "doc/dir" "doc/*.info"
			    "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			    "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el"
				      "tests.el" "*-test.el"
				      "*-tests.el" "LICENSE" "README*"
				      "*-pkg.el"))
			   :source "MELPA" :id hide-mode-line :host
			   github :type git :protocol https :inherit t
			   :depth treeless :ref
			   "ddd154f1e04d666cd004bf8212ead8684429350d"))
 (highlight-thing :source "elpaca-menu-lock-file" :recipe
		  (:package "highlight-thing" :fetcher github :repo
			    "fgeller/highlight-thing.el" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info"
			     "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			     "docs/dir" "docs/*.info" "docs/*.texi"
			     "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el"
				       "tests.el" "*-test.el"
				       "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :id highlight-thing :type
			    git :protocol https :inherit t :depth
			    treeless :ref
			    "fd0309e72727e5332be3397e15edb035b426dd0b"))
 (iedit :source "elpaca-menu-lock-file" :recipe
	(:package "iedit" :repo "victorhge/iedit" :fetcher github
		  :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :id iedit :type git :protocol https
		  :inherit t :depth treeless :ref
		  "14161daa295332a49dda92b97c00d62efd38acfe"))
 (inf-elixir :source "elpaca-menu-lock-file" :recipe
	     (:package "inf-elixir" :fetcher github :repo
		       "J3RN/inf-elixir" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :id inf-elixir :type git
		       :protocol https :inherit t :depth treeless :ref
		       "085f1c198a121c569a161f9fa71e6cce1998cd63"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
	     (:package "inheritenv" :fetcher github :repo
		       "purcell/inheritenv" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :id inheritenv :type git
		       :protocol https :inherit t :depth treeless :ref
		       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (kdl-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "kdl-mode" :fetcher github :repo
		     "taquangtrung/emacs-kdl-mode" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id kdl-mode :host github :type
		     git :protocol https :inherit t :depth treeless
		     :ref "c6ef2a0375b1e179c08a2e0f3809d318d8a0c090"))
 (keychain-environment :source "elpaca-menu-lock-file" :recipe
		       (:package "keychain-environment" :fetcher
				 github :repo
				 "emacsorphanage/keychain-environment"
				 :files
				 ("*.el" "*.el.in" "dir" "*.info"
				  "*.texi" "*.texinfo" "doc/dir"
				  "doc/*.info" "doc/*.texi"
				  "doc/*.texinfo" "lisp/*.el"
				  "docs/dir" "docs/*.info"
				  "docs/*.texi" "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el"
					    "tests.el" "*-test.el"
					    "*-tests.el" "LICENSE"
					    "README*" "*-pkg.el"))
				 :source "MELPA" :id
				 keychain-environment :type git
				 :protocol https :inherit t :depth
				 treeless :ref
				 "199f407f28f8f473790126a1149abba38e4b3164"))
 (latex-to-svg-backend :source "elpaca-menu-lock-file" :recipe
		       (:source "Init file" :package
				"latex-to-svg-backend" :id
				latex-to-svg-backend :host github
				:repo "alberti42/latex-to-svg-backend"
				:type git :protocol https :inherit t
				:depth treeless :ref
				"a165f80e471cc982f1df4c3c6985939eeb0eb215"))
 (ligature :source "elpaca-menu-lock-file" :recipe
	   (:package "ligature" :fetcher github :repo
		     "mickeynp/ligature.el" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id ligature :type git :protocol
		     https :inherit t :depth treeless :ref
		     "e0bc07ec41203b72386a2b878b6c8a65c28f4ced"))
 (llama :source "elpaca-menu-lock-file" :recipe
	(:package "llama" :fetcher github :repo "tarsius/llama" :files
		  ("llama.el" ".dir-locals.el") :source "MELPA" :id
		  llama :type git :protocol https :inherit t :depth
		  treeless :ref
		  "cfea618f14bc8317f8e4947fe10000b229b9a447"))
 (magit :source "elpaca-menu-lock-file" :recipe
	(:package "magit" :fetcher github :repo "magit/magit" :files
		  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
		   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
		   ("githooks" "githooks/*")
		   ("git-hooks" "git-hooks/*")
		   (:exclude "lisp/magit-section.el"))
		  :source "MELPA" :id magit :type git :protocol https
		  :inherit t :depth treeless :ref
		  "cd9117e3a8eb9b50814474d5addfd2ca92af5605"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
		(:package "magit-section" :fetcher github :repo
			  "magit/magit" :files
			  ("lisp/magit-section.el"
			   "docs/magit-section.texi"
			   "magit-section-pkg.el")
			  :source "MELPA" :id magit-section :type git
			  :protocol https :inherit t :depth treeless
			  :ref
			  "cd9117e3a8eb9b50814474d5addfd2ca92af5605"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
	     (:package "marginalia" :repo "minad/marginalia" :fetcher
		       github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :id marginalia :type git
		       :protocol https :inherit t :depth treeless :ref
		       "c5d0139012d2a84f8040219b9aee17db4e145e5c"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
		(:package "markdown-mode" :fetcher github :repo
			  "jrblevin/markdown-mode" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :id markdown-mode :type git
			  :protocol https :inherit t :depth treeless
			  :ref
			  "76cb4ffecfdf95ee769e5cb4608e04202c3c1521"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
	     (:package "nerd-icons" :repo
		       "rainstormstudio/nerd-icons.el" :fetcher github
		       :files (:defaults "data") :source "MELPA" :id
		       nerd-icons :type git :protocol https :inherit t
		       :depth treeless :ref
		       "17faac7977242b470732efd417d3bcc8eb5a830e"))
 (nix-ts-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "nix-ts-mode" :fetcher github :repo
			"nix-community/nix-ts-mode" :files
			("nix-ts-mode.el") :source "MELPA" :id
			nix-ts-mode :type git :protocol https :inherit
			t :depth treeless :ref
			"09c89886a22b8e37ac0de9210ff5b2b79c520fd7"))
 (orderless :source "elpaca-menu-lock-file" :recipe
	    (:package "orderless" :repo "oantolin/orderless" :fetcher
		      github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :id orderless :type git
		      :protocol https :inherit t :depth treeless :ref
		      "5806e3f9401606d16962cffae68188c92deb1272"))
 (org :source "elpaca-menu-lock-file" :recipe
      (:package "org" :host github :repo "emacsmirror/org" :autoloads
		"org-loaddefs.el" :depth treeless :build
		((:not elpaca-build-autoloads)
		 (:before elpaca-build-link elpaca-menu-org--build))
		:files
		(:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi"))
		:source "Org" :id org :type git :protocol https
		:inherit t :ref
		"63022c5a0cb61cd7642625618c0f310d04916d3d"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
	     (:package "org-modern" :repo "minad/org-modern" :fetcher
		       github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :id org-modern :type git
		       :protocol https :inherit t :depth treeless :ref
		       "d106d02cf045cc968c09989e9c48e0f2b44c574c"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
		   (:package "page-break-lines" :fetcher github :repo
			     "purcell/page-break-lines" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info"
			      "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      "docs/dir" "docs/*.info" "docs/*.texi"
			      "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el"
					"tests.el" "*-test.el"
					"*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :id page-break-lines
			     :type git :protocol https :inherit t
			     :depth treeless :ref
			     "bd5eecd6580481fc37ed41c32d3c18cb49f4af57"))
 (paren-face :source "elpaca-menu-lock-file" :recipe
	     (:package "paren-face" :fetcher github :repo
		       "tarsius/paren-face" :files ("paren-face.el")
		       :old-names (parenface) :source "MELPA" :id
		       paren-face :type git :protocol https :inherit t
		       :depth treeless :ref
		       "f9e2f30366937223fc71611f2590c802d577bd22"))
 (parent-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "parent-mode" :fetcher github :repo
			"Fanael/parent-mode" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :id parent-mode :type git
			:protocol https :inherit t :depth treeless
			:ref
			"fbd49857ab2e4cd0c5611c0cc83f93711657b298"))
 (parsebib :source "elpaca-menu-lock-file" :recipe
	   (:package "parsebib" :fetcher github :repo
		     "joostkremers/parsebib" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id parsebib :type git :protocol
		     https :inherit t :depth treeless :ref
		     "5b837e0a5b91a69cc0e5086d8e4a71d6d86dac93"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
	    (:package "pdf-tools" :fetcher github :repo
		      "vedang/pdf-tools" :files
		      (:defaults "README" ("build" "Makefile")
				 ("build" "server"))
		      :source "MELPA" :id pdf-tools :type git
		      :protocol https :inherit t :depth treeless :ref
		      "365f88238f46f9b1425685562105881800f10386"))
 (posframe :source "elpaca-menu-lock-file" :recipe
	   (:package "posframe" :fetcher github :repo
		     "tumashu/posframe" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id posframe :type git :protocol
		     https :inherit t :depth treeless :ref
		     "bdabcec96f127b2daa2f8bf988a71ec146e301d5"))
 (projectile :source "elpaca-menu-lock-file" :recipe
	     (:package "projectile" :fetcher github :repo
		       "bbatsov/projectile" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :id projectile :type git
		       :protocol https :inherit t :depth treeless :ref
		       "f902073c1d10bb125875470c9dcbdcd7c9a1eefb"))
 (pulsar :source "elpaca-menu-lock-file" :recipe
	 (:package "pulsar" :repo
		   ("https://github.com/protesilaos/pulsar" . "pulsar")
		   :tar "1.4.1" :host gnu :files
		   ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
		   :source "GNU ELPA" :id pulsar :type git :protocol
		   https :inherit t :depth treeless :ref
		   "b82c595ff67b127ea1bcb2a17aa423d72cbbe8c4"))
 (puni :source "elpaca-menu-lock-file" :recipe
       (:package "puni" :repo "AmaiKinono/puni" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el"
			    "*-test.el" "*-tests.el" "LICENSE"
			    "README*" "*-pkg.el"))
		 :source "MELPA" :id puni :type git :protocol https
		 :inherit t :depth treeless :ref
		 "04819e0bad5ed62af9e412ef9715b33bb1edc3ba"))
 (queue :source "elpaca-menu-lock-file" :recipe
	(:package "queue" :repo
		  ("https://github.com/emacsmirror/gnu_elpa" . "queue")
		  :tar "0.2" :host gnu :branch "externals/queue"
		  :files ("*" (:exclude ".git")) :source "GNU ELPA"
		  :id queue :type git :protocol https :inherit t
		  :depth treeless :ref
		  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (rotate :source "elpaca-menu-lock-file" :recipe
	 (:package "rotate" :fetcher github :repo
		   "daichirata/emacs-rotate" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :id rotate :type git :protocol
		   https :inherit t :depth treeless :ref
		   "d61c7643b8dbe3fdd59f8ee40212f27bd14cfb61"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
	       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
	       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
	       "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el"
			 "*-test.el" "*-tests.el" "LICENSE" "README*"
			 "*-pkg.el"))
	      :source "MELPA" :id s :type git :protocol https :inherit
	      t :depth treeless :ref
	      "d7c04b84d03481a1ed62ee13dbe595224ccbe57c"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
	      (:package "shell-maker" :fetcher github :repo
			"xenodium/shell-maker" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :id shell-maker :type git
			:protocol https :inherit t :depth treeless
			:ref
			"e089f07fb4f5a8ce226932e192a2c11682a51102"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
	      (:package "shrink-path" :fetcher gitlab :repo
			"bennya/shrink-path.el" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :id shrink-path :type git
			:protocol https :inherit t :depth treeless
			:ref
			"c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (spell-fu :source "elpaca-menu-lock-file" :recipe
	   (:package "spell-fu" :fetcher codeberg :repo
		     "ideasman42/emacs-spell-fu" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :id spell-fu :type git :protocol
		     https :inherit t :depth treeless :ref
		     "103c4aaf0d2ec452359e087bed50504e2bdf4661"))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
		    (:package "string-inflection" :fetcher github
			      :repo "akicho8/string-inflection" :files
			      ("*.el" "*.el.in" "dir" "*.info"
			       "*.texi" "*.texinfo" "doc/dir"
			       "doc/*.info" "doc/*.texi"
			       "doc/*.texinfo" "lisp/*.el" "docs/dir"
			       "docs/*.info" "docs/*.texi"
			       "docs/*.texinfo"
			       (:exclude ".dir-locals.el" "test.el"
					 "tests.el" "*-test.el"
					 "*-tests.el" "LICENSE"
					 "README*" "*-pkg.el"))
			      :source "MELPA" :id string-inflection
			      :type git :protocol https :inherit t
			      :depth treeless :ref
			      "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"))
 (tablist :source "elpaca-menu-lock-file" :recipe
	  (:package "tablist" :fetcher github :repo
		    "emacsorphanage/tablist" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :id tablist :type git :protocol
		    https :inherit t :depth treeless :ref
		    "01f065e387ffe6b7a41f180f257cd12551c7a9c2"))
 (tempel :source "elpaca-menu-lock-file" :recipe
	 (:package "tempel" :repo "minad/tempel" :fetcher github
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :id tempel :type git :protocol
		   https :inherit t :depth treeless :ref
		   "9299c5d0f35dbb02de2565404a082224c4d11654"))
 (transient :source "elpaca-menu-lock-file" :recipe
	    (:package "transient" :fetcher github :repo
		      "magit/transient" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :id transient :type git
		      :protocol https :inherit t :depth treeless :ref
		      "a3e26414f2516f6443182e13e1d98882e1b209d6"))
 (vertico :source "elpaca-menu-lock-file" :recipe
	  (:package "vertico" :repo "minad/vertico" :files
		    (:defaults "extensions/vertico-*.el") :fetcher
		    github :source "MELPA" :id vertico :type git
		    :protocol https :inherit t :depth treeless :ref
		    "a9998a777f1d92348f84d091bb15b87df933a7a2"))
 (vui :source "elpaca-menu-lock-file" :recipe
      (:package "vui" :fetcher github :repo "d12frosted/vui.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		 "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el"
			   "*-test.el" "*-tests.el" "LICENSE"
			   "README*" "*-pkg.el"))
		:source "MELPA" :id vui :type git :protocol https
		:inherit t :depth treeless :ref
		"d8e76e7ccc784ab0441caab04fcf36aba73f5bdb"))
 (vulpea :source "elpaca-menu-lock-file" :recipe
	 (:package "vulpea" :fetcher github :repo "d12frosted/vulpea"
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :id vulpea :type git :protocol
		   https :inherit t :depth treeless :ref
		   "445d4da8d7ea469fec5fe50ac1f203d9338a7251"))
 (vulpea-journal :source "elpaca-menu-lock-file" :recipe
		 (:package "vulpea-journal" :fetcher github :repo
			   "d12frosted/vulpea-journal" :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			    "*.texinfo" "doc/dir" "doc/*.info"
			    "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			    "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el"
				      "tests.el" "*-test.el"
				      "*-tests.el" "LICENSE" "README*"
				      "*-pkg.el"))
			   :source "MELPA" :id vulpea-journal :type
			   git :protocol https :inherit t :depth
			   treeless :ref
			   "f21acb42c73395defcedae281ceb1fbf505f20fb"))
 (vulpea-ui :source "elpaca-menu-lock-file" :recipe
	    (:package "vulpea-ui" :fetcher github :repo
		      "d12frosted/vulpea-ui" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :id vulpea-ui :type git
		      :protocol https :inherit t :depth treeless :ref
		      "d0cc70fcb4f1b06e05897288c33783c4cf4a571d"))
 (vundo :source "elpaca-menu-lock-file" :recipe
	(:package "vundo" :repo "casouri/vundo" :tar "2.4.0" :host
		  github :files ("*" (:exclude ".git" "test")) :source
		  "GNU ELPA" :id vundo :type git :protocol https
		  :inherit t :depth treeless :ref
		  "04caf7ec54e4a7861d3591a1e090adc1fb76b5d1"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
	      (:package "with-editor" :fetcher github :repo
			"magit/with-editor" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :id with-editor :type git
			:protocol https :inherit t :depth treeless
			:ref
			"53115f978576e043e050fd04d0cb7517296727e9"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
	    (:package "ws-butler" :fetcher git :url
		      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git"
		      :branch "elpa/ws-butler" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :id ws-butler :type git
		      :protocol https :inherit t :depth treeless :ref
		      "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a")))
