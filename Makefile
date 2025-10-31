-include .config.mk

PKG = hl-todo

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

all: lisp

help:
	$(info make all        -- Build lisp)
	$(info make lisp       -- Build lisp)
	$(info make redo       -- Build lisp from scratch)
	$(info make clean      -- Remove built files)
	@printf "\n"

redo: clean lisp

lisp: $(ELCS) autoloads check-declare

autoloads: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	--eval "(check-declare-directory default-directory)"

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
