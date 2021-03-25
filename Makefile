.POSIX:

.PHONY: all clean compile test
.SUFFIXES: .el .elc

EMACS = emacs
LDFLAGS = -L .

PACKAGE = memo
VERSION = 1.1.0

# Files

DOC =
EL = memo.el
TEST = memo-test.el

# Targets

all: compile

clean:
	rm -f $(EL:.el=.elc) $(TEST:.el=.elc) $(PACKAGE)-$(VERSION).tar

compile: $(EL:.el=.elc)

package: $(PACKAGE)-$(VERSION).tar

test: $(TEST:.el=.elc)
	$(EMACS) -Q -batch $(LDFLAGS) $(addprefix -l ,$(TEST:.el=.elc)) -f ert-run-tests-batch

# Outputs

$(PACKAGE)-$(VERSION).tar: $(EL) $(DOC)
	tar -cf $@ $^

# Dependencies

memo-test.elc: memo.elc

# Suffix rules

.el.elc:
	$(EMACS) -Q -batch -L . $(LDFLAGS) -f batch-byte-compile $<
