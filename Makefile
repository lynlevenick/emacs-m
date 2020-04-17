.POSIX:

.PHONY: all clean compile test
.SUFFIXES: .el .elc

EMACS = emacs
LDFLAGS = -L .

PACKAGE = m
VERSION = 1.0.0

# Files

DOC =
EL = m.el
TEST = m-test.el

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

m-test.elc: m.elc

# Suffix rules

.el.elc:
	$(EMACS) -Q -batch -L . $(LDFLAGS) -f batch-byte-compile $<
