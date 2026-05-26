ERLC = erlc
ERLC_FLAGS = -o ebin -I include

SRCDIR = src
EBINDIR = ebin

SOURCES = $(wildcard $(SRCDIR)/*.erl)
BEAMS = $(patsubst $(SRCDIR)/%.erl,$(EBINDIR)/%.beam,$(SOURCES))

.PHONY: all clean test

all: $(EBINDIR) $(BEAMS)

$(EBINDIR):
	mkdir -p $(EBINDIR)

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) $<

test: ERLC_FLAGS += -DTEST
test: clean all
	erl -pa ebin -noshell -eval 'eunit:test(gtime, [verbose])' -s init stop

clean:
	rm -f $(EBINDIR)/*.beam
