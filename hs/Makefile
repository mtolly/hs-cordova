erb_files := $(shell zsh -c "echo **/*.erb")
out_files := $(erb_files:%.erb=%)

all: $(out_files)

%: %.erb
	erb $< > $@
