erb_files := $(shell find src/ -type f -name '*.erb')
hs_files := $(erb_files:%.erb=%.hs)

erb: $(hs_files)

%.hs: %.erb
	erb $< | rb/clean_imports.rb > $@

clean:
	rm -f $(hs_files)
