erb_files := $(shell find src/ -type f -name '*.erb')
hs_files := $(erb_files:%.erb=%.hs)

erb: $(hs_files)

%.hs: %.erb
	erb $< > $@

clean:
	rm $(hs_files)
