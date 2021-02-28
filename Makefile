CHICKEN = csc

default: te

te: main.scm
	$(CHICKEN) main.scm -o te

.PHONY: clean
clean:
	rm te
