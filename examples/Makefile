TBC = ../dist/build/tiny-basic-compiler/tiny-basic-compiler
EXECUTABLES = error factorial fibonacci guess increase printExpression singleExpression

.PHONY: clean

all: $(EXECUTABLES)

%.s: %.bas
	$(TBC) $< $@

%.o: %.s
	as -c $< -o $@

%: %.o
	gcc $< -o $@ 

clean:
	rm -f *.s *.o $(EXECUTABLES)
