VESC_TOOL ?= vesc_tool

all: clean float_accessories.vescpkg

float_accessories.vescpkg: 
	$(VESC_TOOL) --buildPkg "float_accessories.vescpkg:float_accessories.lisp:ui.qml:0:README.md:Float Accessories"

clean:
	rm -f float_accessories.vescpkg

.PHONY: all clean float_accessories