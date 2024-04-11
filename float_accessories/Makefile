VESC_TOOL ?= vesc_tool

all: clean float-accessories.vescpkg

float-accessories.vescpkg: 
	$(VESC_TOOL) --buildPkg "float-accessories.vescpkg:float-accessories.lisp:ui.qml:0:README.md:Float Accessories"

clean:
	rm -f float-accessories.vescpkg

.PHONY: all clean float-accessories
