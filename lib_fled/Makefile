VESC_TOOL ?= vesc_tool

# One binary per chip - native libs only run on the chip they were built for.
ESP_TARGETS = esp32c3 esp32c6 esp32s3 esp32p4

all: fled.vescpkg

fled.vescpkg: libs
	$(VESC_TOOL) --buildPkg "fled.vescpkg:fled.lisp:ui.qml:0:README.md:FLED Strip Control"

# Objects are not target-suffixed, so they are removed between targets.
libs:
	for t in $(ESP_TARGETS); do \
		rm -f fled/*.o fled/*.d; \
		$(MAKE) -C fled ESP_TARGET=$$t || exit 1; \
	done
	rm -f fled/*.o fled/*.d

clean:
	rm -f fled.vescpkg
	for t in $(ESP_TARGETS); do \
		$(MAKE) -C fled ESP_TARGET=$$t clean; \
	done

.PHONY: all clean libs
