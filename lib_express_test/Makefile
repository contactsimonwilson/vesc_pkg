VESC_TOOL ?= vesc_tool

# One binary per chip - native libs only run on the chip they were built for.
ESP_TARGETS = esp32c3 esp32c6 esp32s3 esp32p4

all: express_test.vescpkg

express_test.vescpkg: libs
	$(VESC_TOOL) --buildPkg "express_test.vescpkg:express_test.lisp::0:README.md:Express Native Lib Test"

# Objects are not target-suffixed, so they are removed between targets.
libs:
	for t in $(ESP_TARGETS); do \
		rm -f express_test/*.o express_test/*.d; \
		$(MAKE) -C express_test ESP_TARGET=$$t || exit 1; \
	done
	rm -f express_test/*.o express_test/*.d

clean:
	rm -f express_test.vescpkg
	for t in $(ESP_TARGETS); do \
		$(MAKE) -C express_test ESP_TARGET=$$t clean; \
	done

.PHONY: all clean libs
