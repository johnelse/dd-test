.PHONY: clean

dist/build/dd-test/dd-test:
	obuild configure
	obuild build

clean:
	rm -rf dist
