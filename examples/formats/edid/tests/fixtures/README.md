# EDID Fixtures

These were obtained on macOS using:

```bash
ioreg -lr -w 0 -k IODisplayEDID | grep IODisplayEDID | sed 's/.*<\(.*\)>/\1/'
```

The hex-dumps can then be output to fixtures using:

```bash
echo 'HEXDUMP' | xxd -r -p > examples/formats/edid/tests/fixtures/FIXTURE_NAME.bin
````
