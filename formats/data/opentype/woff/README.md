WOFF Test Fonts
===============

The fonts in this subdirectory are from the [WC3 WOFF WG test suite][woff].
Licensed under the [W3C Software and Document Notice and License][licence].

### Font Info

* `valid-001.ttf`: CFF
* `valid-005.ttf`: TTF simple glyph

### Generating The Fonts

The fonts were generated via a docker container as follows.
The container was used because the scripts require Python 2:

    docker run --rm -it -v $(pwd):/woff debian:8-slim

Then in the container:

    cd /woff
    apt update
    apt install fonttools python-numpy
    adduser wmoore
    cd generators
    python FormatTestCaseGenerator.py

Then outside the container the fonts were converted back to TTFs from the WOFF files:

    for f in valid-00*.woff; do woff2sfnt-zopfli "$f" > "${f%.woff}.ttf"; done

where `woff2sfnt-zopfli` is <https://github.com/bramstein/sfnt2woff-zopfli>.

### License

> By obtaining and/or copying this work, you (the licensee) agree that you have read, understood, and will comply with the following terms and conditions.
>
> Permission to copy, modify, and distribute this work, with or without modification, for any purpose and without fee or royalty is hereby granted, provided that you include the following on ALL copies of the work or portions thereof, including modifications:
>
> * The full text of this NOTICE in a location viewable to users of the redistributed or derivative work.
> * Any pre-existing intellectual property disclaimers, notices, or terms and conditions. If none exist, the W3C Software and Document Short Notice should be included.
> * Notice of any changes or modifications, through a copyright statement on the new code or document such as "This software or document includes material copied from or derived from [title and URI of the W3C document]. Copyright © [YEAR] W3C® (MIT, ERCIM, Keio, Beihang)."
>
> ### Disclaimers
>
> THIS WORK IS PROVIDED "AS IS," AND COPYRIGHT HOLDERS MAKE NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE OF THE SOFTWARE OR DOCUMENT WILL NOT INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR OTHER RIGHTS.
>
> COPYRIGHT HOLDERS WILL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF ANY USE OF THE SOFTWARE OR DOCUMENT.
>
> The name and trademarks of copyright holders may NOT be used in advertising or publicity pertaining to the work without specific, written prior permission. Title to copyright in this work will at all times remain with copyright holders.

[woff]: https://github.com/w3c/woff/tree/c8402e6a5a892d45ba8193c22aed06be92833be2
[licence]: http://www.w3.org/Consortium/Legal/2015/copyright-software-and-document

