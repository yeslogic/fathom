# Introduction

Fathom is a domain-specific language that can be used to describe existing binary formats in a declarative way that is both human and machine readable.

Binary formats exist in many forms in computing. Examples of these include:

- Image formats: JPEG, GIF, TIFF, etc.
- Archive formats: ZIP, RAR, GZIP, etc.
- Fonts: OTF, TTF
- &hellip;and many more!

Implementing a parser for a binary format such as those listed above is difficult and error-prone,
especially when it is derived from a natural language specification that leaves room for ambiguity.
