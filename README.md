# bib2sx

`bib2sx` is a command-line tool (written in Racket) for parsing 
BibTeX files (by default) into S-Expressions.

It captures the (potentially recursive) structure of quoting in BibTeX,
which is is important for capitalization.

It also supports outputing to JSON, XML and (back to canonicalized) BibTeX.
 
A [blog post explaining
bib2sx](http://matt.might.net/articles/parsing-bibtex/) covers the tool in
much more detail.

By default, it will not parse names, but it will parse
names into `first`, `von`, `last` and `jr` with `--parse-names`.


## Dependencies

You will need an installation of the [Racket programming language](http://racket-lang.org/download/). 


## Building

To build `bib2sx`, run `make`. 


## Installation

To install to `~/bin/`, run `make install`.



## Usage

```
$ bib2sx [ --inline ] [ --flatten ] [ --parse-names ]
         [ --in sx|bib ] [ --out sx|bib|json|xml ]
         [ --json | --bib | --xml ]
         [ <input-file> ]
```

where:

 + `--inline` will expand variables into definitions from @string
 + `--flatten` will convert values into properly-quoted BibTeX strings
 + `--parse-names` will parse names according to BibTeX's rules
 + `--in <format>` will set the input format: `sx` or `bib` allowed; 
   `bib` is default
 + `--out <format>` will set the output format; `sx`, `bib`, `json`, or `xml`
    allowed; `sx` is default
 + `--json` will output JSON; same as `--out json`
 + `--bib` will output canonicalized BibTeX; same as `--out bib`
 + `--xml` will output XML; same as `--out xml`


## License

bib2sx: A bibtex parser and transformer

Copyright (&copy;) 2015 Matthew Might

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
