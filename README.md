## olinkcheck
A tool to find broken URLs in text-based files.

### Supported Formats
`olinkcheck` currently supports:
- Markdown
- Plaintext
- S Expressions
- YAML
- HTML
- Any combination of these (see [Library](#library))

### Usage
#### CLI
```bash
$ olinkcheck [--annotate] [--exclude-list=EXCLUDE-LIST] [--verbose]
FORMAT FILE
```

For example,  
```bash
$ olinkcheck md file.md
```  
This will list all the web links found in the file, ping them and report their
HTTP status. FILE can be a directory, in which case all the files in that
directory are searched recursively for files matching the corresponding
extension.

The following options are valid for FORMAT:
- `md` for Markdown
- `txt` for Plaintext
- `md_with_yaml` for Markdown containing a YAML header
- `html_with_yaml` for Markdown containing a YAML header and HTML body

The `--verbose` flag reports links that have status `200 OK` as well.  
The `--annotate` flag indicates the broken links in the file itself.

#### Library
Example usage in `utop`:
```ocaml
# #require "olinkcheck";;
# Olinkcheck.Markdown.extract_links "##title [url](http://www.google.com)";;
- : string list = ["http://www.google.com"]
# Olinkcheck.Link.status "http://www.google.com";;
- : int * string = (200, "OK")
```
To handle files with multiple formats, you can use define a `ParserPair` with
`separate` and `join` functions to separate the text into two different formats,
and use `MakePairParser` to get a `Parser` to handle it. A pair parser handling
`YAML` headers in `Markdown`, with some fields in the `YAML` also containing
`Markdown` has been implemented (see
[`YamlMdPair` and `YamlMdParser`](lib/olinkcheck.ml#L184)).
