## olinkcheck
A tool to find broken URLs in text-based files.

### Supported Formats
olinkcheck currently supports:
- Markdown
- Plaintext

### Usage
#### CLI
```bash $ olinkcheck [FORMAT] [FILENAME]```  
For example,  
```bash $ olinkcheck md file.md```  
This will list all the web links found in the file, ping them and report their HTTP status.
#### Library
Example usage in `utop`:
```ocaml
# #require "olinkcheck";;
# Olinkcheck.Markdown.extract_links "##title [url](http://www.google.com)";;
- : string list = ["http://www.google.com"]
# Olinkcheck.Link.status "http://www.google.com";;
- : int * string = (200, "OK")
```
