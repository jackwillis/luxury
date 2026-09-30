# Scheme reference

- `r7rs.pdf`: R7RS-small report, revised February 13, 2021.
- `r7rs.txt`: searchable text extracted in reading order with Poppler (`pdftotext`).

Source: https://standards.scheme.org/official/r7rs.pdf

The report is third-party reference material; its own notices apply.

To regenerate the text:

```bash
pdftotext reference/r7rs.pdf reference/r7rs.txt
```

Useful sections:

- 2.1: identifiers
- 2.2: whitespace and comments
- 6.2: numbers
- 6.3: booleans
- 7.1.1: lexical grammar

The text reads columns sequentially rather than placing them side by side.
Page breaks are preserved. Extraction can distort mathematical symbols and
tables; consult the PDF when notation or reading order is unclear.
