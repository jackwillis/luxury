# Scheme reference

- `r7rs.pdf`: R7RS-small report, revised February 13, 2021.
- `r7rs.txt`: searchable text extracted with Poppler (`pdftotext -layout`).

Source: https://standards.scheme.org/official/r7rs.pdf

The report is third-party reference material; its own notices apply.

To regenerate the text:

```bash
pdftotext -layout reference/r7rs.pdf reference/r7rs.txt
```

Useful sections:

- 2.1: identifiers
- 2.2: whitespace and comments
- 6.2: numbers
- 6.3: booleans
- 7.1.1: lexical grammar

The text preserves the PDF's two-column layout. Consult the PDF when grammar
or reading order is unclear.
