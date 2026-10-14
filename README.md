# open-abap-pdf

Standalone PDF generation in ABAP

Pull requests welcome

Plan is to have the same code run on:
* Classic ECC
* Classic Steampunk
* Embedded Steampunk
* open-abap/transpiled

## Features

- Create multi-page PDF documents
- Add text with customizable fonts (Helvetica, Times-Roman, Courier)
- Set text, draw, and fill colors (RGB)
- Draw shapes: lines, rectangles, circles
- Fluent API for method chaining
- Support for A4, Letter, and custom page sizes
- Unit conversion helpers (mm to points, inches to points)
- Digital signatures, X.509 certificate based, pure ABAP RSA and CMS

## Usage

### Basic Example

```abap
DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
lo_pdf->add_page( ).
lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Hello World!' ).
DATA(lv_pdf_string) = lo_pdf->render( ).
```

### Fluent API

```abap
DATA(lv_pdf) = zcl_open_abap_pdf=>create(
  )->add_page(
  )->set_font( iv_name = 'Helvetica' iv_size = 16
  )->set_text_color( iv_r = 0 iv_g = 0 iv_b = 128
  )->text( iv_x = 100 iv_y = 100 iv_text = 'Blue Text'
  )->render( ).
```

### Drawing Shapes

```abap
DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
lo_pdf->add_page( ).

" Draw a line
lo_pdf->line( iv_x1 = 50 iv_y1 = 100 iv_x2 = 200 iv_y2 = 100 ).

" Draw a rectangle (outline)
lo_pdf->rect( iv_x = 50 iv_y = 150 iv_width = 100 iv_height = 50 iv_style = 'D' ).

" Draw a filled rectangle
lo_pdf->set_fill_color( iv_r = 200 iv_g = 200 iv_b = 255 ).
lo_pdf->rect( iv_x = 50 iv_y = 220 iv_width = 100 iv_height = 50 iv_style = 'F' ).

" Draw a circle
lo_pdf->circle( iv_x = 300 iv_y = 200 iv_radius = 40 iv_style = 'DF' ).

DATA(lv_pdf) = lo_pdf->render( ).
```

### Multiple Pages

```abap
DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).

lo_pdf->add_page( ).
lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Page 1' ).

lo_pdf->add_page( ).
lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Page 2' ).

DATA(lv_pdf) = lo_pdf->render( ).
```

### Custom Page Size

```abap
" Letter size
lo_pdf->add_page(
  iv_width  = zcl_open_abap_pdf=>c_letter_width
  iv_height = zcl_open_abap_pdf=>c_letter_height ).

" Custom size in mm (converted to points)
lo_pdf->add_page(
  iv_width  = zcl_open_abap_pdf=>mm_to_pt( 100 )
  iv_height = zcl_open_abap_pdf=>mm_to_pt( 150 ) ).
```

### Digital Signatures

Cryptographic, certificate based signatures, implemented in plain ABAP: RSA PKCS#1 v1.5 with SHA-256, embedded as detached CMS / PKCS#7 (`/SubFilter /adbe.pkcs7.detached`). The signature covers the whole document and is placed in an invisible signature field on the first page.

```abap
DATA(lo_signer) = zcl_open_abap_pdf_signer=>create_from_pem(
  iv_private_key  = lv_key_pem       " -----BEGIN PRIVATE KEY----- or RSA PRIVATE KEY, unencrypted
  iv_certificates = lv_cert_pem ).   " signing certificate first, then optional chain

DATA(lv_pdf) = zcl_open_abap_pdf=>create(
  )->add_page(
  )->text( iv_x = 50 iv_y = 50 iv_text = 'Signed document'
  )->render_signed(
    io_signer    = lo_signer
    is_signature = VALUE #(
      name     = 'John Doe'
      reason   = 'Approval'
      location = 'Copenhagen' ) ).
```

`zcl_open_abap_pdf_signer=>create( )` accepts DER encoded key (PKCS#1 or PKCS#8) and certificates instead of PEM. The certificate must match the private key, and the signature is verified with the public key before it is returned.

Signing with other means, eg. SSF, an HSM or a remote signing service, is possible by implementing `zif_open_abap_pdf_signer`, which receives the bytes to sign and returns the DER encoded CMS SignedData.

Notes:
- Only RSA keys are supported, encrypted private keys are not supported
- No timestamp authority (TSA) or long term validation information is added
- The CMS is limited to `contents_size` bytes, default 8192, increase for long certificate chains
- Viewers show the signature as valid only if the certificate is trusted, eg. issued by a trusted CA

## API Reference

### Class Methods

| Method | Description |
|--------|-------------|
| `create( )` | Creates a new PDF document instance |
| `mm_to_pt( iv_mm )` | Converts millimeters to points |
| `inch_to_pt( iv_inch )` | Converts inches to points |

### Instance Methods

| Method | Description |
|--------|-------------|
| `add_page( iv_width, iv_height )` | Adds a new page (default A4) |
| `set_font( iv_name, iv_size )` | Sets the current font |
| `set_text_color( iv_r, iv_g, iv_b )` | Sets text color (RGB 0-255) |
| `set_draw_color( iv_r, iv_g, iv_b )` | Sets line/stroke color |
| `set_fill_color( iv_r, iv_g, iv_b )` | Sets fill color |
| `set_line_width( iv_width )` | Sets line width |
| `text( iv_x, iv_y, iv_text )` | Draws text at position |
| `line( iv_x1, iv_y1, iv_x2, iv_y2 )` | Draws a line |
| `rect( iv_x, iv_y, iv_width, iv_height, iv_style )` | Draws a rectangle |
| `circle( iv_x, iv_y, iv_radius, iv_style )` | Draws a circle |
| `render( )` | Returns PDF as string |
| `render_binary( )` | Returns PDF as xstring |
| `render_signed( io_signer, is_signature )` | Returns digitally signed PDF as xstring |
| `get_page_count( )` | Returns number of pages |
| `get_page_width( )` | Returns current page width |
| `get_page_height( )` | Returns current page height |

### Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `c_pt_per_mm` | 2.83465 | Points per millimeter |
| `c_a4_width` | 595.28 | A4 width in points |
| `c_a4_height` | 841.89 | A4 height in points |
| `c_letter_width` | 612 | Letter width in points |
| `c_letter_height` | 792 | Letter height in points |

### Shape Styles

| Style | Description |
|-------|-------------|
| `D` | Draw outline only (default) |
| `F` | Fill only |
| `DF` or `FD` | Draw outline and fill |

## Supported Fonts

- Helvetica (default)
- Times-Roman
- Courier
