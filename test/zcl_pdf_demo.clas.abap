CLASS zcl_pdf_demo DEFINITION PUBLIC.
  PUBLIC SECTION.
    "! Run the demo and return the generated PDF as binary
    CLASS-METHODS run
      RETURNING VALUE(rv_pdf) TYPE xstring.

    "! Run demo and write PDF to console as base64 (for browser viewing)
    CLASS-METHODS run_base64
      RETURNING VALUE(rv_base64) TYPE string.
ENDCLASS.

CLASS zcl_pdf_demo IMPLEMENTATION.

  METHOD run.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).

    " ========== Page 1: Title Page ==========
    lo_pdf->add_page( ).

    " Title
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 36 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 100 iv_text = 'open-abap-pdf' ).

    " Subtitle
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 18 ).
    lo_pdf->set_text_color( iv_r = 100 iv_g = 100 iv_b = 100 ).
    lo_pdf->text( iv_x = 50 iv_y = 140 iv_text = 'A Pure ABAP PDF Generation Library' ).

    " Description
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 12 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 200 iv_text = 'This demo showcases the PDF generation capabilities:' ).
    lo_pdf->text( iv_x = 70 iv_y = 220 iv_text = '- Multiple pages support' ).
    lo_pdf->text( iv_x = 70 iv_y = 240 iv_text = '- Text with different fonts and colors' ).
    lo_pdf->text( iv_x = 70 iv_y = 260 iv_text = '- Lines, rectangles, and circles' ).
    lo_pdf->text( iv_x = 70 iv_y = 280 iv_text = '- Fluent API for easy chaining' ).

    " Decorative elements
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 110 iv_x2 = 400 iv_y2 = 110 ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf | Page 1' ).

    " ========== Page 2: Shapes Demo ==========
    lo_pdf->add_page( ).

    " Header
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Shapes Demo' ).

    " Rectangles section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 100 iv_text = 'Rectangles:' ).

    " Outlined rectangle
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->rect( iv_x = 50 iv_y = 120 iv_width = 80 iv_height = 50 iv_style = 'D' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 10 ).
    lo_pdf->text( iv_x = 60 iv_y = 180 iv_text = 'Outlined' ).

    " Filled rectangle
    lo_pdf->set_fill_color( iv_r = 100 iv_g = 149 iv_b = 237 ).
    lo_pdf->rect( iv_x = 160 iv_y = 120 iv_width = 80 iv_height = 50 iv_style = 'F' ).
    lo_pdf->text( iv_x = 175 iv_y = 180 iv_text = 'Filled' ).

    " Both outlined and filled
    lo_pdf->set_draw_color( iv_r = 139 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_fill_color( iv_r = 255 iv_g = 182 iv_b = 193 ).
    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->rect( iv_x = 270 iv_y = 120 iv_width = 80 iv_height = 50 iv_style = 'DF' ).
    lo_pdf->text( iv_x = 290 iv_y = 180 iv_text = 'Both' ).

    " Circles section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->text( iv_x = 50 iv_y = 230 iv_text = 'Circles:' ).

    " Outlined circle
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 100 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->circle( iv_x = 100 iv_y = 290 iv_radius = 30 iv_style = 'D' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 10 ).
    lo_pdf->text( iv_x = 75 iv_y = 335 iv_text = 'Outlined' ).

    " Filled circle
    lo_pdf->set_fill_color( iv_r = 255 iv_g = 215 iv_b = 0 ).
    lo_pdf->circle( iv_x = 210 iv_y = 290 iv_radius = 30 iv_style = 'F' ).
    lo_pdf->text( iv_x = 193 iv_y = 335 iv_text = 'Filled' ).

    " Both
    lo_pdf->set_draw_color( iv_r = 75 iv_g = 0 iv_b = 130 ).
    lo_pdf->set_fill_color( iv_r = 230 iv_g = 230 iv_b = 250 ).
    lo_pdf->set_line_width( iv_width = 3 ).
    lo_pdf->circle( iv_x = 320 iv_y = 290 iv_radius = 30 iv_style = 'DF' ).
    lo_pdf->text( iv_x = 305 iv_y = 335 iv_text = 'Both' ).

    " Lines section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 400 iv_text = 'Lines with different widths:' ).

    lo_pdf->set_draw_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 420 iv_x2 = 200 iv_y2 = 420 ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 10 ).
    lo_pdf->text( iv_x = 210 iv_y = 423 iv_text = '1pt' ).

    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 440 iv_x2 = 200 iv_y2 = 440 ).
    lo_pdf->text( iv_x = 210 iv_y = 443 iv_text = '2pt' ).

    lo_pdf->set_line_width( iv_width = 4 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 460 iv_x2 = 200 iv_y2 = 460 ).
    lo_pdf->text( iv_x = 210 iv_y = 463 iv_text = '4pt' ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf | Page 2' ).

    " ========== Page 3: Fonts Demo ==========
    lo_pdf->add_page( ).

    " Header
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Fonts Demo' ).

    " Helvetica
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 12 ).
    lo_pdf->text( iv_x = 50 iv_y = 100 iv_text = 'Helvetica 12pt - The quick brown fox jumps over the lazy dog' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 125 iv_text = 'Helvetica 16pt - The quick brown fox' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 20 ).
    lo_pdf->text( iv_x = 50 iv_y = 155 iv_text = 'Helvetica 20pt - Hello World' ).

    " Times-Roman
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 12 ).
    lo_pdf->text( iv_x = 50 iv_y = 210 iv_text = 'Times-Roman 12pt - The quick brown fox jumps over the lazy dog' ).
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 235 iv_text = 'Times-Roman 16pt - The quick brown fox' ).
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 20 ).
    lo_pdf->text( iv_x = 50 iv_y = 265 iv_text = 'Times-Roman 20pt - Hello World' ).

    " Courier
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 12 ).
    lo_pdf->text( iv_x = 50 iv_y = 320 iv_text = 'Courier 12pt - The quick brown fox jumps' ).
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 345 iv_text = 'Courier 16pt - Fixed width font' ).
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 20 ).
    lo_pdf->text( iv_x = 50 iv_y = 375 iv_text = 'Courier 20pt - Code Style' ).

    " Colors section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 440 iv_text = 'Text Colors:' ).

    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 255 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 470 iv_text = 'Red Text' ).

    lo_pdf->set_text_color( iv_r = 0 iv_g = 128 iv_b = 0 ).
    lo_pdf->text( iv_x = 150 iv_y = 470 iv_text = 'Green Text' ).

    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 255 ).
    lo_pdf->text( iv_x = 270 iv_y = 470 iv_text = 'Blue Text' ).

    lo_pdf->set_text_color( iv_r = 255 iv_g = 165 iv_b = 0 ).
    lo_pdf->text( iv_x = 380 iv_y = 470 iv_text = 'Orange Text' ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf | Page 3' ).

    rv_pdf = lo_pdf->render_binary( ).
  ENDMETHOD.

  METHOD run_base64.
    DATA(lv_pdf) = run( ).
    rv_base64 = cl_http_utility=>encode_x_base64( lv_pdf ).
  ENDMETHOD.

ENDCLASS.
