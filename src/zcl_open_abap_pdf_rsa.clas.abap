"! RSA PKCS#1 v1.5 signatures in plain ABAP
"! Big integers are tables of 15 bit limbs, least significant first, so all intermediate
"! products fit into type i
CLASS zcl_open_abap_pdf_rsa DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES ty_limbs TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    "! Create from private key components, unsigned big-endian bytes
    CLASS-METHODS create
      IMPORTING iv_modulus         TYPE xstring
                iv_public_exponent TYPE xstring
                iv_prime1          TYPE xstring
                iv_prime2          TYPE xstring
                iv_exponent1       TYPE xstring
                iv_exponent2       TYPE xstring
                iv_coefficient     TYPE xstring
      RETURNING VALUE(ro_rsa)      TYPE REF TO zcl_open_abap_pdf_rsa
      RAISING   zcx_open_abap_pdf.

    "! RSASSA-PKCS1-v1_5 signature of a SHA-256 hash
    METHODS sign_sha256
      IMPORTING iv_hash             TYPE xstring
      RETURNING VALUE(rv_signature) TYPE xstring
      RAISING   zcx_open_abap_pdf.

    "! Length of the modulus in bytes
    METHODS get_length
      RETURNING VALUE(rv_length) TYPE i.

    "! base ^ exponent mod modulus, modulus must be odd
    CLASS-METHODS mod_pow
      IMPORTING iv_base          TYPE xstring
                iv_exponent      TYPE xstring
                iv_modulus       TYPE xstring
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_open_abap_pdf.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_mont,
        n     TYPE ty_limbs,
        s     TYPE i,
        n0inv TYPE i,
        rr    TYPE ty_limbs,
      END OF ty_mont,
      ty_limbs_tab TYPE STANDARD TABLE OF ty_limbs WITH DEFAULT KEY.

    CONSTANTS c_base TYPE i VALUE 32768.

    DATA mv_length TYPE i.
    DATA mv_public_exponent TYPE xstring.
    DATA mv_exponent1 TYPE xstring.
    DATA mv_exponent2 TYPE xstring.
    DATA mt_prime1 TYPE ty_limbs.
    DATA mt_prime2 TYPE ty_limbs.
    DATA mt_coefficient TYPE ty_limbs.
    DATA ms_mont_n TYPE ty_mont.
    DATA ms_mont_p TYPE ty_mont.
    DATA ms_mont_q TYPE ty_mont.

    CLASS-METHODS from_bytes
      IMPORTING iv_bytes        TYPE xstring
      RETURNING VALUE(rt_limbs) TYPE ty_limbs.

    CLASS-METHODS to_bytes
      IMPORTING it_limbs        TYPE ty_limbs
                iv_length       TYPE i
      RETURNING VALUE(rv_bytes) TYPE xstring
      RAISING   zcx_open_abap_pdf.

    CLASS-METHODS trim
      CHANGING ct_limbs TYPE ty_limbs.

    CLASS-METHODS pad
      IMPORTING it_limbs        TYPE ty_limbs
                iv_count        TYPE i
      RETURNING VALUE(rt_limbs) TYPE ty_limbs.

    CLASS-METHODS compare
      IMPORTING it_a             TYPE ty_limbs
                it_b             TYPE ty_limbs
      RETURNING VALUE(rv_result) TYPE i.

    CLASS-METHODS add
      IMPORTING it_a             TYPE ty_limbs
                it_b             TYPE ty_limbs
      RETURNING VALUE(rt_result) TYPE ty_limbs.

    "! a - b, requires a >= b
    CLASS-METHODS sub
      IMPORTING it_a             TYPE ty_limbs
                it_b             TYPE ty_limbs
      RETURNING VALUE(rt_result) TYPE ty_limbs.

    CLASS-METHODS mul
      IMPORTING it_a             TYPE ty_limbs
                it_b             TYPE ty_limbs
      RETURNING VALUE(rt_result) TYPE ty_limbs.

    CLASS-METHODS mod
      IMPORTING it_a             TYPE ty_limbs
                it_m             TYPE ty_limbs
      RETURNING VALUE(rt_result) TYPE ty_limbs.

    CLASS-METHODS create_mont
      IMPORTING it_n           TYPE ty_limbs
      RETURNING VALUE(rs_mont) TYPE ty_mont
      RAISING   zcx_open_abap_pdf.

    "! Montgomery product a * b * R^-1 mod n, inputs padded to s limbs and less than n
    CLASS-METHODS mont_mul
      IMPORTING it_a             TYPE ty_limbs
                it_b             TYPE ty_limbs
                is_mont          TYPE ty_mont
      RETURNING VALUE(rt_result) TYPE ty_limbs.

    CLASS-METHODS mont_pow
      IMPORTING it_base          TYPE ty_limbs
                iv_exponent      TYPE xstring
                is_mont          TYPE ty_mont
      RETURNING VALUE(rt_result) TYPE ty_limbs.
ENDCLASS.

CLASS zcl_open_abap_pdf_rsa IMPLEMENTATION.

  METHOD create.
    DATA lt_modulus TYPE ty_limbs.

    CREATE OBJECT ro_rsa.
    ro_rsa->mv_length = xstrlen( zcl_open_abap_pdf_asn1=>to_unsigned( iv_modulus ) ).
    IF ro_rsa->mv_length < 64.
      zcx_open_abap_pdf=>raise( 'RSA: modulus too short' ).
    ENDIF.

    ro_rsa->mv_public_exponent = iv_public_exponent.
    ro_rsa->mv_exponent1 = iv_exponent1.
    ro_rsa->mv_exponent2 = iv_exponent2.
    ro_rsa->mt_prime1 = from_bytes( iv_prime1 ).
    ro_rsa->mt_prime2 = from_bytes( iv_prime2 ).
    ro_rsa->mt_coefficient = from_bytes( iv_coefficient ).

    lt_modulus = from_bytes( iv_modulus ).
    IF compare( it_a = mul( it_a = ro_rsa->mt_prime1 it_b = ro_rsa->mt_prime2 ) it_b = lt_modulus ) <> 0.
      zcx_open_abap_pdf=>raise( 'RSA: inconsistent private key, n <> p * q' ).
    ENDIF.

    ro_rsa->ms_mont_n = create_mont( lt_modulus ).
    ro_rsa->ms_mont_p = create_mont( ro_rsa->mt_prime1 ).
    ro_rsa->ms_mont_q = create_mont( ro_rsa->mt_prime2 ).
  ENDMETHOD.

  METHOD get_length.
    rv_length = mv_length.
  ENDMETHOD.

  METHOD sign_sha256.
    DATA lv_encoded TYPE xstring.
    DATA lv_padding TYPE xstring.
    DATA lv_ff TYPE x LENGTH 1 VALUE 'FF'.
    DATA lv_zero TYPE x LENGTH 1 VALUE '00'.
    DATA lv_prefix TYPE xstring.
    DATA lt_message TYPE ty_limbs.
    DATA lt_m1 TYPE ty_limbs.
    DATA lt_m2 TYPE ty_limbs.
    DATA lt_m2p TYPE ty_limbs.
    DATA lt_diff TYPE ty_limbs.
    DATA lt_h TYPE ty_limbs.
    DATA lt_signature TYPE ty_limbs.
    DATA lt_check TYPE ty_limbs.

    IF xstrlen( iv_hash ) <> 32.
      zcx_open_abap_pdf=>raise( 'RSA: SHA-256 hash must be 32 bytes' ).
    ENDIF.

* EMSA-PKCS1-v1_5: 00 01 FF..FF 00 DigestInfo
    lv_prefix = '3031300D060960864801650304020105000420'.
    DO mv_length - 3 - xstrlen( lv_prefix ) - xstrlen( iv_hash ) TIMES.
      CONCATENATE lv_padding lv_ff INTO lv_padding IN BYTE MODE.
    ENDDO.
    lv_encoded = '0001'.
    CONCATENATE lv_encoded lv_padding lv_zero lv_prefix iv_hash INTO lv_encoded IN BYTE MODE.
    lt_message = from_bytes( lv_encoded ).

* CRT
    lt_m1 = mont_pow(
      it_base     = pad( it_limbs = mod( it_a = lt_message it_m = mt_prime1 ) iv_count = ms_mont_p-s )
      iv_exponent = mv_exponent1
      is_mont     = ms_mont_p ).
    lt_m2 = mont_pow(
      it_base     = pad( it_limbs = mod( it_a = lt_message it_m = mt_prime2 ) iv_count = ms_mont_q-s )
      iv_exponent = mv_exponent2
      is_mont     = ms_mont_q ).

    lt_m2p = mod( it_a = lt_m2 it_m = mt_prime1 ).
    IF compare( it_a = lt_m1 it_b = lt_m2p ) >= 0.
      lt_diff = sub( it_a = lt_m1 it_b = lt_m2p ).
    ELSE.
      lt_diff = sub( it_a = add( it_a = lt_m1 it_b = mt_prime1 ) it_b = lt_m2p ).
    ENDIF.
    lt_h = mod( it_a = mul( it_a = mt_coefficient it_b = lt_diff ) it_m = mt_prime1 ).
    lt_signature = add( it_a = lt_m2 it_b = mul( it_a = lt_h it_b = mt_prime2 ) ).

* verify with the public key, protects against faulty keys and calculations
    lt_check = mont_pow(
      it_base     = pad( it_limbs = lt_signature iv_count = ms_mont_n-s )
      iv_exponent = mv_public_exponent
      is_mont     = ms_mont_n ).
    IF compare( it_a = lt_check it_b = lt_message ) <> 0.
      zcx_open_abap_pdf=>raise( 'RSA: signature verification failed, check private key' ).
    ENDIF.

    rv_signature = to_bytes( it_limbs = lt_signature iv_length = mv_length ).
  ENDMETHOD.

  METHOD mod_pow.
    DATA ls_mont TYPE ty_mont.
    DATA lt_result TYPE ty_limbs.

    ls_mont = create_mont( from_bytes( iv_modulus ) ).
    lt_result = mont_pow(
      it_base     = pad( it_limbs = mod( it_a = from_bytes( iv_base ) it_m = ls_mont-n ) iv_count = ls_mont-s )
      iv_exponent = iv_exponent
      is_mont     = ls_mont ).
    rv_result = to_bytes(
      it_limbs  = lt_result
      iv_length = xstrlen( zcl_open_abap_pdf_asn1=>to_unsigned( iv_modulus ) ) ).
  ENDMETHOD.

  METHOD from_bytes.
    DATA lv_offset TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_int TYPE i.
    DATA lv_acc TYPE i.
    DATA lv_mult TYPE i VALUE 1.
    DATA lv_limb TYPE i.

    lv_offset = xstrlen( iv_bytes ) - 1.
    WHILE lv_offset >= 0.
      lv_byte = iv_bytes+lv_offset(1).
      lv_int = lv_byte.
      lv_acc = lv_acc + lv_int * lv_mult.
      lv_mult = lv_mult * 256.
      IF lv_mult >= c_base.
        lv_limb = lv_acc MOD c_base.
        APPEND lv_limb TO rt_limbs.
        lv_acc = lv_acc DIV c_base.
        lv_mult = lv_mult DIV c_base.
      ENDIF.
      lv_offset = lv_offset - 1.
    ENDWHILE.
    IF lv_acc > 0.
      APPEND lv_acc TO rt_limbs.
    ENDIF.

    trim( CHANGING ct_limbs = rt_limbs ).
  ENDMETHOD.

  METHOD to_bytes.
    DATA lv_limb TYPE i.
    DATA lv_acc TYPE i.
    DATA lv_mult TYPE i VALUE 1.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_zero TYPE x LENGTH 1 VALUE '00'.
    DATA lv_length TYPE i.

    LOOP AT it_limbs INTO lv_limb.
      lv_acc = lv_acc + lv_limb * lv_mult.
      lv_mult = lv_mult * c_base.
      WHILE lv_mult >= 256.
        lv_byte = lv_acc MOD 256.
        CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
        lv_acc = lv_acc DIV 256.
        lv_mult = lv_mult DIV 256.
      ENDWHILE.
    ENDLOOP.
    IF lv_acc > 0.
      lv_byte = lv_acc.
      CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
    ENDIF.

    rv_bytes = zcl_open_abap_pdf_asn1=>to_unsigned( rv_bytes ).
    lv_length = xstrlen( rv_bytes ).
    IF lv_length > iv_length.
      zcx_open_abap_pdf=>raise( 'RSA: value too large' ).
    ENDIF.
    DO iv_length - lv_length TIMES.
      CONCATENATE lv_zero rv_bytes INTO rv_bytes IN BYTE MODE.
    ENDDO.
  ENDMETHOD.

  METHOD trim.
    DATA lv_lines TYPE i.
    DATA lv_limb TYPE i.

    lv_lines = lines( ct_limbs ).
    WHILE lv_lines > 0.
      READ TABLE ct_limbs INDEX lv_lines INTO lv_limb.
      IF lv_limb <> 0.
        EXIT.
      ENDIF.
      DELETE ct_limbs INDEX lv_lines.
      lv_lines = lv_lines - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD pad.
    rt_limbs = it_limbs.
    WHILE lines( rt_limbs ) < iv_count.
      APPEND 0 TO rt_limbs.
    ENDWHILE.
  ENDMETHOD.

  METHOD compare.
    DATA lv_index TYPE i.
    DATA lv_a TYPE i.
    DATA lv_b TYPE i.

    lv_index = nmax( val1 = lines( it_a ) val2 = lines( it_b ) ).
    WHILE lv_index > 0.
      CLEAR lv_a.
      CLEAR lv_b.
      READ TABLE it_a INDEX lv_index INTO lv_a.
      READ TABLE it_b INDEX lv_index INTO lv_b.
      IF lv_a > lv_b.
        rv_result = 1.
        RETURN.
      ELSEIF lv_a < lv_b.
        rv_result = -1.
        RETURN.
      ENDIF.
      lv_index = lv_index - 1.
    ENDWHILE.
    rv_result = 0.
  ENDMETHOD.

  METHOD add.
    DATA lv_index TYPE i.
    DATA lv_max TYPE i.
    DATA lv_a TYPE i.
    DATA lv_b TYPE i.
    DATA lv_value TYPE i.
    DATA lv_carry TYPE i.

    lv_max = nmax( val1 = lines( it_a ) val2 = lines( it_b ) ).
    DO lv_max TIMES.
      lv_index = sy-index.
      CLEAR lv_a.
      CLEAR lv_b.
      READ TABLE it_a INDEX lv_index INTO lv_a.
      READ TABLE it_b INDEX lv_index INTO lv_b.
      lv_value = lv_a + lv_b + lv_carry.
      lv_carry = lv_value DIV c_base.
      lv_value = lv_value - lv_carry * c_base.
      APPEND lv_value TO rt_result.
    ENDDO.
    IF lv_carry > 0.
      APPEND lv_carry TO rt_result.
    ENDIF.
    trim( CHANGING ct_limbs = rt_result ).
  ENDMETHOD.

  METHOD sub.
    DATA lv_b TYPE i.
    DATA lv_value TYPE i.
    DATA lv_borrow TYPE i.
    FIELD-SYMBOLS <lv_a> TYPE i.

    LOOP AT it_a ASSIGNING <lv_a>.
      CLEAR lv_b.
      READ TABLE it_b INDEX sy-tabix INTO lv_b.
      lv_value = <lv_a> - lv_b - lv_borrow.
      IF lv_value < 0.
        lv_value = lv_value + c_base.
        lv_borrow = 1.
      ELSE.
        lv_borrow = 0.
      ENDIF.
      APPEND lv_value TO rt_result.
    ENDLOOP.
    trim( CHANGING ct_limbs = rt_result ).
  ENDMETHOD.

  METHOD mul.
    DATA lv_a TYPE i.
    DATA lv_index TYPE i.
    DATA lv_value TYPE i.
    DATA lv_carry TYPE i.
    FIELD-SYMBOLS <lv_b> TYPE i.
    FIELD-SYMBOLS <lv_t> TYPE i.

    DO lines( it_a ) + lines( it_b ) TIMES.
      APPEND 0 TO rt_result.
    ENDDO.

    LOOP AT it_a INTO lv_a.
      lv_index = sy-tabix.
      IF lv_a = 0.
        CONTINUE.
      ENDIF.
      lv_carry = 0.
      LOOP AT it_b ASSIGNING <lv_b>.
        READ TABLE rt_result INDEX lv_index ASSIGNING <lv_t>.
        lv_value = <lv_t> + lv_a * <lv_b> + lv_carry.
        <lv_t> = lv_value MOD c_base.
        lv_carry = lv_value DIV c_base.
        lv_index = lv_index + 1.
      ENDLOOP.
      READ TABLE rt_result INDEX lv_index ASSIGNING <lv_t>.
      <lv_t> = lv_carry.
    ENDLOOP.

    trim( CHANGING ct_limbs = rt_result ).
  ENDMETHOD.

  METHOD mod.
* bitwise long division, used outside the exponentiation hot path only
    DATA lt_m TYPE ty_limbs.
    DATA lv_index TYPE i.
    DATA lv_limb TYPE i.
    DATA lv_pow TYPE i.
    DATA lv_carry TYPE i.
    DATA lv_value TYPE i.
    FIELD-SYMBOLS <lv_r> TYPE i.

    lt_m = it_m.
    trim( CHANGING ct_limbs = lt_m ).

    lv_index = lines( it_a ).
    WHILE lv_index > 0.
      READ TABLE it_a INDEX lv_index INTO lv_limb.
      lv_pow = c_base DIV 2.
      WHILE lv_pow > 0.
        lv_carry = ( lv_limb DIV lv_pow ) MOD 2.
        LOOP AT rt_result ASSIGNING <lv_r>.
          lv_value = <lv_r> * 2 + lv_carry.
          lv_carry = lv_value DIV c_base.
          <lv_r> = lv_value - lv_carry * c_base.
        ENDLOOP.
        IF lv_carry > 0.
          APPEND lv_carry TO rt_result.
        ENDIF.
        IF lines( rt_result ) >= lines( lt_m ) AND compare( it_a = rt_result it_b = lt_m ) >= 0.
          rt_result = sub( it_a = rt_result it_b = lt_m ).
        ENDIF.
        lv_pow = lv_pow DIV 2.
      ENDWHILE.
      lv_index = lv_index - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD create_mont.
    DATA lv_n0 TYPE i.
    DATA lv_inv TYPE i.
    DATA lv_tmp TYPE i.
    DATA lt_r2 TYPE ty_limbs.

    rs_mont-n = it_n.
    trim( CHANGING ct_limbs = rs_mont-n ).
    rs_mont-s = lines( rs_mont-n ).

    READ TABLE rs_mont-n INDEX 1 INTO lv_n0.
    IF rs_mont-s = 0 OR lv_n0 MOD 2 = 0.
      zcx_open_abap_pdf=>raise( 'RSA: modulus must be odd' ).
    ENDIF.

* Newton iteration for n0^-1 mod 2^15, each step doubles the number of correct bits
    lv_inv = 1.
    DO 5 TIMES.
      lv_tmp = ( lv_n0 * lv_inv ) MOD c_base.
      lv_tmp = ( 2 - lv_tmp ) MOD c_base.
      lv_inv = ( lv_inv * lv_tmp ) MOD c_base.
    ENDDO.
    rs_mont-n0inv = ( c_base - lv_inv ) MOD c_base.

* R^2 mod n, R = base^s
    DO 2 * rs_mont-s TIMES.
      APPEND 0 TO lt_r2.
    ENDDO.
    APPEND 1 TO lt_r2.
    rs_mont-rr = pad( it_limbs = mod( it_a = lt_r2 it_m = rs_mont-n ) iv_count = rs_mont-s ).
  ENDMETHOD.

  METHOD mont_mul.
    DATA lt_t TYPE ty_limbs.
    DATA lv_a TYPE i.
    DATA lv_i TYPE i.
    DATA lv_index TYPE i.
    DATA lv_m TYPE i.
    DATA lv_value TYPE i.
    DATA lv_carry TYPE i.
    FIELD-SYMBOLS <lv_b> TYPE i.
    FIELD-SYMBOLS <lv_n> TYPE i.
    FIELD-SYMBOLS <lv_t> TYPE i.

    DO 2 * is_mont-s + 1 TIMES.
      APPEND 0 TO lt_t.
    ENDDO.

* t = a * b
    LOOP AT it_a INTO lv_a.
      lv_index = sy-tabix.
      IF lv_a = 0.
        CONTINUE.
      ENDIF.
      lv_carry = 0.
      LOOP AT it_b ASSIGNING <lv_b>.
        READ TABLE lt_t INDEX lv_index ASSIGNING <lv_t>.
        lv_value = <lv_t> + lv_a * <lv_b> + lv_carry.
        <lv_t> = lv_value MOD c_base.
        lv_carry = lv_value DIV c_base.
        lv_index = lv_index + 1.
      ENDLOOP.
      READ TABLE lt_t INDEX lv_index ASSIGNING <lv_t>.
      <lv_t> = lv_carry.
    ENDLOOP.

* reduction, t = ( t + m * n ) / R
    lv_i = 1.
    WHILE lv_i <= is_mont-s.
      READ TABLE lt_t INDEX lv_i ASSIGNING <lv_t>.
      lv_m = ( <lv_t> * is_mont-n0inv ) MOD c_base.
      IF lv_m <> 0.
        lv_carry = 0.
        lv_index = lv_i.
        LOOP AT is_mont-n ASSIGNING <lv_n>.
          READ TABLE lt_t INDEX lv_index ASSIGNING <lv_t>.
          lv_value = <lv_t> + lv_m * <lv_n> + lv_carry.
          <lv_t> = lv_value MOD c_base.
          lv_carry = lv_value DIV c_base.
          lv_index = lv_index + 1.
        ENDLOOP.
        WHILE lv_carry > 0.
          READ TABLE lt_t INDEX lv_index ASSIGNING <lv_t>.
          lv_value = <lv_t> + lv_carry.
          <lv_t> = lv_value MOD c_base.
          lv_carry = lv_value DIV c_base.
          lv_index = lv_index + 1.
        ENDWHILE.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    DELETE lt_t FROM 1 TO is_mont-s.
    IF compare( it_a = lt_t it_b = is_mont-n ) >= 0.
      lt_t = sub( it_a = lt_t it_b = is_mont-n ).
    ENDIF.
    rt_result = pad( it_limbs = lt_t iv_count = is_mont-s ).
    IF lines( rt_result ) > is_mont-s.
      DELETE rt_result FROM is_mont-s + 1.
    ENDIF.
  ENDMETHOD.

  METHOD mont_pow.
    DATA lt_table TYPE ty_limbs_tab.
    DATA lt_one TYPE ty_limbs.
    DATA lt_base TYPE ty_limbs.
    DATA lt_entry TYPE ty_limbs.
    DATA lv_offset TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_int TYPE i.
    DATA lv_nibble TYPE i.
    DATA lv_started TYPE abap_bool.

    lt_one = pad( it_limbs = VALUE #( ( 1 ) ) iv_count = is_mont-s ).
    lt_base = mont_mul( it_a = it_base it_b = is_mont-rr is_mont = is_mont ).

* fixed window of 4 bits, table index k + 1 holds base^k in Montgomery form
    lt_entry = mont_mul( it_a = lt_one it_b = is_mont-rr is_mont = is_mont ).
    APPEND lt_entry TO lt_table.
    DO 15 TIMES.
      lt_entry = mont_mul( it_a = lt_entry it_b = lt_base is_mont = is_mont ).
      APPEND lt_entry TO lt_table.
    ENDDO.

    READ TABLE lt_table INDEX 1 INTO rt_result.
    WHILE lv_offset < xstrlen( iv_exponent ).
      lv_byte = iv_exponent+lv_offset(1).
      lv_int = lv_byte.
      DO 2 TIMES.
        IF sy-index = 1.
          lv_nibble = lv_int DIV 16.
        ELSE.
          lv_nibble = lv_int MOD 16.
        ENDIF.
        IF lv_started = abap_true.
          DO 4 TIMES.
            rt_result = mont_mul( it_a = rt_result it_b = rt_result is_mont = is_mont ).
          ENDDO.
        ENDIF.
        IF lv_nibble > 0.
          READ TABLE lt_table INDEX lv_nibble + 1 INTO lt_entry.
          rt_result = mont_mul( it_a = rt_result it_b = lt_entry is_mont = is_mont ).
          lv_started = abap_true.
        ENDIF.
      ENDDO.
      lv_offset = lv_offset + 1.
    ENDWHILE.

    rt_result = mont_mul( it_a = rt_result it_b = lt_one is_mont = is_mont ).
    trim( CHANGING ct_limbs = rt_result ).
  ENDMETHOD.

ENDCLASS.
