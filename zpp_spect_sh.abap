FUNCTION zpp_spect_sh.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: lv_offset     TYPE i,
        lv_intlen     TYPE i,
        lv_pos        TYPE i,
        lv_field_len  TYPE i,
        lv_comp_name  TYPE fieldname,
        lv_curr_spec  TYPE zspec_flex_i-specid,
        lv_delete     TYPE abap_bool,
        lv_string_val TYPE c LENGTH 40,
        lv_check_val  TYPE c LENGTH 40,
        lv_tabix      TYPE sy-tabix,
        ls_record     TYPE seahlpres.

  " Only process SELONE and SELECT steps
  IF callcontrol-step <> 'SELONE' AND callcontrol-step <> 'SELECT'.
    EXIT.
  ENDIF.

  " ======================================================================
  " STEP 1: SELONE - DYNAMICALLY BUILD THE SEARCH HELP UI
  " ======================================================================
  IF callcontrol-step = 'SELONE'.

    " Read the field mapping configuration
    SELECT logical_field, description, is_selection, is_output
      FROM zspec_flex_cmap
      WHERE is_selection = 'X' OR is_output = 'X'
      ORDER BY sort_order
      INTO TABLE @DATA(lt_cmap_sel).

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    " Clear existing field descriptors and properties to start fresh
    CLEAR: shlp-fielddescr, shlp-fieldprop.

    lv_pos    = 1.
    lv_offset = 0.

    " Calculate field length to fit within SEAHLPRES-STRING (1000 chars)
    " +1 for the SPECID field we always add first
    DATA(lv_num_fields) = lines( lt_cmap_sel ) + 1.
    lv_field_len = 40.
    IF lv_num_fields * lv_field_len > 1000.
      lv_field_len = 1000 / lv_num_fields.
    ENDIF.
    IF lv_field_len < 10.
      lv_field_len = 10.
    ENDIF.

    lv_intlen = lv_field_len * cl_abap_char_utilities=>charsize.

    " --- Always add SPECID as FIRST field (selection + output) ---
    APPEND INITIAL LINE TO shlp-fielddescr ASSIGNING FIELD-SYMBOL(<fs_desc>).
    <fs_desc>-fieldname  = 'SPECID'.
    <fs_desc>-position   = lv_pos.
    <fs_desc>-offset     = lv_offset.
    <fs_desc>-intlen     = lv_intlen.
    <fs_desc>-leng       = lv_field_len.
    <fs_desc>-outputlen  = lv_field_len.
    <fs_desc>-inttype    = 'C'.
    <fs_desc>-datatype   = 'CHAR'.
    <fs_desc>-domname    = 'CHAR40'.
    <fs_desc>-rollname   = 'CHAR40'.
    <fs_desc>-reptext    = 'Spec ID'.
    <fs_desc>-scrtext_l  = 'Spec ID'.
    <fs_desc>-scrtext_m  = 'Spec ID'.
    <fs_desc>-scrtext_s  = 'SpecID'.

    APPEND INITIAL LINE TO shlp-fieldprop ASSIGNING FIELD-SYMBOL(<fs_prop>).
    <fs_prop>-fieldname  = 'SPECID'.
    <fs_prop>-shlpinput  = 'X'.
    <fs_prop>-shlpoutput = 'X'.
    <fs_prop>-shlpselpos = lv_pos.
    <fs_prop>-shlplispos = lv_pos.

    lv_offset = lv_offset + lv_intlen.
    lv_pos    = lv_pos + 1.

    " --- Now add the rest of the fields from config table ---
    LOOP AT lt_cmap_sel INTO DATA(ls_cmap_sel).
      " Normalize the field name
      TRANSLATE ls_cmap_sel-logical_field TO UPPER CASE.
      REPLACE ALL OCCURRENCES OF ` ` IN ls_cmap_sel-logical_field WITH `_`.

      " Skip if SPECID is in config table (we already added it above)
      IF ls_cmap_sel-logical_field = 'SPECID'.
        CONTINUE.
      ENDIF.

      " --- Build field descriptor ---
      APPEND INITIAL LINE TO shlp-fielddescr ASSIGNING <fs_desc>.
      <fs_desc>-fieldname  = ls_cmap_sel-logical_field.
      <fs_desc>-position   = lv_pos.
      <fs_desc>-offset     = lv_offset.
      <fs_desc>-intlen     = lv_intlen.
      <fs_desc>-leng       = lv_field_len.
      <fs_desc>-outputlen  = lv_field_len.
      <fs_desc>-inttype    = 'C'.
      <fs_desc>-datatype   = 'CHAR'.
      <fs_desc>-domname    = 'CHAR40'.
      <fs_desc>-rollname   = 'CHAR40'.
      <fs_desc>-reptext    = ls_cmap_sel-description.
      <fs_desc>-scrtext_l  = ls_cmap_sel-description.
      <fs_desc>-scrtext_m  = ls_cmap_sel-description.
      <fs_desc>-scrtext_s  = ls_cmap_sel-description.

      " --- Build field property ---
      APPEND INITIAL LINE TO shlp-fieldprop ASSIGNING <fs_prop>.
      <fs_prop>-fieldname  = ls_cmap_sel-logical_field.
      <fs_prop>-shlpinput  = ls_cmap_sel-is_selection.
      <fs_prop>-shlpoutput = ls_cmap_sel-is_output.
      <fs_prop>-shlpselpos = COND #( WHEN ls_cmap_sel-is_selection = 'X' THEN lv_pos ELSE 0 ).
      <fs_prop>-shlplispos = COND #( WHEN ls_cmap_sel-is_output = 'X' THEN lv_pos ELSE 0 ).

      " Advance offset and position
      lv_offset = lv_offset + lv_intlen.
      lv_pos    = lv_pos + 1.
    ENDLOOP.

    " Also update shlp_tab for consistency
    READ TABLE shlp_tab INTO DATA(ls_shlp) INDEX 1.
    IF sy-subrc = 0.
      ls_shlp-fielddescr = shlp-fielddescr.
      ls_shlp-fieldprop  = shlp-fieldprop.
      MODIFY shlp_tab FROM ls_shlp INDEX 1.
    ENDIF.

    EXIT.
  ENDIF.

  " ======================================================================
  " STEP 2: SELECT - FETCH, PIVOT, FILTER AND MAP THE DATA
  " ======================================================================
  IF callcontrol-step = 'SELECT'.

    " 1. Fetch raw row-based data
    SELECT * FROM zspec_flex_i INTO TABLE @DATA(lt_flex).
    IF sy-subrc <> 0.
      callcontrol-step = 'DISP'.
      EXIT.
    ENDIF.
    SORT lt_flex BY specid.

    " 2. Prepare filter ranges from user selection criteria
    TYPES: BEGIN OF ty_selopt,
             sign   TYPE ddsign,
             option TYPE ddoption,
             low    TYPE char70,
             high   TYPE char70,
           END OF ty_selopt,
           tt_selopt TYPE TABLE OF ty_selopt WITH EMPTY KEY.

    DATA: BEGIN OF ls_filter,
            fieldname TYPE fieldname,
            ranges    TYPE tt_selopt,
          END OF ls_filter,
          lt_filters LIKE TABLE OF ls_filter WITH EMPTY KEY.

    LOOP AT shlp-selopt INTO DATA(ls_sel).
      CHECK ls_sel-low IS NOT INITIAL OR ls_sel-high IS NOT INITIAL.
      READ TABLE lt_filters ASSIGNING FIELD-SYMBOL(<fs_filter>)
        WITH KEY fieldname = ls_sel-shlpfield.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_filters ASSIGNING <fs_filter>.
        <fs_filter>-fieldname = ls_sel-shlpfield.
      ENDIF.
      APPEND INITIAL LINE TO <fs_filter>-ranges ASSIGNING FIELD-SYMBOL(<fs_range>).
      <fs_range>-sign   = ls_sel-sign.
      <fs_range>-option = ls_sel-option.
      <fs_range>-low    = ls_sel-low.
      <fs_range>-high   = ls_sel-high.
    ENDLOOP.

    " 3. Build a lookup table: fieldname -> char offset and length within record string
    TYPES: BEGIN OF ty_field_map,
             fieldname TYPE fieldname,
             char_off  TYPE i,
             char_len  TYPE i,
           END OF ty_field_map.
    DATA: lt_field_map TYPE TABLE OF ty_field_map,
          ls_map_work  TYPE ty_field_map.

    DATA(lv_rec_len) = 0.

    LOOP AT shlp-fielddescr INTO DATA(ls_fdesc).
      APPEND INITIAL LINE TO lt_field_map ASSIGNING FIELD-SYMBOL(<fs_map>).
      <fs_map>-fieldname = ls_fdesc-fieldname.
      <fs_map>-char_off  = ls_fdesc-offset / cl_abap_char_utilities=>charsize.
      <fs_map>-char_len  = ls_fdesc-leng.
      DATA(lv_end) = <fs_map>-char_off + <fs_map>-char_len.
      IF lv_end > lv_rec_len.
        lv_rec_len = lv_end.
      ENDIF.
    ENDLOOP.

    " Safety check: seahlpres-string is typically 1000 characters
    IF lv_rec_len > 1000.
      callcontrol-step = 'DISP'.
      EXIT.
    ENDIF.

    " 4. Pivot row-based data into flat record strings
    DATA: lt_result TYPE TABLE OF seahlpres.
    DATA: ls_result TYPE seahlpres.

    CLEAR: lv_curr_spec, ls_result.

    LOOP AT lt_flex INTO DATA(ls_flex_row).
      IF lv_curr_spec <> ls_flex_row-specid.
        " Save previous record if exists
        IF lv_curr_spec IS NOT INITIAL.
          APPEND ls_result TO lt_result.
        ENDIF.
        CLEAR ls_result.
        lv_curr_spec = ls_flex_row-specid.

        " Write SPECID into its position in the record string
        READ TABLE lt_field_map INTO ls_map_work
          WITH KEY fieldname = 'SPECID'.
        IF sy-subrc = 0.
          lv_string_val = lv_curr_spec.
          IF ls_map_work-char_off + ls_map_work-char_len <= 1000.
            ls_result-string+ls_map_work-char_off(ls_map_work-char_len) = lv_string_val.
          ENDIF.
        ENDIF.
      ENDIF.

      " Normalize attribute_id to match field name
      lv_comp_name = ls_flex_row-attribute_id.
      TRANSLATE lv_comp_name TO UPPER CASE.
      REPLACE ALL OCCURRENCES OF ` ` IN lv_comp_name WITH `_`.

      " Write the attribute value at the correct position
      READ TABLE lt_field_map INTO ls_map_work
        WITH KEY fieldname = lv_comp_name.
      IF sy-subrc = 0.
        lv_string_val = ls_flex_row-attribute_value.
        IF ls_map_work-char_off + ls_map_work-char_len <= 1000.
          ls_result-string+ls_map_work-char_off(ls_map_work-char_len) = lv_string_val.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Don't forget the last record
    IF lv_curr_spec IS NOT INITIAL.
      APPEND ls_result TO lt_result.
    ENDIF.

    " 5. Apply User Filters
    LOOP AT lt_result INTO ls_result.
      lv_tabix = sy-tabix.
      lv_delete = abap_false.

      LOOP AT lt_filters INTO ls_filter.
        READ TABLE lt_field_map INTO ls_map_work
          WITH KEY fieldname = ls_filter-fieldname.
        IF sy-subrc = 0.
          IF ls_map_work-char_off + ls_map_work-char_len <= 1000.
            lv_check_val = ls_result-string+ls_map_work-char_off(ls_map_work-char_len).
            IF lv_check_val NOT IN ls_filter-ranges.
              lv_delete = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF lv_delete = abap_true.
        DELETE lt_result INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    " 6. Push filtered results to record_tab
    LOOP AT lt_result INTO ls_result.
      APPEND ls_result TO record_tab.
    ENDLOOP.

    " Bypass standard DB selection, go to display
    callcontrol-step = 'DISP'.
    EXIT.

  ENDIF.

ENDFUNCTION.