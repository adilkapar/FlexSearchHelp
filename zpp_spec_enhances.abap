*$*$-Start: (1)---------------------------------------------------------------------------------$*$*
ENHANCEMENT 1  ZPP_SPEC_ENHANCES.    "active version

  " Access XVBAP from the calling program SAPMV45A to get plant
  FIELD-SYMBOLS: <lt_xvbap> TYPE ANY TABLE,
                 <ls_xvbap> TYPE any,
                 <lv_plant>  TYPE any.

  DATA: lv_plant TYPE werks_d.

  " Try direct work area first (current item context)
  ASSIGN ('(SAPMV45A)VBAP-WERKS') TO <lv_plant>.
  IF sy-subrc = 0 AND <lv_plant> IS NOT INITIAL.
    lv_plant = <lv_plant>.
  ELSE.
    " Fallback: access the global table XVBAP in SAPMV45A
    ASSIGN ('(SAPMV45A)XVBAP[]') TO <lt_xvbap>.
    IF sy-subrc = 0.
      LOOP AT <lt_xvbap> ASSIGNING <ls_xvbap>.
        ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_xvbap> TO <lv_plant>.
        IF sy-subrc = 0 AND <lv_plant> IS NOT INITIAL.
          lv_plant = <lv_plant>.
        ENDIF.
        EXIT. " Take first entry
      ENDLOOP.
    ENDIF.
  ENDIF.

  " Decide which search help to call based on plant
  DATA lv_shlp TYPE shlp_descr.

  IF lv_plant = '3000'.
    " Plant 3000: Call new flexible search help
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'ZSH_SPEC_FLEX'
        shlptype = 'SH'
      IMPORTING
        shlp     = lv_shlp.

    READ TABLE lv_shlp-interface ASSIGNING FIELD-SYMBOL(<specid>) WITH KEY shlpfield = 'SPECID'.
    IF <specid> IS ASSIGNED.
      <specid>-valtabname = 'VBAP'.
      <specid>-valfield   = 'KDMAT'.
    ENDIF.

    " IMPORTANT: Call the search help exit directly so it can build dynamic fields
    " The framework won't call ZPP_SPECT_SH automatically since we're already inside an exit
    CALL FUNCTION 'ZPP_SPECT_SH'
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
      CHANGING
        shlp        = lv_shlp
        callcontrol = callcontrol.

    " Now lv_shlp has the dynamically built fielddescr and fieldprop
    CLEAR shlp_tab[].
    APPEND lv_shlp TO shlp_tab.
    shlp = lv_shlp.
    EXIT.

  ELSE.
    " Plant 2000 or others: use existing rigid search help
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'ZSH_SPEC_RIGID'
        shlptype = 'SH'
      IMPORTING
        shlp     = lv_shlp.

    READ TABLE lv_shlp-interface ASSIGNING <specid> WITH KEY shlpfield = 'SPECID'.
    IF <specid> IS ASSIGNED.
      <specid>-valtabname = 'VBAP'.
      <specid>-valfield   = 'KDMAT'.
    ENDIF.

    CLEAR shlp_tab[].
    APPEND lv_shlp TO shlp_tab.
    shlp = lv_shlp.
    EXIT.

  ENDIF.

ENDENHANCEMENT.
*$*$-End:   (1)---------------------------------------------------------------------------------$*$*